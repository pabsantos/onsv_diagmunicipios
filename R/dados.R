fix_cidades <- function(lista) {
  lista %>% 
    select(Nome_UF, `Código Município Completo`, `Nome_Município`) %>% 
    rename(CODIGO = `Código Município Completo`, NOME = `Nome_Município`,
           UF = Nome_UF) %>% 
    mutate(CODIGO = str_sub(CODIGO,1,6)) %>% 
    janitor::clean_names()
}

load_mortes <- function() {
  if (file.exists("input/datasus.RDS")) {
    mortes <- readRDS("input/datasus.RDS")
  } else {
    mortes <- fetch_datasus(
      year_start = 2018,
      year_end = 2020,
      information_system = "SIM-DOEXT",
      vars = c("CAUSABAS", "CODMUNOCOR", "DTOBITO")
    )
    saveRDS(mortes, "input/datasus.RDS")
  }
  return(mortes)
}

clean_mortes_data <- function(mortes) {
   mortes %>% 
    janitor::clean_names() %>% 
    mutate(
      causa_maior = str_sub(causabas, 1, 2),
      ano = lubridate::year(lubridate::dmy(dtobito)),
      tipo = case_when(
        causa_maior == "V0" ~ "pedestre",
        causa_maior == "V1" ~ "ciclista",
        causa_maior %in% c("V2", "V3") ~ "motociclista",
        TRUE ~ "outros"
      )) %>% 
    filter(causa_maior %in% paste0("V", seq(0, 8, 1)))
}

calc_mortes_indicadores <- function(mortes, ano) {
  mortes <- mortes %>% 
    filter(ano == {{ ano }}) %>% 
    group_by(codmunocor, tipo) %>% 
    summarise(mortes = n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = tipo, values_from = mortes, values_fill = 0) %>% 
    mutate(
      total = outros + ciclista + motociclista + pedestre,
      a3 = pedestre / total,
      a4 = ciclista / total,
      a5 = motociclista / total
    ) %>% 
    select(codmunocor, total:a5)
  
  if (ano == "2018") {
    mortes <- mortes %>% 
      rename(total_2018 = total, a3_2018 = a3, a4_2018 = a4, a5_2018 = a5)
  } else if (ano == "2019") {
    mortes <- mortes %>% 
      rename(total_2019 = total, a3_2019 = a3, a4_2019 = a4, a5_2019 = a5)    
  } else {
    mortes <- mortes %>% 
      rename(total_2020 = total, a3_2020 = a3, a4_2020 = a4, a5_2020 = a5)
  }
  return(mortes)
}

calc_mortes_total <- function(municipios, mortes) {
  municipios %>% 
    janitor::clean_names() %>% 
    select(codigo) %>% 
    left_join(mortes[[1]], by = c("codigo" = "codmunocor")) %>% 
    left_join(mortes[[2]], by = c("codigo" = "codmunocor")) %>% 
    left_join(mortes[[3]], by = c("codigo" = "codmunocor")) %>% 
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>% 
    rowwise() %>% 
    mutate(
      a3 = mean(c(a3_2018, a3_2019, a3_2020)),
      a4 = mean(c(a4_2018, a4_2019, a4_2020)),
      a5 = mean(c(a5_2018, a5_2019, a5_2020))
    ) %>% 
    select(codigo, a3, a4, a5, total_2018, total_2019, total_2020)
}

fix_pib <- function(pib_data) {
  pib_data %>% 
    select(codigo, pib_mil) %>% 
    mutate(codigo = str_sub(codigo, 1, 6))
}

load_frota <- function() {
  files <- list.files("input", pattern = "^frota")
  path <- paste0("input/", files)
  frota <- map(path, readxl::read_xls)
  return(frota)
}

edit_frota <- function(df, ano) {
  if (ano == "2020") {
    df %>% 
      rename(veic_2020 = TOTAL) %>% 
      mutate(B2 = (MOTOCICLETA + MOTONETA + CICLOMOTOR + TRICICLO) / 
               veic_2020) %>% 
      select(codigo, veic_2020, B2)
  } else if (ano == "2019") {
    df %>% 
      rename(veic_2019 = TOTAL) %>% 
      select(codigo, veic_2019)
  } else {
    df %>% 
      rename(veic_2018 = TOTAL) %>% 
      select(codigo, veic_2018)
  }
}

calc_idade_frota <- function(idade_frota) {
  idade_media <- idade_frota %>% 
    janitor::clean_names() %>% 
    filter(!ano_fabricacao_crv %in% c(
      "Sem Informação", "Não se Aplica", "Não Identificado"
    )) %>% 
    mutate(ano_fabricacao_crv = as.numeric(ano_fabricacao_crv)) %>% 
    filter(ano_fabricacao_crv > 1918) %>% 
    mutate(
      idade = 2020 - ano_fabricacao_crv,
      idade_mult = idade * qtd_veiculos
    ) %>% 
    group_by(uf, municipio) %>% 
    summarise(
      total_veiculos = sum(qtd_veiculos),
      idade_mult = sum(idade_mult)
    ) %>% 
    mutate(idade_media = idade_mult / total_veiculos) %>% 
    filter(uf != "Sem Informação") %>% 
    ungroup()
  
  idade_frota <- bind_cols(frota_calculados %>% select(codigo), idade_media) %>% 
    select(codigo, idade_media)
  
  return(idade_frota)
}

fix_idh <- function(idh) {
  idh %>% 
    filter(ANO == 2010) %>% 
    select(Codmun6, IDHM) %>% 
    rename(codigo = Codmun6) %>% 
    mutate(codigo = as.character(codigo))
}

fix_prof <- function(profissionais) {
  profissionais %>% 
    separate(
      Município,
      into = c("codigo", "municipio"),
      sep = " ",
      extra = "merge"
    ) %>% 
    rename(profissionais = Total) %>% 
    select(codigo, profissionais)
}

fix_leitos <- function(leitos) {
  leitos %>% 
    separate(
      Município,
      into = c("codigo", "municipio"),
      sep = " ",
      extra = "merge"
    ) %>% 
    rename(leitos = Quantidade_existente) %>% 
    select(-municipio)
}

join_pop <- function(pop_antiga, pop_nova) {
  pop_antiga <- pop_antiga %>% 
    janitor::clean_names() %>% 
    separate(municipio, into = c("codigo", NA), sep = " ") %>% 
    select(codigo, pop_2018 = x2018, pop_2019 = x2019)
  
  pop_nova <- pop_nova %>% 
    janitor::clean_names() %>% 
    mutate(
      codigo = str_sub(paste0(as.character(cod_uf), cod_munic), 1, 6),
      populacao_estimada = as.numeric(populacao_estimada)
    ) %>% 
    select(codigo, pop_2020 = populacao_estimada)
  
  pop <- pop_antiga %>% 
    left_join(pop_nova, by = c("codigo"))
  
  return(pop)
}

join_all <- function(cidades) {
  cidades %>% 
    left_join(mortes_total, by = "codigo") %>% 
    left_join(frota_calculados, by = "codigo") %>% 
    left_join(idade_frota, by = "codigo") %>% 
    left_join(idh, by = "codigo") %>% 
    left_join(leitos, by = "codigo") %>% 
    left_join(pib, by = "codigo") %>% 
    left_join(pop, by = "codigo") %>% 
    left_join(profissionais, by = "codigo") %>% 
    left_join(snt, by = "codigo")
}

calc_indicadores <- function(cidades_dados) {
  cidades_dados %>%
    mutate(
      a1_2018 = total_2018 / pop_2018 * 100000,
      a1_2019 = total_2019 / pop_2019 * 100000,
      a1_2020 = total_2020 / pop_2020 * 100000,
      a2_2018 = total_2018 / veic_2018 * 10000,
      a2_2019 = total_2019 / veic_2019 * 10000,
      a2_2020 = total_2020 / veic_2020 * 10000,
      media_mortes = (total_2018 + total_2019 + total_2020) / 3,
      a1 = (a1_2018 + a1_2019 + a1_2020) / 3,
      a2 = (a2_2018 + a2_2019 + a2_2020) / 3,
      b1 = idade_media, 
      b3 = leitos / pop_2020 * 100000,
      b4 = profissionais / pop_2020 * 100000,
      c1 = veic_2020 / pop_2020 * 1000,
      c2 = pib_mil * 1000 / pop_2020,
      c3 = IDHM,
      porte = case_when(
        pop_2020 < 20000 ~ "Menor porte",
        pop_2020 > 100000 ~ "Maior porte",
        TRUE ~ "Médio porte"
      ),
      regiao = case_when(
        str_sub(codigo, 1, 1) == "1" ~ "Norte",
        str_sub(codigo, 1, 1) == "2" ~ "Nordeste",
        str_sub(codigo, 1, 1) == "3" ~ "Sudeste",
        str_sub(codigo, 1, 1) == "4" ~ "Sul",
        TRUE ~ "Centro-Oeste"
      )
    ) %>% 
    select(
      codigo, regiao, uf, nome, porte, snt, a1, a2, a3, a4, a5, b1, b2 = B2, 
      b3, b4, c1, c2, c3, media_mortes
    ) %>% 
    replace(is.na(.), 0)
}