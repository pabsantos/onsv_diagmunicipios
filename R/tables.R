extract_summary <- function(indicadores, mun_porte, grupo) {
  indicadores <- indicadores %>% 
    left_join(ind_originais, by = "ibge_cod") %>% 
    filter(porte == mun_porte)
  if (grupo == "a") {
    indicadores <- indicadores %>% 
      select(original_a1:original_a5, cluster = cluster_a)
  } else if (grupo == "b") {
    indicadores <- indicadores %>% 
      select(original_b1:original_b4, cluster = cluster_b)
  } else {
    indicadores <- indicadores %>% 
      select(original_c1:original_c3, cluster = cluster_c)
  }
  indicadores <- indicadores %>% 
    pivot_longer(-cluster, names_to = "indicador", values_to = "n") %>% 
    group_by(indicador, cluster) %>% 
    summarise(
      mean = mean(n),
      median = median(n),
      sd = sd(n),
      q1 = quantile(n, 0.25),
      q3 = quantile(n, 0.75)
    ) %>% 
    mutate(cluster = case_when(
      cluster == 1 ~ "Cluster 1",
      cluster == 2 ~ "Cluster 2",
      TRUE ~ "Cluster 3"
    ))
  if (grupo == "a") {
    indicadores <- indicadores %>% 
      mutate(indicador = case_when(
        indicador == "original_a1" ~ "Indicador A1",
        indicador == "original_a2" ~ "Indicador A2",
        indicador == "original_a3" ~ "Indicador A3",
        indicador == "original_a4" ~ "Indicador A4",
        TRUE ~ "Indicador A5"
      ))
  } else if (grupo == "b") {
    indicadores <- indicadores %>% 
      mutate(indicador = case_when(
        indicador == "original_b1" ~ "Indicador B1",
        indicador == "original_b2" ~ "Indicador B2",
        indicador == "original_b3" ~ "Indicador B3",
        TRUE ~ "Indicador B4"
      ))
  } else {
    indicadores <- indicadores %>% 
      mutate(indicador = case_when(
        indicador == "original_c1" ~ "Indicador C1",
        indicador == "original_c2" ~ "Indicador C2",
        TRUE ~ "Indicador C3"
      ))
  }
  return(indicadores)
}

create_summary_table <- function(summary_table) {
  summary_table %>% 
    gt(rowname_col = "cluster") %>% 
    fmt_number(columns = mean:q3, decimals = 2) %>%
    cols_label(
      cluster = "Cluster",
      mean = "Média",
      median = "Mediana",
      sd = "Desvio Padrão",
      q1 = "1º Quartil",
      q3 = "3º Quartil"
    ) %>% 
    cols_move(columns = median, after = sd)
}

extract_ranking <- function(mun_porte, grupo, levels, order, indicadores) {
  indicadores <- indicadores %>% 
    left_join(ind_originais, by = "ibge_cod") %>% 
    filter(porte == mun_porte)
  if (grupo == "a") {
    indicadores <- indicadores %>% 
      filter(cluster_a == levels) %>% 
      select(nome, uf, regiao, original_a1:original_a5, pc1 = a_pc1)
  } else if (grupo == "b") {
    indicadores <- indicadores %>% 
      filter(cluster_b == levels) %>% 
      select(nome, uf, regiao, original_b1:original_b4, pc1 = b_pc1)
  } else {
    indicadores <- indicadores %>% 
      filter(cluster_c == levels) %>% 
      select(nome, uf, regiao, original_c1:original_c3, pc1 = c_pc1)
  }
  if (order == "grande") {
    indicadores <- indicadores %>% 
      arrange(-pc1)
  } else {
    indicadores <- indicadores %>% 
      arrange(pc1)
  }
  indicadores %>% 
    slice_head(n = 20)
}

create_ranking_tables <- function(ranking_data, grupo) {
  ranking_data <- ranking_data %>% 
    gt() %>% 
    cols_label(
      nome = "Município",
      uf = "UF",
      regiao = "Região",
      pc1 = "PC1"
    )
  if (grupo == "a") {
    ranking_data %>% 
      fmt_number(columns = original_a1:pc1, decimals = 2) %>% 
      cols_label(
        original_a1 = "A1",
        original_a2 = "A2",
        original_a3 = "A3",
        original_a4 = "A4",
        original_a5 = "A5"
      )
  } else if (grupo == "b") {
    ranking_data %>% 
      fmt_number(columns = original_b1:pc1, decimals = 2) %>% 
      cols_label(
        original_b1 = "B1",
        original_b2 = "B2",
        original_b3 = "B3",
        original_b4 = "B4"
      )
  } else {
    ranking_data %>% 
      fmt_number(columns = original_c1:pc1, decimals = 2) %>% 
      cols_label(
        original_c1 = "C1",
        original_c2 = "C2",
        original_c3 = "C3"
      )
  }
}

extract_prioridade <- function(municipios, prioridade) {
  municipios %>% 
    mutate(across(starts_with("cluster"), ~as.double(.x))) %>% 
    left_join(
      prioridade,
      by = c("porte", "cluster_a", "cluster_b", "cluster_c")
    ) %>% 
    group_by(prioridade, porte) %>% 
    summarise(quantidade = n())
}