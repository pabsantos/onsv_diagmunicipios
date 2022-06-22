# Setup -------------------------------------------------------------------
library(microdatasus)
library(tmap)
library(sf)
library(factoextra)
library(gt)
library(tidyverse)

set.seed(1234)

source("R/dados.R")
source("R/mapas.R")
source("R/calc.R")
source("R/tables.R")

# Dados -------------------------------------------------------------------
lista_cidades <- readxl::read_xls("input/lista_municipios.xls")
mortes <- load_mortes()

pib <- read_csv(
  "input/pib2019.csv", 
  col_names = c("codigo", "municipio", "pib_mil"),
  col_types = c("c", "c", "d"), 
  skip = 1
)

frota <- load_frota()
idade_frota <- readxl::read_xlsx("input/idade_frota_2020.xlsx")
idh <- readxl::read_xlsx("input/atlas_idh.xlsx", sheet = "MUN 91-00-10")
snt <- readxl::read_xlsx(
  "input/municipalizacao2021.xlsx",
  col_types = c("text", "text"),
  col_names = c("codigo", "snt"),
  skip = 1
)

leitos <- read_csv2("input/leitos_2020.csv")
profissionais <- read_csv2("input/profissionais_2020.csv")
pop_antiga <- read_csv("input/populacao2019-8-7.csv")
pop_2020 <- readxl::read_excel("input/POP2020_20220419.xls")

br_mun <- st_read("input/BRMUE250GC_SIR.shp")
br_uf <- st_read("input/BRUFE250GC_SIR.shp")

cluster_levels <- read_csv("input/cluster_levels.csv")
prioridade <- read_csv("input/prioridade.csv")

# Construcao da tabela de indicadores -------------------------------------
lista_cidades <- fix_cidades(lista_cidades)

mortes_clean <- clean_mortes_data(mortes)
mortes_ind <- map(
  c("2018", "2019", "2020"), 
  ~calc_mortes_indicadores(ano = .x, mortes_clean)
)

mortes_total <- calc_mortes_total(lista_cidades, mortes_ind)

pib <- fix_pib(pib)

frota_calculados <- map2(frota, c("2018", "2019", "2020"), edit_frota) %>% 
  reduce(left_join)

idade_frota <- calc_idade_frota(idade_frota)
idh <- fix_idh(idh)
leitos <- leitos %>% fix_leitos()
profissionais <- profissionais %>% fix_prof()
pop <- join_pop(pop_antiga, pop_2020)

indicadores_municipais <- lista_cidades %>% join_all() %>% calc_indicadores()

# Mapas de indicadores ----------------------------------------------------
br_mun <- join_mun(mun = br_mun, indicadores = indicadores_municipais)

br_bbox <- st_bbox(br_mun)
br_bbox["xmax"] <- br_bbox["xmax"] - 3.5

uf_map <- br_uf %>% plot_uf_map()

breaks <- list(
  c(0, 0.5, 10, 20, 30, 40, max(indicadores_municipais$a1)),
  c(0, 0.15, 5, 10, 15, 20, max(indicadores_municipais$a2)),
  c(0, 0.01, 0.1, 0.2, 0.3, 0.4, max(indicadores_municipais$a3)),
  c(0, 0.004, 0.05, 0.10, 0.15, 0.20, max(indicadores_municipais$a4)),
  c(0, 0.01, 0.15, 0.30, 0.45, 0.60, max(indicadores_municipais$a5)),
  c(5, 10, 15, 20, 25, max(indicadores_municipais$b1)),
  c(0.05, 0.25, 0.35, 0.50, 0.65, max(indicadores_municipais$b2)),
  c(0, 3, 50, 150, 250, max(indicadores_municipais$b3)),
  c(0, 800, 1000, 1200, 1500, max(indicadores_municipais$b4)),
  c(0, 200, 400, 550, 750, max(indicadores_municipais$c1)),
  c(0, 5000, 10000, 20000, 40000, max(indicadores_municipais$c2)),
  c(0, 0.499, 0.599, 0.699, 0.799, max(indicadores_municipais$c3))
)

ind_maps <- pmap(
  list(indicadores, breaks, ind_palettes, ind_titles, ind_labels),
  plot_indicadores
)

# map2(
#   ind_maps,
#   paste0("output/", indicadores, ".png"),
#   ~tmap_save(tm = .x, filename = .y, width = 6, height = 6)
# )

# Analise estatistica -----------------------------------------------------
municipios_analise <- indicadores_municipais %>% transform_indicadores()
corr_plot <- municipios_analise %>% plot_corr()

ggsave("output/corr_plot.png", corr_plot, width = 5, height = 4, units = "in")

var_groups <- c("a", "b", "c")
porte_groups <- c("Menor porte", "Médio porte", "Maior porte")
pca_groups <- expand_grid(var_groups, porte_groups)

pca_results <- map2(
  pca_groups$var_groups,
  pca_groups$porte_groups,
  ~calc_pca(.x, .y, municipios_analise)
)

pca_plots <- map(pca_results, plot_pca)

ext_pca <- pmap(
  list(pca_groups$var_groups, pca_groups$porte_groups, pca_results),
  extract_pca
)

pca_a <- bind_rows(ext_pca[[1]], ext_pca[[2]], ext_pca[[3]])
pca_b <- bind_rows(ext_pca[[4]], ext_pca[[5]], ext_pca[[6]])
pca_c <- bind_rows(ext_pca[[7]], ext_pca[[8]], ext_pca[[9]])

municipios_analise <- municipios_analise %>% 
  left_join(pca_a) %>% 
  left_join(pca_b) %>% 
  left_join(pca_c)

kclusters <- map2(
  pca_groups$var_groups, pca_groups$porte_groups,
  calc_kmeans, df = municipios_analise
)

cluster_a <- bind_rows(kclusters[[1]], kclusters[[2]], kclusters[[3]])
cluster_b <- bind_rows(kclusters[[4]], kclusters[[5]], kclusters[[6]])
cluster_c <- bind_rows(kclusters[[7]], kclusters[[8]], kclusters[[9]])

municipios_analise <- municipios_analise %>% 
  left_join(cluster_a) %>% 
  left_join(cluster_b) %>% 
  left_join(cluster_c) %>% 
  mutate_at(vars(starts_with("cluster")), as.factor)

clusters_plot <- map2(
  pca_groups$var_groups, pca_groups$porte_groups, 
  plot_clusters, df = municipios_analise
)

map2(
  pca_plots,
  clusters_plot,
  ~ggpubr::ggarrange(.x, .y, ncol = 2, nrow = 1)
)

map2(
  paste0(
    "output/pca_", pca_groups$var_groups, "_", pca_groups$porte_groups, ".png"
  ),
  pca_plots,
  ~ggsave(.x, .y, device = "png", width = 5, height = 3.5)
)

map2(
  paste0(
    "output/cluster_", pca_groups$var_groups,
    "_", pca_groups$porte_groups, ".png"
  ),
  clusters_plot,
  ~ggsave(.x, .y, device = "png", width = 5, height = 3.5)
)

write_csv(municipios_analise, "output/diagnostico_municipios.csv")


# Tabelas do relatorio ----------------------------------------------------
ind_originais <- indicadores_municipais %>% 
  select(ibge_cod = codigo, a1:c3) %>% 
  rename_with(~str_c("original_", .), starts_with(c("a", "b", "c")))

summaries <- map2(
  pca_groups$porte_groups,
  pca_groups$var_groups,
  ~extract_summary(municipios_analise, .x, .y)
)

summary_tables <- map(summaries, create_summary_table)

map2(
  summary_tables,
  paste0(
    "output/summary_table_", pca_groups$porte_groups, "_"
    , pca_groups$var_groups, ".rtf"
  ),
  gtsave
)

levels <- c(cluster_levels$pior[1:6], cluster_levels$melhor[7:9])

ranking_municipios <- pmap(
  list(
    pca_groups$porte_groups,
    pca_groups$var_groups,
    levels,
    cluster_levels$ordem_pc
  ),
  extract_ranking, municipios_analise
)

ranking_tables <- map2(
  ranking_municipios,
  pca_groups$var_groups,
  create_ranking_tables
)

map2(
  ranking_tables,
  paste0(
    "output/ranking_table_", pca_groups$porte_groups, "_",
    pca_groups$var_groups, ".rtf"
  ),
  gtsave
)

prioridade_lista <- extract_prioridade(municipios_analise, prioridade)

prioridade_lista %>%
  ungroup() %>% 
  filter(prioridade < 10) %>% 
  gt() %>% 
  gtsave("output/prioridade_table.rtf")

# Tabelas finais ------------------------------------------------------------

final_table <- municipios_analise %>% 
  mutate(across(starts_with("cluster"), ~as.double(.x))) %>% 
  left_join(
    prioridade,
    by = c("porte", "cluster_a", "cluster_b", "cluster_c")
  ) %>% 
  left_join(ind_originais, by = "ibge_cod")


sem_mortes_table <- indicadores_municipais %>% 
  filter(media_mortes == 0)

outlier <- function(x) {
  x > 3 | x < -3
}

outliers_table <- indicadores_municipais %>% 
  rename(ibge_cod = codigo) %>% 
  filter(media_mortes != 0) %>% 
  mutate(across(starts_with(c("a", "b", "c")), ~scale(.x) %>% as.numeric())) %>% 
  filter(if_any(starts_with(c("a", "b", "c")), outlier))

write_csv2(final_table, "output/final_table.csv")
write_csv(sem_mortes_table, "output/sem_mortes_table.csv")
write_csv(outliers_table, "output/outliers_table.csv")
