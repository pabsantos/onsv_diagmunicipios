transform_indicadores <- function(indicadores) {
  indicadores %>% 
    rename(ibge_cod = codigo) %>% 
    filter(media_mortes != 0) %>% 
    mutate(
      across(starts_with(c("a", "b", "c")), ~scale(.x) %>% as.numeric())
    ) %>% 
    filter(
      across(starts_with(c("a", "b", "c")), ~.x < 3),
      across(starts_with(c("a", "b", "c")), ~.x > -3)
    )
}


plot_corr <- function(data) {
  GGally::ggcorr(
    data %>% select(starts_with(c("a", "b", "c"))),
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_size = 3,
    label_round = 2
  )
}

calc_pca <- function(var, mun_porte, data) {
  data %>% 
    filter(porte == mun_porte) %>% 
    select(starts_with(var)) %>% 
    prcomp(center = FALSE, scale. = FALSE)
}

plot_pca <- function(pca) {
  fviz_pca(
    pca,
    geom = c("point"), 
    repel = TRUE, 
    col.var = "red",
    col.ind = "midnightblue",
    alpha.ind = 0.1,
    title = ""
  ) + 
    coord_equal() +
    scale_x_continuous(minor_breaks = FALSE) +
    scale_y_continuous(minor_breaks = FALSE) +
    theme_bw(base_size = 8)
}

# 
# save_pca <- function(plot, var, porte) {
#   name <- paste0("output/pca_", var, "_", porte, ".png")
#   ggsave(name, plot = plot, device = "png", width = 3.5, height = 3.5)
# }
# 
# pmap(list(pca_plots, pca_groups$var_groups, pca_groups$porte_groups), save_pca)

extract_pca <- function(var, mun_porte, pca) {
  df <- municipios_analise %>% 
    filter(porte == mun_porte) %>% 
    select(ibge_cod) %>% 
    bind_cols(as_tibble(pca$x[,1:2]))
  
  if (var == "a") {
    df %>% rename(a_pc1 = PC1, a_pc2 = PC2)
  } else if (var == "b") {
    df %>% rename(b_pc1 = PC1, b_pc2 = PC2)
  } else {
    df %>% rename(c_pc1 = PC1, c_pc2 = PC2)
  }
}

calc_kmeans <- function(var, mun_porte, df) {
  df1 <- df %>% 
    filter(porte == mun_porte)
  
  if (var == "a") {
    df1 <- df1 %>% 
      select(a_pc1, a_pc2)
    kmeans <- kmeans(df1, 3)
    k <- tibble(cluster_a = kmeans$cluster)
  } else if (var == "b") {
    df1 <- df1 %>% 
      select(b_pc1, b_pc2)
    kmeans <- kmeans(df1, 3)
    k <- tibble(cluster_b = kmeans$cluster)
  } else {
    df1 <- df1 %>% 
      select(c_pc1, c_pc2)
    kmeans <- kmeans(df1, 3)
    k <- tibble(cluster_c = kmeans$cluster)
  }
  
  df2 <- df %>% 
    filter(porte == mun_porte) %>% 
    select(ibge_cod) %>% 
    bind_cols(k)
  
  return(df2)
}

plot_clusters <- function(var, porte_mun, df) {
  df1 <- df %>% 
    filter(porte == porte_mun)
  
  if (var == "a") {
    plot <- ggplot(df1, aes(x = a_pc1, y = a_pc2, color = cluster_a))
  } else if (var == "b") {
    plot <- ggplot(df1, aes(x = b_pc1, y = b_pc2, color = cluster_b))
  } else {
    plot <- ggplot(df1, aes(x = c_pc1, y = c_pc2, color = cluster_c))
  }
  
  plot +
    geom_point(alpha = 0.5) +
    coord_equal() +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_brewer(palette = "Set2") +
    labs(x = "PC1", y = "PC2", color = "Clusters:") +
    theme_bw(base_size = 8)
}