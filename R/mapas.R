join_mun <- function(mun, indicadores) {
  mun %>% 
    mutate(codigo = str_sub(as.character(CD_GEOCMU), 1, 6)) %>% 
    left_join(indicadores, by = "codigo")
}

plot_uf_map <- function(uf) {
  uf_map <- tm_shape(uf, bbox = br_bbox) +
    tm_borders(lwd = 0.3, col = "grey20")
}  

plot_indicadores <- function(col, breaks, palette, title, labels) {
  tm_shape(br_mun, bbox = br_bbox) +  
    tm_fill(
      col = col,
      style = "fixed",
      breaks = breaks,
      palette = palette,
      colorNA = "white",
      showNA = FALSE,
      legend.show = FALSE
    ) +
    tm_add_legend(
      title = title,
      labels = labels,
      type = "fill",
      col = palette,
      border.lwd = 0
    ) +
    tm_layout(frame = FALSE) +
    uf_map
}

indicadores <- c(
  "a1", "a2", "a3", "a4", "a5", "b1", "b2", "b3", "b4", "c1", "c2", "c3"
)

ind_palettes <- list(
  c("#8fd861", "#c0dd6c", "#f2e376", "#ffce73", "#ffb06a", "#ff9362"),
  c("#8fd861", "#c0dd6c", "#f2e376", "#ffce73", "#ffb06a", "#ff9362"),
  c("#8fd861", "#c0dd6c", "#f2e376", "#ffce73", "#ffb06a", "#ff9362"),
  c("#8fd861", "#c0dd6c", "#f2e376", "#ffce73", "#ffb06a", "#ff9362"),
  c("#8fd861", "#c0dd6c", "#f2e376", "#ffce73", "#ffb06a", "#ff9362"),
  c("#8fd861", "#cddf6e", "#ffdd77", "#ffb86c", "#ff9362"),
  c("#fff8c4", "#fedb81", "#feaa38", "#ec7114", "#b74202"),
  c("#ff9362", "#ffb86c", "#ffdd77", "#cddf6e", "#8fd861"),
  c("#ff9362", "#ffb86c", "#ffdd77", "#cddf6e", "#8fd861"),
  c("#fff8c4", "#fedb81", "#feaa38", "#ec7114", "#b74202"),
  c("#fff8c4", "#fedb81", "#feaa38", "#ec7114", "#b74202"),
  c("#ff9362", "#ffb86c", "#ffdd77", "#cddf6e", "#8fd861")
)

ind_titles <- c(
  "Mortes por\n100 mil habitantes:",
  "Mortes por\n10 mil veículos:",
  "Porcentagem de\nmortes de pedestres:",
  "Porcentagem de\nmortes de ciclistas:",
  "Porcentagem de\nmortes de motociclistas:",
  "Idade média",
  "Porcentagem de\nmotocicletas:",
  "Leitos por\n100 mil habitantes:",
  "Profissionais por\n100 mil habitantes:",
  "Veículos por\nmil habitantes:",
  "PIB per capita:",
  "IDHM:"
)

ind_labels <- list(
  c("Sem mortes", "0 a 10", "10 a 20", "20 a 30", "30 a 40", "Acima de 40"),
  c("Sem mortes", "0 a 5", "5 a 10", "10 a 15", "15 a 20", "Acima de 20"),
  c(
    "Sem mortes", "0 a 10%", "10% a 20%", "20% a 30%",
    "30% a 40%", 'Acima de 40%'
  ),
  c(
    "Sem mortes", "0 a 5%", "5% a 10%", "10% a 15%",
    "15% a 20%", "Acima de 20%"
  ),
  c(
    "Sem mortes", "0 a 15%", "15% a 30%",
    "30% a 45%", "45% a 60%", "Acima de 60%"
  ),
  c(
    "5 a 10 anos", "10 a 15 anos", "15 a 20 anos",
    "20 a 25 anos", "Acima de 25 anos"
  ),
  c("5% a 25%", "25% a 35%", "35% a 50%", "50% a 65%", "65% a 95%"),
  c("Sem leitos", "0 a 50", "50 a 150", "150 a 250", "Acima de 250"),
  c("Até 800", "800 a 1000", "1000 a 1200", "1200 a 1500", "Acima de 1500"),
  c("Até 200", "200 a 400", "400 a 550", "550 a 750", "Acima de 750"),
  c(
    "Até R$5.000,00", "R$5.000,00 a R$10.000,00", "R$10.000,00 a R$20.000,00",
    "R$20.000,00 a R$40.000,00", "Acima de R$40.000,00"
  ),
  c(
    "Até 0,499 (muito baixo)", "0,500 a 0,599 (baixo)", "0,600 a 0,699 (médio)",
    "0,700 a 0,799 (alto)", "Acima de 0,800 (muito alto)"
  )
)