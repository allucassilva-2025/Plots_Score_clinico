library(dplyr)
library(ggplot2)
library(patchwork)

SINTOMAS <- c("MIALGIA","FEBRE","CEFALEIA","DOR_COSTAS")

# rótulos bonitos
rotulos_sintomas <- c(
  "MIALGIA"    = "Mialgia",
  "FEBRE"      = "Febre",
  "CEFALEIA"   = "Cefaleia",
  "DOR_COSTAS" = "Dor nas costas"
)

# nomes bonitos das doenças
rotulos_doencas <- c(
  "Dengue"      = "Dengue",
  "Chikungunya" = "Chikungunya"
)

# paleta Okabe-Ito (colorblind-friendly)
cores_cb <- c(
  "Dengue"      = "#0072B2",
  "Chikungunya" = "#D55E00"
)

# filtra só PCR+
deng_p <- deng %>% filter(RESUL_PCR_ %in% c(1, "1"))
chik_p <- chik %>% filter(RESUL_PCR_ %in% c(1, "1"))

pct_sintoma <- function(df, var){
  100 * mean(df[[var]] == 1, na.rm = TRUE)
}

tab <- bind_rows(lapply(SINTOMAS, function(s) {
  tibble(
    sintoma = s,
    doenca  = factor(c("Dengue", "Chikungunya"), levels = c("Dengue", "Chikungunya")),
    pct     = c(pct_sintoma(deng_p, s), pct_sintoma(chik_p, s))
  )
}))

plot_sintoma <- function(nome_sintoma){
  ggplot(filter(tab, sintoma == nome_sintoma),
         aes(x = doenca, y = pct, fill = doenca)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.3, size = 4) +
    scale_fill_manual(
      values = cores_cb,
      breaks = names(rotulos_doencas),
      labels = rotulos_doencas,
      name = "Doença"
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      title = rotulos_sintomas[[nome_sintoma]],
      x = NULL,
      y = "Proporção de casos (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

p1 <- plot_sintoma("MIALGIA")
p2 <- plot_sintoma("FEBRE")
p3 <- plot_sintoma("CEFALEIA")
p4 <- plot_sintoma("DOR_COSTAS")

((p1 | p2) / (p3 | p4)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparação da proporção de sintomas entre dengue e chikungunya (casos PCR positivos)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  ) &
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )




