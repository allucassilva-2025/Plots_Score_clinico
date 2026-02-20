# ========= TESTE COMPLETO (círculo “cheio” + sem sobreposição) =========
library(dplyr)
library(ggplot2)
library(ggimage)

set.seed(129)

# --- 1) Parâmetros ---
n <- 1000
n_pos <- 5
n_feito <- 20

img_geral <- "human_white_96_tight.png"
img_feito <- "human_pcr_96_tight.png"
img_pos   <- "human_pos_96_tight.png"

# --- 2) Gera malha hexagonal no círculo com espaçamento máximo possível (sem “amassar”) ---
circle_coords_hex <- function(n = 1000, R = 1, it = 25) {
  dx0 <- sqrt((pi * R^2) / n)
  lo <- dx0 * 0.7
  hi <- dx0 * 1.6
  best <- NULL
  
  for (i in 1:it) {
    dx <- (lo + hi) / 2
    dy <- dx * sqrt(3) / 2
    ys <- seq(-R, R, by = dy)
    
    pts <- bind_rows(lapply(seq_along(ys), function(k) {
      y <- ys[k]
      shift <- ifelse(k %% 2 == 0, dx/2, 0)
      xs <- seq(-R, R, by = dx) + shift
      tibble(x = xs, y = y)
    })) %>%
      filter(x^2 + y^2 <= R^2) %>%
      mutate(d = sqrt(x^2 + y^2))
    
    if (nrow(pts) >= n) {
      best <- pts
      lo <- dx    # tenta aumentar espaçamento
    } else {
      hi <- dx    # precisa diminuir espaçamento
    }
  }
  
  pts <- best %>%
    arrange(d) %>%
    slice_head(n = n)
  
  # reescala pra encostar no raio (círculo “completo”)
  s <- R / max(pts$d)
  transmute(pts, x = x * s, y = y * s)
}

df <- circle_coords_hex(n = n, R = 1)

# --- 3) Define status EXATO e mapeia para imagens ---
status <- sample(c(
  rep("PCR+", n_pos),
  rep("PCR feito", n_feito - n_pos),
  rep("Geral", n - n_feito)
))

df <- df %>%
  mutate(
    status = status,
    image = case_when(
      status == "PCR+"      ~ img_pos,
      status == "PCR feito" ~ img_feito,
      TRUE                  ~ img_geral
    )
  )

p <- ggplot(df, aes(x, y)) +
  geom_image(aes(image = image), size = 0.02) +
  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

print(p)


ggsave("PCRxchik.png", plot = p, width = 8, height = 8, dpi = 300)
