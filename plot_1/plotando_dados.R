n_total     <- nrow(deng)
n_pcr_feito <- sum(!is.na(deng$DT_PCR))
n_pcr_pos   <- sum(deng$RESUL_PCR_ %in% c("1", 1), na.rm = TRUE)

pct_pcr_feito <- n_pcr_feito / n_total
pct_pcr_pos   <- n_pcr_pos   / n_total

plot_1000_bonecos(n_total = 1000, n_pcr_pos = 29, n_pcr_feito = 37, seed=123)