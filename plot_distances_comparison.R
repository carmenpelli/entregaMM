# ============================================================
# Distancias interatómicas del tripéptido ARA
# Comparación entre 298 K y 400 K
# Pares analizados:
#   [CO-ala-1]   -> 15 16
#   [CACB-arg-3] -> 19 21
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 1. Librerías
# ------------------------------------------------------------
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 2. Función para leer archivos de GROMACS (-xvg none)
# ------------------------------------------------------------
read_distance_file <- function(file, pair_label, temperature_label) {
  data <- read.table(file, header = FALSE)
  colnames(data) <- c("Time_ps", "Distance_nm")
  data$Pair <- pair_label
  data$Temperature <- temperature_label
  return(data)
}

# ------------------------------------------------------------
# 3. Lectura de datos
# ------------------------------------------------------------
data_co_298 <- read_distance_file(
  file = "~/MM/entregaMM/4-analysisARA/dist_CO_298.xvg",
  pair_label = "CO-ala-1 (15–16)",
  temperature_label = "298 K"
)

data_co_400 <- read_distance_file(
  file = "~/MM/entregaMM/4-analysisARA400/dist_CO_400.xvg",
  pair_label = "CO-ala-1 (15–16)",
  temperature_label = "400 K"
)

data_cacb_298 <- read_distance_file(
  file = "~/MM/entregaMM/4-analysisARA/dist_CACB_298.xvg",
  pair_label = "CACB-arg-3 (19–21)",
  temperature_label = "298 K"
)

data_cacb_400 <- read_distance_file(
  file = "~/MM/entregaMM/4-analysisARA400/dist_CACB_400.xvg",
  pair_label = "CACB-arg-3 (19–21)",
  temperature_label = "400 K"
)

# Unir todos los datos
data_all <- bind_rows(
  data_co_298,
  data_co_400,
  data_cacb_298,
  data_cacb_400
)

# ------------------------------------------------------------
# 4. Estadísticos descriptivos
# ------------------------------------------------------------
stats_all <- data_all %>%
  group_by(Pair, Temperature) %>%
  summarise(
    Mean_nm = mean(Distance_nm),
    SD_nm   = sd(Distance_nm),
    Min_nm  = min(Distance_nm),
    Max_nm  = max(Distance_nm),
    .groups = "drop"
  )

print(stats_all)

# ------------------------------------------------------------
# 5. Figura
# ------------------------------------------------------------
plot_distances <- ggplot(
  data_all,
  aes(x = Time_ps, y = Distance_nm, color = Temperature)
) +
  geom_line(linewidth = 0.6) +
  geom_hline(
    data = stats_all,
    aes(yintercept = Mean_nm, color = Temperature),
    linetype = "dashed",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Pair, ncol = 1, scales = "free_y") +
  labs(
    title = "Distancias interatómicas del tripéptido ARA",
    x = "Tiempo (ps)",
    y = "Distancia (nm)",
    color = "Temperatura"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(plot_distances)

# ------------------------------------------------------------
# 6. Guardado de la figura
# ------------------------------------------------------------
ggsave(
  filename = "distancias_comparacion.png",
  plot = plot_distances,
  width = 7,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 7. Guardado de estadísticos
# ------------------------------------------------------------
write.table(
  stats_all,
  file = "distancias_estadisticos.txt",
  quote = FALSE,
  sep = "\t",
  row.names = FALSE
)

cat("\nFigura guardada como: distancias_comparacion.png\n")
cat("Estadísticos guardados como: distancias_estadisticos.txt\n")

