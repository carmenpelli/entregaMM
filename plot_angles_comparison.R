# ============================================================
# Ángulos de enlace del tripéptido ARA
# Comparación entre 298 K y 400 K
# Tríos analizados:
#   [CA-C-O-ala-2]    -> 9 15 16
#   [CA-CB-CG-arg-3]  -> 19 21 24
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
read_angle_file <- function(file, angle_label, temperature_label) {
  data <- read.table(file, header = FALSE)
  colnames(data) <- c("Time_ps", "Angle_deg")
  data$Angle <- angle_label
  data$Temperature <- temperature_label
  return(data)
}

# ------------------------------------------------------------
# 3. Lectura de datos
# ------------------------------------------------------------
data_caco_298 <- read_angle_file(
  file = "~/MM/entregaMM/4-analysisARA/ang_CACO_298.xvg",
  angle_label = "CA-C-O-ala-2 (9–15–16)",
  temperature_label = "298 K"
)

data_caco_400 <- read_angle_file(
  file = "~/MM/entregaMM/4-analysisARA400/ang_CACO_400.xvg",
  angle_label = "CA-C-O-ala-2 (9–15–16)",
  temperature_label = "400 K"
)

data_cacbcg_298 <- read_angle_file(
  file = "~/MM/entregaMM/4-analysisARA/ang_CACBCG_298.xvg",
  angle_label = "CA-CB-CG-arg-3 (19–21–24)",
  temperature_label = "298 K"
)

data_cacbcg_400 <- read_angle_file(
  file = "~/MM/entregaMM/4-analysisARA400/ang_CACBCG_400.xvg",
  angle_label = "CA-CB-CG-arg-3 (19–21–24)",
  temperature_label = "400 K"
)

# Unir todos los datos
data_all <- bind_rows(
  data_caco_298,
  data_caco_400,
  data_cacbcg_298,
  data_cacbcg_400
)

# ------------------------------------------------------------
# 4. Estadísticos descriptivos
# ------------------------------------------------------------
stats_all <- data_all %>%
  group_by(Angle, Temperature) %>%
  summarise(
    Mean_deg = mean(Angle_deg),
    SD_deg   = sd(Angle_deg),
    Min_deg  = min(Angle_deg),
    Max_deg  = max(Angle_deg),
    .groups = "drop"
  )

print(stats_all)

# ------------------------------------------------------------
# 5. Figura
# ------------------------------------------------------------
plot_angles <- ggplot(
  data_all,
  aes(x = Time_ps, y = Angle_deg, color = Temperature)
) +
  geom_line(linewidth = 0.6) +
  geom_hline(
    data = stats_all,
    aes(yintercept = Mean_deg, color = Temperature),
    linetype = "dashed",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Angle, ncol = 1, scales = "free_y") +
  labs(
    title = "Ángulos de enlace del tripéptido ARA",
    x = "Tiempo (ps)",
    y = "Ángulo (grados)",
    color = "Temperatura"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(plot_angles)

# ------------------------------------------------------------
# 6. Guardado de la figura
# ------------------------------------------------------------
ggsave(
  filename = "angulos_comparacion.png",
  plot = plot_angles,
  width = 7,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 7. Guardado de estadísticos
# ------------------------------------------------------------
write.table(
  stats_all,
  file = "angulos_estadisticos.txt",
  quote = FALSE,
  sep = "\t",
  row.names = FALSE
)

cat("\nFigura guardada como: angulos_comparacion.png\n")
cat("Estadísticos guardados como: angulos_estadisticos.txt\n")

