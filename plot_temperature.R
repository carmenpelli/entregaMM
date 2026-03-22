# ============================================================
# Temperatura del sistema - ARA (298 K vs 400 K)
# Figura comparativa en dos paneles
# ============================================================

rm(list = ls())

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 1. Lectura de datos
# ------------------------------------------------------------

temp_298 <- read.table("~/MM/entregaMM/4-analysisARA/temp_298.xvg", header = FALSE)
temp_400 <- read.table("~/MM/entregaMM/4-analysisARA400/temp_400.xvg", header = FALSE)

colnames(temp_298) <- c("Time_ps", "Temperature_K")
colnames(temp_400) <- c("Time_ps", "Temperature_K")

temp_298$Simulation <- "298 K"
temp_400$Simulation <- "400 K"

data_all <- bind_rows(temp_298, temp_400)

# ------------------------------------------------------------
# 2. Estadísticos
# ------------------------------------------------------------

stats_all <- data_all %>%
  group_by(Simulation) %>%
  summarise(
    Mean_K = mean(Temperature_K),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3. Figura
# ------------------------------------------------------------

plot_temp <- ggplot(data_all, aes(x = Time_ps, y = Temperature_K, color = Simulation)) +
  
  geom_line(linewidth = 0.6) +
  
  # Línea media en gris
  geom_hline(
    data = stats_all,
    aes(yintercept = Mean_K),
    color = "grey40",
    linetype = "dashed",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  facet_wrap(~ Simulation, ncol = 1, scales = "free_y") +
  
  scale_color_manual(values = c("298 K" = "#E64B35", "400 K" = "#4DBBD5")) +
  
  labs(
    title = "Evolución temporal de la temperatura",
    x = "Tiempo (ps)",
    y = "Temperatura (K)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(plot_temp)

# ------------------------------------------------------------
# 4. Guardado
# ------------------------------------------------------------

ggsave(
  "temperatura_comparacion.png",
  plot = plot_temp,
  width = 7,
  height = 6,
  dpi = 300
)

