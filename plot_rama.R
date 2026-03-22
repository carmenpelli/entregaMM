# ============================================================
# Análisis diédrico completo - ALA2
# φ, ψ y diagrama de Ramachandran
# ============================================================

rm(list = ls())

library(ggplot2)
library(patchwork)

# ------------------------------------------------------------
# 1. Lectura de datos
# ------------------------------------------------------------

data <- read.table("~/MM/entregaMM/4-analysisARA/rama_298_ala.dat", header = FALSE)
colnames(data) <- c("frame", "phi", "psi")

data$time_ps <- seq(0, length.out = nrow(data), by = 0.001)

# ------------------------------------------------------------
# 2. φ vs tiempo
# ------------------------------------------------------------

plot_phi <- ggplot(data, aes(x = time_ps, y = phi)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = expression("Evolución temporal de " * phi),
    x = "Tiempo (ps)",
    y = "Ángulo (grados)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3. ψ vs tiempo
# ------------------------------------------------------------

plot_psi <- ggplot(data, aes(x = time_ps, y = psi)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = expression("Evolución temporal de " * psi),
    x = "Tiempo (ps)",
    y = "Ángulo (grados)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 4. Ramachandran (densidad)
# ------------------------------------------------------------

plot_rama <- ggplot(data, aes(x = phi, y = psi)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(name = "Frecuencia") +
  labs(
    title = "Mapa de densidad de conformaciones (Ramachandran)",
    x = expression(phi~"(°)"),
    y = expression(psi~"(°)")
  ) +
  xlim(-180, 180) +
  ylim(-180, 180) +
  theme_minimal()

# ------------------------------------------------------------
# 5. Composición final
# ------------------------------------------------------------

final_plot <- (plot_phi | plot_psi) / plot_rama

print(final_plot)

# ------------------------------------------------------------
# 6. Guardado
# ------------------------------------------------------------

ggsave("rama_298_full.png", final_plot, width = 8, height = 6, dpi = 300)
