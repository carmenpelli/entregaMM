# ============================================================
# Radio de giro (Rg) - Tripéptido ARA (298 K y 400 K)
# ============================================================

# Cargar librerías necesarias
library(ggplot2)

# ------------------------------------------------------------
# 1. Lectura de datos
# ------------------------------------------------------------

# Leer archivo sin cabecera generado por GROMACS (-xvg none)
data <- read.table("gyrate.xvg", header = FALSE)

# Asignar nombres a las columnas
colnames(data) <- c("Time_ps", "Rg_nm")

# ------------------------------------------------------------
# 2. Cálculo de estadísticos
# ------------------------------------------------------------

# Media del radio de giro
mean_rg <- mean(data$Rg_nm)

# Desviación estándar
sd_rg <- sd(data$Rg_nm)

# ------------------------------------------------------------
# 3. Representación gráfica
# ------------------------------------------------------------

plot_rg <- ggplot(data, aes(x = Time_ps, y = Rg_nm)) +
  
  # Curva principal
  geom_line(size = 0.6) +
  
  # Línea de la media
  geom_hline(yintercept = mean_rg, linetype = "dashed") +
  
  # Etiquetas
  labs(
    title = "Radio de giro del tripéptido ARA a 298 K", # o 400 K
    x = "Tiempo (ps)",
    y = "Radio de giro (nm)"
  ) +
  
  # Estética limpia
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Mostrar gráfico
print(plot_rg)

# ------------------------------------------------------------
# 4. Guardado de la figura
# ------------------------------------------------------------

ggsave(
  filename = "gyrate_298.png", # o gyrate_400.png
  plot = plot_rg,
  width = 6,
  height = 4,
  dpi = 300
)

# ------------------------------------------------------------
# 5. Mostrar resultados numéricos en consola
# ------------------------------------------------------------

cat("Radio de giro medio:", round(mean_rg, 4), "nm\n")
cat("Desviación estándar:", round(sd_rg, 4), "nm\n")
