# ============================================================
# Distancias interatómicas - ARA (298 K vs 400 K)
# ============================================================

rm(list = ls())
library(ggplot2)

# ------------------------------------------------------------
# 1. Lectura de datos
# ------------------------------------------------------------

data_298 <- read.table("~/MM/entregaMM/4-analysisARA/dist.xvg", header = FALSE)
data_400 <- read.table("~/MM/entregaMM/4-analysisARA400/dist.xvg", header = FALSE)

colnames(data_298) <- c("Time_ps", "Distance_nm")
colnames(data_400) <- c("Time_ps", "Distance_nm")

data_298$Temp <- "298 K"
data_400$Temp <- "400 K"

data_all <- rbind(data_298, data_400)

# ------------------------------------------------------------
# 2. Estadísticos
# ------------------------------------------------------------

mean_298 <- mean(data_298$Distance_nm)
sd_298   <- sd(data_298$Distance_nm)

mean_400 <- mean(data_400$Distance_nm)
sd_400   <- sd(data_400$Distance_nm)

# ------------------------------------------------------------
# 3. Gráfica
# ------------------------------------------------------------

plot_dist <- ggplot(data_all, aes(x = Time_ps, y = Distance_nm, color = Temp)) +
  
  geom_line(linewidth = 0.7) +
  
  geom_hline(yintercept = mean_298, linetype = "dashed") +
  geom_hline(yintercept = mean_400, linetype = "dotted") +
  
  labs(
    title = "Distancia interatómica del tripéptido ARA",
    x = "Tiempo (ps)",
    y = "Distancia (nm)",
    color = "Temperatura"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(plot_dist)

# ------------------------------------------------------------
# 4. Guardado
# ------------------------------------------------------------

ggsave("dist_comparacion.png", plot = plot_dist, width = 6, height = 4, dpi = 300)

# ------------------------------------------------------------
# 5. Resultados numéricos
# ------------------------------------------------------------

cat("298 K -> media:", round(mean_298,4), "sd:", round(sd_298,4), "\n")
cat("400 K -> media:", round(mean_400,4), "sd:", round(sd_400,4), "\n")