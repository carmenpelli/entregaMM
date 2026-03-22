# ============================================================
# Distribución de velocidades de 5 átomos
# A partir de un archivo veloc.xvg generado por GROMACS
# Formato esperado:
# tiempo  vx1 vy1 vz1 |v|1  vx2 vy2 vz2 |v|2 ...
# ============================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Lectura de datos
# ------------------------------------------------------------

vel <- read.table("veloc.xvg", header = FALSE)

ncols <- ncol(vel)
n_atoms <- (ncols - 1) / 4

if ((ncols - 1) %% 4 != 0) {
  stop("El archivo no tiene el formato esperado: tiempo + bloques de 4 columnas por átomo.")
}

cat("Número de átomos detectados en veloc.xvg:", n_atoms, "\n")

# ------------------------------------------------------------
# 2. Selección de 5 átomos
# ------------------------------------------------------------
# Cambia estos índices si quieres elegir otros átomos.
# Deben estar entre 1 y n_atoms.

atom_indices <- c(1, 2, 3, 4, 5)

# Etiquetas para la figura
# Puedes sustituirlas por nombres reales si los conoces.
atom_labels <- c("Átomo 1", "Átomo 2", "Átomo 3", "Átomo 4", "Átomo 5")

if (length(atom_indices) != 5 || length(atom_labels) != 5) {
  stop("Debes proporcionar exactamente 5 índices de átomo y 5 etiquetas.")
}

# ------------------------------------------------------------
# 3. Extraer vx, vy, vz para los 5 átomos
# ------------------------------------------------------------

extract_atom_components <- function(data, atom_index, atom_label) {
  # En R:
  # col 1 = tiempo
  # para el átomo i:
  # vx = 2 + (i-1)*4
  # vy = 3 + (i-1)*4
  # vz = 4 + (i-1)*4
  # |v| = 5 + (i-1)*4
  
  vx_col <- 2 + (atom_index - 1) * 4
  vy_col <- 3 + (atom_index - 1) * 4
  vz_col <- 4 + (atom_index - 1) * 4
  
  df <- data.frame(
    Atom = atom_label,
    vx = data[[vx_col]],
    vy = data[[vy_col]],
    vz = data[[vz_col]]
  )
  
  df_long <- df %>%
    pivot_longer(cols = c(vx, vy, vz),
                 names_to = "Component",
                 values_to = "Velocity")
  
  return(df_long)
}

vel_list <- lapply(seq_along(atom_indices), function(i) {
  extract_atom_components(vel, atom_indices[i], atom_labels[i])
})

vel_long <- bind_rows(vel_list)

# Etiquetas bonitas para facetas/leyenda interna
vel_long$Component <- factor(
  vel_long$Component,
  levels = c("vx", "vy", "vz"),
  labels = c("Velocidad X (v_x)", "Velocidad Y (v_y)", "Velocidad Z (v_z)")
)

# ------------------------------------------------------------
# 4. Figura
# ------------------------------------------------------------

p <- ggplot(vel_long, aes(x = Velocity, fill = Component)) +
  geom_histogram(bins = 40, color = NA, alpha = 0.75) +
  facet_grid(Atom ~ Component, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Velocidad X (v_x)" = "#66BD63",
      "Velocidad Y (v_y)" = "#5E72E4",
      "Velocidad Z (v_z)" = "#F8766D"
    )
  ) +
  labs(
    title = "Distribución de velocidades por átomo",
    x = "Velocidad (nm/ps)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

print(p)

# ------------------------------------------------------------
# 5. Guardado
# ------------------------------------------------------------

ggsave(
  filename = "velocidades_5atomos400.png",
  plot = p,
  width = 9,
  height = 10,
  dpi = 300
)
