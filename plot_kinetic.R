# ============================================================
# Energía cinética - comparación 298 K vs 400 K
# ============================================================

rm(list = ls())
library(ggplot2)
library(dplyr)

k298 <- read.table("~/MM/entregaMM/4-analysisARA/kinetic_298.xvg", header = FALSE)
k400 <- read.table("~/MM/entregaMM/4-analysisARA400/kinetic_400.xvg", header = FALSE)

colnames(k298) <- c("Time_ps", "Energy")
colnames(k400) <- c("Time_ps", "Energy")

k298$Simulation <- "298 K"
k400$Simulation <- "400 K"

data_all <- bind_rows(k298, k400)

stats <- data_all %>%
  group_by(Simulation) %>%
  summarise(mean_E = mean(Energy), .groups = "drop")

p <- ggplot(data_all, aes(Time_ps, Energy, color = Simulation)) +
  geom_line(linewidth = 0.6) +
  geom_hline(data = stats, aes(yintercept = mean_E),
             linetype = "dashed", color = "grey40") +
  facet_wrap(~Simulation, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("298 K" = "#E64B35", "400 K" = "#4DBBD5")) +
  labs(title = "Energía cinética del sistema",
       x = "Tiempo (ps)", y = "Energía (kJ/mol)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("energia_cinetica.png", p, width = 7, height = 6, dpi = 300)

print(stats)
