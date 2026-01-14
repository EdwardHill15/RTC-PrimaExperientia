# =============================================================================
# R SIMULATION: Three Experiential Objects/Processes in STC Ontology
# 1. SOLID Object/Process (High Inertia αI=0.65)
# 2. FLUID Object/Process (High Cohesion βC=0.60) 
# 3. GASEOUS Process (High Radiation γR=0.65)
# Resonance Theory of Consciousness (RTC/STC) - Edward F. Hillenaar
# =============================================================================

# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

set.seed(42)
cat("=== SIMULATION: SOLID • FLUID • GASEOUS EXPERIENTIAL PROCESSES ===\n")

# Time vector for resonance cycles (3 seconds of experience)
t <- seq(0, 3, length.out = 1200)

# PRIMA EXPERIENTIA BASE WAVES (normalized [0,1])
I <- sin(2 * pi * 0.8 * t) * exp(-0.15 * t)           # INERTIA: low freq, persistent
C <- cos(2 * pi * 1.4 * t) * exp(-0.08 * t)          # COHESION: medium freq, continuous  
R <- sin(2 * pi * 2.8 * t + pi/3)                    # RADIATION: high freq, dispersive
V <- sin(2 * pi * 4.2 * t) * (1 + 0.3 * sin(2 * pi * 0.3 * t))  # VIBRATION: complex

normalize <- function(x) (x - min(x)) / (max(x) - min(x))
I_norm <- normalize(I); C_norm <- normalize(C) 
R_norm <- normalize(R); V_norm <- normalize(V)

# SIMULATION PARAMETERS: Three Experiential Regimes
experiential_regimes <- data.frame(
  Process = c("SOLID Object", "FLUID Process", "GASEOUS Process"),
  alpha_I = c(0.65, 0.15, 0.10),    # Inertia dominance
  beta_C  = c(0.10, 0.60, 0.15),    # Cohesion dominance
  gamma_R = c(0.15, 0.15, 0.65),    # Radiation dominance
  delta_V = c(0.10, 0.10, 0.10),    # Vibration baseline
  stringsAsFactors = FALSE
)

print("=== EXPERIENTIAL REGIMES ===")
print(experiential_regimes)

# COMPUTE MIND-EXPERIENCE M_e = αI + βC + γR + δV for each regime
M_e_solid    <- 0.65*I_norm + 0.10*C_norm + 0.15*R_norm + 0.10*V_norm
M_e_fluid    <- 0.15*I_norm + 0.60*C_norm + 0.15*R_norm + 0.10*V_norm  
M_e_gaseous  <- 0.10*I_norm + 0.15*C_norm + 0.65*R_norm + 0.10*V_norm

# SIMULATION DATA: Amplitude envelopes over time (experiential intensity)
experience_intensity <- data.frame(
  Time = rep(t, 4),
  Amplitude = c(M_e_solid, M_e_fluid, M_e_gaseous, 
                rowMeans(cbind(M_e_solid, M_e_fluid, M_e_gaseous))),  # Composite
  Type = rep(c("SOLID Object", "FLUID Process", "GASEOUS Process", "Composite"), 
             each = length(t))
)

# PLOT 1: EXPERIENTIAL PROCESSES SIMULATION
p1 <- ggplot(experience_intensity, aes(x = Time, y = Amplitude, color = Type)) +
  geom_line(linewidth = 2, alpha = 0.9) +
  labs(title = "Simulation: Three Experiential Objects/Processes",
       subtitle = "SOLID (αI=0.65) • FLUID (βC=0.60) • GASEOUS (γR=0.65)",
       x = "Experiential Time (seconds)", y = "Resonance Amplitude",
       caption = expression(M[e]==alpha~I + beta~C + gamma~R + delta~V)) +
  scale_color_manual(values = c("SOLID Object" = "#8B4513",      # Brown/orange
                                "FLUID Process" = "#4169E1",     # Royal blue  
                                "GASEOUS Process" = "#FF1493",    # Deep pink
                                "Composite" = "black")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(color = guide_legend(ncol = 2))

print(p1)

# PLOT 2: PRIMA EXPERIENTIA CONTRIBUTIONS TO EACH PROCESS
contrib_data <- data.frame(
  Time = rep(t, 12),
  Amplitude = c(
    # SOLID: dominant I
    0.65*I_norm, 0.10*C_norm, 0.15*R_norm, 0.10*V_norm,
    # FLUID: dominant C  
    0.15*I_norm, 0.60*C_norm, 0.15*R_norm, 0.10*V_norm,
    # GASEOUS: dominant R
    0.10*I_norm, 0.15*C_norm, 0.65*R_norm, 0.10*V_norm
  ),
  Process = rep(c("SOLID Object", "FLUID Process", "GASEOUS Process"), each = 4*length(t)),
  Component = rep(c("Inertia(I)", "Cohesion(C)", "Radiation(R)", "Vibration(V)"), 3*length(t))
)

p2 <- ggplot(contrib_data, aes(x = Time, y = Amplitude, color = Component)) +
  geom_line(linewidth = 1.1, alpha = 0.8) +
  facet_wrap(~Process, ncol = 3, scales = "free_y") +
  labs(title = "Prima Experientia Contributions by Experiential Type",
       x = "Time (s)", y = "Weighted Amplitude") +
  scale_color_brewer(type = "qual", palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

print(p2)

# SIMULATION STATISTICS
cat("\n=== SIMULATION STATISTICS ===\n")
sim_stats <- experience_intensity %>%
  group_by(Type) %>%
  summarise(
    Mean_Amplitude = sprintf("%.3f", mean(Amplitude)),
    SD_Amplitude = sprintf("%.3f", sd(Amplitude)),
    Max_Peak = sprintf("%.3f", max(Amplitude)),
    Persistence = sprintf("%.3f", sum(Amplitude)/length(Amplitude))  # Temporal stability
  ) %>%
  filter(Type != "Composite")

print(sim_stats)

# DYNAMIC REGIME CHARACTERISTICS
cat("\n=== EXPERIENTIAL REGIME CHARACTERISTICS ===\n")
cat("SOLID Object: High persistence, stable resonance (Inertia-dominant)\n")  
cat("FLUID Process: Continuous flow, medium stability (Cohesion-dominant)\n")
cat("GASEOUS Process: Rapid dispersion, volatile (Radiation-dominant)\n")

cat("\n=== SIMULATION COMPLETE ===\n")
cat("Outputs: Main trajectory plot + component decomposition + statistics\n")
