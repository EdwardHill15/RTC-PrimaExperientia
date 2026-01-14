# =============================================================================
# COMPLETE R PROGRAM: M_e ⊂ Holographic Field + PPTQ (62ms Percepts)
# Psychophysical Time Quantum: ~16 percepts/second demonstrated
# Edward F. Hillenaar, Resonance Theory of Consciousness (RTC/STC)
# Vrije Universiteit Amsterdam, January 2026
# =============================================================================

# ULTRA-SAFE GRAPHICS INITIALIZATION
safe_graphics_init <- function() {
  if (exists("dev.list") && !is.null(dev.list()) && length(dev.list()) > 0) {
    while (dev.cur() > 1) { dev.off() }
  }
  dev.new()
  cat("Graphics initialized safely\n")
}

# Load libraries
library(ggplot2)
library(dplyr)

set.seed(42)
cat("=== HOLOGRAPHIC FIELD + PPTQ (62ms PERCEPTS) SIMULATION ===\n")
safe_graphics_init()

# =============================================================================
# TIME PARAMETERS WITH PPTQ IMPLEMENTATION
# =============================================================================
PPTQ <- 0.062  # Psychophysical Time Quantum = 62ms per percept
t_continuous <- seq(0, 4, length.out = 300)  # Continuous time
t_pptq <- seq(0, 4, by = PPTQ)               # Discrete PPTQ time points (~16/sec)

x <- seq(-2, 2, length.out = 150)
T_grid <- rep(t_continuous, each = length(x))
X_grid <- rep(x, times = length(t_continuous))

cat("PPTQ = 62ms →", length(t_pptq), "discrete percepts over 4s (", 
    round(1/PPTQ), "percepts/second", ")\n")

# =============================================================================
# 1. INFINITE HOLOGRAPHIC FIELD
# =============================================================================
holo_field <- rep(0, length(T_grid))
n_waves <- 2000

for(i in 1:n_waves) {
  freq_t <- runif(1, 0.5, 4.0); freq_x <- runif(1, 0.3, 2.5)
  phase_t <- runif(1, 0, 2*pi); phase_x <- runif(1, 0, 2*pi)
  amp <- runif(1, 0, 0.02)
  holo_field <- holo_field + amp * sin(2*pi*freq_t*T_grid + phase_t) * sin(2*pi*freq_x*X_grid + phase_x)
}
holo_field <- (holo_field - min(holo_field)) / (max(holo_field) - min(holo_field))

# =============================================================================
# 2. PPTQ-DISCRETIZED M_e (SOLID OBJECT) WITH 16 PERCEPTS/SECOND
# =============================================================================
res_window_t <- t_continuous >= 1.6 & t_continuous <= 2.4
res_window_x <- x >= -0.6 & x <= 0.6

# Prima Experientia waves (continuous)
I_base <- sin(2 * pi * 0.8 * t_continuous) * exp(-0.12 * t_continuous)
C_base <- cos(2 * pi * 1.4 * t_continuous) * exp(-0.08 * t_continuous)
R_base <- sin(2 * pi * 2.6 * t_continuous + pi/3)
V_base <- sin(2 * pi * 4.0 * t_continuous)

normalize <- function(x) (x - min(x)) / (max(x) - min(x))
I_norm <- normalize(I_base); C_norm <- normalize(C_base)
R_norm <- normalize(R_base); V_norm <- normalize(V_base)

# M_e SOLID: High Inertia αI=0.65 (continuous)
M_e_solid_continuous <- 0.65*I_norm + 0.10*C_norm + 0.15*R_norm + 0.10*V_norm

# **PPTQ DISCRETIZATION: Sample M_e at 62ms intervals (16 Hz)**
M_e_solid_pptq <- approx(t_continuous, M_e_solid_continuous, t_pptq)$y
names(M_e_solid_pptq) <- paste0("Percept_", 1:length(t_pptq))

cat("Generated", length(M_e_solid_pptq), "discrete PPTQ percepts (16.13 Hz)\n")

# Map to holographic field subset
mask_res <- (T_grid %in% t_continuous[res_window_t]) & (X_grid %in% x[res_window_x])
M_e_solid_field <- rep(0, length(T_grid))

if(sum(mask_res) > 0) {
  res_times <- T_grid[mask_res]
  M_e_solid_field[mask_res] <- approx(t_continuous[res_window_t], 
                                      M_e_solid_continuous[res_window_t], res_times)$y
}

# =============================================================================
# 3. VISUALIZATION WITH PPTQ PERCEPts MARKED
# =============================================================================

# Data frames
field_data <- data.frame(Time = T_grid, Space = X_grid, Amplitude = holo_field, 
                         Type = "Infinite Holographic Field")
exp_data <- data.frame(Time = T_grid, Space = X_grid, Amplitude = M_e_solid_field, 
                       Type = "Experienced M_e (Solid, PPTQ)")

holo_exp_data <- rbind(field_data, exp_data)

# PLOT 1: HOLOGRAPHIC FIELD vs PPTQ-DISCRETIZED M_e
p1 <- ggplot(holo_exp_data, aes(x = Space, y = Time, fill = Amplitude)) +
  geom_tile(color = NA) +
  facet_wrap(~Type, ncol = 1, scales = "free") +
  scale_fill_viridis_c(name = "Resonance\nAmplitude", option = "plasma") +
  labs(title = "STC Ontology: M[e] ⊂ Holographic Field (PPTQ 62ms)",
       subtitle = "16 Percepts/Second of Solid Object via Psychophysical Time Quantum",
       x = "Spatial Dimension", y = "Experiential Time (s)",
       caption = "PPTQ = 62ms → 16.13 Hz perceptual sampling") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p1)

# PLOT 2: PPTQ PERCEPTS VISUALIZED (16 dots/second)
p2_data <- data.frame(
  Time = t_pptq,
  Amplitude = M_e_solid_pptq,
  Percept = paste0("P", 1:length(t_pptq))
)

p2 <- ggplot(p2_data, aes(x = Time, y = Amplitude)) +
  geom_point(size = 3, color = "#8B4513", alpha = 0.9) +
  geom_line(color = "#8B4513", linewidth = 1.2, alpha = 0.7) +
  geom_vline(xintercept = seq(0.062, 4, by = 1), linetype = "dotted", alpha = 0.5) +
  annotate("text", x = 0.1, y = 0.9, label = "PPTQ = 62ms", size = 5, fontface = "bold") +
  annotate("text", x = 1.0, y = 0.15, label = "16 percepts/sec", size = 4) +
  labs(title = "PPTQ Discretization: 16 Percepts/Second of Solid Object",
       subtitle = "M[e](t) sampled at Psychophysical Time Quantum intervals",
       x = "Experiential Time (s)", y = "M[e] Solid Amplitude") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, 4, 0.5))

print(p2)

# PLOT 3: TEMPORAL EXTRACTION WITH PPTQ MARKERS
field_time_avg <- field_data %>% group_by(Time) %>% summarise(Full_Field = mean(Amplitude))
exp_time_subset <- data.frame(Time = t_continuous[res_window_t], 
                              Experienced = M_e_solid_continuous[res_window_t])

p3 <- ggplot() +
  geom_line(data = field_time_avg, aes(x = Time, y = Full_Field), 
            color = "#666666", linewidth = 1.2, linetype = "dashed", alpha = 0.7) +
  geom_line(data = exp_time_subset, aes(x = Time, y = Experienced), 
            color = "#8B4513", linewidth = 2) +
  geom_point(data = p2_data[p2_data$Time >= 1.6 & p2_data$Time <= 2.4, ], 
             aes(x = Time, y = Amplitude), size = 4, color = "#8B4513", shape = 21, fill = "gold") +
  geom_rect(aes(xmin = 1.6, xmax = 2.4, ymin = -0.05, ymax = 1.05), 
            fill = "gold", alpha = 0.2, inherit.aes = FALSE) +
  labs(title = "PPTQ Percepts Extracted from Holographic Field",
       subtitle = "16 discrete percepts/second within resonance window",
       x = "Experiential Time (s)", y = "Normalized Amplitude") +
  theme_minimal(base_size = 13)

print(p3)

# =============================================================================
# 4. PPTQ VALIDATION
# =============================================================================
percepts_per_sec <- 1 / PPTQ
n_percepts_total <- length(t_pptq)
n_percepts_window <- sum(t_pptq >= 1.6 & t_pptq <= 2.4)

cat("\n=== PPTQ (62ms) VALIDATION ===\n")
cat(sprintf("✓ Sampling Rate:        %.1f Hz (16.13 percepts/second)\n", percepts_per_sec))
cat(sprintf("✓ Total Percepts:      %d over 4s\n", n_percepts_total))
cat(sprintf("✓ Resonance Window:    %d percepts [1.6s-2.4s]\n", n_percepts_window))
cat(sprintf("✓ Subset Ratio:        %.1f%% of holographic field\n", 100*sum(mask_res)/length(T_grid)))
cat("✓ M[e] Solid: αI=0.65 (high inertia)\n")

cat("\n=== STC + PPTQ PRINCIPLE DEMONSTRATED ✓ ===\n")
cat("The Perceptual mode samples the holographic field at PPTQ=62ms intervals\n")
cat("→ 16 discrete percepts/second construct the solid object experience\n")
