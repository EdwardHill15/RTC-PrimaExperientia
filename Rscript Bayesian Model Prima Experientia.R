# =============================================================================
# COMPLETE R PROGRAM: Prima Experientia in STC Ontology
# Implements Solid/Fluid/Gaseous objects + Color Tints via delta(V)
# Edward F. Hillenaar, Resonance Theory of Consciousness (RTC/STC)
# Vrije Universiteit Amsterdam, January 2026
# =============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

cat("=== Prima Experientia STC Ontology Model ===\n")

# =============================================================================
# 1. TIME VECTOR AND BASE PRIMA EXPERIENTIA WAVE PATTERNS
# =============================================================================
# Time vector for resonance cycles
t <- seq(0, 10, length.out = 1000)

# Define four primary complex wave patterns
I <- sin(2 * pi * 0.5 * t) * exp(-0.1 * t)    # Inertia (I): low freq, damping
C <- cos(2 * pi * 1.0 * t) * exp(-0.05 * t)  # Cohesion (C): medium freq, slow damping
R <- sin(2 * pi * 2.0 * t + pi/4)            # Radiation (R): higher freq, phase shift
V <- sin(2 * pi * 3.0 * t + pi/2) * (1 + 0.5 * sin(2 * pi * 0.2 * t))  # Vibration (V): modulated

# Normalize to [0,1] range
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
I_norm <- normalize(I)
C_norm <- normalize(C)
R_norm <- normalize(R)
V_norm <- normalize(V)  # Base V for tint modulation

cat("Base Prima Experientia waves generated: I, C, R, V\n")

# =============================================================================
# 2. OBJECT TYPES: Solid (high αI), Fluid (high βC), Gaseous (high γR)
# =============================================================================
object_types <- data.frame(
  type = c("Solid", "Fluid", "Gaseous", "Balanced"),
  alpha_I = c(0.6, 0.2, 0.1, 0.3),
  beta_C  = c(0.1, 0.5, 0.2, 0.4),
  gamma_R = c(0.2, 0.2, 0.6, 0.2),
  delta_V = c(0.1, 0.1, 0.1, 0.1)
)

cat("Object types defined with dominant components\n")
print(object_types)

# =============================================================================
# 3. COLOR TINTS: delta(V) modulation for qualia (Red, Green, Blue, Violet)
# =============================================================================
tints <- c("Red", "Green", "Blue", "Violet")
tint_freqs <- c(0.8, 1.2, 1.5, 1.8)    # Hue-specific frequencies
tint_amps <- c(0.3, 0.4, 0.35, 0.45)   # Saturation amplitudes

V_tints <- list()
for (i in seq_along(tints)) {
  tint_mod <- tint_amps[i] * sin(2 * pi * tint_freqs[i] * t)
  V_tints[[i]] <- normalize(V_norm + tint_mod)
}
names(V_tints) <- tints

# Colorful object variants (elevated delta_V)
object_types_color <- rbind(
  object_types[1:3, ],  # Original achromatic
  data.frame(type = "Color Solid",   alpha_I = 0.50, beta_C = 0.10, gamma_R = 0.10, delta_V = 0.30),
  data.frame(type = "Color Fluid",   alpha_I = 0.15, beta_C = 0.40, gamma_R = 0.15, delta_V = 0.30),
  data.frame(type = "Color Gaseous", alpha_I = 0.10, beta_C = 0.15, gamma_R = 0.50, delta_V = 0.25)
)

# =============================================================================
# 4. MIND-EXPERIENCE (M_e) CALCULATION: αI + βC + γR + δV
# =============================================================================
compute_M_e <- function(alpha, beta, gamma, delta, V_tint = V_norm) {
  alpha * I_norm + beta * C_norm + gamma * R_norm + delta * V_tint
}

# Base M_e for all object types
M_e_types <- list()
for (i in 1:nrow(object_types)) {
  coeffs <- object_types[i, 2:5]
  M_e_types[[i]] <- compute_M_e(coeffs$alpha_I, coeffs$beta_C, coeffs$gamma_R, coeffs$delta_V)
}
names(M_e_types) <- object_types$type

# Colorful M_e examples
M_e_color_solid   <- compute_M_e(0.50, 0.10, 0.10, 0.30, V_tints$Red)
M_e_color_fluid   <- compute_M_e(0.15, 0.40, 0.15, 0.30, V_tints$Green)
M_e_color_gaseous <- compute_M_e(0.10, 0.15, 0.50, 0.25, V_tints$Violet)

cat("M_e computed for all object types + color variants\n")

# =============================================================================
# 5. VISUALIZATION: Waves and M_e Superpositions
# =============================================================================
# Base waves + selected M_e
wave_data <- data.frame(
  t = rep(t, 8),
  wave = c(I_norm, C_norm, R_norm, V_norm,
           M_e_types$Solid, M_e_types$Fluid, M_e_types$Gaseous, M_e_types$Balanced),
  type = c("Inertia (I)", "Cohesion (C)", "Radiation (R)", "Vibration (V)",
           "M_e: Solid", "M_e: Fluid", "M_e: Gaseous", "M_e: Balanced")
)

p1 <- ggplot(wave_data, aes(x = t, y = wave, color = type)) +
  geom_line(linewidth = 1.2, alpha = 0.85) +
  labs(title = "Prima Experientia and Mind-exp (M_e) by Object Type",
       subtitle = expression(M[e]==alpha~I + beta~C + gamma~R + delta~V),
       x = "Time (Resonance Cycle)", y = "Normalized Amplitude") +
  theme_minimal(base_size = 11) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

print(p1)

# Color demonstration
color_data <- data.frame(
  t = rep(t, 5),
  wave = c(V_norm, V_tints$Red, V_tints$Green, V_tints$Blue, M_e_color_solid),
  type = c("V Base", "V Red Tint", "V Green Tint", "V Blue Tint", "M_e Color Solid")
)

p2 <- ggplot(color_data, aes(x = t, y = wave, color = type)) +
  geom_line(linewidth = 1.4, alpha = 0.9) +
  labs(title = "delta(V) Color Tints: Vibration Qualia Generator",
       subtitle = "V_tint = V_base + amp × sin(freq × t)",
       x = "Time", y = "Normalized Amplitude") +
  theme_minimal(base_size = 11) +
  scale_color_manual(values = c("gray50", "red3", "forestgreen", "blue3", "magenta")) +
  theme(legend.position = "right")

print(p2)

# =============================================================================
# 6. BAYESIAN GAMMA PREDICTION MODELS
# =============================================================================
bayes_update <- function(obs_data, label, k = 2, a0 = 1, b0 = 1) {
  n <- length(obs_data)
  sum_obs <- sum(obs_data)
  a_post <- a0 + n * k
  b_post <- b0 + sum_obs
  post_pred <- rgamma(5000, shape = a_post, rate = b_post)
  
  cat(sprintf("\n%s Bayesian Gamma Prediction:\n", label))
  cat(sprintf("Posterior λ ~ Gamma(%.1f, %.1f), Pred Mean = %.3f\n",
              a_post, b_post, a_post / b_post))
  
  p_pred <- ggplot(data.frame(x = post_pred), aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.7, fill = "steelblue") +
    stat_function(fun = function(x) dgamma(x, a_post, b_post), 
                  color = "red", linewidth = 1.3) +
    labs(title = sprintf("Posterior Predictive: %s M_e", label),
         x = expression(M[e]), y = "Density") +
    theme_minimal()
  
  print(p_pred)
  return(list(a_post = a_post, b_post = b_post, pred_samples = post_pred))
}

# Simulate observations scaled by dominant coefficients
obs_solid    <- rgamma(40, shape = 3,   rate = 1 / 0.6)  # High I
obs_fluid    <- rgamma(40, shape = 2.5, rate = 1 / 0.5)  # High C
obs_gaseous  <- rgamma(40, shape = 2,   rate = 1 / 0.6)  # High R
obs_color    <- rgamma(40, shape = 2.5, rate = 1 / 0.8)  # High V (I+V)

solid_bayes   <- bayes_update(obs_solid, "Solid (αI=0.6)")
fluid_bayes   <- bayes_update(obs_fluid, "Fluid (βC=0.5)")
gaseous_bayes <- bayes_update(obs_gaseous, "Gaseous (γR=0.6)")
color_bayes   <- bayes_update(obs_color, "Color Solid (δV=0.3)")

# =============================================================================
# 7. SUMMARY TABLES
# =============================================================================
pred_summary <- data.frame(
  Type = c("Solid", "Fluid", "Gaseous", "Color Solid"),
  Dominant = c("Inertia α=0.6", "Cohesion β=0.5", "Radiation γ=0.6", "Vibration δ=0.3"),
  Pred_Mean = sprintf("%.3f", c(mean(solid_bayes$pred_samples), 
                                mean(fluid_bayes$pred_samples),
                                mean(gaseous_bayes$pred_samples),
                                mean(color_bayes$pred_samples))),
  Pred_SD = sprintf("%.3f", c(sd(solid_bayes$pred_samples),
                              sd(fluid_bayes$pred_samples),
                              sd(gaseous_bayes$pred_samples),
                              sd(color_bayes$pred_samples)))
)

cat("\n=== PREDICTIVE SUMMARY ===\n")
print(pred_summary)

tint_table <- data.frame(
  Tint = tints,
  Frequency = tint_freqs,
  Amplitude = tint_amps,
  Qualia = c("Warm Red", "Balanced Green", "Cool Blue", "Vivid Violet")
)

cat("\n=== VIBRATION (V) TINT PARAMETERS ===\n")
print(tint_table)

cat("\n=== STC ONTOLOGY MODEL COMPLETE ===\n")
cat("Key outputs: 4 wave plots, 4 Bayesian predictives, summary tables\n")
