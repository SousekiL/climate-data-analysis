# ============================================================================
# Extreme Value Theory: GEV vs POT Methods Comparison
# Application: US Property Insurance Claims Analysis
# ============================================================================

# Install and load required packages
# install.packages(c("evir", "ismev", "extRemes", "ggplot2", "gridExtra"))

library(evir) # For EVT analysis
library(ismev) # For GEV and GPD fitting
library(extRemes) # Additional EVT tools
library(ggplot2) # For visualization
library(gridExtra) # For multiple plots
# install.packages("actuar") # only once
library(actuar)

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# PART 1: DATA GENERATION
# ============================================================================

# Simulate 10 years of daily insurance claims data (2500 observations)
# We simulate from a heavy-tailed distribution (Pareto) to represent
# property insurance losses with catastrophic events

n_years <- 10
days_per_year <- 250
n_total <- n_years * days_per_year

# Generate base claims from lognormal distribution
base_claims <- rlnorm(n_total, meanlog = 10, sdlog = 1.5)

# Add some extreme events (catastrophes) - 1% probability
n_catastrophes <- round(n_total * 0.01)
catastrophe_indices <- sample(1:n_total, n_catastrophes)
base_claims[catastrophe_indices] <- base_claims[catastrophe_indices] *
  rpareto(n_catastrophes, shape = 2, scale = 5)

# Scale to millions of dollars
claims <- base_claims / 1000

# Create time index
time_index <- seq(as.Date("2014-01-01"), by = "day", length.out = n_total)
claims_data <- data.frame(date = time_index, loss = claims)

# Summary statistics
cat("=== Data Summary ===\n")
cat("Total observations:", n_total, "\n")
cat("Mean loss: $", round(mean(claims), 2), "M\n")
cat("Median loss: $", round(median(claims), 2), "M\n")
cat("Max loss: $", round(max(claims), 2), "M\n")
cat("95th percentile: $", round(quantile(claims, 0.95), 2), "M\n")
cat("99th percentile: $", round(quantile(claims, 0.99), 2), "M\n\n")

# ============================================================================
# PART 2: EXPLORATORY DATA ANALYSIS
# ============================================================================

# Plot 1: Time series of losses
p1 <- ggplot(claims_data, aes(x = date, y = loss)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_point(
    data = claims_data[claims_data$loss > quantile(claims, 0.99), ],
    color = "red",
    size = 2
  ) +
  labs(
    title = "Daily Insurance Losses (Red: 99th Percentile Exceedances)",
    x = "Date",
    y = "Loss (Million USD)"
  ) +
  theme_minimal()

# Plot 2: Histogram with tail focus
p2 <- ggplot(claims_data, aes(x = loss)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Losses",
    x = "Loss (Million USD)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot 3: QQ plot against normal distribution
p3 <- ggplot(claims_data, aes(sample = loss)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(
    title = "QQ Plot vs Normal Distribution",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Display plots
grid.arrange(p1, p2, p3, ncol = 1)

# ============================================================================
# PART 3: GEV METHOD (BLOCK MAXIMA)
# ============================================================================

cat("=== GEV METHOD (BLOCK MAXIMA) ===\n\n")

# Extract annual maxima (one maximum per year)
claims_data$year <- format(claims_data$date, "%Y")
annual_maxima <- aggregate(loss ~ year, data = claims_data, FUN = max)

cat("Annual Maxima:\n")
print(annual_maxima)
cat("\n")

# Fit GEV distribution using Maximum Likelihood Estimation
gev_fit <- gev.fit(annual_maxima$loss)

cat("--- GEV Parameter Estimates ---\n")
cat("Location (μ):", round(gev_fit$mle[1], 4), "\n")
cat("Scale (σ):", round(gev_fit$mle[2], 4), "\n")
cat("Shape (ξ):", round(gev_fit$mle[3], 4), "\n")
cat("Standard Errors:", round(gev_fit$se, 4), "\n\n")

# Interpretation of shape parameter
if (gev_fit$mle[3] > 0.1) {
  cat("Interpretation: ξ > 0 indicates Fréchet type (heavy tail)\n")
  cat("This suggests the presence of extreme catastrophic losses.\n\n")
} else if (gev_fit$mle[3] < -0.1) {
  cat("Interpretation: ξ < 0 indicates Weibull type (bounded tail)\n\n")
} else {
  cat("Interpretation: ξ ≈ 0 indicates Gumbel type (exponential tail)\n\n")
}

# Diagnostic plots for GEV
par(mfrow = c(2, 2))
gev.diag(gev_fit)
par(mfrow = c(1, 1))

# Calculate return levels manually
gev_return_level <- function(T, mu, sigma, xi) {
  if (abs(xi) < 1e-6) {
    # Gumbel case
    return(mu - sigma * log(-log(1 - 1 / T)))
  } else {
    # Fréchet or Weibull case
    return(mu + sigma / xi * ((-log(1 - 1 / T))^(-xi) - 1))
  }
}

# Calculate return levels for specified return periods
return_periods <- c(10, 20, 50, 100, 200)
gev_return_levels <- sapply(return_periods, function(T) {
  gev_return_level(T, gev_fit$mle[1], gev_fit$mle[2], gev_fit$mle[3])
})

cat("--- GEV Return Levels ---\n")
for (i in 1:length(return_periods)) {
  cat(
    return_periods[i],
    "-year return level: $",
    round(gev_return_levels[i], 2),
    "M\n"
  )
}

# Calculate VaR and ES at high confidence levels
# For annual maxima, we use the GEV quantile function
gev_quantile <- function(p, mu, sigma, xi) {
  if (abs(xi) < 1e-6) {
    # Gumbel case
    return(mu - sigma * log(-log(p)))
  } else {
    # Fréchet or Weibull case
    return(mu + sigma * ((-log(p))^(-xi) - 1) / xi)
  }
}

prob_levels <- c(0.95, 0.99, 0.995, 0.999)
cat("--- GEV-based Risk Measures (Annual Maximum) ---\n")
for (p in prob_levels) {
  var_gev <- gev_quantile(p, gev_fit$mle[1], gev_fit$mle[2], gev_fit$mle[3])
  cat("VaR at", p * 100, "%: $", round(var_gev, 2), "M\n")
}
cat("\n")

# ============================================================================
# PART 4: POT METHOD (PEAKS OVER THRESHOLD)
# ============================================================================

cat("=== POT METHOD (PEAKS OVER THRESHOLD) ===\n\n")

# Step 1: Threshold Selection using Mean Excess Plot
cat("--- Step 1: Threshold Selection ---\n")

# Mean Excess Plot
par(mfrow = c(1, 2))
meplot(claims)
title("Mean Excess Plot")

# Try different thresholds
thresholds_test <- quantile(claims, probs = seq(0.90, 0.99, by = 0.01))

# Parameter stability plot
shape_estimates <- numeric(length(thresholds_test))
scale_estimates <- numeric(length(thresholds_test))

for (i in 1:length(thresholds_test)) {
  u <- thresholds_test[i]
  exceedances <- claims[claims > u] - u
  if (length(exceedances) > 20) {
    # Need sufficient exceedances
    fit_temp <- gpd(claims, threshold = u)
    shape_estimates[i] <- fit_temp$par.ests[1]
    scale_estimates[i] <- fit_temp$par.ests[2]
  } else {
    shape_estimates[i] <- NA
    scale_estimates[i] <- NA
  }
}

plot(
  thresholds_test,
  shape_estimates,
  type = "b",
  xlab = "Threshold",
  ylab = "Shape Parameter (ξ)",
  main = "Parameter Stability Plot",
  col = "blue",
  pch = 16
)
abline(h = gev_fit$mle[3], col = "red", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("GPD estimates", "GEV estimate"),
  col = c("blue", "red"),
  lty = c(1, 2),
  lwd = 2
)

par(mfrow = c(1, 1))

# Select threshold at 95th percentile (common choice)
# In practice, use the stability plot to choose
threshold_selected <- quantile(claims, 0.95)
cat(
  "Selected threshold: $",
  round(threshold_selected, 2),
  "M (95th percentile)\n"
)

# Count exceedances
exceedances <- claims[claims > threshold_selected]
n_exceedances <- length(exceedances)
cat("Number of exceedances:", n_exceedances, "\n")
cat("Exceedance rate:", round(n_exceedances / n_total, 4), "\n\n")

# Step 2: Fit GPD to exceedances
cat("--- Step 2: Fit GPD Distribution ---\n")

gpd_fit <- gpd(claims, threshold = threshold_selected)

cat("GPD Parameter Estimates:\n")
cat("Shape (ξ):", round(gpd_fit$par.ests[1], 4), "\n")
cat("Scale (σ):", round(gpd_fit$par.ests[2], 4), "\n")
cat("Standard Errors:", round(gpd_fit$par.ses, 4), "\n\n")

# Compare shape parameters
cat("--- Shape Parameter Comparison ---\n")
cat("GEV shape (ξ):", round(gev_fit$mle[3], 4), "\n")
cat("GPD shape (ξ):", round(gpd_fit$par.ests[1], 4), "\n")
cat("Difference:", round(abs(gev_fit$mle[3] - gpd_fit$par.ests[1]), 4), "\n")
if (abs(gev_fit$mle[3] - gpd_fit$par.ests[1]) < 0.1) {
  cat("The two methods give consistent shape parameter estimates.\n\n")
} else {
  cat(
    "Note: Significant difference between methods may indicate model issues.\n\n"
  )
}

# Diagnostic plots for GPD
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(gpd_fit) # This will display all plots one after the other
par(mfrow = c(1, 1)) # Reset to single plot layout

# Step 3: Calculate risk measures using POT
cat("--- Step 3: POT-based Risk Measures ---\n")

# VaR calculation using GPD
# For a probability p, VaR is calculated as:
# VaR_p = u + (σ/ξ) * [(n/N_u * (1-p))^(-ξ) - 1]
# where u is threshold, N_u is number of exceedances, n is total observations

calculate_var_pot <- function(p, threshold, xi, sigma, n_total, n_exceed) {
  if (abs(xi) < 1e-6) {
    # Gumbel case
    return(threshold + sigma * log(n_total / n_exceed * (1 - p)))
  } else {
    # Fréchet or Weibull case
    return(
      threshold +
        (sigma / xi) *
          ((n_total / n_exceed * (1 - p))^(-xi) - 1)
    )
  }
}

# Expected Shortfall (ES) calculation
# ES_p = VaR_p + (σ + ξ * (VaR_p - u)) / (1 - ξ)
calculate_es_pot <- function(var_p, threshold, xi, sigma) {
  if (xi < 1) {
    return(var_p + (sigma + xi * (var_p - threshold)) / (1 - xi))
  } else {
    return(Inf) # ES undefined for ξ >= 1
  }
}

cat("POT-based VaR and ES:\n")
for (p in prob_levels) {
  var_pot <- calculate_var_pot(
    p,
    threshold_selected,
    gpd_fit$par.ests[1],
    gpd_fit$par.ests[2],
    n_total,
    n_exceedances
  )
  es_pot <- calculate_es_pot(
    var_pot,
    threshold_selected,
    gpd_fit$par.ests[1],
    gpd_fit$par.ests[2]
  )
  cat("VaR at", p * 100, "%: $", round(var_pot, 2), "M\n")
  cat("ES at", p * 100, "%: $", round(es_pot, 2), "M\n\n")
}

# ============================================================================
# PART 5: COMPARISON AND VALIDATION
# ============================================================================

cat("=== METHOD COMPARISON ===\n\n")

# Compare VaR estimates from both methods
comparison_table <- data.frame(
  Probability = prob_levels * 100,
  GEV_VaR = sapply(prob_levels, function(p) {
    gev_quantile(p, gev_fit$mle[1], gev_fit$mle[2], gev_fit$mle[3])
  }),
  POT_VaR = sapply(prob_levels, function(p) {
    calculate_var_pot(
      p,
      threshold_selected,
      gpd_fit$par.ests[1],
      gpd_fit$par.ests[2],
      n_total,
      n_exceedances
    )
  })
)

comparison_table$Difference <- comparison_table$POT_VaR -
  comparison_table$GEV_VaR
comparison_table$Pct_Diff <- (comparison_table$Difference /
  comparison_table$GEV_VaR) *
  100

cat("VaR Comparison Table:\n")
print(round(comparison_table, 2))
cat("\n")

# Visualize comparison
p_compare <- ggplot(comparison_table, aes(x = Probability)) +
  geom_line(aes(y = GEV_VaR, color = "GEV"), size = 1.2) +
  geom_line(aes(y = POT_VaR, color = "POT"), size = 1.2) +
  geom_point(aes(y = GEV_VaR, color = "GEV"), size = 3) +
  geom_point(aes(y = POT_VaR, color = "POT"), size = 3) +
  scale_color_manual(values = c("GEV" = "red", "POT" = "blue")) +
  labs(
    title = "VaR Comparison: GEV vs POT Methods",
    x = "Probability Level (%)",
    y = "Value at Risk (Million USD)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_compare)

# ============================================================================
# PART 6: BACKTESTING
# ============================================================================

cat("\n=== BACKTESTING ===\n\n")

# Simple backtesting: check if empirical quantiles match estimated VaR
empirical_quantiles <- quantile(claims, probs = prob_levels)

backtest_table <- data.frame(
  Probability = prob_levels * 100,
  Empirical = empirical_quantiles,
  GEV_VaR = comparison_table$GEV_VaR,
  POT_VaR = comparison_table$POT_VaR
)

backtest_table$GEV_Error <- (backtest_table$GEV_VaR -
  backtest_table$Empirical) /
  backtest_table$Empirical *
  100
backtest_table$POT_Error <- (backtest_table$POT_VaR -
  backtest_table$Empirical) /
  backtest_table$Empirical *
  100

cat("Backtesting Results (% Error from Empirical Quantiles):\n")
print(round(backtest_table, 2))
cat("\n")

# ============================================================================
# PART 7: RECOMMENDATIONS
# ============================================================================

cat("=== RECOMMENDATIONS ===\n\n")

cat("1. SHAPE PARAMETER CONSISTENCY:\n")
if (abs(gev_fit$mle[3] - gpd_fit$par.ests[1]) < 0.1) {
  cat(
    "   ✓ Both methods agree on tail behavior (ξ ≈",
    round(mean(c(gev_fit$mle[3], gpd_fit$par.ests[1])), 3),
    ")\n"
  )
  cat("   This increases confidence in the extreme value analysis.\n\n")
} else {
  cat("   ⚠ Methods show different tail behaviors\n")
  cat(
    "   Consider: data quality issues, threshold selection, or sample size.\n\n"
  )
}

cat("2. SAMPLE SIZE CONSIDERATION:\n")
cat("   - GEV uses", nrow(annual_maxima), "annual maxima\n")
cat("   - POT uses", n_exceedances, "exceedances\n")
if (n_exceedances > 5 * nrow(annual_maxima)) {
  cat(
    "   → POT provides more information and likely more reliable estimates.\n\n"
  )
} else {
  cat("   → Consider lowering threshold or using GEV if data is limited.\n\n")
}

cat("3. PRACTICAL RECOMMENDATION FOR THIS DATA:\n")
if (gpd_fit$par.ests[1] > 0.2) {
  cat("   - Heavy tail detected (ξ =", round(gpd_fit$par.ests[1], 3), ")\n")
  cat("   - POT method is preferred for tail risk measurement\n")
  cat(
    "   - Suitable for: Operational risk capital, catastrophe reinsurance pricing\n\n"
  )
} else {
  cat("   - Moderate tail behavior detected\n")
  cat(
    "   - Both methods are acceptable; POT preferred for more data utilization\n\n"
  )
}

cat("4. REGULATORY CONTEXT:\n")
cat("   - For Solvency II SCR calculation: Use POT with 99.5% VaR\n")
cat("   - For Basel III operational risk: Use POT with 99.9% VaR\n")
cat("   - For reinsurance pricing: Use POT with multiple thresholds\n\n")

cat("=== END OF ANALYSIS ===\n")
