# =========================================================
# stats_algorithms.R — Statistical comparison of GA, DE, PSO
# Based on rubric point 6 (10% of grade)
# ==================
#install.packages("car")
library(car)

if (!all(c("GA_best", "DE_best", "PSO_best", "TABU_best") %in% ls())) {
  stop("Error: Missing GA_best, DE_best, PSO_best or TABU_best vectors.")
}


# --- Create data frame for analysis ---
df <- data.frame(
  value = c(GA_best, DE_best, PSO_best, TABU_best),
  method = factor(rep(c("GA", "DE", "PSO", "TABU"),
                      each = length(GA_best)))
)

cat("\n===== DATA SUMMARY =====\n")
print(aggregate(value ~ method, df, summary))


# =========================================================
# 1) ANOVA TEST (Parametric)
# =========================================================
cat("\n===== ANOVA TEST =====\n")

anova_res <- aov(value ~ method, data = df)
print(summary(anova_res))

# Check normality of residuals
shapiro_res <- shapiro.test(residuals(anova_res))
cat("\nShapiro-Wilk normality test on residuals:\n")
print(shapiro_res)

# Check homogeneity of variances
levene_test <- car::leveneTest(value ~ method, df)
cat("\nLevene Test for homogeneity of variances:\n")
print(levene_test)


# =========================================================
# 2) KRUSKAL–WALLIS (non-parametric alternative)
# =========================================================
cat("\n===== KRUSKAL–WALLIS TEST =====\n")
kw <- kruskal.test(value ~ method, df)
print(kw)


# =========================================================
# 3) PAIRWISE WILCOXON TESTS (non-parametric pairwise)
# =========================================================
cat("\n===== PAIRWISE WILCOXON TESTS =====\n")
wilc <- pairwise.wilcox.test(df$value, df$method,
                             p.adjust.method = "bonferroni")
print(wilc)


# =========================================================
# 4) PAIRWISE t-TESTS (parametric pairwise)
# =========================================================
cat("\n===== PAIRWISE t-TESTS =====\n")
t <- pairwise.t.test(df$value, df$method,
                     p.adjust.method = "bonferroni")
print(t)


# =========================================================
# 5) AUTOMATIC INTERPRETATION
# =========================================================

cat("\n===== INTERPRETATION (AUTO-GENERATED) =====\n")

interpret <- function(anova_res, kw_res, t_res, wilcox_res) {
  
  # Detect significance
  sig_anova  <- summary(anova_res)[[1]][["Pr(>F)"]][1] < 0.05
  sig_kw     <- kw_res$p.value < 0.05
  
  cat("\n--- Global Tests ---\n")
  
  if (sig_anova) {
    cat("ANOVA: Significant differences detected between GA, DE, PSO (p < 0.05).\n")
  } else {
    cat("ANOVA: No significant differences between GA, DE, PSO.\n")
  }
  
  if (sig_kw) {
    cat("Kruskal–Wallis: Significant differences detected (p < 0.05).\n")
  } else {
    cat("Kruskal–Wallis: No significant differences found.\n")
  }
  
  cat("\n--- Pairwise Comparisons ---\n")
  
  # Pairwise t-tests
  cat("\nPairwise t-tests (Bonferroni-adjusted):\n")
  print(t_res$p.value)
  
  # Pairwise Wilcoxon tests
  cat("\nPairwise Wilcoxon tests (Bonferroni-adjusted):\n")
  print(wilcox_res$p.value)
  
  cat("\n--- Summary Interpretation ---\n")
  
  # Determine best method
  best_means <- tapply(df$value, df$method, mean)
  best_method <- names(which.min(best_means))
  
  cat(sprintf("Mean performance (lower = better): GA = %.2f, DE = %.2f, PSO = %.2f\n",
              best_means["GA"], best_means["DE"], best_means["PSO"]))
  
  cat(sprintf("Best overall method: %s\n", best_method))
  
  cat("\nInterpretation:\n")
  
  if (sig_kw | sig_anova) {
    cat(sprintf("There are statistically significant differences between at least two algorithms.\n%s appears to outperform the others on average.\n",
                best_method))
  } else {
    cat("There are NO statistically significant differences between the algorithms.\nThey perform similarly.\n")
  }
}

interpret(anova_res, kw, t, wilc)
