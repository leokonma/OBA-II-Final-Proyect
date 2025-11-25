# ============================
# 0) Load required modules
# ============================
library(genalg)
library(DEoptim)
library(pso)

setwd("C:/Users/leodo/OneDrive/Escritorio/optimization/third delivery/OBA-II-Final-Proyect")

source("main_Data.R")
source("stations.R")
source("demand_supply.R")
source("graph_utils.R")
source("class_algorithms.R")

# ============================
# 1) Prepare graph + variables
# ============================
mapping_data <- make_mapping(all_names, N_OUTER)
outer_abbr <- mapping_data$outer_abbr
PT <- mapping_data$PT

g_data <- make_graph_with_weights(outer_abbr, PT)
g <- g_data$g
coords <- g_data$coords

minutes_per_block <- 2
E(g)$time_weight <- E(g)$weight * minutes_per_block

ds <- build_demand_supply(outer_abbr, N_OUTER, N_SURPLUS, PT)
d <- ds$demand
s <- ds$surplus

objective_numeric <- function(route, graph, PT, S0, demand, surplus,
                              minutes_per_block = 2, cost_per_min_eur = 0.6,
                              penalty_per_unit = 0.5) {
  res <- objective_distance_extended(route, graph, PT, S0, demand, surplus,
                                     minutes_per_block, cost_per_min_eur,
                                     penalty_per_unit)
  return(res$distance_blocks)
}

eval_fun <- function(route, ...) {
  objective_numeric(
    route,
    graph = g, PT = PT, S0 = S0,
    demand = d, surplus = s
  )
}

# ============================
# 2) Wrapper for algorithms
# ============================

run_one_GA <- function() {
  res <- ga_search(
    outer_abbr = outer_abbr, fn = eval_fun,
    graph = g, PT = PT, S0 = S0, demand = d,
    surplus = s, penalty_per_unit = 0.5
  )
  list(best = res$best_cost, history = res$history, time = res$time_secs)
}

run_one_DE <- function() {
  res <- de_search(
    outer_abbr = outer_abbr, fn = eval_fun,
    graph = g, PT = PT, S0 = S0, demand = d,
    surplus = s, penalty_per_unit = 0.5
  )
  list(best = res$best_cost, history = res$history, time = res$time_secs)
}

run_one_PSO <- function() {
  res <- pso_search(
    outer_abbr = outer_abbr, fn = eval_fun,
    graph = g, PT = PT, S0 = S0, demand = d,
    surplus = s, penalty_per_unit = 0.5
  )
  list(best = res$best_cost, history = res$history, time = res$time_secs)
}

# NEW — TABU SEARCH
run_one_TABU <- function() {
  res <- tabu_search(
    par = sample(outer_abbr), fn = eval_fun,
    graph = g, PT = PT, S0 = S0, demand = d,
    surplus = s, penalty_per_unit = 0.5
  )
  list(best = res$best_cost, history = res$history, time = res$time_secs)
}

# ============================
# 3) RUN EXPERIMENTS
# ============================

Runs <- 10         # número de corridas
max_iters <- 25    # longitud estándar de convergencia

# Matrices de historial
GA_hist <- matrix(NA, nrow = Runs, ncol = max_iters)
DE_hist <- matrix(NA, nrow = Runs, ncol = max_iters)
PSO_hist <- matrix(NA, nrow = Runs, ncol = max_iters)
TABU_hist <- matrix(NA, nrow = Runs, ncol = max_iters)

# Vectores de resultados finales
GA_best <- numeric(Runs)
DE_best <- numeric(Runs)
PSO_best <- numeric(Runs)
TABU_best <- numeric(Runs)

GA_time <- numeric(Runs)
DE_time <- numeric(Runs)
PSO_time <- numeric(Runs)
TABU_time <- numeric(Runs)

# Padding
pad_history <- function(vec, max_len) {
  n <- length(vec)
  if (n >= max_len) return(vec[1:max_len])
  c(vec, rep(tail(vec, 1), max_len - n))
}

# === MAIN LOOP ===
for (i in 1:Runs) {
  cat(sprintf("\n--- RUN %d OF %d ---\n", i, Runs))
  
  ga <- run_one_GA()
  de <- run_one_DE()
  pso <- run_one_PSO()
  tabu <- run_one_TABU()   # NEW
  
  GA_hist[i, ] <- pad_history(ga$history, max_iters)
  DE_hist[i, ] <- pad_history(de$history, max_iters)
  PSO_hist[i, ] <- pad_history(pso$history, max_iters)
  TABU_hist[i, ] <- pad_history(tabu$history, max_iters)
  
  GA_best[i] <- ga$best
  DE_best[i] <- de$best
  PSO_best[i] <- pso$best
  TABU_best[i] <- tabu$best   # NEW
  
  GA_time[i] <- ga$time
  DE_time[i] <- de$time
  PSO_time[i] <- pso$time
  TABU_time[i] <- tabu$time   # NEW
}

# ============================
# 4) AGGREGATED STATISTICS
# ============================

avg_GA <- colMeans(GA_hist, na.rm = TRUE)
avg_DE <- colMeans(DE_hist, na.rm = TRUE)
avg_PSO <- colMeans(PSO_hist, na.rm = TRUE)
avg_TABU <- colMeans(TABU_hist, na.rm = TRUE)   # NEW

cat("\n===== AVERAGE BEST VALUES =====\n")
print(c(GA = mean(GA_best), DE = mean(DE_best), PSO = mean(PSO_best), TABU = mean(TABU_best)))

cat("\n===== AVERAGE TIMES (sec) =====\n")
print(c(GA = mean(GA_time), DE = mean(DE_time), PSO = mean(PSO_time), TABU = mean(TABU_time)))

# ============================
# 5) PLOT: Average Convergence
# ============================

plot(avg_GA, type="l", lwd=3, col="blue",
     ylim = range(c(avg_GA, avg_DE, avg_PSO, avg_TABU)),
     xlab="Iteration", ylab="Average best",
     main="Average Convergence (GA, DE, PSO, TABU)")

lines(avg_DE, lwd=3, col="darkgreen")
lines(avg_PSO, lwd=3, col="red")
lines(avg_TABU, lwd=3, col="purple")   # NEW

legend("topright",
       legend=c("GA", "DE", "PSO", "TABU"),
       col=c("blue", "darkgreen", "red", "purple"),
       lwd=3)

# ============================
# 6) BOXPLOT (Best Values)
# ============================

library(ggplot2)

df_box <- data.frame(
  value = c(GA_best, DE_best, PSO_best, TABU_best),
  method = factor(rep(c("GA", "DE", "PSO", "TABU"), each = Runs))
)

ggplot(df_box, aes(x = method, y = value, fill = method)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Best Objective Values (GA, DE, PSO, TABU)",
    x = "Algorithm",
    y = "Objective Value"
  ) +
  scale_fill_brewer(palette = "Set2")

# ============================
# 7) Summary Table
# ============================

comp_results <- data.frame(
  Method = c("GA", "DE", "PSO", "TABU"),
  Avg_Best = c(mean(GA_best), mean(DE_best), mean(PSO_best), mean(TABU_best)),
  Avg_Time = c(mean(GA_time), mean(DE_time), mean(PSO_time), mean(TABU_time)),
  Var_Best = c(var(GA_best), var(DE_best), var(PSO_best), var(TABU_best))
)

print(comp_results)

