# ================================================================
# final_algorithm.R — ejecución del modelo logístico (optimización por distancia con interpretación económica)
# ================================================================

# ==== 0) Preparación ====
rm(list = ls())
cat("\014")
set.seed(123)
library(igraph)

# ==== 1) Directorio base ====
setwd("C:/Users/leodo/OneDrive/Escritorio/optimization/third delivery/OBA-II-Final-Proyect")

# ==== 2) Cargar módulos ====
source("main_Data.R")
source("stations.R")
source("demand_supply.R")
source("graph_utils.R")
source("plot_utils.R")
source("class_algorithms.R")

# ==== 3) Generar el grafo ====
mapping_data <- make_mapping(all_names, N_OUTER)
outer_abbr <- mapping_data$outer_abbr
PT <- mapping_data$PT

g_data <- make_graph_with_weights(outer_abbr = outer_abbr, PT = PT)
g <- g_data$g
coords <- g_data$coords

minutes_per_block <- 2  # tiempo promedio por bloque
E(g)$time_weight <- E(g)$weight * minutes_per_block


# ==== 3.1) Añadir pesos en tiempo estimado (minutos) ====
minutes_per_block <- 2  # tiempo promedio por manzana
E(g)$time_weight <- E(g)$weight * minutes_per_block

# ==== 4) Preparar demanda y superávit ====
ds <- build_demand_supply(outer_abbr = outer_abbr,
                          N_OUTER = N_OUTER,
                          N_SURPLUS = N_SURPLUS,
                          PT = PT)
d <- ds$demand
s <- ds$surplus

cat("\nDemanda (d):\n"); print(d)
cat("\nSuperávit (s):\n"); print(s)

# ==== 5) Parámetros base (en unidades reales) ====
minutes_per_block <- 2        # tiempo medio por bloque (min)
cost_per_min_eur   <- 0.6     # coste operativo por minuto (€)
penalty_per_unit   <- 0.5     # penalización por unidad no servida (en bloques)
lambda <- NULL                

# ==== 6) Función objetivo extendida ====
objective_distance_extended <- function(route, graph, PT, S0, demand, surplus,
                                        minutes_per_block = 2,
                                        cost_per_min_eur = 0.6,
                                        penalty_per_unit = 0.5) {
  full_route <- c(PT, route, PT)
  total_distance <- 0
  
  # ---- calcular distancia total en bloques ----
  for (i in seq_along(full_route[-1])) {
    from <- full_route[i]
    to   <- full_route[i + 1]
    total_distance <- total_distance + igraph::distances(graph, v = from, to = to)
  }
  
  # ---- penalización si el stock no cubre la demanda ----
  total_demand <- sum(demand[demand > 0])
  unmet <- max(0, total_demand - S0)
  penalty_blocks <- unmet * penalty_per_unit
  
  # ---- conversión a tiempo y costo ----
  total_blocks <- as.numeric(total_distance + penalty_blocks)
  total_minutes <- total_blocks * minutes_per_block
  total_cost <- total_minutes * cost_per_min_eur
  
  return(list(
    distance_blocks = total_blocks,
    time_minutes = total_minutes,
    cost_eur = total_cost,
    penalty_blocks = penalty_blocks
  ))
}

# ==== 7) Función wrapper para algoritmos ====
objective_numeric <- function(route, graph, PT, S0, demand, surplus,
                              minutes_per_block = 2, cost_per_min_eur = 0.6,
                              penalty_per_unit = 0.5) {
  res <- objective_distance_extended(route, graph, PT, S0, demand, surplus,
                                     minutes_per_block, cost_per_min_eur, penalty_per_unit)
  return(res$distance_blocks)
}

# ==== 8) Visualizar red base ====
plot_network_time(g, PT = PT, coords = coords,
                  main = "Distribution network — edges weighted by estimated time (min)")

plot_network(g, PT = PT, demand = d, surplus = s, coords = coords,
             main = "Distribution network — optimization by distance (blocks)")

# ==== 9) Ejecutar algoritmos ====

results <- run_all_algorithms(
  outer_abbr,
  FUN = function(route, ...) objective_numeric(route, graph=g, PT=PT, S0=S0, demand=d, surplus=s),
  graph = g, PT = PT, S0 = S0, demand = d, surplus = s
)

# ==== 10) Identificar mejor resultado ====
all_costs <- sapply(results, function(x) x$best_cost)
best_idx <- which.min(all_costs)
best_res <- results[[best_idx]]

# ---- obtener valores detallados ----
best_detail <- objective_distance_extended(best_res$best_route, g, PT, S0, d, s,
                                           minutes_per_block, cost_per_min_eur,
                                           penalty_per_unit)

cat("\n=== MEJOR RESULTADO ===\n")
cat("Método:", best_res$method, "\n")
cat("Ruta óptima:\nPT ->", paste(best_res$best_route, collapse = " -> "), "-> PT\n")
cat("Distancia total (bloques):", round(best_detail$distance_blocks, 2), "\n")
cat("Tiempo estimado (min):", round(best_detail$time_minutes, 2), "\n")
cat("Coste operativo (€):", round(best_detail$cost_eur, 2), "\n")
cat("Penalización aplicada (bloques):", round(best_detail$penalty_blocks, 2), "\n")

# ==== 11) Graficar mejor ruta ====
plot_best_route(g, route = best_res$best_route, PT = PT, coords = coords,
                color_route = "darkred")

# ==== 12) Exportar resultados ====
# ================================================================
# Crear summary_df con información extendida (sin exportar a CSV)
# ================================================================
summary_df <- data.frame(
  Method = sapply(results, function(x) x$method),
  Distance_Blocks = round(sapply(results, function(x) x$best_cost), 2),
  Time_Seconds = round(sapply(results, function(x) x$time_secs), 3),
  
  Time_Min = round(sapply(results, function(x)
    objective_distance_extended(
      x$best_route, g, PT, S0, d, s,
      minutes_per_block, cost_per_min_eur, penalty_per_unit
    )$time_minutes), 2),
  
  Cost_EUR = round(sapply(results, function(x)
    objective_distance_extended(
      x$best_route, g, PT, S0, d, s,
      minutes_per_block, cost_per_min_eur, penalty_per_unit
    )$cost_eur), 2),
  
  Penalty_Blocks = round(sapply(results, function(x)
    objective_distance_extended(
      x$best_route, g, PT, S0, d, s,
      minutes_per_block, cost_per_min_eur, penalty_per_unit
    )$penalty_blocks), 2),
  
  Route = sapply(results, function(x)
    paste(x$best_route, collapse = " -> "))
)

cat("\n=== SUMMARY OF RESULTS (EXTENDED) ===\n")
print(summary_df)


a <- summary_df %>%
  filter(Method, Distance_Blocks ,Cost_EUR)  