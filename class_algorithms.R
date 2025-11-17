# ================================================================
# class_algorithms.R — unified and improved versions of class methods
# Adapted to route optimization minimizing Manhattan distance
# ================================================================

#install.packages("genalg")
#install.packages("DEoptim")
#install.packages("pso")

# ------------------------------------------------
# 0. Helper: generate permutations (for small domains)
# ------------------------------------------------
generate_permutations <- function(v) {
  if (length(v) == 1) return(list(v))
  res <- list()
  for (i in seq_along(v)) {
    rest <- v[-i]
    sub_perms <- generate_permutations(rest)
    for (perm in sub_perms) {
      res <- append(res, list(c(v[i], perm)))
    }
  }
  res
}

# ------------------------------------------------
# 1. BLIND SEARCH
# ------------------------------------------------
fsearch <- function(outer_abbr, FUN, samples = 1000, ...) {
  t_start <- proc.time()
  
  best_cost <- Inf
  best_route <- NULL
  
  for (i in 1:samples) {
    route <- sample(outer_abbr)
    cost <- FUN(route, ...)
    
    if (cost < best_cost) {
      best_cost <- cost
      best_route <- route
    }
  }
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  list(
    method = "Blind Search",
    best_route = best_route,
    best_cost = best_cost,
    time_secs = exec_time,
    history = rep(best_cost, samples)
  )
}


# ------------------------------------------------
# 2. MONTE CARLO SEARCH
# ------------------------------------------------
mcsearch <- function(N, outer_abbr, FUN, type = "min", ...) {
  t_start <- proc.time()
  
  routes <- replicate(N, sample(outer_abbr), simplify = FALSE)
  costs <- sapply(routes, FUN, ...)
  
  idx <- ifelse(type == "min", which.min(costs), which.max(costs))
  best_route <- routes[[idx]]
  best_cost <- costs[idx]
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  list(
    method = "Monte Carlo Search",
    best_route = best_route,
    best_cost = best_cost,
    time_secs = exec_time,
    history = cummin(costs)
  )
}


# ------------------------------------------------
# 3. GRID SEARCH (local refinement)
# ------------------------------------------------
gsearch <- function(levels = 3, outer_abbr, FUN, type = "min", ...) {
  t_start <- proc.time()
  
  current_best <- sample(outer_abbr)
  best_cost <- FUN(current_best, ...)
  history <- numeric(levels)
  history[1] <- best_cost
  
  for (level in 1:levels) {
    neighbors <- replicate(20, {
      r <- current_best
      swap <- sample(1:length(r), 2)
      r[swap] <- r[rev(swap)]
      r
    }, simplify = FALSE)
    
    costs <- sapply(neighbors, FUN, ...)
    idx <- ifelse(type == "min", which.min(costs), which.max(costs))
    current_best <- neighbors[[idx]]
    best_cost <- costs[idx]
    history[level] <- best_cost
  }
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  list(
    method = "Grid Search",
    best_route = current_best,
    best_cost = best_cost,
    time_secs = exec_time,
    history = history
  )
}


# ------------------------------------------------
# 4. HILL CLIMBING
# ------------------------------------------------
hclimbing <- function(par, fn, maxit = 1000, report = 200, type = "min", ...) {
  t_start <- proc.time()
  
  best <- par
  fbest <- fn(par, ...)
  history <- numeric(maxit)
  history[1] <- fbest
  
  for (i in 1:maxit) {
    neighbor <- best
    swap <- sample(1:length(best), 2)
    neighbor[swap] <- neighbor[rev(swap)]
    fneighbor <- fn(neighbor, ...)
    
    improved <- (type == "min" && fneighbor < fbest)
    if (improved) {
      best <- neighbor
      fbest <- fneighbor
    }
    
    history[i] <- fbest
  }
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  list(
    method = "Hill Climbing",
    best_route = best,
    best_cost = fbest,
    time_secs = exec_time,
    history = history
  )
}


# ------------------------------------------------
# 5. SIMULATED ANNEALING (R base)
# ------------------------------------------------
sannealing <- function(par, fn, graph, PT, S0, demand, surplus,
                       penalty_per_unit = 0.5,
                       T_init = 1000, T_min = 1e-3, alpha = 0.95,
                       maxit = 3000, report = 500) {
  
  t_start <- proc.time()
  
  current_route <- par
  current_cost  <- fn(par, graph=graph, PT=PT, S0=S0,
                      demand=demand, surplus=surplus,
                      penalty_per_unit=penalty_per_unit)
  
  best_route <- current_route
  best_cost  <- current_cost
  T <- T_init
  
  history <- numeric(maxit)
  history[1] <- current_cost
  
  for (i in 1:maxit) {
    neighbor <- current_route
    swap <- sample(1:length(neighbor), 2)
    neighbor[swap] <- neighbor[rev(swap)]
    
    neighbor_cost <- fn(neighbor, graph=graph, PT=PT, S0=S0,
                        demand=demand, surplus=surplus,
                        penalty_per_unit=penalty_per_unit)
    
    delta <- neighbor_cost - current_cost
    if (delta < 0 || runif(1) < exp(-delta / T)) {
      current_route <- neighbor
      current_cost <- neighbor_cost
    }
    
    if (current_cost < best_cost) {
      best_route <- current_route
      best_cost  <- current_cost
    }
    
    history[i] <- best_cost
    T <- alpha * T
    if (T < T_min) break
  }
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  list(
    method = "Simulated Annealing",
    best_route = best_route,
    best_cost = best_cost,
    time_secs = exec_time,
    history = history
  )
}


tabu_search <- function(
    par, fn, 
    graph, PT, S0, demand, surplus,
    penalty_per_unit = 0.5,
    tabu_size = 30,
    maxit = 800,         # un poco menos de iteraciones
    report = 200
) {
  t_start <- proc.time()   # ---- INICIO TIMER
  
  best <- par
  best_cost <- fn(best, graph=graph, PT=PT, S0=S0,
                  demand=demand, surplus=surplus,
                  penalty_per_unit=penalty_per_unit)
  
  current <- best
  current_cost <- best_cost
  
  tabu_list <- list()
  best_history <- numeric(maxit)
  best_history[1] <- best_cost
  
  for (i in 1:maxit) {
    
    neighbors <- replicate(20, {
      n <- current
      pos <- sample(1:length(n), 2)
      n[pos] <- n[rev(pos)]
      n
    }, simplify = FALSE)
    
    candidate_costs <- sapply(neighbors, fn, graph=graph, PT=PT,
                              S0=S0, demand=demand, surplus=surplus,
                              penalty_per_unit=penalty_per_unit)
    
    order_idx <- order(candidate_costs)
    selected <- NULL
    
    for (idx in order_idx) {
      cand <- neighbors[[idx]]
      key <- paste(cand, collapse = "-")
      if (!(key %in% tabu_list)) {
        selected <- cand
        selected_cost <- candidate_costs[idx]
        tabu_list <- c(key, tabu_list)
        if (length(tabu_list) > tabu_size) {
          tabu_list <- tabu_list[1:tabu_size]
        }
        break
      }
    }
    
    if (!is.null(selected)) {
      current <- selected
      current_cost <- selected_cost
      
      if (current_cost < best_cost) {
        best <- current
        best_cost <- current_cost
      }
    }
    
    best_history[i] <- best_cost
    
    if (report > 0 && i %% report == 0) {
      cat(sprintf("Iter %d | Tabu best = %.2f\n", i, best_cost))
    }
  }
  
  exec_time <- (proc.time() - t_start)[["elapsed"]]  # ---- FIN TIMER
  
  list(
    method = "Tabu Search",
    best_route = best,
    best_cost = best_cost,
    history = best_history,
    time_secs = exec_time
  )
}
 
library(genalg)

ga_search <- function(
    outer_abbr, fn,
    graph, PT, S0, demand, surplus,
    penalty_per_unit = 0.5,
    popSize = 30,     # antes 50
    iters  = 80       # antes 200
) {
  eval_wrap <- function(route_ord) {
    route <- outer_abbr[order(route_ord)]
    fn(route, graph=graph, PT=PT, S0=S0,
       demand=demand, surplus=surplus,
       penalty_per_unit=penalty_per_unit)
  }
  
  t_start <- proc.time()
  G <- rbga(
    stringMin = rep(0, length(outer_abbr)),
    stringMax = rep(1, length(outer_abbr)),
    popSize  = popSize,
    iters    = iters,
    evalFunc = eval_wrap,
    verbose  = FALSE
  )
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  best_idx   <- which.min(G$evaluations)
  best_route <- outer_abbr[order(G$population[best_idx, ])]
  best_cost  <- min(G$evaluations)
  
  list(
    method      = "Genetic Algorithm",
    best_route  = best_route,
    best_cost   = best_cost,
    history     = G$best,   # best fitness por generación
    time_secs   = exec_time
  )
}

library(DEoptim)

de_search <- function(
    outer_abbr, fn,
    graph, PT, S0, demand, surplus,
    penalty_per_unit = 0.5,
    itermax = 80,    # antes 200
    NP      = 30     # antes 50
) {
  eval_wrap <- function(x) {
    route <- outer_abbr[order(x)]
    fn(route, graph=graph, PT=PT, S0=S0,
       demand=demand, surplus=surplus,
       penalty_per_unit=penalty_per_unit)
  }
  
  lower <- rep(0, length(outer_abbr))
  upper <- rep(1, length(outer_abbr))
  
  control <- DEoptim.control(
    itermax = itermax,
    NP      = NP,
    trace   = 10,       # algo de feedback
    storepopfrom = 1,
    storepopfreq = 1
  )
  
  t_start <- proc.time()
  res <- DEoptim(
    fn    = eval_wrap,
    lower = lower,
    upper = upper,
    control = control
  )
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  best_route <- outer_abbr[order(res$optim$bestmem)]
  best_cost  <- res$optim$bestval
  
  # intentar sacar historia de mejor valor
  best_history <- tryCatch(
    res$member$bestvalit,
    error = function(e) rep(NA_real_, itermax)
  )
  
  list(
    method     = "Differential Evolution",
    best_route = best_route,
    best_cost  = best_cost,
    history    = best_history,
    time_secs  = exec_time
  )
}

library(pso)

pso_search <- function(
    outer_abbr, fn,
    graph, PT, S0, demand, surplus,
    penalty_per_unit = 0.5,
    maxit = 100,   # antes 200
    s     = 30     # antes 50
) {
  eval_wrap <- function(x) {
    route <- outer_abbr[order(x)]
    fn(route, graph=graph, PT=PT, S0=S0,
       demand=demand, surplus=surplus,
       penalty_per_unit=penalty_per_unit)
  }
  
  lower <- rep(0, length(outer_abbr))
  upper <- rep(1, length(outer_abbr))
  
  C <- list(
    maxit       = maxit,
    s           = s,
    trace       = 1,
    REPORT      = 1,
    trace.stats = TRUE
  )
  
  t_start <- proc.time()
  res <- psoptim(
    par   = rep(NA, length(outer_abbr)),
    fn    = eval_wrap,
    lower = lower,
    upper = upper,
    control = C
  )
  exec_time <- (proc.time() - t_start)[["elapsed"]]
  
  best_route <- outer_abbr[order(res$par)]
  best_cost  <- res$value
  
  best_history <- tryCatch(
    res$stats$error,
    error = function(e) rep(NA_real_, maxit)
  )
  
  list(
    method     = "Particle Swarm Optimization",
    best_route = best_route,
    best_cost  = best_cost,
    history    = best_history,
    time_secs  = exec_time
  )
}


# ------------------------------------------------
# 6. RUNNER — unified comparison
# ------------------------------------------------
run_all_algorithms <- function(
    outer_abbr, FUN, graph, PT, S0, demand, surplus,
    penalty_per_unit = 0.5,
    samples = 300, maxit = 1000
) {
  results <- list()
  
  cat("\n=== BLIND SEARCH ===\n")
  results$blind <- fsearch(
    outer_abbr, FUN, samples = samples,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$blind)
  
  cat("\n=== MONTE CARLO SEARCH ===\n")
  results$mc <- mcsearch(
    N = samples, outer_abbr, FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$mc)
  
  cat("\n=== GRID SEARCH ===\n")
  results$grid <- gsearch(
    levels = 3, outer_abbr, FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$grid)
  
  cat("\n=== HILL CLIMBING ===\n")
  results$hill <- hclimbing(
    par = sample(outer_abbr), fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit,
    maxit = maxit
  )
  print(results$hill)
  
  cat("\n=== SIMULATED ANNEALING ===\n")
  results$sa <- sannealing(
    par = sample(outer_abbr), fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit,
    maxit = 3000
  )
  print(results$sa)
  
  cat("\n=== TABU SEARCH ===\n")
  results$tabu <- tabu_search(
    par = sample(outer_abbr), fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$tabu)
  
  cat("\n=== GENETIC ALGORITHM ===\n")
  results$ga <- ga_search(
    outer_abbr, fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$ga)
  
  cat("\n=== DIFFERENTIAL EVOLUTION ===\n")
  results$de <- de_search(
    outer_abbr, fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$de)
  
  cat("\n=== PARTICLE SWARM OPTIMIZATION ===\n")
  results$pso <- pso_search(
    outer_abbr, fn = FUN,
    graph = graph, PT = PT, S0 = S0, demand = demand,
    surplus = surplus, penalty_per_unit = penalty_per_unit
  )
  print(results$pso)
  
  # --- Identify best ---
  all_costs <- sapply(results, function(x) x$best_cost)
  best_idx <- which.min(all_costs)
  best_method <- names(results)[best_idx]
  
  cat("\n=== SUMMARY (RUNNER) ===\n")
  cat("Best method:", best_method, "\n")
  cat("Best cost:", all_costs[best_idx], "\n")
  cat("Route:", paste(results[[best_idx]]$best_route, collapse = " -> "), "\n")
  
  return(results)
}
