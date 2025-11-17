# 🚚 **Optimization of Distribution Network in R**

### *Metaheuristics • Graph Optimization • Statistical Benchmarking • Simulation Framework*

This project implements a complete **simulation and optimization environment** to analyze a **distribution network** composed of a **central depot (PT)** and multiple **outer stations** with **demand** or **surplus** units. The system is optimized using a broad collection of **metaheuristic algorithms**, and evaluated statistically to determine which performs best.

Developed entirely in **R**, the framework integrates:

* Graph construction (Manhattan distances)
* Scenario simulation (demand & surplus assignment)
* Optimization (GA, DE, PSO, etc.)
* Benchmarking & statistical comparisons
* Automated route plotting and reporting

This corresponds to the requirements of the **Third Delivery** final optimization proyect.

---

# 📦 **Project Structure**

```
OBA-II-Final-Project/
├── class_algorithms.R         # Blind, Hill Climbing, Monte Carlo, SA, Grid Search
├── compare_algorithms.R       # GA vs DE vs PSO benchmarking (Third Delivery)
├── final_stats_benchmark.R    # Statistical evaluation (ANOVA, KW, Wilcoxon, t-tests)
├── final_algorithm.R          # Unified execution pipeline
├── demand_supply.R            # Demand/surplus generator
├── graph_utils.R              # Graph builder (Manhattan distances)
├── plot_utils.R               # Plotting utilities for networks & routes
├── main_Data.R                # Global parameters & scenario initialization
├── stations.R                 # Station name cleaning & PT mapping
└── README.md                  # You are here
```

---

# **1. Stations Module (`stations.R`)**

Handles cleaning and abbreviation of station names, and defines the **central depot PT**.

### Key Functions

* **`abbr()`** — Standardizes and abbreviates station names.
* **`make_mapping()`** — Builds mapping between PT and the outer stations.

---

# **2. Graph Construction (`graph_utils.R`)**

Builds a full **Manhattan-distance network** connecting PT with all stations.

### Key Functions

* **`manhattan_dist()`** — Computes distances between coordinate pairs.
* **`make_graph_with_weights()`** — Generates igraph network with edge weights (distance/time).

This graph is used by all optimization algorithms.

---

# **3. Demand & Surplus Simulation (`demand_supply.R`)**

Creates balanced **demand** and **surplus** station lists.

### Key Functions

* **`build_demand_supply()`** — Assigns balanced quantities for optimization.

---

# **4. Visualization Utilities (`plot_utils.R`)**

All visualizations use **igraph**.

### Key Functions

* **`plot_network()`** — Base network visualization with PT, demand, and surplus.
* **`plot_network_time()`** — Same graph but weighted by time.
* **`plot_best_route()`** — Highlights the optimal route found by any algorithm.

This module now includes your new **custom colors**, cleaner labeling, and improved layout.

---

# **5. Class Algorithms (`class_algorithms.R`)**

### Algorithms:

* **Blind Search**
* **Monte Carlo Search**
* **Grid Search** (local refinement)
* **Hill Climbing**
* **Simulated Annealing**

### third delivery Algorithms implemented:

* **Genetic Algorithm (GA)** — Selection, crossover, mutation
* **Differential Evolution (DE)** — Population-based vector perturbation
* **Particle Swarm Optimization (PSO)** — Velocity & cognitive/social influence coefficients

Each is run **10 times** to gather:

* Best route value per run
* Execution time
* Convergence curve

Stored in:

* `GA_best`, `DE_best`, `PSO_best`
* `GA_time`, `DE_time`, `PSO_time`
* `GA_hist`, `DE_hist`, `PSO_hist`

---

# **6. Benchmarking Script (`compare_algorithms.R`)**

Runs GA, DE, and PSO **multiple times** and collects:

* Average performance
* Variance across runs
* Average convergence curves
* Execution time per algorithm

Outputs a consolidated data frame:

```
Method | Avg_Best | Avg_Time | Var_Best
```

GA, DE, PSO are plotted together showing average convergence.

---

# **7. Statistical Analysis (Third Delivery) — `final_stats_benchmark.R`**

Includes **parametric and non-parametric tests**:

### ✔ Global Tests

* **ANOVA**
* **Kruskal–Wallis**

### ✔ Pairwise Tests

* **t-tests (Bonferroni-adjusted)**
* **Wilcoxon rank-sum (Bonferroni-adjusted)**

### ✔ Interpretation Engine

Automatically generates conclusions:

* Whether differences are significant
* Which algorithm performs best on average
* Evidence from all tests

In your results:
📌 *PSO had the best mean performance and significant improvements vs DE.*

---

# **8. Final Unified Algorithm (`final_algorithm.R`)**

Combines all modules and produces:

* Scenario initialization
* Graph construction
* Demand & surplus allocation
* Route optimization with all algorithms (class + GA/DE/PSO)
* Summary of results
* Best route plot
* Exportable outputs

This is the final file you run when delivering the project.

---

# **9. Scenario Setup (`main_Data.R`)**

Defines all constants and parameters:

* `N_OUTER`, `N_SURPLUS`, `seed`, etc.
* Mapping between PT and outer stations
* Graph construction
* Demand & surplus generation

MUST BE RUN **before** any algorithm:

```r
source("main_Data.R")
```

---

# 🧪 **10. How to Run the Entire Pipeline**

```r
source("main_Data.R")
source("class_algorithms.R")
source("compare_algorithms.R")
source("final_stats_benchmark.R")
source("final_algorithm.R")
```

---
