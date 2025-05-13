# Load required libraries
library(dplyr)
library(pso)
library(ggplot2)
library(igraph)
library(animation)

# Load and prepare the dataset
internet_data <- read.csv("Internet Speed.csv")
head(internet_data)

# Normalize features and simulate packet loss
internet_data$norm_latency <- scale(internet_data$Ping_latency)
internet_data$norm_download <- scale(internet_data$Download_speed)
internet_data$norm_upload <- scale(internet_data$Upload_speed)
internet_data$loss <- runif(nrow(internet_data), 0, 0.2)

# Fitness function
evaluate_fitness <- function(chromosome) {
  chromosome$congestion_score <- (0.5 / chromosome$bandwidth) +
    (0.3 * chromosome$loss) +
    (0.2 * chromosome$latency)
  return(sum(chromosome$congestion_score))
}

# Create basic tree topology
create_topology <- function() {
  data.frame(
    from = c("Root", "Root", "A", "A", "B", "B"),
    to = c("A", "B", "C", "D", "E", "F"),
    stringsAsFactors = FALSE
  )
}

# Visualize network topology
visualize_topology <- function(solution) {
  edges <- create_topology()
  edges$bandwidth <- solution$bandwidth
  edges$latency <- solution$latency
  edges$loss <- solution$loss
  
  # Compute congestion score
  edges$congestion <- (0.5 / edges$bandwidth) + 
    (0.3 * edges$loss) + 
    (0.2 * edges$latency)
  
  # Apply jitter to avoid duplicate values
  congestion_vals <- jitter(edges$congestion, factor = 1e-3)
  breaks <- quantile(congestion_vals, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
  
  # Ensure uniqueness of breakpoints
  if (length(unique(breaks)) < 4) {
    breaks <- sort(unique(c(
      min(congestion_vals),
      median(congestion_vals),
      max(congestion_vals - 1e-3),
      max(congestion_vals)
    )))
  }
  
  # Prevent further cut errors
  if (length(unique(breaks)) < 4) {
    breaks <- c(0, 0.33, 0.66, 1)  # hardcoded fallback
    edges$congestion <- scales::rescale(edges$congestion)  # scale 0–1
  }
  
  edges$color <- cut(edges$congestion,
                     breaks = breaks,
                     labels = c("green", "orange", "red"),
                     include.lowest = TRUE)
  
  g <- graph_from_data_frame(edges, directed = TRUE)
  plot(g,
       edge.label = round(edges$congestion, 2),
       edge.color = as.character(edges$color),
       edge.width = 2,
       vertex.color = "skyblue",
       vertex.label.color = "black",
       vertex.size = 30,
       main = "PSO Optimized Network Topology with Congestion Levels")
}

# PSO Optimization
run_pso <- function(data) {
  fitness_log <- numeric()
  solution_history <- list()
  
  objective <- function(x) {
    chromosome <- data.frame(
      edge = paste0("Link_", 1:6),
      bandwidth = x[1:6],
      latency = x[7:12],
      loss = x[13:18]
    )
    fit <- evaluate_fitness(chromosome)
    fitness_log <<- c(fitness_log, fit)
    solution_history[[length(fitness_log)]] <<- chromosome
    return(fit)
  }
  
  result <- psoptim(par = runif(18, min = 1, max = 100),
                    fn = objective,
                    lower = rep(1, 18),
                    upper = c(rep(100, 6), rep(10, 6), rep(0.2, 6)),
                    control = list(maxit = 30, trace = 1))
  
  optimized <- data.frame(
    edge = paste0("Link_", 1:6),
    bandwidth = result$par[1:6],
    latency = result$par[7:12],
    loss = result$par[13:18]
  )
  
  return(list(solution = optimized, fitness_history = fitness_log, history = solution_history))
}

# Plot fitness progression
plot_fitness_pso <- function(fitness_values) {
  fitness_df <- data.frame(Iteration = 1:length(fitness_values), Fitness = fitness_values)
  
  ggplot(fitness_df, aes(x = Iteration, y = Fitness)) +
    geom_line(color = "blue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "PSO: Fitness vs. Iteration",
      x = "Iteration",
      y = "Fitness Score"
    ) +
    theme_minimal()
}

animate_topology_pso <- function(history_list, max_frames = 10, step = 1) {
  saveGIF({
    for (i in seq(1, min(length(history_list), max_frames), by = step)) {
      visualize_topology(history_list[[i]])
    }
  }, movie.name = "pso_network_evolution.gif", interval = 0.8, ani.width = 800, ani.height = 600)
}


# --- Run everything ---
set.seed(42)
pso_result <- run_pso(internet_data)

cat("\nBest optimized routing configuration (PSO):\n")
print(pso_result$solution)

# Fitness curve
plot_fitness_pso(pso_result$fitness_history)

# Final network visualization
visualize_topology(pso_result$solution)

# Animation (optional, creates GIF)
animate_topology_pso(pso_result$history)
