# Load required libraries
library(dplyr)
library(ggplot2)
library(pso)
library(igraph)
library(animation)

# Load the dataset (adjust path as needed)
internet_data <- read.csv("C:/Users/ASUS/Documents/Internet Speed.csv")

# Quick view
head(internet_data)

# Normalize latency and speeds
internet_data$norm_latency <- scale(internet_data$Ping_latency)
internet_data$norm_download <- scale(internet_data$Download_speed)
internet_data$norm_upload <- scale(internet_data$Upload_speed)

# Simulate loss if not available
internet_data$loss <- runif(nrow(internet_data), 0, 0.2)

# Create a basic tree topology
create_topology <- function() {
  edges <- data.frame(
    from = c("Root", "Root", "A", "A", "B", "B"),
    to = c("A", "B", "C", "D", "E", "F"),
    stringsAsFactors = FALSE
  )
  return(edges)
}

create_chromosome <- function(data, num_links = 6) {
  sampled <- data[sample(1:nrow(data), num_links), ]
  chromosome <- data.frame(
    edge = paste0("Link_", 1:num_links),
    bandwidth = sampled$Upload_speed,
    latency = sampled$Ping_latency,
    loss = sampled$loss
  )
  return(chromosome)
}

evaluate_fitness <- function(chromosome) {
  chromosome$congestion_score <- (0.5 / chromosome$bandwidth) + 
    (0.3 * chromosome$loss) + 
    (0.2 * chromosome$latency)
  return(sum(chromosome$congestion_score))
}

crossover <- function(parent1, parent2) {
  child <- parent1
  mask <- sample(c(TRUE, FALSE), nrow(parent1), replace = TRUE)
  child[mask, 2:4] <- parent2[mask, 2:4]
  return(child)
}

mutate <- function(chromosome, mutation_rate = 0.1) {
  for (i in 1:nrow(chromosome)) {
    if (runif(1) < mutation_rate) {
      chromosome$bandwidth[i] <- runif(1, 1, 100)
      chromosome$latency[i] <- runif(1, 1, 10)
      chromosome$loss[i] <- runif(1, 0, 0.2)
    }
  }
  return(chromosome)
}

# Visualize the optimized topology
topology_animation <- function(history) {
  saveGIF({
    for (solution in history) {
      edges <- create_topology()
      edges$bandwidth <- solution$bandwidth
      edges$latency <- solution$latency
      edges$loss <- solution$loss
      edges$congestion <- (0.5 / edges$bandwidth) + (0.3 * edges$loss) + (0.2 * edges$latency)
      edges$label <- paste("BW:", round(edges$bandwidth, 1), "LAT:", round(edges$latency, 1))
      edges$color <- cut(edges$congestion,
                         breaks = quantile(edges$congestion, probs = c(0, 0.33, 0.66, 1)),
                         labels = c("green", "orange", "red"),
                         include.lowest = TRUE)
      g <- graph_from_data_frame(edges, directed = TRUE)
      plot(g,
           edge.label = edges$label,
           edge.color = as.character(edges$color),
           edge.width = 2,
           vertex.color = "skyblue",
           vertex.label.color = "black",
           vertex.size = 30,
           main = "Animated Network Topology")
    }
  }, movie.name = "ga_network_topology.gif", interval = 0.8, ani.width = 800, ani.height = 600)
}

run_ga <- function(data, generations = 30, pop_size = 10) {
  edges <- create_topology()
  population <- lapply(1:pop_size, function(x) create_chromosome(data))
  
  best_fitness <- Inf
  best_solution <- NULL
  population_history <- list()
  fitness_over_time <- numeric(generations)
  
  for (gen in 1:generations) {
    fitness_scores <- sapply(population, evaluate_fitness)
    cat("Generation", gen, "- Best Fitness:", round(min(fitness_scores), 4), "\n")
    
    fitness_over_time[gen] <- min(fitness_scores)
    if (min(fitness_scores) < best_fitness) {
      best_fitness <- min(fitness_scores)
      best_solution <- population[[which.min(fitness_scores)]]
    }
    
    population_history[[gen]] <- population[[which.min(fitness_scores)]]
    selected <- population[order(fitness_scores)][1:(pop_size/2)]
    
    new_population <- list()
    while (length(new_population) < pop_size) {
      parents <- sample(selected, 2, replace = TRUE)
      child <- crossover(parents[[1]], parents[[2]])
      child <- mutate(child)
      new_population[[length(new_population) + 1]] <- child
    }
    
    population <- new_population
  }
  
  # Plot fitness over generations
  fitness_df <- data.frame(Generation = 1:generations, Fitness = fitness_over_time)
  ggplot(fitness_df, aes(x = Generation, y = Fitness)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "darkred") +
    ggtitle("Fitness vs. Generation") +
    theme_minimal() +
    ylab("Best Fitness") +
    xlab("Generation")
  
  topology_animation(population_history)
  return(list(best_solution = best_solution, fitness_history = fitness_over_time))
  
}

set.seed(42)
ga_result <- run_ga(internet_data)

best_solution <- ga_result$best_solution
fitness_over_time <- ga_result$fitness_history


# Static visualization
visualize_topology <- function(best_solution) {
  edges <- create_topology()
  edges$bandwidth <- best_solution$bandwidth
  edges$latency <- best_solution$latency
  edges$loss <- best_solution$loss
  edges$congestion <- (0.5 / edges$bandwidth) + (0.3 * edges$loss) + (0.2 * edges$latency)
  edges$color <- cut(edges$congestion,
                     breaks = quantile(edges$congestion, probs = c(0, 0.33, 0.66, 1)),
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
       main = "Optimized Network Topology with Congestion Levels")
}

visualize_topology(best_solution)
plot_fitness_over_generations <- function(fitness_values) {
  fitness_df <- data.frame(Generation = 1:length(fitness_values), Fitness = fitness_values)
  
  ggplot(fitness_df, aes(x = Generation, y = Fitness)) +
    geom_line(color = "blue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "Fitness vs. Generation",
      x = "Generation",
      y = "Best Fitness Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# Now plot Graph

#plot_fitness_over_generations(fitness_over_time)
# Load required libraries
library(dplyr)
library(pso)
library(ggplot2)
library(igraph)
library(animation)

# Load and prepare the dataset
internet_data <- read.csv("C:/Users/ASUS/Documents/Internet Speed.csv")
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

# Load required libraries
library(dplyr)
library(ggplot2)
library(igraph)
library(animation)

# Load the dataset
internet_data <- read.csv("C:/Users/ASUS/Documents/Internet Speed.csv")

# Normalize and prepare features
internet_data$norm_latency <- scale(internet_data$Ping_latency)
internet_data$norm_download <- scale(internet_data$Download_speed)
internet_data$norm_upload <- scale(internet_data$Upload_speed)
internet_data$loss <- runif(nrow(internet_data), 0, 0.2)

# Create basic tree topology
create_topology <- function() {
  data.frame(
    from = c("Root", "Root", "A", "A", "B", "B"),
    to = c("A", "B", "C", "D", "E", "F"),
    stringsAsFactors = FALSE
  )
}

# Fitness function
evaluate_fitness <- function(chromosome) {
  chromosome$congestion_score <- (0.5 / chromosome$bandwidth) + 
    (0.3 * chromosome$loss) + 
    (0.2 * chromosome$latency)
  return(sum(chromosome$congestion_score))
}

# Visualize network topology
visualize_topology <- function(solution, title = "Optimized Topology (SA)") {
  edges <- create_topology()
  edges$bandwidth <- solution$bandwidth
  edges$latency <- solution$latency
  edges$loss <- solution$loss
  edges$congestion <- (0.5 / edges$bandwidth) + (0.3 * edges$loss) + (0.2 * edges$latency)
  
  # Prevent non-unique break errors
  if (length(unique(edges$congestion)) < 4) {
    edges$congestion <- jitter(edges$congestion, amount = 1e-4)
  }
  
  edges$color <- cut(edges$congestion,
                     breaks = quantile(edges$congestion, probs = c(0, 0.33, 0.66, 1)),
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
       main = title)
}

# Optional: Animate optimization (limited due to SANN's nature)
animate_sa <- function(history_list) {
  saveGIF({
    for (chromosome in history_list) {
      visualize_topology(chromosome, title = "SA Optimization Step")
    }
  }, movie.name = "sa_network_evolution.gif", interval = 1, ani.width = 800, ani.height = 600)
}

# Run Simulated Annealing optimization
run_sa <- function(data, max_iter = 50) {
  fitness_history <- numeric()
  population_history <- list()
  
  objective <- function(x) {
    chromosome <- data.frame(
      edge = paste0("Link_", 1:6),
      bandwidth = x[1:6],
      latency = x[7:12],
      loss = x[13:18]
    )
    fitness <- evaluate_fitness(chromosome)
    fitness_history <<- c(fitness_history, fitness)
    population_history[[length(fitness_history)]] <<- chromosome
    return(fitness)
  }
  
  result <- optim(par = runif(18, 1, 100),
                  fn = objective,
                  method = "SANN",
                  control = list(maxit = max_iter, trace = TRUE))
  
  optimized <- data.frame(
    edge = paste0("Link_", 1:6),
    bandwidth = result$par[1:6],
    latency = result$par[7:12],
    loss = result$par[13:18]
  )
  
  return(list(
    solution = optimized,
    fitness = fitness_history,
    history = population_history
  ))
}

# Fitness vs Iteration plot
plot_fitness_sa <- function(fitness_vector) {
  df <- data.frame(Iteration = 1:length(fitness_vector), Fitness = fitness_vector)
  ggplot(df, aes(x = Iteration, y = Fitness)) +
    geom_line(color = "blue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    labs(title = "SA: Fitness vs Iteration", x = "Iteration", y = "Fitness Score") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Run SA and visualize
set.seed(123)
sa_result <- run_sa(internet_data,max_iter = 30)

cat("\n✅ Final SA-Optimized Routing Configuration:\n")
print(sa_result$solution)

# Plot fitness curve
plot_fitness_sa(sa_result$fitness)

# Show topology
visualize_topology(sa_result$solution)

# Optional: Save animated evolution (uncomment to run)
animate_sa(sa_result$history)



# Load libraries
library(ggplot2)
library(dplyr)

# Iteration values
iterations <- 1:30

# Replace these with your actual results if needed
ga_mean <- c(5.387, 5.058, 4.341, 3.648, 3.476, 3.191, 2.999, 2.851, 2.785, 3.079,
             2.754, 2.658, 2.588, 2.477, 2.438, 2.396, 2.358, 2.326, 2.312, 2.296,
             2.290, 2.280, 2.277, 2.269, 2.263, 2.262, 2.257, 2.307, 2.359, 2.223)
ga_sd <- c(0.52, 0.464, 0.439, 0.500, 0.502, 0.425, 0.330, 0.234, 0.283, 0.479,
           0.308, 0.353, 0.349, 0.311, 0.279, 0.244, 0.223, 0.199, 0.197, 0.189,
           0.188, 0.182, 0.181, 0.180, 0.174, 0.173, 0.165, 0.259, 0.229, 0.145)

pso_mean <- c(4.516, 4.167, 3.222, 2.466, 1.992, 1.714, 1.583, 1.510, 1.478, 1.556,
              1.421, 1.382, 1.349, 1.342, 1.336, 1.333, 1.329, 1.328, 1.327, 1.326,
              1.326, 1.325, 1.323, 1.323, 1.322, 1.320, 1.319, 1.327, 1.318, 1.309)
pso_sd <- c(0.616, 0.628, 0.556, 0.335, 0.166, 0.116, 0.082, 0.077, 0.071, 0.069,
            0.070, 0.061, 0.057, 0.054, 0.055, 0.053, 0.052, 0.052, 0.054, 0.055,
            0.055, 0.055, 0.053, 0.051, 0.050, 0.048, 0.047, 0.055, 0.051, 0.045)

sa_mean <- c(6.999, 6.784, 6.846, 6.650, 6.135, 5.745, 5.889, 5.705, 5.884, 6.084,
             5.900, 5.720, 5.653, 5.550, 5.357, 5.231, 5.148, 5.040, 4.981, 4.811,
             4.747, 4.733, 4.708, 4.646, 4.610, 4.586, 4.648, 4.855, 4.799, 4.690)
sa_sd <- c(1.708, 1.932, 1.702, 1.404, 1.324, 1.371, 1.537, 1.433, 1.552, 1.761,
           1.665, 1.546, 1.587, 1.455, 1.295, 1.199, 1.107, 1.047, 1.021, 0.944,
           0.941, 0.917, 0.911, 0.851, 0.800, 0.770, 0.919, 1.425, 1.441, 1.344)

# Combine all data into one data frame
df <- data.frame(
  Iteration = rep(iterations, 3),
  Fitness = c(ga_mean, pso_mean, sa_mean),
  SD = c(ga_sd, pso_sd, sa_sd),
  Algorithm = factor(rep(c("GA", "PSO", "SA"), each = 30), levels = c("GA", "PSO", "SA"))
)

# Plot
p<-ggplot(df, aes(x = Iteration, y = Fitness, color = Algorithm, group = Algorithm)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Fitness - SD, ymax = Fitness + SD), width = 0.3) +
  labs(
    title = "Comparison of GA, PSO, and SA Optimization Algorithms",
    subtitle = "Fitness Score vs Iteration (Error Bars = ±1 SD)",
    x = "Iteration / Generation",
    y = "Fitness Score (Lower is Better)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  )
print(p)
