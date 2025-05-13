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

