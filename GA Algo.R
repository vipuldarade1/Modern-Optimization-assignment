# Load required libraries
library(dplyr)
library(ggplot2)
library(pso)
library(igraph)
library(animation)

# Load the dataset (adjust path as needed)
internet_data <- read.csv("Internet Speed.csv")

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


