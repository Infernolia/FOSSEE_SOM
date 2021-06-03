set.seed(222)
library(dplyr)
#install.packages("data.table")
library(data.table)

# 1) Reading the data and scaling it
data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- as.matrix(X)

# 2) euclidean_distance function
euclidean_distance <- function(x, y) {
  ret <- sum((x - y)^2)
  return(ret)
}

# 3) Creating a 16*3 grid for the SOM
create_grid <- function(n,p) {
  ret <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
  return(ret)
}


grid <- create_grid(16,3)



# 1) Decaying radius function
decay_radius_function <- function(radius, current_iteration, time_constant) {
  ret <- radius * exp(-current_iteration / time_constant)
  return(ret)
}

# 2) Decaying learning rate
decay_learning_rate <- function(learning_rate, current_iteration, n_iteration) {
  ret <- learning_rate * exp(-current_iteration / n_iteration)
  return(ret)
}

# 3) A function to calculate influence over neighboring neurons
influence_calculation <- function(distance, radius) {
  ret <- exp(-(distance^2) / (2 * (radius^2)))
  return(ret)
}


BMU_old <- function(x, input_grid) { 
  distance <- 0
  min_distance <- 10000000 # Setting high min dist value
  min_ind <- -1 # Setting random min_ind value
  for (e in 1:nrow(input_grid)) # Iterating through grid
  {
    distance <- euclidean_distance(x, input_grid[e, ]) # euclidean_distance distance
    if (distance < min_distance) {
      min_distance <- distance # Updating min distance for winning unit
      min_ind <- e # Updating winning neuron
    }
  }
  return(min_ind) #returns index of BMU
}

# Function to return winning neuron
BMU <- function(x, input_grid) { #x is a single row of data and input_grid is the grid
  y <- sqrt(colSums((t(input_grid)-x)^2))
  z<- which(y == min(y), arr.ind = TRUE)
  return(z)
}


start <- Sys.time()
end <- Sys.time()
random_input_row <- x[sample(1:nrow(x), size = 1, replace = F), ] 
index <- BMU(random_input_row, grid)
print(index)
end - start




















# Function to create a Self Organizing Map

SOM <- function(x, input_grid) {
  n_iteration <- 400 # Defining number of iterations
  initial_learning_rate <- 0.05 # Defining initial learning rate
  initial_radius <- 3 # Defining initial radius
  time_constant <- n_iteration / log(initial_radius) # Initializing time constant
  for (i in 1:n_iteration) # Looping through for training
  {
    random_input_row <- x[sample(1:nrow(x), size = 1, replace = F), ] # Selecting random input row from given data set
    new_radius <- decay_radius_function(initial_radius, i, time_constant) # Decaying radius
    new_learning_rate <- max(decay_learning_rate(initial_learning_rate, i, n_iteration), 0.01) # Decaying learning rate
    index <- BMU(random_input_row, input_grid) # Finding best matching unit for given input row
    for (j in 1:nrow(input_grid))
    {
      old_weights <- input_grid[j, ]
      lateral_distance <- euclidean_distance(c(index / (sqrt(nrow(input_grid))+1), index %% (sqrt(nrow(input_grid))+1)), c(j / (sqrt(nrow(input_grid))+1), j %% (sqrt(nrow(input_grid))+1))) # Distance between returned BMU and current neuron
      if (lateral_distance <= new_radius) # If w_dist is less than radius
      {
        influence <- influence_calculation(lateral_distance, new_radius) # Influencing other nodes
        new_weights <- old_weights + (new_learning_rate * influence * (random_input_row - old_weights)) # Calculating new weights
        for (k in 1:3)
        {
          input_grid[j, k] <- new_weights[1,k] # Updating new weights
        }
      }
    }
  }
  return(input_grid)
}

gridSOM=SOM(data,grid)
gridSOM
