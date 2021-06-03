#This is the R Script for loading the data and creating the grid.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Reading the data and scaling it

data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- X

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) A function to return the sum of squared distance between x and y.

euclidean_distance <- function(x, y) {
  ret <- sum((x - y)^2)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) Function to create a SOM grid.
# n is the number of neurons.
# p is the number of columns in the original dataframe.
# data table is used for faster computation.

create_grid <- function(n,p) {
  ret <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a 4*4 grid using the function defined above.
set.seed(222)
grid <- create_grid(16,3)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


#This is the R Script that contains functions to decay radius, learning rate and calculate influence.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Function to decay the radius exponentially over time. 
# rds is the initial radius that is passed.
# cur_iter represents the current iteration.
# time_constant is the time constant that is calculated before the 

decay_radius_function <- function(radius, current_iteration, time_constant) {
  ret <- radius * exp(-current_iteration / time_constant)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) Function to decay the learning rate.
# lr is the current learning rate.
# cur_iter is the current iteration
# n_iteration is the number of iterations.

decay_learning_rate <- function(learning_rate, current_iteration, n_iteration) {
  ret <- learning_rate * exp(-current_iteration / n_iteration)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) A function to calculate influence over neighboring neurons
#dstnc is the lateral distance.
#rds is the current neighbourhood radius.

influence_calculation <- function(distance, radius) {
  ret <- exp(-(distance^2) / (2 * (radius^2)))
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#R Script to find the winning neuron / Best Matching unit of a SOM grid

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#R Script to find the winning neuron / Best Matching unit of a SOM grid

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Vectorized Implementation - Function to return the winning neuron.
# x is a single row of data and input_grid is the grid

BMU <- function(x, input_grid) { #x is a single row of data and input_grid is the grid
  y <- sqrt(colSums((t(input_grid)-x)^2))
  z<- which(y == min(y), arr.ind = TRUE)
  return(z)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest BMU Implementation using vectorisation.
#x is a single row of data and input_grid is the grid

BMU_Vectorised <- function(x, input_grid) { 
  dist_mtrx=rowSums(sweep(input_grid,2,x)^2) #Calculating the distance of this row from all the neurons using matrix operations.
  min_ind=which.min(dist_mtrx) #Finding the location of the neuron with the minimum distance.
  return (min_ind-1) #Returning the zero-indexed value of the winning neuron.
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#R Script to create a Self Organising Map.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Importing the Library

library(dplyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest implmentation of a self organising map.
#x is the input and input_grid is the SOM grid that will be updated iteratively.

SOM <- function(x, input_grid) {
  n_iteration <- 400 # Defining number of iterations
  initial_learning_rate <- 0.05 # Defining initial learning rate
  initial_radius <- 3 # Defining initial radius
  time_constant <- n_iteration / log(initial_radius) # Initializing time constant
  lateral_distance_points=expand.grid(1:sqrt(nrow(input_grid)),1:sqrt(nrow(input_grid)))#Initialising physical locations of neurons to figure out lateral distance.
  rows=sqrt(nrow(input_grid)) #The square grid is used here - so taking the number of rows as square root of number of entries in the grid.
  n_epochs=10 #Defining the number of epochs.
  for(ne in 1:n_epochs)
  {
    print(ne)
    old_grid=input_grid
    for (i in 1:n_iteration) # Looping through for training
    {
      sample_input_row <- as.vector(unlist(x[sample(1:nrow(x), size = 1, replace = F), ])) # Selecting random input row from given data set
      print(sample_input_row)
      break
      new_radius <- decay_radius_function(initial_radius, i, time_constant) # Decaying radius
      new_learning_rate <- max(decay_learning_rate(initial_learning_rate, i, n_iteration), 0.01) # Decaying learning rate
      index_temp <- BMU_Vectorised(sample_input_row, input_grid) # Finding best matching unit for given input row
      index_new=c((as.integer(index_temp/rows))+1,(index_temp%%rows)+1) #Converting a 1D co-ordinate to a 2D co-ordinate for finding lateral distance on the map.
      lateral_distance=sqrt(rowSums(sweep(lateral_distance_points,2,index_new)^2)) #Finding Euclidean distance between the given best matching units and all units on the map.
      rn=which(lateral_distance<=new_radius) #Finding neurons that are within the radius of the winning unit.
      inf=influence_calculation(lateral_distance[rn],new_radius) #Calculating the influence of the winning neuron on neighbours.
      diff_grid=(sweep(input_grid[rn,],2,sample_input_row))*-1 #A temporary matrix that stores the difference between the data point and the weights of the winning neuron & neighbours.
      updated_weights=new_learning_rate*inf*diff_grid #The updating operation on the winning and neighbouring neurons.
      input_grid[rn,]=input_grid[rn,]+updated_weights #Now updating those grid entries that are either the winning neuron or its neighbours.
      if(isTRUE(all.equal(old_grid,input_grid)))
      {
        print(i)
        print("Converged")
      }
    }
  }
  return(input_grid) #Returning the updated SOM weights.
}

gridSOM=SOM(data,grid) 
gridSOM

rm(list=ls())