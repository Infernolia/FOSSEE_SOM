#### generate some RGB data ####

## select the number of random RGB vectors for training data

size_data <- 10000

## generate dataframe of random RGB vectors

sample <- as.data.frame(matrix(nrow = size_data, ncol = 3))
colnames(sample) <- c('R', 'G', 'B')

sample$R <- sample(0:255, size_data, replace = T)
sample$G <- sample(0:255, size_data, replace = T)
sample$B <- sample(0:255, size_data, replace = T)


# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(sample))



#Now lets initialize the weights of the neural network.
#Creating a 5 x 5 neural network.

weights_matrix <- matrix(rnorm(75), nrow = 25, ncol =3 )
colnames(weights_matrix) <-  c('R', 'G', 'B')



#Lets make a function to calculate euclidean distance.
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Three functions for the decaying learning rate, sigma and neighbourhood
decay_radius <- function (init_radius,i,time_constant) init_radius*exp(-i/time_constant)
decay_learning_rate <- function(init_learning_rate,i,n_iter) init_learning_rate* exp(-i/n_iter)
neighbourhood <- function(distance,radius) exp(-(distance**2)/(2*(radius**2)))

#Defining the training parameters
n_iter=2000
init_learning_rate=0.1
init_radius=max(5,5)/2
time_constant=n_iter/log(init_radius)

#As a part of competition process we need to find BMU. So let's create function to find BMU.

find_bmu <- function(random_input)
  {
  
    min_dist=1000000
    min_index = 0
    
    for (i in 1:length(1:25))
    {
      if (euclidean(weights_matrix[i,],random_input) < min_dist)
      {
        min_dist <- euclidean(weights_matrix[i,],random_input)
        min_index <- i
      }
      
      
    }
    return(min_index)
  
}


#Let's train the Self Organizing Map.

tuller <- as.data.frame(weights_matrix)
View(tuller)

input_indices <- 1:size_data

for (i in 1:n_iter)
{
  
  #Randomly selecting input row from given dataset
  current_row_index <- sample(input_indices, 1, replace = F)
  random_input_row <- data_train_matrix[current_row_index,]
  
  r <- decay_radius(init_radius,i,time_constant)
  l <- decay_learning_rate(init_learning_rate,i,n_iter)
  bmu_idx <- find_bmu(random_input_row)
  bmu_row <- bmu_idx/5
  bmu_col <- bmu_idx%%5
  
  
for (x in 1:5)
{
  for (y in 1:5)
  {
    lateral_distance <- euclidean(c(x,y), c(bmu_row, bmu_col))
    neighbourhood_current <- neighbourhood(lateral_distance, r)
    old_weight <- weights_matrix[5*(x-1)+y,]
    new_weight <- old_weight + (l*neighbourhood_current*(random_input_row-old_weight))
    weights_matrix[5*(x-1)+y,1] <- as.numeric(new_weight[1])
    weights_matrix[5*(x-1)+y,2] <- as.numeric(new_weight[2])
    weights_matrix[5*(x-1)+y,3] <- as.numeric(new_weight[3])
  }
}

  
}


ddf<- as.data.frame(weights_matrix)


View(ddf)


#x <- 0:255
#ddf1$R <- ddf$R * attr(ddf$R.x, 'scaled:scale') + attr(ddf$R.x, 'scaled:center')
#ddf1$G <- ddf$G * attr(ddf$G.x, 'scaled:scale') + attr(ddf$G.x, 'scaled:center')
#ddf1$B <- ddf$B * attr(ddf$B.x, 'scaled:scale') + attr(ddf$B.x, 'scaled:center')

write.csv(tuller,"C:/Users/Aboli/Desktop/tmp/FOSSEE/Code/som_old.csv")
write.csv(ddf,"C:/Users/Aboli/Desktop/tmp/FOSSEE/Code/som_new.csv")



ggplot() + geom_point(data = ddf, aes(x = R, y = G,  color = B))
ggplot() + geom_point(data = tuller, aes(x = R, y = G,  color = B))