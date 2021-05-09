library(ggplot2)

set.seed(11)

#### generate some RGB data ####

## select the number of random RGB vectors for training data

size_data <- 10000

## generate dataframe of random RGB vectors

n <- 3 #n is the number of input dimensions

sample <- as.data.frame(matrix(nrow = size_data, ncol = n))
colnames(sample) <- paste0("a" , 1:n)


for(i in 1:n) #iterating over columns
{

  sample[,i]<- sample(0:255, size_data, replace = T) #RGB data of 10000 rows
}

View(sample)

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
#data_train_matrix <- as.matrix(scale(sample))
data_train_matrix <- as.matrix(sample)


#Now lets initialize the weights of the neural network.
#Creating a p x q neural network.
p <- 5
q <- 5



#weights_matrix <- matrix(rnorm(p*q*n), nrow = (p*q), ncol = n)
weights_matrix <- matrix(runif(p*q*n, min=0, max=255), nrow = (p*q), ncol = n)
 
colnames(weights_matrix) <-  colnames(data_train_matrix)



#Lets make a function to calculate euclidean distance.
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Three functions for the decaying learning rate, sigma and neighbourhood
decay_radius <- function (init_radius,i,time_constant) init_radius*exp(-i/time_constant)
decay_learning_rate <- function(init_learning_rate,i,n_iter) init_learning_rate* exp(-i/n_iter)
neighbourhood <- function(distance,radius) exp(-(distance**2)/(2*(radius**2)))

#Defining the training parameters
n_iter=2000
init_learning_rate=0.1
init_radius=max(p,q)/2
time_constant=n_iter/log(init_radius)

#As a part of competition process we need to find BMU. So let's create function to find BMU.

find_bmu <- function(random_input)
{
  
  min_dist=1000000
  min_index = 0
  
  for (i in 1:length(1:(p*q)))
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
  bmu_row <- bmu_idx/q
  bmu_col <- bmu_idx%%q
  
  
  for (x in 1:p)
  {
    for (y in 1:q)
    {
      lateral_distance <- euclidean(c(x,y), c(bmu_row, bmu_col))
      neighbourhood_current <- neighbourhood(lateral_distance, r)
      old_weight <- weights_matrix[q*(x-1)+y,]
      new_weight <- old_weight + (l*neighbourhood_current*(random_input_row-old_weight))
      for(t in 1:n)
      {
      weights_matrix[q*(x-1)+y,t] <- as.numeric(new_weight[t])
      }
    
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



ggplot() + geom_point(data = ddf, aes(x = a1, y = a2,  color = a3))
ggplot() + geom_point(data = tuller, aes(x = a1, y = a2,  color = a3))

plot(data_train_matrix, col="red", pch = 1)

for(i in 1:p*q){
  for (j in 1:n) {
    points(weights_matrix[i,j], col="blue", pch=2)
  }
}




colors <- ddf
colnames(colors) <- c("red","green","blue")
colors$red <- as.numeric(colors$red)
colors$green <- as.numeric(colors$green)
colors$blue <- as.numeric(colors$blue)
colors$hex = rgb(colors$red, colors$blue, colors$green, maxColorValue = 255)

View(colors)

# Map each color to an x-coordinate for easy plotting.
colors2d <-sapply(1:nrow(colors), function(row) {
  color <- colors[row,]
  (color$red * 256 * 256) + (color$green * 256) + color$blue
})
colors$x <- colors2d


# Plot the colors - the more data, the more apparent the gradient will be.
plot(x=colors$x, col=colors$hex, pch=19, cex=2, main='Colors', xlab='', ylab='2D Color', yaxt='n', xaxt='n')





