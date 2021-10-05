# Reading the COVID-19 cases data set.
df <- read.csv("Resources/summarymohfw1update.csv",header=FALSE)

# There are 571 dates on which the cases were recorded in the data set. By iterating
# over all these dates the confirmed cases will be calculated by adding the corresponding
# number of cases of Indian and Foreign nationals infected with COVID-19.

for(i in 1:571)
{
  tcin <- 4*(i-1) + 2
  tcif <- 4*(i-1) + 3
  cured <- 4*(i-1) + 4
  death <- 4*(i-1) + 5
  
  date <- df[1,4*(i-1) + 2]
  colname <- paste0("conf",i)
  df$a <- 0
  df[1,colname] <- date
  df[2,colname] <- "Confirmed"
  for(j in 3:39)
  {
    df[j,colname] <- as.integer(df[j,tcin]) + as.integer(df[j,tcif])
  }
  
  names(df)[ncol(df)] <- colname
  
}

# Removing the redundant column 'a' created during the above step of preprocessing.
df = subset(df,select = -c(2286))
  
# Creating an empty data frame to store the final cases data. 
finaldf = data.frame()

# Appending the cases data in a common format of Date, State, Indian National cases,
# Foreign National cases, Cured, Deaths and Total cases in the 'finaldf' dataframe. 
for(i in 1:571)
{
  tcin <- 4*(i-1) + 2
  tcif <- 4*(i-1) + 3
  cured <- 4*(i-1) + 4
  death <- 4*(i-1) + 5
  
  date <- df[1,4*(i-1) + 2]
  colname <- paste0("conf",i)
  tempdf = subset(df,select = c(1,tcin,tcif,cured,death))
  tempdf$Confirmed <- 0
  for(j in 3:39)
  {
    tempdf[j,"Confirmed"] <- as.integer(df[j,colname])
  }
  
  tempdf$Date <- date
  tempdf <- tempdf[-c(1,2,39), ]
  colnames(tempdf)<- c("State.UnionTerritory","ConfirmedIndianNational","ConfirmedForeignNational","Cured","Deaths","Confirmed","Date")
  tempdf  <- tempdf[, c(7,1,2,3,4,5,6)]
  if(i==1)
  {
    finaldf <- tempdf
  }
  else
  {
    finaldf <- rbind.data.frame(finaldf,tempdf)
  }
  
}

# Saving the final data frame to a csv file.
write.csv(finaldf,"Resources/covid_data.csv")