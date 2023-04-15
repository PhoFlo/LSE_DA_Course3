# Set the directory.
getwd()
setwd(dir='C:/Users/Phoebe/Documents/Data_Analytics/LSE_DA301_Module_4_files/Data')

############################################

# (1) Import the data.

# Import tidyverse.
library(tidyverse)

# Create data frame.
data <- read.csv("bike_details.csv", header = T)

# View and sense-check the data frame.
View(data)
str(data)
dim(data)
typeof(data)
class(data)

# (2) Export the data.
write_csv(data,file='data_output/bike_data.csv')