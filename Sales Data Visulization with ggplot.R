####
## A mini project I've done in R to help a company to visulize its sales data with ggplot.
####


install.packages('readxl')
library(readxl)
install.packages('tidyr',repos = 'http://cran.us.r-project.org')
library(tidyr)
install.packages('dplyr')
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

df_raw <- read_excel("~/Downloads/Cosmos_Data/Sales Summary by Item - year.xlsx", sheet = "Table", na = "0")
df_raw[is.na(df_raw)] <- 0

month <-as.factor(1:12)
# class(month)
df <- cbind(month, df_raw)
# class(df)
# head(df)
# tail(df)
# str(df)

item <- colnames(df)

for(i in 2:ncol(df)){ 
  plot <- ggplot(df, aes(month, df[ ,i])) + geom_point(color="dark blue") + xlab("Month") + ylab("Sale Qty.") + ggtitle(item[i])

  max <- max(df[,i], na.rm=TRUE)
  min <- min(df[,i], na.rm=TRUE)
  mean <- round(mean(df[,i], na.rm=TRUE), 2)
  median <- median(df[,i], na.rm=TRUE)
  sd <- round(sd(df[,i], na.rm=TRUE), 2)
  
  print(paste('Sales Plot for', item[i], ':'))
  print(plot)
  print(paste('The maximum sales quantity for ', colnames(df)[i], ' is ', max))
  print(paste('The minimum sales quantity for ', colnames(df)[i], ' is ', min))
  print(paste('The average sales quantity for the year for ', colnames(df)[i], ' is ', mean))
  print(paste('The median sales quantity for the year for ', colnames(df)[i], ' is ', median))
  print(paste('The standard deviation for the sales quantity for ', colnames(df)[i], ' is ', sd))
  cat("\n")
  cat("\n")
}


 

 
 
