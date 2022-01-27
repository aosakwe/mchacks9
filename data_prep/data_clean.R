#Generating Formatted CSV file to use in analysis
#This script was only necessary to generate a usable version of the input csv provided
#If one desires to change the distribution of passengers arrivals, just change the clean_data.csv file in ../Input/clean_data.csv
library(tidyverse)


raw <- read.csv("./Input/raw_data.csv")
head(raw)

out <- data.frame(matrix(data = NA, ncol = 4,nrow = 171/3))
out$X1 <- raw[seq(1,nrow(raw),by=3),]
out$X2 <- raw[seq(2,nrow(raw),by=3),]
out$X3 <- raw[seq(3,nrow(raw),by=3),]

#Converting time to a decimal value to facilitate manipulation later on
times <- strsplit(out[,2],split = ":")
out$X4 <-  unlist(lapply(times, function(a) as.numeric(strsplit(a,split = ":")[[1]]) + as.numeric(strsplit(a,split = ":")[[2]])/60))


head(out)
colnames(out) <- c("Station","Arrival_Time", "Passengers","Num_Arr_Time")

write.csv(out,file = "./Input/clean_data.csv",row.names = FALSE)





