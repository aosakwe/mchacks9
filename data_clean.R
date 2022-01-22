#Generating Formatted CSV file
library(tidyverse)


strsplit(raw[1,],split = " ")
raw <- read.csv("data.csv", stringsAsFactors = TRUE)

head(raw)

out <- data.frame(matrix(data = NA, ncol = 3,nrow = 171/3))
out$X1 <- raw[seq(1,nrow(raw),by=3),]
out$X2 <- raw[seq(2,nrow(raw),by=3),]
out$X3 <- raw[seq(3,nrow(raw),by=3),]
head(out)
colnames(out) <- c("Station","Arrival Time", "# Passengers")

write.csv(out,file = "clean_data.csv",row.names = FALSE)
