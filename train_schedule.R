## Script to Analyse Train data and generate data frames required to simulate
#it
library(ggplot2)
library(tidyverse)

traffic <- read.csv("clean_data.csv")
traffic$Passengers <- as.numeric(traffic$Passengers)
traffic$Arrival_Time <- as.factor(traffic$Arrival_Time)
traffic$Num_Arr_Time <- as.numeric(traffic$Num_Arr_Time)

#Distribution of traffic at each station over time
explore <- ggplot(data = traffic, aes(x = Num_Arr_Time, y = Passengers, color = Station)) + geom_line() + labs(title = "Distribution of traffic over time", x = "Arrival Time", y = "Number of Passengers")
explore

# We have two traits to change in our schedule:
# 1) the order of our trains (and therefore the capacity)
# 2) the frequency of our trains (which will in turn affect the total capacity 
# in a given hour)

# Plotting the traffic over time shows us exactly when traffic peaks at each 
# station, and therefore when we need the greatest capacity and frequency

# The goal here is to first arrange the trains in an order that would match this 
# distribution,and then adjust the frequency/schedule to best accommodate the
# traffic.

train_size <- c(rep(400,times = 12),rep(200,times = 4))
trains <- ifelse(train_size == 200, "L4","L8")
output <- data.frame(matrix(data = NA, nrow = 16, ncol = 14))
colnames(output) <- read.table("colname.txt", sep = "\n")[,1]
output$TrainNum <- seq(1:16)
output$TrainType <- as.factor(trains)
fix(output)

##Checking if my simulation works and generates the expected output
example <- read.table("example_out.txt",header = TRUE)
#store copy of output to test algorithm on
test <- output
test$A_ArrivalTime <- example$A_ArrivalTime
convert_time <-strsplit(example$A_ArrivalTime,split = ":")
#test$A_ArrivalTime <- unlist(lapply(convert_time, function(a) as.numeric(a[[1]]) + as.numeric(a[[2]])/60))



 ##### Helper Functions ###########

#Embark function

old_embark <- function(train,train_stop){
  #remaining seats on train
  remain <- train$max - length(train$Passengers)
  if (remain == 0){
    return(list(unlist(train$Passengers),unlist(train_stop$Passengers)))
  }
  if (remain > length(train_stop$Passengers)){
    new_boarded <- c(train$Passengers,train_stop$Passengers)
    new_waiting <- c()
  }else {
    new_boarded <- c(train$Passengers,train_stop$Passengers[1:remain])
    new_waiting <- train_stop$Passengers[-c(1:(remain))]
  }
  return(list(new_boarded,new_waiting))
}

#New embark
#only store total number of passengers on train
embark <- function(train,train_stop){
  #remaining seats on train
  remain <- train$max - train$Passengers
  if (remain == 0){
    return(list(train$Passengers,train_stop$Passengers,c()))
  }
  if (remain > length(train_stop$Passengers)){
    on_board <- train$Passengers + length(train_stop$Passengers)
    new_boarded <- train_stop$Passengers
    new_waiting <- c()
  }else {
    on_board <- train$Passengers + length(train_stop$Passengers[1:remain])
    new_boarded <- train_stop$Passengers[1:remain]
    new_waiting <- train_stop$Passengers[-c(1:(remain))]
  }
  return(list(on_board,new_waiting,new_boarded))
}


#Calculate the wait times 
calc_wait <- function(new_boarded, cur_time){
  convert_time <-strsplit(new_boarded,split = ":")
  new_boarded <- unlist(lapply(convert_time, function(a) as.numeric(a[[1]]) + as.numeric(a[[2]])/60))
  convert_cur <- unlist(strsplit(cur_time,split = ":"))
  
  cur_time <- as.numeric(convert_cur[1]) + as.numeric(convert_cur[2])/60
  new_boarded <- cur_time - new_boarded
  return(new_boarded)
}
#################


###### Generating Objects  ########

#list Object to store stations
station <- list(Passengers = c(), cur_train = FALSE)
stations <- list(A = c(), B = c(), C = c(), U = c())
for (name in  names(stations)){
  stations[[name]] <- station
}


#list of train list objects
#old_l4_train <- list(Passengers = c(),cur_station = "N", time_to_next = 0,max = 200,stop_time = 0,next_station= "N", on_track = FALSE,arrived = FALSE,arriv_times = c())
#old_l8_train <- list(Passengers = c(),cur_station = "N", time_to_next = 0,max = 400,stop_time = 0,next_station= "N",on_track = FALSE,arrived = FALSE,arriv_times = c())

l4_train <- list(Passengers = 0,cur_station = "N", time_to_next = 0,max = 200,stop_time = 0,next_station= "N", on_track = FALSE,arrived = FALSE,arriv_times = c())
l8_train <- list(Passengers = 0,cur_station = "N", time_to_next = 0,max = 400,stop_time = 0,next_station= "N",on_track = FALSE,arrived = FALSE,arriv_times = c())

trains <- vector(mode = "list", length = 16)
for (i in 1:length(trains)){
  if (i == 1){trains[[i]] <- l4_train}
  else if (i <= 13){ trains[[i]] <- l8_train
  }else{trains[[i]] <- l4_train}
}

#Make list of all times
times <- seq(0,59)
times <- as.character(times)
times[str_length(times) == 1] <- str_c("0",times[str_length(times) == 1])
times <- c(str_c("7:",times,sep = ""),str_c("8:",times,sep = ""),str_c("9:",times,sep = ""),str_c("10:",times,sep = ""))
times <- as.factor(times)



#Itinerary matrix
dist <- data.frame(matrix(data = NA,nrow = 3, ncol = 2))
rownames(dist) <- c("A","B","C")
colnames(dist) <- c("time_to_next","next_station")
dist$time_to_next <- c(8,9,11)
dist$next_station <- c("B","C","U")



