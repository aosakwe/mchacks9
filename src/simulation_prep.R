## Script to Analyse Train data and generate data frames required to simulate
#it
## This script loads all the required input files and user-built functions
## The main simulator() function is in simulator.R
## The simulation itself is in simulation.R

library(tidyverse)
library(reshape2)

#Data frame storing arrival times of passengers at Stations
traffic <- read.csv("./Input/clean_data.csv")
traffic$Passengers <- as.numeric(traffic$Passengers)
traffic$Arrival_Time <- as.factor(traffic$Arrival_Time)
traffic$Num_Arr_Time <- as.numeric(traffic$Num_Arr_Time)

#Distribution of Passengers Arrivals at each station over time
explore <- ggplot(data = traffic, aes(x = Num_Arr_Time, y = Passengers, color = Station)) + geom_line() + labs(title = "Distribution of Passenger Arrivals over time", x = "Arrival Time", y = "Number of Passengers")
#png(filename = "passenger_plot.png")
print(explore)
#dev.off()

#Loading train order and set of possible schedules
input <- read.csv("./Input/schedules.csv")
train_order <- input[,1]
input <- input[,-1]


#Generating Results/Output data frame
Output <- data.frame(matrix(data = NA, nrow = 16, ncol = 14))
colnames(Output) <- read.table("./Input/colname.txt", sep = "\n")[,1]
Output$TrainNum <- seq(1:16)
Output$TrainType <- train_order
#fix(Output)




###### Generating Objects  ########
#Built objects for stations and trains
#Used these to track passenger arrival, boarding and disembarkation

#The objects themselves are list objects which I stored in larger list objects
#Found this to make things easier than building class-specific methods with
#conventional OOP in S3 or S4 objects

#list Object to store stations
station <- list(Passengers = c(), cur_train = FALSE)
Stations <- list(A = c(), B = c(), C = c(), U = c())
for (name in  names(Stations)){
  Stations[[name]] <- station
}


#list Object to store trains

#old_l4_train <- list(Passengers = c(),cur_station = "N", time_to_next = 0,max = 200,stop_time = 0,next_station= "N", on_track = FALSE,arrived = FALSE,arriv_times = c())
#old_l8_train <- list(Passengers = c(),cur_station = "N", time_to_next = 0,max = 400,stop_time = 0,next_station= "N",on_track = FALSE,arrived = FALSE,arriv_times = c())

l4_train <- list(Passengers = 0,cur_station = "N", time_to_next = 0,max = 200,stop_time = 0,next_station= "N", on_track = FALSE,arrived = FALSE,arriv_times = c())
l8_train <- list(Passengers = 0,cur_station = "N", time_to_next = 0,max = 400,stop_time = 0,next_station= "N",on_track = FALSE,arrived = FALSE,arriv_times = c())

Trains <- vector(mode = "list", length = 16)
for (i in 1:length(Trains)){
  if (train_order[i] == "L4"){Trains[[i]] <- l4_train}
  else{Trains[[i]] <- l8_train}
}



#### Generating  other data frames and vectors that will help in simulator.R
#Make list of all times
times <- seq(0,59)
times <- as.character(times)
times[str_length(times) == 1] <- str_c("0",times[str_length(times) == 1])
times <- c(str_c("7:",times,sep = ""),str_c("8:",times,sep = ""),str_c("9:",times,sep = ""),str_c("10:",times,sep = ""))
times <- as.factor(times)



#Itinerary matrix
#Used this to determine what the next station was and how long to get there
#based on what station the train is at
dist <- data.frame(matrix(data = NA,nrow = 3, ncol = 2))
rownames(dist) <- c("A","B","C")
colnames(dist) <- c("time_to_next","next_station")
dist$time_to_next <- c(8,9,11)
dist$next_station <- c("B","C","U")

# Data Frame to make a plot I can use to assess efficiency later
Optim_eval <- data.frame(matrix(data = NA, nrow = 240,ncol = 19))
colnames(Optim_eval) <- c("A","B","C",seq(1:16))
rownames(Optim_eval) <- times




##### Helper Functions ###########

#Embark function

#Initially stored each individual passenger in train objects
# old_embark <- function(train,train_stop){
#   #remaining seats on train
#   remain <- train$max - length(train$Passengers)
#   if (remain == 0){
#     return(list(unlist(train$Passengers),unlist(train_stop$Passengers)))
#   }
#   if (remain > length(train_stop$Passengers)){
#     new_boarded <- c(train$Passengers,train_stop$Passengers)
#     new_waiting <- c()
#   }else {
#     new_boarded <- c(train$Passengers,train_stop$Passengers[1:remain])
#     new_waiting <- train_stop$Passengers[-c(1:(remain))]
#   }
#   return(list(new_boarded,new_waiting))
# }

#New embark
#We now only store total number of passengers on train
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



