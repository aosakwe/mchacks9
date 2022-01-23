#Script to simulate train schedule
library(ggplot2)
library(stringr)
library(reshape2)

#running in RStudio
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)



#backups I use to be able to quickly rerun simulation
tmp_stations <- stations
tmp_trains <- trains


###### Run simulation from here ######
#Reset list objects
stations <- tmp_stations
trains <- tmp_trains


## Simulation
#Going to iterate through every minute, and update the number of passengers
#tweaked times based on when trains were always full in the simulation with
#example schedule
start_times <- c("7:00","7:10","7:15","7:23","7:27","7:37","7:47","7:57","8:07","8:20","8:30","8:45","9:10","9:30","9:40","10:00")
#test$A_ArrivalTime <-- these are the start_times from the example schedule

waiting_times <- c()

for (i in times){
  #Update # of passengers at station
  if (i %in% traffic$Arrival_Time) {
    stations[["A"]][["Passengers"]] <- c(stations[["A"]][["Passengers"]],rep(i,traffic$Passengers[traffic$Arrival_Time == i & traffic$Station == "A"]))
    stations[["B"]][["Passengers"]] <- c(stations[["B"]][["Passengers"]],rep(i,traffic$Passengers[traffic$Arrival_Time == i & traffic$Station == "B"]))
    stations[["C"]][["Passengers"]] <- c(stations[["C"]][["Passengers"]],rep(i,traffic$Passengers[traffic$Arrival_Time == i & traffic$Station == "C"]))
  }
  
  
  #Move all trains up by one (except trains that need to stay in station)
  ##See which trains can leave their Station
  ###See which trains enter a station
  ####Embark passengers that are there
  for (j in seq(1:16)){ 
    #check if the train can arrive to station A
    if (i %in% start_times & !trains[[j]]$on_track){
      start_times <- start_times[-1]
      trains[[j]]$arriv_times <- c(trains[[j]]$arriv_times,i)
      output$A_ArrivalTime[j] <- i
      output$A_AvailCap[j] <- trains[[j]]$max
      trains[[j]]$on_track <- TRUE
      trains[[j]]$stop_time <- 3
      
      
      stations[["A"]]$cur_train <- TRUE
      trains[[j]]$cur_station <- "A"
      
      out <- embark(trains[[j]],stations[["A"]])
      trains[[j]]$Passengers <- unlist(out[[1]])
      stations[["A"]]$Passengers <- unlist(out[[2]])
      empty <- is.null(unlist(out[[3]]))
      if (!empty){waiting_times <- c(waiting_times,calc_wait(unlist(out[[3]]),i))}
    }
    else if(!trains[[j]]$on_track){next
    }else{
      #check if it is at a station
     
      if (trains[[j]]$on_track){
        
        if(trains[[j]]$arrived){next}
        if(trains[[j]]$cur_station != "N"){
          #in a station
          #Fill em up first
          
          out <- embark(trains[[j]],stations[[trains[[j]]$cur_station]])
          trains[[j]]$Passengers <- unlist(out[[1]])
          stations[[trains[[j]]$cur_station]]$Passengers <- unlist(out[[2]])
          empty <- is.null(unlist(out[[3]]))
          if (!empty){waiting_times <- c(waiting_times,calc_wait(unlist(out[[3]]),i))}
          
          ###
          #Reduce stop time and if 0 move
          trains[[j]]$stop_time = trains[[j]]$stop_time - 1
          if (trains[[j]]$stop_time == 0){
            
            if (trains[[j]]$cur_station == "B"){
              output$B_Boarding[j] <- output$B_AvailCap[j] - (trains[[j]]$max - trains[[j]]$Passengers)
            }else if (trains[[j]]$cur_station == "C"){
              output$C_Boarding[j] <- output$C_AvailCap[j] - (trains[[j]]$max - trains[[j]]$Passengers)
            }else if (trains[[j]]$cur_station == "A"){
              output$A_Boarding[j]  <- output$A_AvailCap[j] - (trains[[j]]$max - trains[[j]]$Passengers)
            }
            
            trains[[j]]$time_to_next <- dist[trains[[j]]$cur_station,1]
            trains[[j]]$next_station <- dist[trains[[j]]$cur_station,2]
            trains[[j]]$cur_station <- "N"
            stations[[trains[[j]]$cur_station]]$cur_train = FALSE
            
          }
        }else{
          trains[[j]]$time_to_next = trains[[j]]$time_to_next - 1
          if (trains[[j]]$time_to_next == 0){
            trains[[j]]$cur_station <- trains[[j]]$next_station
            if (trains[[j]]$cur_station == "B"){
              output$B_ArrivalTime[j] <- i
              output$B_AvailCap[j] <- trains[[j]]$max - trains[[j]]$Passengers
            }else if (trains[[j]]$cur_station == "C"){
              output$C_ArrivalTime[j] <- i
              output$C_AvailCap[j] <- trains[[j]]$max - trains[[j]]$Passengers
            }
            trains[[j]]$stop_time <- 3
            stations[[trains[[j]]$cur_station]]$cur_train = TRUE
            trains[[j]]$arriv_times <- c(trains[[j]]$arriv_times,i)
            if (trains[[j]]$cur_station == "U"){
              output$U_Arrival[j] <- i
              output$U_AvailCap[j] <- trains[[j]]$max - trains[[j]]$Passengers
              output$U_Offloading[j] <- trains[[j]]$Passengers
              trains[[j]]$arrived = TRUE
              next
            }
            
            out <- embark(trains[[j]],stations[[trains[[j]]$cur_station]])
            trains[[j]]$Passengers <- unlist(out[[1]])
            stations[[trains[[j]]$cur_station]]$Passengers <- unlist(out[[2]])
            empty <- is.null(unlist(out[[3]]))
            if (!empty){waiting_times <- c(waiting_times,calc_wait(unlist(out[[3]]),i))}
          }
        }
      }
      
    }
    #optim_eval[as.character(i),as.character(j)] <- trains[[j]]$max - trains[[j]]$Passengers
  }
  # optim_eval[as.character(i),"A"] <- length(stations[["A"]]$Passengers)
  # optim_eval[as.character(i),"B"] <- length(stations[["B"]]$Passengers)
  # optim_eval[as.character(i),"C"] <- length(stations[["C"]]$Passengers)
}

#Calculate avg waiting time and convert to minutes
mean(waiting_times)*60
# write.csv(output,file = "best_results_4m76_output220122.csv",row.names = FALSE)
# ### SIMULATION CODE IS NOW COMPLETE. BELOW IS OPTIONAL ###
# 
# #Plot optim_eval values
# convert_time <- strsplit(rownames(optim_eval),split = ":")
# optim_eval$Time <- unlist(lapply(convert_time, function(a) as.numeric(a[[1]]) + as.numeric(a[[2]])/60)) 
# backup <- optim_eval
# optim_eval <- backup
# optim_eval <- melt(optim_eval[,c(1:3,20)] ,  id.vars = 'Time', variable.name = 'series')
# ggplot(optim_eval, aes(Time, value,color = series,group = 1)) +
#   geom_line() 



# 
# 
# train_load <- c()
# for (train in trains){
#   train_load <- c(train_load,train$Passengers)
# }
# #total
# train_load - example$U_Offloading
# compare <- cbind(train_load,example$U_Offloading)
# compare <- cbind(compare,compare[,1] - compare[,2])
# colnames(compare) <- c("me","test","diff")
# view(output)

