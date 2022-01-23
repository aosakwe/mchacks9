#reeducing trains
tmp2 <- stations
tmp <- trains

######
stations <- tmp2
trains <- tmp


start_times <- test$A_ArrivalTime

## Simulation
#Going to iterate through every minute, and update the number of passengers
for (i in times){
  #Update passengers at station
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
      
      trains[[j]]$on_track <- TRUE
      trains[[j]]$stop_time <- 3
      
      
      stations[["A"]]$cur_train <- TRUE
      trains[[j]]$cur_station <- "A"
      out <- embark(trains[[j]],stations[["A"]])
      trains[[j]]$Passengers <- unlist(out[[1]])
      stations[["A"]]$Passengers <- unlist(out[[2]])
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
          ###
          #Reduce stop time and if 0 move
          trains[[j]]$stop_time = trains[[j]]$stop_time - 1
          if (trains[[j]]$stop_time == 0){
            trains[[j]]$time_to_next <- dist[trains[[j]]$cur_station,1]
            trains[[j]]$next_station <- dist[trains[[j]]$cur_station,2]
            trains[[j]]$cur_station <- "N"
            stations[[trains[[j]]$cur_station]]$cur_train = FALSE
          }
        }else{
          trains[[j]]$time_to_next = trains[[j]]$time_to_next - 1
          if (trains[[j]]$time_to_next == 0){
            trains[[j]]$cur_station = trains[[j]]$next_station
            trains[[j]]$stop_time = 3
            stations[[trains[[j]]$cur_station]]$cur_train = TRUE
            trains[[j]]$arriv_times <- c(trains[[j]]$arriv_times,i)
            if (trains[[j]]$cur_station == "U"){
              trains[[j]]$arrived = TRUE
              next
            }
            
            out <- embark(trains[[j]],stations[[trains[[j]]$cur_station]])
            trains[[j]]$Passengers <- unlist(out[[1]])
            stations[[trains[[j]]$cur_station]]$Passengers <- unlist(out[[2]])
          }
        }
      }
      
    }
    
  }
  
}


