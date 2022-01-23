#Script to simulate train schedule

#backups
tmp2 <- stations
tmp <- trains

tmp3 <- stations
tmp4 <- trains
######
stations <- tmp3
trains <- tmp4




## Simulation
#Going to iterate through every minute, and update the number of passengers
start_times <- test$A_ArrivalTime
waiting_times <- c()
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
    
  }
  
}
mean(waiting_times)*60
write.csv(output,file = "results_output220122.csv",row.names = FALSE)




train_load <- c()
for (train in trains){
  train_load <- c(train_load,train$Passengers)
}
#total
train_load - example$U_Offloading
compare <- cbind(train_load,example$U_Offloading)
compare <- cbind(compare,compare[,1] - compare[,2])
colnames(compare) <- c("me","test","diff")
view(output)

