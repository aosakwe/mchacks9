#This is the main script for the schedule simulator
##It will run the simulator() method on every proposed  schedule and train order
##It then prints out the results file for the schedule with the lowest mean waiting time
## This script is dependent on simulation_prep.R AND simulator.R

source("./src/simulation_prep.R")
source("./src/simulator.R")



#Store column name of each set of schedule times
#Use this to run simulator() on each schedule
Start_times <- colnames(input)


simu_results <- lapply(Start_times, function(a) 
  simulator(start_times = input[,a],trains = Trains, stations = Stations,output = Output,optim_eval = Optim_eval))


#Storing some summary stats to show on the dashboard
wait_times <- unlist(lapply(simu_results, function(a) a[[1]]))
best_sim <- simu_results[[which.min(wait_times)]]
worst_sim <- simu_results[[which.max(wait_times)]]
#fileID <- paste(unlist(strsplit(date(), " "))[c(3,2,5)],collapse = '')


#Store best_sim outputs in results and visuals directory
#write.csv(best_sim[[2]],file =  paste("./results/best_result_table",fileID,".csv",sep = ''),row.names = FALSE)

#png(filename = paste("./visuals/best_result_plot",fileID,".png",sep = ''))
#print(best_sim[[3]])
#dev.off()
