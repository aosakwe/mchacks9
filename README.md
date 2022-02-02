# mchacks9  Train Schedule Simulator App
Mchacks 9 Project Repository for RailVision Train Schedule Optimization Project

### Brief
This app simulates a train schedule between 4 train stations and calculates the average waiting time for passengers across the serviced line. It then presents the results in an interactive web app built using the Shiny package in R.

## Current best average wait time: *4.04 minutes*

## Requirements To Run Simulation in RStudio:
To try this out you can download the repository and open the "mchacks.Rproj" file to open the project. You can then modify the schedules.csv file in Input to run the simulation on schedules you make with a given order of trains. The app itself is run by executing the code in the app.R script. This will run the simulation and open the web app to display the results.
### R packages required:
tidyverse, reshape2, shinydashboard


![Dio v. SBB](https://github.com/aosakwe/mchacks9/blob/shiny/visuals/SBB_DIO.png)

## Sample of Dashboard:
### App Homepage
![Homepage](https://github.com/aosakwe/mchacks9/blob/shiny/visuals/sample_dash1.png)

### Plot Section
![Plots](https://github.com/aosakwe/mchacks9/blob/shiny/visuals/sample_dash2.png)

## Visuals
### Passenger Arrivals
![Plot of Passenger Arrivals](https://github.com/aosakwe/mchacks9/blob/shiny/visuals/passenger_plot.png)

### Boarding Efficiency
![Plot of Boarding Efficiency](https://github.com/aosakwe/mchacks9/blob/shiny/visuals/best_result_plot26Jan2022.png)



