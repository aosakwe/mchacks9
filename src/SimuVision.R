### This script will be used to launch a local R shiny dashboard that can be used to evaluate train schedules in a more interactive manner
## Also using this as a good excuse to learn how the shiny package works to add it to my arsenal of R tools
### IMPORTANT ###
# Much like simulation.R, this script depends on simulation_prep.R and simulator.R to work

library(shiny)
source("./src/simulation_prep.R")
source("./src/simulator.R")


