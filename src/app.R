### This script will be used to launch a local R shiny dashboard that can be used to evaluate train schedules in a more interactive manner
## Also using this as a good excuse to learn how the shiny package works to add it to my arsenal of R tools
### IMPORTANT ###
# Much like simulation.R, this script depends on simulation_prep.R,simulator.R AND simulation.R to work


##Loading Dependencies
library(shiny)
library(shinydashboard)
source("./src/simulation_prep.R")
source("./src/simulator.R")
source("./src/simulation.R")

##Creating dashboard template
ui <- dashboardPage(
  dashboardHeader(title = "Schedule Evaluator"),
  dashboardSidebar(
    menuItem("Plots",tabName = "plots",icon = icon("fas fa-chart-bar")),
    menuItem("Tables",tabName = "tables", icon = icon("fas fa-table"))
  ),
  dashboardBody(
    tabItems(
      #Creating initial ggplot
      tabItem(tabName = "plots",
        fluidRow(
          box(title = "Controls", sliderInput("slider","Schedule #",1,length(simu_results),1,step = 1))
        ),
        fluidRow(
          box(plotOutput("plot1",height = 700)))
      ),
      
      #Tab with tables
      tabItem(tabName = "tables")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    data <- simu_results[[input$slider]][[3]]
    print(data)
  })
}

shinyApp(ui, server)


