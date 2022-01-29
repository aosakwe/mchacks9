### This script will be used to launch a local R shiny dashboard that can be used to evaluate train schedules in a more interactive manner
## Also using this as a good excuse to learn how the shiny package works to add it to my arsenal of R tools
### IMPORTANT ###
# Much like simulation.R, this script depends on simulation_prep.R,simulator.R AND simulation.R to work

### IMPORTANT ###
#setwd() need to run the entire app automatically
#needs to be commented OUT if running manually (i.e: if you are just opening the project and running the whole script or highlighting chunks to run)
setwd("../")
##################

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
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("fas fa-home"),selected = TRUE),
      menuItem("Plots",tabName = "plots",icon = icon("fas fa-chart-bar")),
      menuItem("Tables",tabName = "tables", icon = icon("fas fa-table")),
      sliderInput("slider","Select a Schedule",1,length(simu_results),1,step = 1)
    )
  ),
  dashboardBody(
    tabItems(
      #Creating Home Page
      tabItem(tabName = "home",
        fluidRow(
          box(title = "Train Schedule Evaluator","Welcome to my train schedule evaluator app! Here you can submit a csv of candidate train schedules and evaluate their efficiency at servicing three train stops.")
        ),
        fluidRow(
          infoBox(title = "Schedules Tested",length(simu_results), icon = icon("list"),fill = TRUE,width = 4),
          infoBox(title = "Best Average", paste(round(best_sim[[1]],2)," minutes",sep = ''),icon = icon("fas fa-medal"),fill = TRUE, color = "green",width = 4),
          infoBox(title = "Worst Average", paste(round(worst_sim[[1]],2)," minutes",sep = ''),icon = icon("fas fa-briefcase-medical"),fill = TRUE, color = "red",width = 4)
        ),
        fluidRow(
          box(plotOutput("plot0",height = 700),width = 1000)
        )
      ),
      #Creating initial ggplot
      tabItem(tabName = "plots",
        fluidRow(
          #box(title = "Schedule #", sliderInput("slider","Use the slider to select a schedule to evaluate.",1,length(simu_results),1,step = 1)),
          infoBoxOutput("sched")
        ),
        fluidRow(
          box(plotOutput("plot1",height = 700),width = 1000)
        )
      ),
      
      #Tab with tables
      tabItem(tabName = "tables",
        fluidRow(
          #box(title = "Schedule #", sliderInput("slider","Use the slider to select a schedule to evaluate.",1,length(simu_results),1,step = 1)),
            infoBoxOutput("sched2")
        ),
        fluidRow(
          box(tableOutput("table1"),width = 10)
        )  
      )
    )
  )
)

server <- function(input, output) {
  output$plot0 <- renderPlot({
    print(explore)
  })
  output$plot1 <- renderPlot({
    data <- simu_results[[input$slider]][[3]]
    print(data)
  })
  output$sched <- renderInfoBox({
    infoBox(title = "Current Schedule",input$slider,icon = icon("chart-bar"),fill = TRUE, color = "blue",width = 3)
  })
  output$sched2 <- renderInfoBox({
    infoBox(title = "Current Schedule",input$slider,icon = icon("chart-bar"),fill = TRUE, color = "blue",width = 3)
  })
  output$table1 <- renderTable(simu_results[[input$slider]][[2]],rownames = FALSE,spacing = "xs")
}

shinyApp(ui, server)


