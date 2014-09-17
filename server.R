library(shiny)
library(stringr)
library(grid)
library(iNZightTS)
library(iNZightPlots)
library(gpairs)
library(devtools)
library(iNZightRegression)

# Globally define a place where all users can share some reactive data. # not too much used in this case
vars <- reactiveValues(users=NULL)



shinyServer(function(input, output, session) {
  
  source('internal.R', local = TRUE)
  
  
  
  source('tools/home/home.R', local = TRUE)
  source('tools/data/data_ui_and_tabs.R', local = TRUE)
  source('tools/data/filter.R', local = TRUE)
  source('tools/data/manage.R', local = TRUE)
  source('tools/data/multisort.R', local = TRUE)
  source('tools/data/transform.R', local = TRUE)
  #source('tools/app/report.R', local = TRUE)
  #source('tools/app/state.R', local = TRUE)
  source('tools/basic/basic.R', local = TRUE)
  source('tools/basic/basic_ui.R', local = TRUE)
  source('tools/basic/observer.R', local = TRUE)
  source('tools/timeseries/timeseries.R', local = TRUE)
  source('tools/timeseries/ts1.R', local = TRUE)
  source('tools/quick explore/view.R', local = TRUE)
  source('tools/Model/model_ui.R', local = TRUE)
  source('tools/Model/chooser.R', local = TRUE)
  source('tools/Model/functionalityTool.R', local = TRUE)
  source('tools/Model/menu.R', local = TRUE)
  source('tools/Model/modalTool.R', local = TRUE)
  source('tools/Model/model_observe.R', local = TRUE)
  
  
  # source data & analysis tools
  # from R3.0.2, R.utils::sourceDirectory NOT WORK
  # it was used for recursive load the script into the environment
  #R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
  #R.utils::sourceDirectory('tools/chatroom', recursive = TRUE)
  #R.utils::sourceDirectory('tools/data', recursive = TRUE)
  #R.utils::sourceDirectory('tools/app', recursive = TRUE)
  #R.utils::sourceDirectory('tools/home', recursive = TRUE)
  #R.utils::sourceDirectory('tools/basic', recursive = TRUE)
  #R.utils::sourceDirectory('tools/timeseries', recursive = TRUE)
  #R.utils::sourceDirectory('tools/explore', recursive = TRUE)
  #R.utils::sourceDirectory('tools/client', recursive = TRUE)
  #R.utils::sourceDirectory('tools/view', recursive = TRUE)
  #R.utils::sourceDirectory('tools/Model', recursive = TRUE)
  #R.utils::sourceDirectory('tools/Filter', recursive = TRUE)
  # R.utils::sourceDirectory('../base/tools/data', recursive = TRUE)
  # R.utils::sourceDirectory('../base/tools/app', recursive = TRUE)
  
  output$plot33 <- renderPlot({
    progress <- shiny::Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    plot(cars)
  })
  
})