# ts1.R is used for server-side R function and object
# not.Time is a logical variable to using in Filter to filtering out non-Time variable
# TSdatasetInput is worked as the local module copy of the data
# iNZightTS.data is worked as the iNZightTS object input single variable
# iNZightTS.Mdata is worked as the iNZightTS object input multiple variables
# Date.choice and validate to avoid we load all the functions even we are using a data not for time series
# plot1 for rawplot
# plot2 for seasonalplot
# plot3 for decompositionplot
# plot4 for forecastplot
# forecast.value for summary result from forecastplot
# changesize.multiseries for ploting and adjusted the mutliseries plot based on the number of variables
# changesize.compare for ploting and adjusted the compare plot based on the number of variables

not.Time <- function(var) {
  !inherits(var, "Time")
}



TSdatasetInput <- reactive({

  getdata()
  
})


iNZightTS.data = reactive({
  
  
  if (is.null(TSdatasetInput()))
    return()
  
  con1 <- NULL
  input$provide
  con1 <- isolate(c(input$startdate, input$seasonnumber))
  
  con2 <- NULL
  input$provide
  con2 <- isolate(input$frequency)
  
  update1 <- FALSE
  if (!all(c("","") %in% con1))
    update1 <- TRUE
  
  update2 <- FALSE
  if (con2 != "")
    update2 <- TRUE
  
  if ((update1 & !update2) || (!update1 & update2))
    out <- iNZightTS(TSdatasetInput(),
                     var=which(names(TSdatasetInput()) %in% input$vars2))
    
  if (!update1 & !update2)
    out <- iNZightTS(TSdatasetInput(),
                   var=which(names(TSdatasetInput()) %in% input$vars2))
  
  if (update1 & update2)
    out <- iNZightTS(TSdatasetInput(),
                     start = as.numeric(con1),
                     freq =  as.numeric(con2),
                     var=which(names(TSdatasetInput()) %in% input$vars2))

  
  out
  
  
})




iNZightTS.Mdata = reactive({
  
  
  if (is.null(TSdatasetInput()))
    return()
  
  
  if (length(input$vars3)<=1)
    return(NULL)
  
  var = which(names(TSdatasetInput()) %in% input$vars3) 
  if (is.null(var))
    return(NULL)
  
  con1 <- NULL
  input$provide
  con1 <- isolate(c(input$startdate, input$seasonnumber))
  
  con2 <- NULL
  input$provide
  con2 <- isolate(input$frequency)
  
  update1 <- FALSE
  if (!all(c("","") %in% con1))
    update1 <- TRUE
  
  update2 <- FALSE
  if (con2 != "")
    update2 <- TRUE
  
  
  if ((update1 & !update2) || (!update1 & update2))
    out <- iNZightTS(TSdatasetInput(),
                     var=var)
  
  if (!update1 & !update2)
    out <- iNZightTS(TSdatasetInput(),
                     var=var)
  
  if (update1 & update2)
    out <- iNZightTS(TSdatasetInput(),
                     start = as.numeric(con1),
                     freq =  as.numeric(con2),
                     var=var)
  
  
  out
  
}, label="2")



output$changesize.multiseries <- renderUI({
  if (!is.null(input$vars3)){
    col = input$vars3
    ncol = length(col)
    if (ncol <= 8)
      plotOutput("plot5", height = "600px")
    else
      plotOutput("plot5", height = "800px")
  }
})

output$changesize.compare <- renderUI({
  if (!is.null(input$vars3)){
    col = input$vars3
    ncol = length(col)
    if (ncol <= 8)
      plotOutput("plot6", height = "600px")
    else
      plotOutput("plot6", height = "800px")
  }
})




output$pickup <- renderUI({
  
  
  selectInput("vars1", "Select *TIME* Variable: ", names(TSdatasetInput()))
  
})

output$seasonPattern <- renderUI({
  
  radioButtons("multi", "Seasonal Pattern:", 
               c("Additive" = FALSE,
                 "Multiplicative" = TRUE))
  
})

output$ylabinput <- renderUI({
  
  textInput("ylab", "y label", value = "")
  
})

Date.choice <- reactive( { 
  
  ## Date choice always have to return FALSE to block the graphics activity.
  ## unless we are sure the structure is correct...
  
  if (is.null(TSdatasetInput()))
    return(FALSE)
  
  if (is.null(input$vars1) || input$vars1 == "")
    return(FALSE)
  
  if (!(input$vars1 %in% names(TSdatasetInput())))
    return(FALSE)
  
  
  if (any(is.na(iNZightTS:::get.ts.structure(TSdatasetInput()[, input$vars1]))))
    FALSE
  else
    TRUE
  
})


output$validate <- renderText({
  
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (is.null(input$vars1))
    return()
  
  if (Date.choice())
    ""
  else
    "THIS DATA IS INVALID FOR TIME SERIES ANALYSIS.."
})


output$RespVars <- renderUI({
  
  
  series.data <- Filter(not.Time, TSdatasetInput())
  selectInput("vars2", "Select Date Variables: ", names(series.data))
  
})

output$RespVars2 <- renderUI({
  
  
  series.data <- Filter(not.Time, TSdatasetInput())
  selectInput("vars3", "Select Date Variables: ", names(series.data), 
              multiple = TRUE)
  
})

output$plot1 = renderPlot({
  
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  rawplot(iNZightTS.data(), ylab = input$ylab, multiplicative = input$multi) 
  
})

#output$validate.season <- renderUI({

#  target <- iNZightTS.data()$tsObj


#  if (!is.null(attr(target, "levels")))
#    return(NULL)
#  else
#    h3("Time Series does not have a seasonal component")


#})  

output$plot2 <- renderPlot({
  
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  
  seasonplot(iNZightTS.data(), multiplicative = input$multi, ylab = input$ylab)
  
})


output$plot4 <- renderPlot({
  
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  forecastplot(iNZightTS.data(), multiplicative = input$multi, ylab = input$ylab)
  
})


output$plot3 = renderPlot({
  
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  if (input$uni_recom)
    iNZightTS:::recompose(decompositionplot(iNZightTS.data(), 
                          multiplicative = input$multi, 
                          ylab = input$ylab), animate = FALSE)
  else
    decompositionplot(iNZightTS.data(), ylab = input$ylab, 
                      multiplicative = input$multi)
  
})



output$plot6 = renderPlot({
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  if (is.null(iNZightTS.Mdata()))
    return()
  
  compareplot(iNZightTS.Mdata(), multiplicative = input$multi, ylab = input$ylab)
  
})

output$plot5 = renderPlot({
  
  if (is.null(TSdatasetInput())) 
    return()
  
  if (!Date.choice())
    return()
  
  if (is.null(iNZightTS.Mdata()))
    return()
  
  multiseries(iNZightTS.Mdata(), ylab = input$ylab, multiplicative = input$multi)
  
})

output$forecast.value = renderPrint({
  
  
  if (is.null(TSdatasetInput()))
    return()
  
  if (!Date.choice())
    return()
  
  p <- forecastplot(iNZightTS.data(), show = FALSE, multiplicative = input$multi)
  p
})



observe({

  # we need this observer to help us reset the default setting
  
  if (!is.null(input$timefix) &&input$timefix != "2"){
    
    updateTextInput(session, "startdate", value = "")
    updateTextInput(session, "seasonnumber", value = "")
    updateTextInput(session, "frequency", value = "")
  }

})