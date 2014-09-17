#######################################
# Shiny interface for viewing 1 dimension or 2 dimension variables
#######################################
# view ui and tabs: reactive user interface
# viewcontrol1: for single variable
# viewcontrol2: for multiple variables
# test.text: return length of the variable names in current data
# plot_summary: UI for plot and summary
# Recursive: help us achieve loop/recursive functional here
# viewofdata: copy of data using here
# viewofdataname: copy of data variables name using here
# var1_plot: all single variable plot
# var1_sum: all single variable summary
# pairsplot: gparis for any number of variable 1 by 1 matrix.
# var2_plot: fixed one variable and display the relationship with this variable to all the others.
# missnessplot: missing pattern plot
# missnessshow: missing pattern summary
# missstuff: ui of missnessplot and missnessshow


output$view_ui_and_tabs <- renderUI({
  
  
  
  
  list(
    includeCSS("www/style2.css"),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js")
    ),
    
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        div(class = "busy",
            p("Calculation in progress ..."),
            img(src="ajaxloaderq.gif")
        ),
        uiOutput('viewcontrol1'),
        uiOutput('viewcontrol2'),
        uiOutput('viewcontrol3')
      ),
      mainPanel(id = "viewswitch",
                #h1(sessionVars$username),
                tabsetPanel(id = "viewswitch",
                            tabPanel("All 1-variable", uiOutput("plot_summary")),
                            tabPanel("Explore 2-variable plots", 
                                     plotOutput("var2_plot"),
                                     uiOutput('all_plot')
                                     ),
                            tabPanel("pairs...", plotOutput("pairsplot")),
                            tabPanel("Missing Values", uiOutput("missstuff"))
                )
      )
    )
  )
})

output$viewcontrol1 <- renderUI({
  
  if (!is.null(input$viewswitch) && input$viewswitch == "All 1-variable") {
    list(
      wellPanel(
        verbatimTextOutput("test.text")
      ),
      actionButton("next.signal", label = "Next"),
      actionButton("last.signal", label = "Last"))
  }
  else
    return(NULL)
})

output$viewcontrol2 <- renderUI({
  
  if (!is.null(input$viewswitch) && input$viewswitch == "Explore 2-variable plots") {
    list(
      wellPanel(
        selectInput("include.vars", 
                    "Select variable 1)",
                    Viewofdataname(),
                    multiple = FALSE,
                    selectize = FALSE)
      ),
      wellPanel(
        verbatimTextOutput("test.text2")
      ),
      actionButton("next.signal2", label = "Next"),
      actionButton("last.signal2", label = "Last")
    )
  }
  else
    return(NULL)
})

output$viewcontrol3 <- renderUI({
  
  if (!is.null(input$viewswitch) && input$viewswitch == "pairs...") {
    list(
      wellPanel(
        selectInput("include.vars2", 
                    "Select Variables for 2-dimension pairs matrix plot\n(shift/Ctrl can help you select multiple)",
                    Viewofdataname(),
                    multiple = TRUE,
                    selectize = FALSE)
      ),
      actionButton("generate.signal2", label = "Generate")
    )
  }
  else
    return(NULL)
})



output$test.text <- renderPrint({
  
  cat(Recursive$index, "(", length(Viewofdataname()),")", "\n")

})

output$test.text2 <- renderPrint({
  
  cat(Recursive2$index, "(", length(Viewofdataname()),")", "\n")
  
})

output$plot_summary <- renderUI({
  
  list(
    plotOutput("var1_plot"),
    verbatimTextOutput("var1_sum")
  )
  
  
})


Recursive <- reactiveValues(index = 0)
Recursive2 <- reactiveValues(index = 0)

observe({
  
  ## new data in, return to default index
  
  input$datasets
  Recursive$index <- 0
})






observe({
  
  ## new data in, return to default index
  
  input$datasets
  Recursive2$index <- 0 
})


Viewofdata <- reactive({
  
  getdata()
  
})




Viewofdataname <- reactive({
  
  names(Viewofdata())
  
})

output$var1_plot<- renderPlot({
  
  if (Recursive$index>0)
    iNZightPlot(Viewofdata()[, Recursive$index], varnames = list(x=Viewofdataname()[Recursive$index]))
  else{
    plot(1,1,type="n",axes=FALSE,xlab="" ,ylab="")
    text(1,1, "click the next button for the next plot", cex=2)
  }
})


output$var1_sum<- renderPrint({
  
  if (Recursive$index>0)
    getPlotSummary(Viewofdata()[, Recursive$index], varnames = list(x=Viewofdataname()[Recursive$index]))
  else
    summary(Viewofdata())
})





output$var2_plot<- renderPlot({
  
  
  if (Recursive2$index>0){
    if (Viewofdataname()[Recursive2$index] != input$include.vars) {
      iNZightPlot(
        x = Viewofdata()[, input$include.vars],          
        y = Viewofdata()[, Recursive2$index],
        varnames = list(
          x=input$include.vars, 
          y=Viewofdataname()[Recursive2$index]))
    }
    else {
      iNZightPlot(
         
        x = Viewofdata()[, input$include.vars],
        varnames = list(
          x=input$include.vars))
    }
  }
  else{
      plot(1,1,type="n",axes=FALSE,xlab="" ,ylab="")
      text(1,1, "Select 1 variable first", cex=2)
    } 
    
})

output$all_plot <- renderUI({
  
  if (!is.null(Viewofdata())){
  numbervars <- ncol(Viewofdata())
  
  lapply(1:numbervars, function(i) {
    
    plotOutput(paste0('plotVarto',i))
    
  })
  }
  
})


output$pairsplot <- renderPlot({
  
  input$generate.signal2
  isolate({
    if (!is.null(input$include.vars2) && length(input$include.vars2)>1)
      gpairs(Viewofdata()[, input$include.vars2])
    else {
      plot(1,1,type="n",axes=FALSE,xlab="" ,ylab="")
      text(1,1, "You have to select more than 1 variable", cex=2)
    }
    
  })
  
})




# Recursive
observe({
  # recursive the index 
  
  input$next.signal
  if (!is.null(input$next.signal)) {
    if (isolate(Recursive$index) < length(Viewofdataname())){
      Recursive$index <- isolate(Recursive$index) + 1
    }
    else{
      Recursive$index <- 0
    }
  }
  
})


observe({
  # recursive the index 
  
  input$last.signal
  if (!is.null(input$last.signal)) {
    if (isolate(Recursive$index) <= length(Viewofdataname()) & isolate(Recursive$index) >0 ){
      Recursive$index <- isolate(Recursive$index) - 1
    }
    else
      Recursive$index <- length(Viewofdataname()) 
  }
  
})


# Recursive2
observe({
  # recursive the index 
  ## here existing a bug "exploratory 2-variable plot" can't return to index 0
  
  input$next.signal2
  if (!is.null(input$next.signal2) ) {
    if (isolate(Recursive2$index) < length(Viewofdataname())){
      Recursive2$index <- isolate(Recursive2$index)+ 1
    }
    else
      Recursive2$index <- 0 
  }
  
})

observe({
  # recursive the index 
  
  input$last.signal2
  if (!is.null(input$last.signal2) ) {
    if (isolate(Recursive2$index) <= length(Viewofdataname()) & isolate(Recursive2$index) >0 )
      Recursive2$index <- isolate(Recursive2$index) - 1
    else
      Recursive2$index <- length(Viewofdataname()) 
  }
  
})

## missingness
output$missnessplot <- renderPlot({
  
  
  iNZightMR:::plotcombn(getdata())
  
  
})


output$missnessshow <- renderPrint({
  
  iNZightMR:::calcmissing.data.frame(getdata())
  
  
})

output$missstuff <- renderUI({
  
  if (sum(is.na(getdata()))>0)
    list(
      h1("Explore Missing Now"),
      plotOutput("missnessplot", height = "600px"),
      verbatimTextOutput("missnessshow")
    )
  else
    div("Data Clean", class= "alert alert-success")
  
})