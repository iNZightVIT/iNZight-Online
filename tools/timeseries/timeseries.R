# timeseries_ui is working as the ui for time series module
# which is called in ui.R --- tabPanel("TimeSeries", uiOutput('timeseries_ui'))

output$timeseries_ui <- renderUI({
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("timefix", label = "", 
                   choices = c("Select *Time* Variable" = "1", 
                               "Provide Time information" = "2"), 
                   selected = "1"),
      
      conditionalPanel(
        condition = "input.timefix==1",
        uiOutput("pickup")
      ), 
      
      conditionalPanel(
        condition = "input.timefix==2", 
        textInput("startdate", "Start Date"),
        textInput("seasonnumber", "Season Number"),
        textInput("frequency", "Frequency"),
        actionButton('provide', "Provide Time Info")
      ),
      
      tags$hr(),
      
      uiOutput('ylabinput'),
      uiOutput('seasonPattern'),
      
      wellPanel(
        div(class="row-fluid", 
            div(class="span5", 
                p(strong("Single Series: ")),
                checkboxInput(inputId = "uni_raw", label = "Time Series Plot", value = TRUE),
                checkboxInput(inputId = "uni_seanson", label = "Seasonal effect", value = TRUE),
                checkboxInput(inputId = "uni_decom",  label = "Decompose"),
                checkboxInput(inputId = "uni_recom",  label = "Add Trend + Seasonal"),
                checkboxInput(inputId = "uni_predict", label = "Predict")), 
            div(class="span5", uiOutput("RespVars")),
            div(class="span1", helpPopup('Choose the series variable','Only support single selection.'))
            
        )
      ),
      
      wellPanel(
        div(class="row-fluid", 
            div(class="span5", 
                p(strong("Several Series: ")),
                checkboxInput(inputId= "single_series", label = "Single-Plot"),
                checkboxInput(inputId = "multi_series", label = "Multiple-Plot")
            ),
            div(class="span5", uiOutput("RespVars2")),
            div(class="span1",helpPopup('Choose multiple series variable', 'Give multiple selection comparison result.'))
        )
      )  
    ),
    
    
    mainPanel(
      
      h3(textOutput("validate"), style="color:red"),
      
      
      conditionalPanel(
        condition = "input.uni_raw", 
        br(),
        plotOutput("plot1")
      ),
      
      
      conditionalPanel(
        condition = "input.uni_seanson", 
        br(),
        plotOutput("plot2")
      ),
      
      conditionalPanel(
        condition = "input.uni_decom", 
        br(),
        plotOutput("plot3")
      ),
      
      conditionalPanel(
        condition = "input.uni_predict", 
        br(),
        plotOutput("plot4")
      ),
      
      conditionalPanel(
        condition = "input.uni_predict",
        br(),
        verbatimTextOutput("forecast.value")
      ),
      
      conditionalPanel(
        condition = "input.multi_series",
        br(),
        uiOutput('changesize.multiseries')
      ),
      
      conditionalPanel(
        condition = "input.single_series",
        br(),
        uiOutput('changesize.compare')
      ),
      br()
      

      
    )
    
  )
  
})