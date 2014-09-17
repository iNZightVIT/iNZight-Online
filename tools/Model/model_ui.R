


DATA <- reactive({
  
  getdata()
  
})

# initial <- reactiveValues(get = FALSE)
# 
# 
# observe({
#   
#   if (input$nav_radiant == "Model")
#     initial$get <- TRUE
# })


output$ui_model <- renderUI({


   out <- fluidPage(
    includeCSS("www/special.css"),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js")
    ),
    div(class = "busy", style="z-index:9999;",
        p("Calculation in progress ..."),
        img(src="ajaxloaderq.gif")
    ),
    fluidRow(
      column(width = 3,
             span(
               br(),
               selectInput('sourceVar', "Choose variables of interest to  panel", 
                           colnames(DATA()),
                           multiple= TRUE,
                           selectize = FALSE)
             )
      ),
      column(width = 2,
             verticalLayout(
               br(),
               br(),
               br(),
               br(),
               actionPbutton("Into","Add", icon= icon("ok")),
               br(),
               splitButton("Interaction", 'l1', "Fraction", "l2", "Factorial"),
               br(),
               splitButton("Polynomial", 'l3', "Square", "l4", "Cubi"),
               br(),
               p("Current Model:")
             )
      ),
      column(width = 7,
             absolutePanel(
               splitLayout(
                 selectInput('responseVar', "Response (Y)", colnames(DATA()), selectize = FALSE),
                 selectInput('type', "Model", 
                             c("Least Squares"="gaussian", "Logistic Regression"="binomial", "Poisson Regression"="poisson"), selectize = FALSE)
                 
               ),
               uiOutput('chooser'),
               splitLayout(
                 selectInput('selectModel', "", 
                             '-',
                             selectize = FALSE),
                 div(
                   actionPbutton("Into1","Fit model", icon= icon("ok")), 
                   actionRbutton("Into4","Clean Result", icon= icon("wrong"))
                 )
               )
             )
      )
    ),
    br(),
    uiOutput("formula.check"),
    uiOutput("result.check"),
    toolList(
      dropdownPill(label = "Summary",
                   optionLink("sumNowModel", "Current Model")                 
      ),
      dropdownPill(label = "Categorical Comparison",
                   optionModalLink("comPlot", "#comPlot_modal","Comparison Plot"),
                   optionModalLink("comMat", "#comMat_modal","Comparison Matrix")
      ),
      dropdownPill(label = "Graphics Diagnostics",
                   optionModalLink("spm", href="#spm_modal", "Scatter Plot Matrix"),
                   optionModalLink("bplots", "#bplots_modal","Basic Plots"),
                   optionModalLink("prp", "#prp_modal","Partial Residual Plot")
      ),
      dropdownPill(label = "Normality Checks",
                   optionModalLink("nqq", "#nqq_modal","Normal Q-Q"),
                   optionModalLink("nch", "#nch_modal","Norm Check Histogram"),
                   optionModalLink("ha", "#ha_modal","Histogram Array"),
                   optionModalLink("qqpi", "#qqpi_modal","Q-Q Plot Inference")
      )
    ),
    verbatimTextOutput("result"),
    div(style="height:100px",""),
    ## modal events are listing below.
    ## In bootstrap, modal window is writing here but blocking and waiting for user to activiate them.
    ## this example shows us renderUI has some unknown problem...
    ## This example show how validate help us solve the renderUI problem..
    modalEvent(id="comPlot_modal", header = "Comparison Plot", 
               uiOutput('comPlotVarSelect'),
               plotOutput('comPlotWin')),
    modalEvent(id="comMat_modal", header = "Comparison Matrix",
               uiOutput('comMatVarSelect'),
               verbatimTextOutput('comMatWin')),
    modalEvent(id="spm_modal", header = "Scatter Plot Matrix", 
               plotOutput('spmWin')),
    modalEvent(id="bplots_modal", header = "Basic Plots",
               plotOutput('plotlm1'),
               plotOutput('plotlm2'),
               plotOutput('plotlm3'),
               plotOutput('plotlm4')),
    modalEvent(id="prp_modal",header = "Partial Residual Plot", 
               uiOutput('prpVarSelect'),
               plotOutput('prpWin')
               ),
    modalEvent(id="nqq_modal",header = "Normal Q-Q", plotOutput('nqqWin')),
    modalEvent(id="nch_modal",header = "Norm Check Histogram", plotOutput('nchWin')),
    modalEvent(id="ha_modal",header = "Histogram Array", plotOutput('haWin')),
    modalEvent(id="qqpi_modal",header = "Q-Q Plot Inference", plotOutput('qqpiWin'))
  )
  out

})


# observe({
#   
#   input$datasets
#   initial$get <- FALSE
#   
# })


