#######################################
# Shiny interface for data functions
#######################################
# data ui and tabs
output$data_ui_and_tabs <- renderUI({
  list(
    includeCSS("www/style2.css"),
    # includeMathJax("www/js/MathJax.js"),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js")
      # tags$script(src = "js/MathJax.js?config=TeX-AMS-MML_HTMLorMML") 
      #tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
      #  type = 'text/javascript')
    ),

    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        div(class = "busy", style="z-index=9999;",
          p("Calculation in progress ..."),
          img(src="ajaxloaderq.gif")
        ),
        wellPanel(
          uiOutput("uiDatasets")
        ),
        conditionalPanel(condition = "input.datatabs == 'Input'",
          uiOutput("ui_Manage")
        ),
        #conditionalPanel(condition = "input.datatabs == 'View'",
        #  uiOutput("ui_View")
        #),
        #conditionalPanel(condition = "input.datatabs == 'Visualize'",
        #  uiOutput("ui_Visualize")
        #),
        #conditionalPanel(condition = "input.datatabs == 'Explore'",
        #  uiOutput("ui_Explore")
        #),
        #conditionalPanel(condition = "input.datatabs == 'Merge'",
        #  uiOutput("ui_Merge")
        #),
        conditionalPanel(condition = "input.datatabs == 'Manipulate variables'",
          uiOutput("ui_Transform")
        ),
        conditionalPanel(condition = "input.datatabs == 'Filter Dataset'||input.datatabs == 'Aggregate data' ",
        uiOutput("filter_ui")
        ),
        conditionalPanel(condition = "input.datatabs == 'Sort data by variables'",
                         uiOutput("sortList_ui")
        )
      ),
      mainPanel(id = "datatabs",
        uiOutput("tabs_data")
      )
    )
  )
})

# data tabs
output$tabs_data <- renderUI({
  tabsetPanel(id = "datatabs",
    tabPanel("Input", htmlOutput("htmlDataExample"), 
      HTML(paste('<label>', input$globalrowNum, '(max) rows shown. </label>')),
      conditionalPanel(condition = "input.man_add_descr == false",
        HTML(dataDescriptionOutput('html'))
      ),
      conditionalPanel(condition = "input.man_add_descr == true",
        HTML("<label>Add data description:</label>"),
        tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
      )
    ),
    tabPanel("Manipulate variables", 
             div(style="overflow-x:auto;",htmlOutput("transform_data")), 
             verbatimTextOutput("transform_summary"),
             verbatimTextOutput("error_result"), 
             plotOutput('BlankSpace')),
    tabPanel("Filter Dataset", 
             htmlOutput('view_data')
    ),
    tabPanel("Aggregate data",
             htmlOutput('view_data2')
    ),
    tabPanel("Sort data by variables",
             htmlOutput('view_data3')
             )
  )
})

observe({
  ## this one fix the problem of input different dataset crash the webapp down
  
  #if (input$nav_radiant != "Basic"){
  #input$datasets   ## the best way is to find if user change a dataset
  input$datasets
  updateSelectInput(session, "x", selected = " ")
  updateSelectInput(session, "y", selected = " ")
  updateSelectInput(session, "A", selected = " ")
  updateSelectInput(session, "B", selected = " ")
  
}, priority = -10000)



