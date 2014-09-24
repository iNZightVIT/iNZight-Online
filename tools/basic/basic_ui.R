output$basicdefine <- renderUI({
  
  fixedPage(responsive= FALSE,
    fixedRow(
      column(width=10, offset = 2,
             tabsetPanel(
               tabPanel("Plot", 
                        includeCSS("www/style2.css"),
                        # includeMathJax("www/js/MathJax.js"),
                        tags$head(
                          tags$script(src = "js/jquery-ui.custom.min.js"),
                          tags$script(src = "js/busy.js")
                          #tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
                          #            type = 'text/javascript')
                        ),
                        column(width = 8,
                               div(class = "busy", style="z-index=9999;",
                                   p("Calculation in progress ..."),
                                   img(src="ajaxloaderq.gif")
                               ),
                               conditionalPanel(
                                 condition = "input.x != ' '",
                                 plotOutput('inzPlot', height = "600px")
                               ),
                               h3("Graphics Control Panel"),
                               tabsetPanel(
                                 tabPanel("Variables Panel", 
                                          fluidRow(
                                            column(width =4, offset = 1,
                                                   uiOutput('var1.dd')),
                                            column(width =4, offset = 1, 
                                                   uiOutput('var2.dd'))                                      
                                          ),
                                          fluidRow(
                                            column(width =4, offset = 1,
                                                   uiOutput('sb1.dd')),
                                            column(width =4, offset = 1, 
                                                   uiOutput('sb2.dd'))                                      
                                          ),
                                          fluidRow(
                                            column(width =4, offset = 1,
                                                   conditionalPanel(
                                                     condition = "input.A !=' '",
                                                     uiOutput('sb1_sl')
                                                   )
                                            ),
                                            column(width =4, offset = 1,
                                                   conditionalPanel(
                                                     condition = "input.B !=' '",
                                                     uiOutput('sb2_sl')))                                      
                                          ),
                                          span(
                                            actionButton("redefault", "Clear All!"),
                                            radioButtons("defaultStyle", "Change to defaultStyle!", 
                                                         c("Small","Large"), inline=TRUE)
                                          ),
                                          tags$br(),
                                          fixedRow(
                                            ## input a big white pic here.
                                            
                                          )
                                 ),
                                 tabPanel("Add to Plot",
                                          fluidRow(
                                            column(width =6,
                                                   #uiOutput('radio.add')
                                                   radioButtons('ADD', "Add to Plot",
                                                                c("Code more variables" = "1",
                                                                  "Change plot appearance" = "2",
                                                                  "Customize Labels" = "3"))
                                            ),
                                            column(width = 6, 
                                                   uiOutput('basanno')
                                            )
                                          ),
                                          tags$br(),
                                          fluidRow(
                                            column(width = 5, offset = 1,
                                                   uiOutput('numanno')
                                            ),
                                            column(width = 5, offset = 1,
                                                   uiOutput('atc.ui'),
                                                   uiOutput('axyl.ui'),
                                                   uiOutput('ajit.ui'),
                                                   uiOutput('arug.ui'),
                                                   uiOutput('ap2l.ui')
                                            )
                                          ),
                                          tags$br()
                                 ),
                                 tabPanel("Add Inference",
                                          fluidRow(
                                            column(width = 4, 
                                                   uiOutput('addinfr.switch')
                                            ),
                                            column(width = 8, 
                                                   uiOutput('addinfr.panel'),
                                                   uiOutput('addinfr.panel2'),
                                                   actionButton("GOInf", "Show Changes!!"),
                                                   actionButton("CutInf", "Remove Inference!")
                                            )
                                          )
                                          
                                 )
                               )
                        )
               ),
               tabPanel("Get Summary", 
                        verbatimTextOutput('sumadd')),
               tabPanel("Get Inference", 
                        verbatimTextOutput('suminf'))
             )
      )
    )
    
  )
  
  
})
