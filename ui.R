library(shiny)

shinyUI(fluidPage(tags$head(tags$script("alert('Please Note, iNZight Online will soon be replaced by our new improved version iNZight Lite. Go to http://docker.stat.auckland.ac.nz/ to check out our beta version.')")),
                  navbarPage(HTML("<a href='/futurelearn2014/'>iNZight online</a>"),
                             id = "nav_radiant", collapsable = TRUE,
                             tabPanel("Data", uiOutput('data_ui_and_tabs')),
                             tabPanel("Basic", uiOutput("basicdefine")),
                             tabPanel("TimeSeries", uiOutput('timeseries_ui')),
                             tabPanel("Quick Explore", uiOutput('view_ui_and_tabs')),
                             tabPanel("Model", uiOutput('ui_model')),
                             tabPanel("Home", uiOutput('home_side_and_main')))
                  )
)