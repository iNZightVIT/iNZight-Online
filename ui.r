shinyUI(
  fluidPage(
    theme = "iNZight.css",
    div(class="image-header",img(src="images/inzight.logo.png")),
    navbarPage("iNZight-Online", id="selector",
               inverse=T,
               collapsable=F,
               fluid=T,
               responsive=T,
               tabPanel("About",uiOutput('about.panel')),
               # gui elements of the data import panel
               navbarMenu("Data",id="data",
                          tabPanel("Current Data",uiOutput('current.data')), # presents the current loaded data set
                          tabPanel("Switch Data",uiOutput('switch.data.panel')), # presents all loaded data sets and enables to switch between them. See gui-elements/switch.data.panel.R
                          tabPanel("Load Data",uiOutput('load.data.panel')), # enables to load a user data set
                          tabPanel("Remove Data",uiOutput("remove.data.panel")) # remove previously uploaded data sets
                          ),
               navbarMenu("Modify data",
                          tabPanel("Transform columns",uiOutput('transform.columns')),
                          tabPanel("Reorder Levels",uiOutput('reorder.levels')),
                          tabPanel("Compare dates"),
                          tabPanel("Add columns"),
                          tabPanel("Remove columns")
                          ),
               navbarMenu("Quick Plots",
                          tabPanel("A"),
                          tabPanel("B"),
                          tabPanel("C")
                          )
             #              tabPanel("Basic", uiOutput("basicdefine")),           
             #              tabPanel("TimeSeries", uiOutput('timeseries_ui')),
             #              tabPanel("Quick Explore", uiOutput('view_ui_and_tabs')),
             #              tabPanel("Model", uiOutput('ui_model')),
             #              tabPanel("Home", uiOutput('home_side_and_main'))
             ),
    div(class="page-footer",img(src="images/inzight.logo.footer.png"))
    )
)