help.advanced.graphics = function(){
  helpModal('Advanced Plots','advanced_plots',inclMD("gui-elements/notes/advanced.graphics.md"))
}

get.inference.section = function(){
  ret = list()
  if(!is.null(data)){
    ret= list()
  }
  ret
}

get.plot.section = function(){
  ret = list()
  if(!is.null(data)){
    ret = list(
      fluidRow(column(plotOutput("advanced.plot"),width=12)),br(),
      fluidRow(column(h6("Change plot appearance"),
                      tabsetPanel(id="graph_selector",type="pills",
                                 tabPanel("Add to plot",add.to.plot()),
                                 tabPanel("Change labels"),
                                 tabPanel("To be named")
                                 ),
                      width=12))
      )
  }else{
    ret = list(h4("No data available, please select or import a data set."))
  }
  ret
}

add.to.plot = function(){
  list(helpText("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
}

get.graphics.section = function(){
  ret = list()
  if(!is.null(data)){
    ret=list(selectInput(inputId="vari1",label="Select first variable",choices=colnames(data)),br(),
             selectInput(inputId="subs1",label="Subset first variable",choices=c("none",colnames(data))),br(),
             conditionalPanel(condition="input.subs1!='none'",
                              helpText("Select the level which should be plotted."),
                              selectInput(inputId="sub1_level",label="Subset Level",choices="")),br(),
             selectInput(inputId="vari2",label="Select second variable",choices=c("none",colnames(data)[-1])),br(),
             selectInput(inputId="subs2",label="Subset second variable",choices=c("none",colnames(data)[-1])),br(),
             conditionalPanel(condition="input.subs2!='none'",
                              helpText("Select the filtering level for the values in the plot."),
                              selectInput(inputId="sub2_level",label="Subset Level",choices="")),br(),
             radioButtons(inputId="graphics.style",label="Select style",choices=c("Small","Large"), selected="Small",inline=T),br(),
#              checkboxInput(inputId="add_plot", label='Change appearance of plot.'),br(),
#              conditionalPanel(condition="input.add_plot",
#                               ),br(),
             actionButton(inputId="reset.graphics",label="Reset all"),br(),
             help.advanced.graphics(),"HELP",br()
    )
  }
  ret
}

advanced.graphics.panel = function(){
  fluidPage(
    fluidRow(
        column(get.graphics.section(),
            width=3
          ),
        column(get.plot.section(),
          width=6
        ),
        column(get.inference.section(),
          width=3
        )
      )
    )  
}