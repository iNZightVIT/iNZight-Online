help.switch = function(){
  helpModal('Switch data','switch_data',inclMD("gui-elements/notes/switch.data.md"))
}

# returns all files and directories in the data directory
# and tests whether it is a directory, files are removed. 
get.data.dirs = function(){
  list.files("data",include.dirs=T,full.names=T)[file.info(paste("data",list.files("data"),sep="/"))[,"isdir"]]
}

# puts together a list of shiny widgets to fill the sidebar
get.sidebar.switch = function(data.select){
  choices1=get.data.dirs()
  if(is.null(data.select)){
    ret = list(selectInput(inputId="data_select",
                           label="Select Data set category",
                           choices=basename(choices1),
                           selected=basename(choices1[1])))
  }else{
    ret = list(selectInput(inputId="data_select",
                           label="Select Data set category",
                           choices=basename(choices1),
                           selected=data.select)
               )
  }
  for(i in 1:length(choices1)){
    radio.list = get.radio.list(choices1[i],"")
    if(!is.null(radio.list)){
      ret[[i+1]] = conditionalPanel(condition=paste0("input.data_select=='",basename(choices1[i]),"'"),
                                    radio.list)
    }else{
      ret[["no.button"]] = c(ret[["no.button"]],choices1[i])
    }
  }
  ret
}

get.switch.data.main = function(has.input){
  if(is.null(data)){
    if(!has.input){
      list(div(class="page-divider"),
           h1("No data selected!"),br(),
           div(class="page-divider"),
           h1("No Data to select!"),br(),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }else{
      list(div(class="page-divider"),
           h1("No data selected!"),br(),
           div(class="page-divider"),
           h1("Data set to switch to"),br(),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }
  }else{
    if(!has.input){
      list(div(class="page-divider"),
           h1(paste("Selected data set: ",data.name)),br(),
           p(paste("Dimension rows: ",dim(data)[1],sep="")),#br(),
           p(paste("Dimension columns: ",dim(data)[2],sep="")),
           p(paste("Column names: ",paste(colnames(data),collapse=", "),sep="")),
           div(class="page-divider"),
           h1("No Data to select!"),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }else{
      list(div(class="page-divider"),
           h1(paste("Selected data set: ",data.name)),br(),
           p(paste("Dimension rows: ",dim(data)[1],sep="")),#br(),
           p(paste("Dimension columns: ",dim(data)[2],sep="")),
           p(paste("Column names: ",paste(colnames(data),collapse=", "),sep="")),
           div(class="page-divider"),
           h1("Data set to switch to"),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }
  }
}

#switch.data.panel creates reactive panel for input files
switch.data.panel = function(data.select){
  sidebar.widgets = get.sidebar.switch(data.select)
  no.button = ""
  if("no.button"%in%names(sidebar.widgets)){
    no.button = basename(sidebar.widgets[["no.button"]])
    sidebar.widgets[["no.button"]] = NULL
  }
  if(!is.null(data.select)){
    if(data.select%in%no.button){
      sidebarLayout(
        sidebarPanel(sidebar.widgets,br(),br(),help.switch(),"HELP",br(),
                     HTML("&nbsp;"),br()),
        mainPanel(get.switch.data.main(T))
      )
    }else{
      sidebarLayout(
        sidebarPanel(sidebar.widgets,
                     actionButton(inputId="change_set",label="Select Set"),
                     br(),br(),help.switch(),"HELP",br(),HTML("&nbsp;"),br()),
        mainPanel(get.switch.data.main(T))
      )
    }
  }else{
    sidebarLayout(
      sidebarPanel(sidebar.widgets),
      mainPanel(get.switch.data.main(F))
    )
  }
}
