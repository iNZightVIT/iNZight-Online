help.reorder = function(){
  helpModal('Reorder Levels','reorder_levels',inclMD("gui-elements/notes/reorder.levels.md"))
}

get.reorder.sidebar =  function(choices2=c(),selected1=""){
  choices1 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = colnames(data)[unlist(lapply(1:ncol(data),function(index,data){(is.factor(data[,index])|is.character(data[,index]))},data))]
  }else{
    choices2=c()
  }
  list(selectInput("select.column","Select Column",choices=c("",choices1),multiple=F,selectize=F,selected=selected1),br(),
       selectInput("select.item", "Select in new Order",choices=choices2,multiple=T,selectize=T),br(),
       actionButton("reorder","Reorder"),br(),br(),help.reorder(),"HELP",br(),HTML(""))
}

reorder.levels.panel =function(choices=c(),selected=""){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(get.reorder.sidebar(choices)),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.reorder.sidebar(choices)),
      mainPanel(verbatimTextOutput("maintext.reorder"))
    )
  }
}