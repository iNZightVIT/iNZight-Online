help.current = function(){
  helpModal('Current data','current_data',inclMD("gui-elements/notes/current.data.md"))
}

current.data = function(){
  ret = list()
  if(is.null(data)){
    ret[[1]] = h4("No data is selected or imported. Please import or selct data.")
  }else{
    ret[[1]] = h4(paste0("Current data: ",data.name))
  }
  ret[[2]] = dataTableOutput(outputId="current")
  ret[[3]] = br()
  ret[[4]] = br()
  ret[[5]] = help.current()
  ret[[6]] = "HELP"
  ret[[7]] = br()
  ret[[8]] = HTML("&nbsp;")
  fluidPage(ret)
}