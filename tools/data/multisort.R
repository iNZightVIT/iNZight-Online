

output$sortList_ui <- renderUI({
  
  list(
    selectInput('sortVar1', label = "Sort data by 1st Variable",
                choices = c('',names(getdata())), 
                selectize = FALSE),
    radioButtons('incdecr1', label = "", choices = c("increasing","decreasing")),
    selectInput('sortVar2', label = "Sort data by 2nd Variable",
                choices = c('',names(getdata())), 
                selectize = FALSE),
    radioButtons('incdecr2', label = "",  choices = c("increasing","decreasing")),
    selectInput('sortVar3', label = "Sort data by 3rd Variable",
                choices = c('',names(getdata())), 
                selectize = FALSE),
    radioButtons('incdecr3', label = "",  choices = c("increasing","decreasing")),
    selectInput('sortVar4', label = "Sort data by 4th Variable",
                choices = c('',names(getdata())), 
                selectize = FALSE),
    radioButtons('incdecr4', label = "",  choices = c("increasing","decreasing")),
    textInput('sortDataName', label = "Provide a new name to sorted data"),
    actionButton('getConfirm', label ="Confirm Sorting")
    )
  
})


sortList_main <- reactive({
  
  dataSet <- getdata()
  argList <- list()
  for (i in 1:4) {
    con <- eval(parse(text=paste0("input$sortVar",i)))
    con2 <- eval(parse(text=paste0("input$incdecr",i)))
    if (is.null(con))
      return(dataSet)
    if (con != '') {
      datai <- dataSet[, con]
      if (inherits(datai, "character") | inherits(datai, "factor"))
        datai <- xtfrm(datai)
      if (con2 != "increasing")
        datai <- -datai
      argList[[length(argList) + 1]] <- datai
    }
    
  }
  
  if (identical(argList,list()))
    return(dataSet)
  idx <- do.call("order", argList)
  
  dataSet[idx, ]
  
})

output$view_data3 <- reactive({
  
  dat = sortList_main() # Filter_main()
  nr = min(nrow(dat), input$globalrowNum)
  dat <- dat[1:nr,, drop = FALSE]
  
  #dat <- data.frame(date2character_dat(dat))
  
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste(html, '<label>', input$globalrowNum, 'rows shown. See View-tab for details.</label>') 
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
  
})

observe({
  
  input$getConfirm
  if (!is.null(input$getConfirm) && input$getConfirm > 0)
    isolate({
      newData <- sortList_main()
      newName <- values[["datasetlist"]][1] 
      if (!is.null(input$sortDataName) && input$sortDataName != '')
        newName <- input$sortDataName
      if (newName %in% values[["datasetlist"]])
        newName <- paste0(newName, ".new")
      values[[newName]] <- newData
      values[["datasetlist"]] <- c(newName, values[["datasetlist"]])
      
      
    })
  
  
})