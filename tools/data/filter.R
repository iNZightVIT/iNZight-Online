

output$filter_ui <- renderUI({
  
    wellPanel(
      textInput("newDataName", label = "Provide a name to new data"),
      uiOutput('ui_group_filter'),
      uiOutput('ui_group_aggregate'),
      actionButton("getFilter","Generate Filter sample")
    )
  
  
})




output$view_data <- reactive({
  
  dat = Filter_main() # Filter_main()
  nr = min(nrow(dat), input$globalrowNum)
  dat <- dat[1:nr,, drop = FALSE]
  
  dat <- data.frame(date2character_dat(dat))
  
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste(html, '<label>', input$globalrowNum, 'rows shown. See View-tab for details.</label>') 
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
  
})

output$view_data2 <- reactive({
  
  dat = Aggregate_main() # Aggregate_main()
  nr = min(nrow(dat), input$globalrowNum)
  dat <- dat[1:nr,, drop = FALSE]
  
  dat <- data.frame(date2character_dat(dat))
  
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste(html, '<label>', input$globalrowNum, 'rows shown. See View-tab for details.</label>') 
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
  
})



view.data <- reactive({
  
  ## because getdata() is absort the signal from input$datasets
  ## here we need something that we need something called the change
  ## so a new independent datasets seletion dropdown list is created to acoording this.
  values[[input$datasets]]
  
})



output$rowIndexModuel <- renderUI({
  
  if (input$fliter.choices == "row.index")
    textInput('rowIndexModuel', label = "Given row index number to remove row number", 
              value = '')
  
})

output$factorModuel1 <- renderUI({
  
  if (input$fliter.choices == "cat.level")
    selectInput('factorModuel1', label = "Filter data by:",
                choices = c('',names(Filter(is.factor, view.data()))), 
                selectize = FALSE)
  
  
})

output$factorModuel2 <- renderUI({
  
  if (!is.null(input$factorModuel1) && input$factorModuel1 != "" & input$fliter.choices == "cat.level")
    selectInput('factorModuel2', label = "Select levels to include\n(Hold Ctrl to choose many)",
                choices = c("", levels(view.data()[, input$factorModuel1])), 
                multiple = TRUE, 
                selectize = FALSE)
  
})

output$condModuel <- renderUI({
  
  if (!is.null(input$fliter.choices) && input$fliter.choices == "subset")
    list(textInput('condModuel', label = "Type in your subsetting expression: ", 
              value = ''),
         helpText("eg: X >=20", "eg: X == 20")
         )
  
  
})

output$randomModuel <- renderUI({
  
  if (input$fliter.choices == "rsample")
    list(
      numericInput('randomModuel1', label = 'Number of Samples',
                   step =1,
                   value = 1, min =1),
      numericInput('randomModuel2', label = 'Specify the size of your sample', value = 0, min =0, 
                   step =10
                   )
    )
  
  
})

output$ui_group_filter <- renderUI({
  
  if (!is.null(input$datatabs) && input$datatabs == "Filter Dataset")
    list(
      radioButtons("fliter.choices", 
                   "choices of Filter type",
                   c(
                     #"Row Index" = "row.index",
                     "levels of a categorical variable" = "cat.level",
                     "numeric condition" = "subset",
                     "randomly" = "rsample")
      ),
      uiOutput('rowIndexModuel'),
      uiOutput('factorModuel1'),
      uiOutput('factorModuel2'),
      uiOutput('condModuel'),
      uiOutput('randomModuel')
    )
  
})

output$ui_group_aggregate <- renderUI({
  
  if (!is.null(input$datatabs) && input$datatabs == "Aggregate data")
    list(
      selectInput("aggreVars", label = "Aggregate over variables", 
                  choices = c("",names(Filter(is.factor, view.data()))), 
                  multiple = TRUE,
                  selectize = FALSE, 
                  selected = ""),
      
      selectInput("sumFunc", "Summaries functions: ",
                  choices = c("Mean"="mean", 
                              "Median" = "median", 
                              "Sum" = "sum",
                              "Standard Deviation" = "sd",
                              "IQR" = "IQR",
                              "Count" = "length"),
                  multiple = TRUE,
                  selectize = FALSE)
    )
  
})

Filter_main <- reactive({
  
  dat <- view.data()
  
  if (!is.null(input$fliter.choices) && input$fliter.choices == 'subset') {
    if(!is.null(input$condModuel) && input$condModuel != '') {
      
      condition = gsub('\\s', '', input$condModuel)
      validate(
        need(!grepl("\\=(\\<|\\>|\\!)", condition, perl=TRUE), "Wrong Syntax!"), # =<\=>\=! right syntax should be stop here to avoid interpreter to get = > /= < ,etc..
        need(!grepl("\\'|\"", condition, perl=TRUE), "Wrong Syntax!"), # no '' or "" cause here is just numeric
        need(grepl("([[:alnum:]]+)[[:punct:]]{1,2}([[:alnum:]]+)", condition), "Unfinish Statement!") # have to be A ==/>=/... B
        # Here is totally for numeric variable subset
        # cause == 'a' is not recognised 
        # one can use this
        # "([[:alnum:]]+)[[:punct:]]{1,2}((\\'|\")?)([[:alnum:]]+)"
        # but the other thing is avoiding user write 'A" or "A'
        # an easy way is to only allow '' or ""
        
      )
      e <- new.env(parent = emptyenv())
      ## another way to do this is to check names(methods:::.BasicFunsList)
      ## and get the function name you want
      e$`>=` <- get(">=", baseenv())
      e$`>` <- get(">", baseenv())
      e$`<=` <- get("<=", baseenv())
      e$`<` <- get("<", baseenv())
      e$`==` <- get("==", baseenv())
      e$`!=` <- get("!=", baseenv())
      e$`(` <- get("(", baseenv())
      e$`&` <- get("&", baseenv())
      e$`&&` <- get("&&", baseenv())
      e$`|` <- get("|", baseenv())
      e$`||` <- get("||", baseenv())
      e$expression <- expression
      e$eval <- eval
      e$subset <- subset
      e$do.call <- do.call
      e1 <- list2env(dat, envir=e) # this will load all variable in a list form in environment
      # here need to cut the space or non-makesense name to follow R rule 
      
      e1$lo = parse(text=condition)
      test <- try(local(eval(lo), envir=e1),silent = TRUE)
      validate(
        need(class(test)!="try-error", "Wrong Syntax")
      )
      
      # test above is testing
      # so here we re-execute the code again..
      
      selcom <- input$condModuel
      seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
      
      if(!is(seldat, 'try-error')) {
        if(is.data.frame(seldat)) {
          validate(
            need(nrow(seldat)>0, "No result matching")
          )
          return(seldat)
        }
      }
      
#       selcom <- input$condModuel
#       seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
#       
#       if(!is(seldat, 'try-error')) {
#         if(is.data.frame(seldat)) {
#           return(seldat)
#         }
#       }
    }
  }
  
  if (!is.null(input$fliter.choices) && input$fliter.choices == 'cat.level') {
    if(!is.null(input$factorModuel1) && input$factorModuel1 != '') {
      selcom <- input$factorModuel2
      selvar <- as.character(dat[, input$factorModuel1]) 
      id = which(selvar %in% selcom)
      seldat <- dat[id, ]
      if(!is(seldat, 'try-error')) {
        if(is.data.frame(seldat)) {
          return(seldat)
        }
      }
    }
  }
  
  if (!is.null(input$fliter.choices) && input$fliter.choices == 'rsample') {
    if(!is.null(input$randomModuel1) ) {
      sampleNum <- ifelse(!is.na(input$randomModuel1), as.numeric(input$randomModuel1), 1)
      sampleSize <- ifelse(!is.na(input$randomModuel2), as.numeric(input$randomModuel2), 0)
      nr = nrow(dat)
      #if sampleNum*sampleSize > nr
      # return error message
      id = sample(1:nr, size = sampleNum*sampleSize, replace = sampleNum*sampleSize > nr)
      if (sampleNum >1){
        NewVar = as.factor(rep(1:sampleNum, each = sampleSize))
        newDat = data.frame(dat[id, ], sample.num = NewVar)
      }
      else
        newDat = dat[id,]
      return(newDat)
    }
  }
  
  
  
  dat
})


Aggregate_main <- reactive({
  
  dat <- view.data()
  
  if (!is.null(input$aggreVars) && !input$aggreVars %in% names(Filter(is.factor, view.data())) )
    return(dat)
  
  if (is.null(input$sumFunc) || input$sumFunc == '') 
    return(dat)
  
  if (!is.null(input$sumFunc)) {
    selVars <- input$aggreVars
    selVars <- selVars[!selVars %in% '']
    fun <- input$sumFunc
    out <- ddply(na.omit(dat), as.quoted(selVars), numcolwise(fun[1]))
    nameVec <- c(selVars, paste0(names(Filter(is.numeric, dat)), ".", fun[1]))
    names(out) <- nameVec
    if (length(fun) > 1) {
      for (i in 2:length(fun)){
        out1 <- ddply(na.omit(dat), as.quoted(selVars), numcolwise(fun[i]))  
        nameVec <- c(selVars, paste0(names(Filter(is.numeric, dat)), ".", fun[i]))
        names(out1) <- nameVec 
        out = join(out,out1, by=selVars)
      }
    }
    id = sort(names(out)[-(1:length(selVars))])
    out[, c(selVars, id)]
  }
  
})

conditionM <- reactive({
  
  input$rowIndexModuel
  
})

observe({
  
  if (!is.null(input$rowIndexModuel)){
    p = isolate(conditionM())
    d = substitute(p)
    # d = paste0("c(", d, ")")
    ge = try(parse(text = d), silent = TRUE)
    if (!is(ge, "try-error"))
      print(ge)
  }
})


observe({
  
  input$getFilter
  if (!is.null(input$getFilter) && input$getFilter > 0)
  isolate({
    if (!is.null(input$datatabs) && input$datatabs == "Filter Dataset"){
      newData <- Filter_main()
      newName <- values[["datasetlist"]][1] 
      if (!is.null(input$newDataName) && input$newDataName != '')
        newName <- input$newDataName
      if (newName %in% values[["datasetlist"]])
        newName <- paste0(newName, ".new")
      values[[newName]] <- newData
      values[["datasetlist"]] <- c(newName, values[["datasetlist"]])
      updateTextInput(session, 'rowIndexModuel', value = '')
      updateSelectInput(session, 'factorModuel1', selected = '')
      updateSelectInput(session, 'factorModuel2', selected = "")
      updateTextInput(session, 'condModuel', value = '')
      updateNumericInput(session, 'randomModuel1', value = 1)
      updateNumericInput(session, 'randomModuel2', value = 0)
    }
    if (!is.null(input$datatabs) && input$datatabs == "Aggregate data"){
      newData <- Aggregate_main()
      newName <- values[["datasetlist"]][1] 
      if (!is.null(input$newDataName) && input$newDataName != '')
        newName <- input$newDataName
      if (newName %in% values[["datasetlist"]])
        newName <- paste0(newName, ".new")
      
      values[[newName]] <- newData
      values[["datasetlist"]] <- c(newName, values[["datasetlist"]])
      updateSelectInput(session, 'aggreVars', selected = '')
      updateSelectInput(session, 'sumFunc', selected = '')
    }
  })
  
  
})
