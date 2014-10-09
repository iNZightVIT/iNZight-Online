#######################################
# Manage datasets in/out of iNZight
#######################################


#################

output$ui_Manage <- renderUI({
  list(wellPanel(
      radioButtons(inputId = "dataType", label = "Load data:", c(
        #".rda" = "rda", 
        ".csv" = "csv",                                              
        #"clipboard" = "clipboard", 
        "examples" = "examples",
        "URL" = "URL"), 
        inline=TRUE,
        selected = "csv"),
      conditionalPanel(condition = "input.dataType != 'URL' && input.dataType != 'examples'",
        checkboxInput('header', 'Header', TRUE),
        checkboxInput('rowNames', 'First column as row names', FALSE),
        radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), ','),
        fileInput('uploadfile', '', multiple=TRUE)
      ),
      conditionalPanel(condition = "input.dataType != 'csv' && input.dataType != 'examples'",
        textInput("URL_input", "Paste in data set URL", ""),
        checkboxInput('header', 'Header', TRUE),
        checkboxInput('rowNames', 'First column as row names', FALSE),
        radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), ',')
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        actionButton('loadClipData', 'Paste data')
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton('loadExampleData', 'Load examples')
      )
    ),
    wellPanel(
      radioButtons(inputId = "saveAs", label = "Save data:", 
                   c(
                     ".rda" = "rda", 
                     ".csv" = "csv", 
                     #".txt" = "tab"
                     "copy to clipboard" = "clipboard"
                     ), 
        selected = "csv"),
      checkboxInput("man_add_descr","Add/edit data description", FALSE),
      conditionalPanel(condition = "input.man_add_descr == true",
        actionButton('updateDescr', 'Update description')
      ),
      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        actionButton('saveClipData', 'Copy data')
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard' && input.man_add_descr == false",
        downloadButton('downloadData', 'Save')
      )
    ),
    wellPanel(
      numericInput('globalrowNum', 'Adjusting the showing row number in this App', value = 15, min = 0, step = 1)
      ),
    wellPanel(
      # returnTextInput("data_rename", "Rename dataset:", input$datasets),
      textInput("data_rename", "", input$datasets),
      actionButton('renameButton', 'Rename dataset')
     ),
    wellPanel(
      uiOutput("uiRemoveDataset"),
      actionButton('removeDataButton', 'Remove data')
    )
    #helpModal('Manage','manageHelp',inclMD("tools/help/manage.md"))
  )
})

# updating the dataset description
observe({
  if(is.null(input$updateDescr) || input$updateDescr == 0) return()
  isolate({
    values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
    updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
  })
})

observe({
  if(is.null(input$data_rename)) return()
  if(is.null(input$renameButton) || input$renameButton == 0) return()

  isolate({
    # names(values)[names(values)==input$datasets] <- input$data_rename 
    values[[input$data_rename]] <- getdata()
    values[[input$datasets]] <- NULL
    values[[paste0(input$data_rename,"_descr")]] <- values[[paste0(input$datasets,"_descr")]]
    values[[paste0(input$datasets,"_descr")]] <- NULL

    ind <- which(input$datasets == values[['datasetlist']])
    values[['datasetlist']][ind] <- input$data_rename

    updateSelectInput(session, "datasets", label = "Already Loaded:", choices = values$datasetlist,
      selected = input$data_rename)
  })
})

# managing the description of the dataset
dataDescriptionOutput <- function(ret = 'html') {
  descr <- values[[paste0(input$datasets,"_descr")]]
  if(is.null(descr) || descr == "") {
    return("")  # if there is no data description
  } else {
    # if there is a data description and the 'add/edit' box has been checked
    ifelse(ret == 'md',return(descr), 
      return(suppressWarnings(markdownToHTML(text = descr, stylesheet="www/empty.css"))))
  }
}

# removing datasets
output$uiRemoveDataset <- renderUI({
  # Drop-down selection of data set to remove
  selectInput(inputId = "removeDataset", label = "Remove data from memory:", 
    choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

observe({
  # removing datasets
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({

    datasets <- values[['datasetlist']]
    if(length(datasets) > 1) {         # don't remove the last dataset
      removeDataset <- input$removeDataset  
      if(length(datasets) == length(removeDataset)) {
        # datasets <- ""
        removeDataset <- removeDataset[-1]
      }
      for(rem in removeDataset) {
        values[[rem]] <- NULL
      }
      datasets <- datasets[-which(datasets %in% removeDataset)]
      values[['datasetlist']] <- datasets
    }
  })
})

# saving data
observe({
  # 'saving' data to clipboard
  if(is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
    } else { 
      write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    }
    updateRadioButtons(session = session, inputId = "saveAs", label = "Save data:", 
                       c(
                         ".rda" = "rda", 
                         ".csv" = "csv", 
                         "copy to clipboard" = "clipboard"), 
                       selected = "csv")
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets

    if(ext == 'rda') {
      if(input$man_data_descr != "") {

        # save data description
        dat <- getdata()
        attr(dat,"description") <- values[[paste0(robj,"_descr")]]
        assign(robj, dat)
        save(list = robj, file = file)
      } else {
        assign(robj, getdata())
        save(list = robj, file = file)
      }
    } else if(ext == 'csv') {
      assign(robj, getdata())
      write.csv(get(robj), file)
    }
  },
  contentType = "text/csv"  # optional
)

# loading data
observe({
  # loading files from disk
  inFile <- input$uploadfile
  # if(!is.null(inFile) && length(inFile) > 0) {
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      # iterating through the files to upload
      for(i in 1:(dim(inFile)[1])) loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$dataType)
    })
  }
})

observe({
  #if(input$URL_input=="") return()
  url = input$URL_input
  isolate({
    if(!is.null(input$URL_input)){
      if(url!=""){
        loadUserData(basename(url),url,"URL")  
      }
    }
  })
})

# loading all examples files (linked to helpfiles)
observe({
  if(is.null(input$loadExampleData) || input$loadExampleData == 0) return()
  isolate({

    # loading data bundled with Radiant
    path <- "data/data_pack/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # loading data available for Rady students
    path <- "data/data_rady/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # sorting files alphabetically
    values[['datasetlist']] <- sort(values[['datasetlist']])
  })
})

# observe({
#   # 'reading' data from clipboard
#   if(is.null(input$loadClipData) || input$loadClipData == 0) return()
#   isolate({
#     os_type <- .Platform$OS.type
#     if (os_type == 'windows') {
#       
#       dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
#       if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
#     } else { 
# 
#       dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
#       if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
#     }
# 
#     values[['xls_data']] <- as.data.frame(dat)
#     values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
#     updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
#   })
# })

loadUserData <- function(filename, uFile, ext) {
  # ext <- file_ext(filename)
  # filename <- "test.rda"
  # ext <- 'rda'
  ext2 = strsplit(filename,".",fixed=T)[[1]]
  objname <- sub(paste(".",ext2[length(ext2)],sep = ""),"",basename(filename))
  message(paste("ext=XXX",objname,"XXX",sep=""))
  # ext <- tolower(ext)

  # if(ext == 'rda' || ext == 'rdata') {
  if(ext == 'rda') {
    # objname will hold the name of the object(s) inside the R datafile
    robjname <- load(uFile)

    if(length(robjname) > 1) {
      # keeping this bit for legacy
      values[[objname]] <- as.data.frame(get(robjname[-which(robjname == "description")]))
      values[[paste0(objname,"_descr")]] <- get("description")
    } else {
      values[[objname]] <- as.data.frame(get(robjname))
      values[[paste0(objname,"_descr")]] <- attr(values[[objname]], "description")
    }
  } else if(ext == 'sav') {
    values[[objname]] <- as.data.frame(as.data.set(spss.system.file(uFile)))
  } else if(ext == 'dta') {
    values[[objname]] <- read.dta(uFile)
  } else if(ext == 'csv') {
    rowNames <- NULL
    if (input$rowNames) 
      rowNames <- 1
    
    values[[objname]] <- read.csv(uFile, header=input$header, comment.char = "#",
                                  sep=input$sep, row.names = rowNames, quote = "",
                                  na.strings=c("NA","NULL"))
  }else if(ext == 'URL'){
    rowNames <- NULL
    if (input$rowNames) 
      rowNames <- 1
    tryCatch({
      values[[objname]] <- read.table(uFile, header=input$header, comment.char = "#",
                                    sep=input$sep, row.names = rowNames, quote = "",
                                    na.strings=c("NA","NULL"))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", uFile))
      message("Here's the original error message:")
      message(cond)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", uFile))
      message("Here's the original warning message:")
      message(cond)
    },
    finally={
      message(paste("Processed URL:", uFile))
    })
  }
  if(objname!=""&objname%in%names(values)){
    if(length(values[['datasetlist']]) == 0 || values[['datasetlist']][1] == '') {
      values[['datasetlist']] <- c(objname)
    } else {
      values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
    }
  }
}

output$uiDatasets <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "datasets", label = "Already Loaded:", choices = values$datasetlist, 
    selected = state_init("datasets"), multiple = FALSE)
})

output$htmlDataExample <- renderText({

  
  
  dat <- getdata()
  if(is.null(dat)) return()

  # Show only the first k rows
  nr.intial <- 0
  if (!is.null(input$globalrowNum))
    nr.intial <- input$globalrowNum
  nr <- min(nr.intial,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])
  dat <- date2character_dat(dat) # dealing with dates
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
})

