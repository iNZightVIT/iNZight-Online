## shiny validate function will be using for validate the variables type for different 
## data manipulation task.
## however we use others functions instead of need()
## more information you can check the link:
## http://shiny.rstudio.com/articles/validation.html 

not_number <- function(input) {
  
  if (class(input) == "characeter" || class(input) == "factor") {
    "Not accepting categorical variable"
  } else if (class(input) == " ") {
    FALSE
  } else {
    NULL
  } 
}


not_character <- function(input) {
  
  if (class(input) == "numeric" || class(input) == "integer") {
    "Not accepting numeric variable"
  } else if (class(input) == " ") {
    FALSE
  } else {
    NULL
  } 
}



reactiveMulti <- reactiveValues(is = FALSE)

# UI-elements for transform
output$uiTr_columns <- renderUI({
  # uiTr_columns is use to select variables
  # input$tr_columns selected variables
	cols <- varnames()  ## varnames() change to Filter different variable types 
	selectInput("tr_columns", "Select Variable(s):", choices  = as.list(cols), 
              selected = NULL, multiple = reactiveMulti$is, selectize = FALSE)
})

output$uiTr_reorder_levs <- renderUI({
	# if(is.null(input$tr_columns)) return()
	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
  if(!is.null(Fct) && !isFct) return()
  dat <- getdata()
  returnOrder("tr_reorder_levs", levels(dat[,input$tr_columns[1]]))	
})

standardize_1sd <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(as.numeric(scale(x)))
	# if(is.numeric(x)) return(scale(x))
}

centerVar <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(x - mean(x, na.rm = TRUE))
	x
}

medianSplit <- function(x) cut(x, breaks=quantile(x,c(0,.5,1)), include.lowest=TRUE, labels=c("Below","Above"))

decileSplit <- function(x) cut(x, breaks=quantile(x,seq(0,1,.1)), include.lowest=TRUE, labels=seq(1,10,1))

shift <- function(x,shift_by){
  # ?????????????
	# from http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))

  if (length(shift_by) > 1)
    return(sapply(shift_by,shift, x = x))

  # prefer to have positive number create lags as normal in ts-literature
  shift_by <- -shift_by

  out <- NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out <- c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0)
    out <- c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out <- x
  out
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st <<- standardize_1sd
cent <<- centerVar 
msp <<- medianSplit
dec <<- decileSplit

## special
is.Date <<- function(x) is(x, "Date")
nmissing <<- function(x) sum(is.na(x))

# lagx <<- shift

d_mdy <<- function(x) as.Date(mdy(as.character(x)))
d_dmy <<- function(x) as.Date(dmy(as.character(x)))
d_ymd <<- function(x) as.Date(ymd(as.character(x)))

trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", 
	"Center" = "cent", "Standardize" = "st", "Invert" = "inv", "Median split" = "msp", "Deciles" = "dec")

type_options <- list("None" = "", "As Catergory(s)" = "as.factor",  "As number" = "as.numeric")
#, "As integer" = "as.integer",
#	"As character" = "as.character"
  #, "As date (mdy)" = "d_mdy", "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd"
  

trans_types <- list("None" = "", "Convert variable type" = "type", 
                    "Transform Variables" = "change", "Create New Variables" = "create", 
                    #"Clipboard" = "clip", 
                    "Recode" = "recode", 
                    "Rename Variables" = "rename", "Reorder Columns" = "reorder_cols", 
                    "Reorder Levels" = "reorder_levs", 
                    "Delete Variables" = "remove", "Remove missing" = "na.remove"
                    , "Drop observations" = "sub_filter"
)

output$ui_Transform <- renderUI({

	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    
    selectInput("tr_changeType", "Transformation type:", trans_types, 
                selected = "", selectize = FALSE),
    conditionalPanel(condition = "input.tr_changeType == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, 
                  selected = "", selectize = FALSE)
    ),
    conditionalPanel(condition = "input.tr_changeType == 'change'",
	    selectInput("tr_transfunction", "Apply function:", trans_options,
	                selectize = FALSE)
    ),
    conditionalPanel(condition = "input.tr_changeType == 'create'",
	    returnTextInput("tr_transform", "Type in an expression to comput a new variable", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'recode'",
	    list(returnTextInput("tr_recode_from", 
                       "Map values from:'A','B' "
                       ),
	         returnTextInput("tr_recode_to", 
	                         "Map values to:'A','B' "
	         )
	    )
    ),
    conditionalPanel(condition = "input.tr_changeType == 'rename'",
	   	returnTextInput("tr_rename", "Rename (separate by ','):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'sub_filter'",
      returnTextInput("tr_subset", "Drop Observations (e.g., price > 5000)", '')
    ),
	 uiOutput("uiTr_columns"),
    conditionalPanel(condition = "input.tr_changeType != ''",
	    actionButton("addtrans", "Save changes")
	  ),
    conditionalPanel(condition = "input.tr_changeType == 'reorder_cols'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    returnOrder("tr_reorder_cols", varnames())
    ),
    conditionalPanel(condition = "input.tr_changeType == 'reorder_levs'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    uiOutput("uiTr_reorder_levs")
    )
	 
  ), 
  
  helpModal('Manipulate variables','manipulatevarID',inclMD("tools/help/transform.md"))
  )
})

observe({
  
  if (is.null(input$tr_changeType))
    return()
  
  if (input$tr_changeType != 'type')
    updateSelectInput(session, "tr_typefunction", selected = '')
  
  if (input$tr_changeType != 'change')
    updateSelectInput(session, "tr_transfunction", selected = '')
  
  
  
  
})

view.message <- reactiveValues(error = NULL)

transform_main <- reactive({

	# if(input$datatabs != 'Transform') return()
	# if(is.null(input$datasets)) return()
	# if(is.null(input$tr_changeType) || input$tr_changeType == '') return()
	if(is.null(input$tr_changeType)) return()

	dat <- getdata()

	if(input$tr_changeType == "") {
		if(!is.null(input$tr_columns)) {
		  if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
			dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		} else {
			return()
		}
		# return(dat)
	}

	if(input$tr_changeType == 'reorder_cols') {
	  if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
    if(is.null(input$tr_reorder_cols)) {
      ordVars <- colnames(dat)
 	  } else {
   	  ordVars <- input$tr_reorder_cols
    }
 	  return(dat[,ordVars, drop = FALSE])
  }

	if(input$tr_changeType == 'na.remove') {
    
		if(!is.null(input$tr_columns)) {
		  if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
			return(dat[complete.cases(dat[,input$tr_columns]),])
		} else {
	 	  return(na.omit( dat ))
		}
  }

	if(input$tr_changeType == 'sub_filter') {
	  if(input$tr_subset != '') {
	    non_space = gsub('\\s', '', input$tr_subset)
      # step 1: find pattern
      numID <- FALSE
      if (grepl('^[[:digit:]]+$',non_space))  ## single digit
        numID <- TRUE
      else if (grepl('^[[:digit:]]+(\\,|\\:)',non_space))  ## digits
        numID <- TRUE
      
      
      validate(
        need(numID, "Wrong Syntax")
        )
        
        e <- new.env(parent = emptyenv())
        e$`:` <- get(":", baseenv())
        e$`(` <- get("(", baseenv())
        e$expression <- expression
        e$eval <- eval
        e$c <- c
        condition <- paste0("c(", non_space, ")")
        e$lo = parse(text=condition)
        test <- try(local(eval(lo), envir=e),silent = TRUE)
        validate(
          need(class(test)!="try-error", "Wrong Syntax")
          )
        id <- unique(test)
        seldat <- dat[-id,]
        if(is.data.frame(seldat)) {
          validate(
            need(nrow(seldat)>0, "No result matching")
          )
          return(seldat)
        }
  	}
  }
  
  
	
  
	if(input$tr_changeType == 'combine') {
	  
	}
  

	if(!is.null(input$tr_columns)) {

		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		if(input$tr_transfunction != '') {
      
      for (k in 1:ncol(dat)) {
      validate(
        not_number(dat[,k])
      )
      }
      
			cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))
			dat <- cbind(dat,colwise(input$tr_transfunction)(dat))
			colnames(dat) <- cn
		}
		if(input$tr_typefunction != '') {
			# dat <- cbind(dat,colwise(input$tr_typefunction)(dat))
			dat <- colwise(input$tr_typefunction)(dat)
      if (input$tr_typefunction == "as.factor")
        post <- "cat"
      if (input$tr_typefunction == "as.numeric")
        post <- "num"
      names(dat) = paste0(names(dat),".", post)
		}
	} else {
		# if(input$tr_changeType != "" && input$tr_changeType != "sub_filter") return()
		if(!input$tr_changeType %in% c("", "sub_filter", "create", "clip")) return()
	}

	if(!is.null(input$tr_columns) & input$tr_changeType == 'reorder_levs') {
    if(!is.null(input$tr_reorder_levs)) {
    	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
		  if(isFct) dat[,input$tr_columns[1]] <- factor(dat[,input$tr_columns[1]], levels = input$tr_reorder_levs)
    }
  }

	if(input$tr_changeType ==  'recode') {
    
    if (is.null(input$tr_recode_from))
      return()
    if (is.null(input$tr_recode_to))
      return()
    if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
    dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
    for (k in 1:ncol(dat)) {
      validate(
        not_character(dat[,k])
      )
    }
    
    if (input$tr_recode_from != '' & input$tr_recode_to != '') {
      if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
      getFrom = strsplit(input$tr_recode_from, ",")[[1]]
      getTo = strsplit(input$tr_recode_to, ",")[[1]]
      if (length(getFrom) != length(getTo)){
        p <- try(mapvalues(dat[, input$tr_columns[1]], getFrom, getTo, warn_missing = FALSE), silent = TRUE)
        if (class(p)=="try-error"){
          view.message$error <- "Unequal Length (You may need to check whether you use ',' to split the values)"
          return(dat)
        }
      }
      else
        view.message$error <- FALSE
      
      newvar <- mapvalues(dat[, input$tr_columns[1]], getFrom, getTo)
      cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="."))
      dat <- data.frame(dat,newvar)
      colnames(dat) <- cn
      
      
    }
    #if (input$tr_recode_from != '' & input$tr_recode_to == '') {
      # assign content into view.message$error
      
      
    #} 
    #if (input$tr_recode_from == '' & input$tr_recode_to != '') {
      # assign content into view.message$error
      
      
    #}
    
	}


	if(input$tr_changeType == 'rename') {
		if(!is.null(input$tr_columns) && input$tr_rename != '') {
		  if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
			rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
			rcom <- rcom[1:min(length(rcom),length(input$tr_columns))]
			names(dat)[1:length(rcom)] <- rcom
		}
	}

	if(input$tr_changeType == 'create') {
	  if(!all(input$tr_columns %in% colnames(dat))) return() ## to protect changing data set but the variable name not updating and generating error
		if(input$tr_transform != '') {
      originSource <- getdata()
      validate(
        need(grepl("\\:\\=", input$tr_transform), "Not accept syntax")
        )
      varname = gsub("(.*)\\:\\=(.*)","\\1",input$tr_transform)
      formu = gsub("(.*)\\:\\=(.*)","\\2",input$tr_transform)
      validate(
        need(formu!="", "Empty formula given")
        )
		  e <- new.env(parent = emptyenv())
      ## another way to do this is to check names(methods:::.BasicFunsList)
      ## and get the function name you want
		  e$`+` <- get("+", baseenv())
		  e$`-` <- get("-", baseenv())
		  e$`*` <- get("*", baseenv())
		  e$`/` <- get("/", baseenv())
		  e$`(` <- get("(", baseenv())
      e$`:` <- function(e1,e2) paste0(e1,":",e2)
      e$expression <- expression
      e$eval <- eval
		  e1 <- list2env(originSource, envir=e)
		  e1$lo = parse(text=formu)
		  test <- try(local(eval(lo), envir=e1),silent = TRUE)
      validate(
        need(class(test)!="try-error", "Wrong")
        )
      out = test

			
				cn <- c(colnames(dat),varname)
				dat <- cbind(dat,out)
				colnames(dat) <- cn

				head(dat)

			
		}
	}

	dat
})

output$transform_data <- reactive({
  
	dat <- transform_main()
	if(is.null(dat)) return(invisible())
	if(is.character(dat)) return(dat)

	nr <- min(nrow(dat),input$globalrowNum)
	dat <- dat[1:nr,, drop = FALSE]
  
  # data <- data.frame(date2character_dat(dat))
	dat <- date2character_dat(dat)

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
	html <- paste(html, '<label>', input$globalrowNum, 'rows shown. See Input-tab for details.</label>') 
	html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html

})

output$transform_summary <- renderPrint({

	# if(isolate(input$datatabs) != 'Transform') return(invisible())

	dat <- transform_main()
	if(is.null(dat)) return(invisible()) 			# ...

	isFct <- sapply(dat, is.factor)
	isNum <- sapply(dat, is.numeric)
	isDate <- sapply(dat, is.Date)
	isChar <- sapply(dat, is.character)
	isLogic <- sapply(dat, is.logical)

	if(sum(isNum) > 0) {
		cat("Summarize numeric variables:\n")
		# print(describe(dat[,isNum])[,c("n","mean","median","min","max","range","sd","se","skew","kurtosis")])
		res <- data.frame(psych::describe(dat[isNum])[,c("n","mean","median","min","max","sd"
                                              #,"se","skew","kurtosis"
                                              )])
		res$missing <- c(colwise(nmissing)(dat[,isNum, drop = FALSE]))
		print(res)
		cat("\n")
	}
	if(sum(isFct) > 0) {
		cat("Summarize catergorical variable:\n")
		print(summary(dat[,isFct]))
		cat("\n")
	}
	if(sum(isDate) > 0) {
		cat("Earliest dates:\n")
		print(colwise(min)(dat[,isDate]))
		cat("\nFinal dates:\n")
		print(colwise(max)(dat[,isDate]))
		cat("\n")
	}
	if(sum(isChar) > 0) {
		#cat("Summarize character variables:\n")
		#print(table(dat[,isChar]))
	  cat("Summarize catergorical variable:\n")
	  print(summary(dat[,isChar]))
		cat("\n")
	}
	if(sum(isLogic) > 0) {
		cat("Summarize logical variables:\n")
		print(table(dat[,isLogic]))
		cat("\n")
	}
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform_main()
		if(is.null(dat)) return()
		if(is.character(dat)) return(dat)

		if(input$tr_changeType == 'remove') {
		  values[[input$datasets]][,colnames(dat)] <- list(NULL)
	  } else if(input$tr_changeType == 'type') {
      values[[input$datasets]] <- data.frame(values[[input$datasets]], dat)
		} else if(input$tr_changeType == 'na.remove') {
		  values[[input$datasets]] <- dat
		} else if(input$tr_changeType == 'sub_filter') {
		  values[[input$datasets]]<- dat
		} else if(input$tr_changeType == 'rename') {
			changedata_names(input$tr_columns, colnames(dat))
		} else if(input$tr_changeType == 'reorder_cols') {
		  values[[input$datasets]] <- values[[input$datasets]][,input$tr_reorder_cols]
	  } else {
			changedata(dat, colnames(dat))
		}

		# reset input values once the changes have been applied
	 	updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
	 	updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
	 	updateTextInput(session = session, inputId = "tr_rename", label = "Rename (separate by ','):", value = '')
	 	updateTextInput(session = session, inputId = "tr_copyAndPaste", label = "", '')
		updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "")
	})
})

output$error_result <- renderPrint({
  
  if (is.null(view.message$error))
    return(cat(" "))
  
  if (!is.logical(isolate(view.message$error)))
    print(isolate(view.message$error)) 
  else
    return("Correct process")
})

output$BlankSpace <- renderPlot({
  
  plot(1,1, xlab=" ",ylab=" ", type= "n",axes = FALSE)
  
})


observe({
  
  if (!is.null(input$tr_changeType)){
  if (input$tr_changeType %in% c("recode","rename", "reorder_levs")) {
    
    reactiveMulti$is <- FALSE
  }
  else
    reactiveMulti$is <- TRUE
  
  }
})