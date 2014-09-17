# Please check model_ui.R for how input id corresponding to the below events
# l1: Fraction
# l2: Factorial
# l3: Square
# l4: Cubi
# Into1: fit model
# Into4: clean the print result

schoice <- reactiveValues(left = NULL, right = NULL)

mchoice <- list()  # a fixed list that storage message but no need to be reactive.


outputResult <- reactiveValues(model = NULL)

output$selection <- renderPrint({
  out <- input$mychooser
  if (!is.null(out)) 
    names(out) <- c("Exploratory Variable(s)", "Confounder Variable(s)")
  out
})

result.process <- reactive({
  
  input$Into1
  if (!is.null(input$Into1) && input$Into1 > 0) {
    isolate({
      if (length(schoice$left)<1)
        return("")
      
      if (length(schoice$right)<1)
        formula <- paste0(input$responseVar,"~", paste0(schoice$left, collapse = " + "))
      else
        formula <- paste0(input$responseVar,"~",
                          paste0(
                            paste0(schoice$left, collapse = " + "),
                            "+",
                            paste0(schoice$right, collapse = " + ")
                          )
        )
      
      
      if (input$type!="gaussian")
        func <- paste0("glm(", formula, ", data=DATA, family = ", input$type,")")
      else
        func <- paste0("lm(", formula, ", data=DATA)")
      
      
      return(func)
    })
  }
  ""
})

output$result.check <- renderUI({
  
  input$Into1
  if (!is.null(input$Into1) && input$Into1 > 0) {
    isolate({
      out <- try(eval(parse(text=result.process()), list(DATA = DATA())), silent =TRUE)
      if (is(out,"try-error"))
        alertBlock(out, "alert alert-error")
    })
  }
})

output$formula.check <- renderUI({
  
  out <- ""
  
  change <- reactive(mchoice)
  
  if (input$selectModel != "-")
    out <- capture.output(change()[[input$selectModel]]$call)
  
  #div(class="well", id = "formulasheet", span(out))
  div(class="well", align = "center", style="max-width:100%",span(out))
  #div(style="height:30px;background-color: gray;
  #    padding: 12px;margin-bottom: 5px;", align="center", out)
  
})


output$result <- renderPrint({
  
  # display summary result in R output format
  
  i = 0
  while (i <= length(outputResult$model)) {
    cat(outputResult$model[i], "\n")
    i = i+1
  }
  
  
})

observe({
  
  # fit model according to storage formula
  
  if (!is.null(input$Into1) &&  input$Into1 > 0) {
    isolate({
      out <- try(eval(parse(text=result.process()), list(DATA = DATA())), silent =TRUE)
      if (!is(out,"try-error") & length(schoice$left)>0){
        
        
        ## we want to update the DATA into global environment
        assign('DATA', getdata(), envir = .GlobalEnv)
        
        mchoice[[length(mchoice) + 1]] <<- out
        
        target <- paste0("model.", length(mchoice)) 
        
        names(mchoice)[length(mchoice)] <<- target 
        
        updateSelectInput(session, 'selectModel', choices = c("-",names(mchoice)),
                          selected = target)
        
        outputResult$model <- append(outputResult$model, paste(">",target))
        if (length(schoice$right)>0)
          outputResult$model <- append(outputResult$model, 
                                       capture.output(iNZightSummary(out, exclude = schoice$right)))
        else
          outputResult$model <- append(outputResult$model, 
                                       capture.output(iNZightSummary(out)))
      }
      
    })
  }
  
})




observe({
  
  # click current model to display the selcted model summary
  
  input$sumNowModel
  
  if (!is.null(input$sumNowModel) && input$sumNowModel > 0) {
    isolate({
      
      if (input$selectModel=="-")
        return()
      
      out <- mchoice[[input$selectModel]]
      outputResult$model <- append(outputResult$model, paste(">",input$selectModel))
      if (length(schoice$right)>0)
        outputResult$model <- append(outputResult$model, 
                                     capture.output(iNZightSummary(out, exclude = schoice$right)))
      else
        outputResult$model <- append(outputResult$model, 
                                     capture.output(iNZightSummary(out)))
    })
  }
})

observe({
  
  # clean result
  
  if (!is.null(input$Into4) && input$Into4 >0) {
    isolate({
      outputResult$model <- NULL
    })
  }
  
})


output$chooser <- renderUI({
  
  chooserInput("mychooser", "Variables of Interest", "Confounders",
               schoice$left, schoice$right, size = 10, multiple = TRUE
  )
  
})

observe({
  
  input$Into
  
  isolate({
    ## Filter the selecting input from source variables.
    temp <- c(schoice$left, schoice$right)
    names(temp) <- temp
    ## temp is used to create full selected vars list
    id <- !input$sourceVar %in% temp
    ## id is the selected var location by binary TRUE/FALSE
    schoice$left <- append(schoice$left, input$sourceVar[id])
  })
  
})

observe({
  
  input$mychooser
  
  isolate(schoice$left <- input$mychooser$left)
  isolate(schoice$right <- input$mychooser$right)
})

observe({
  
  input$l1
  d = NULL
  if (!is.null(input$l1) && input$l1 >0) {
    isolate({
      if (length(input$sourceVar)<2)
        return()
      d <- paste0(input$sourceVar, collapse=":")
      temp <- c(schoice$left, schoice$right)
      names(temp) <- temp
      ## temp is used to create full selected vars list
      id <- !d %in% temp
      ## id is the selected var location by binary TRUE/FALSE
      schoice$left <- append(schoice$left, d[id])
      
    })
  }
  
})

observe({
  
  input$l2
  d = NULL
  if (!is.null(input$l2) && input$l2 > 0) {
    isolate({
      if (length(input$sourceVar)<2)
        return()
      d <- paste0(input$sourceVar, collapse="*")
      temp <- c(schoice$left, schoice$right)
      names(temp) <- temp
      ## temp is used to create full selected vars list
      id <- !d %in% temp
      ## id is the selected var location by binary TRUE/FALSE
      schoice$left <- append(schoice$left, d[id])
      
    })
  }
  
})

observe({
  
  input$l3
  d = NULL
  if (!is.null(input$l3) && input$l3 >0) {
    isolate({
      d <- paste0("I(",input$sourceVar, "^2)")
      temp <- c(schoice$left, schoice$right)
      names(temp) <- temp
      ## temp is used to create full selected vars list
      id <- !d %in% temp
      ## id is the selected var location by binary TRUE/FALSE
      schoice$left <- append(schoice$left, d[id])
      
    })
  }
  
  
})


observe({
  
  input$l4
  d = NULL
  if (!is.null(input$l4) && input$l4 >0) {
    isolate({
      d <- paste0("I(",input$sourceVar, "^3)")
      temp <- c(schoice$left, schoice$right)
      names(temp) <- temp
      ## temp is used to create full selected vars list
      id <- !d %in% temp
      ## id is the selected var location by binary TRUE/FALSE
      schoice$left <- append(schoice$left, d[id])
      
    })
  }
  
  
})



observe({
  
  input$datasets
  # found dataset change then update the following: 
  
  schoice$left <- NULL
  schoice$right <- NULL
  mchoice <<- list() # mchoice is not reactive value so we use <<- for parent frame assign
  
})

observe({
  
  input$Into1
  
  # Let A, B are factor variables.
  # this is using for remove the old selection of the below selectInput list
  # Suppose model1: y~A, model2:y~B. Because model1 exists A but model2 does not.
  # the selectInput can't find A any more. So we update to "" then because
  # I write some condition to stop running function if seeing "".
  # Then the procedure will update B so the whole process is A ->""->B.
  # If model1:y~A, model2:y~A+B, the problem is not existing. (Problem is responding error message that we can't find B)
  
  updateSelectInput(session, 'comPlotVarSelect', choices = "")
  updateSelectInput(session, 'comMatVarSelect', choices = "")
  updateSelectInput(session, 'prpVarSelect', choices = "")
})


