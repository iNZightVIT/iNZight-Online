## MENUBARS 
## sub menu 
## modelXXX is controlling the Filter dataset for particular class/type variable 
## modal is using for popping up modal window in Bootstrap
## comPlot: Comparison Plot
## comMat: Comparison Matrix
## spm：Scatter Plot Matrix
## bplots：Basic Plots
## prp: Partial Residual Plot
## nqq: Normal Q-Q
## nch: Norm Check Histogram
## ha: Histogram Array
## qqpi: Q-Q Plot Inference
################################################################################

modelFactor <- reactive({
  
  if (input$selectModel=="-")
    return("")
  
  out <- mchoice[[input$selectModel]]
  
  out <- Filter(is.factor, out$model)
  if (ncol(out)<1)
    return("")
  
  out
})


modelNum <- reactive({
  
  if (input$selectModel=="-")
    return("")
  
  out <- mchoice[[input$selectModel]]
  
  out <- Filter(is.numeric, out$model)
  if (ncol(out)<1)
    return("")
  
  out
})

  
# comPlot_modal
output$comPlotVarSelect <- renderUI({
  
  
  if (length(names(modelFactor()))!=0)
    selectInput('comPlotVarSelect', label = "Variable(s) List:", 
                choices = names(modelFactor())
    )
  else
    selectInput('comPlotVarSelect', label = "Variable(s) List:", 
                choices = c("No categorical variable(s) in the list"="")
    )
  
}) 

output$comPlotWin <- renderPlot({

  validate(
    need(!is.null(input$comPlotVarSelect),
         FALSE),
    need(input$comPlotVarSelect != "", 
         "Only allow numeric interactions and single factors")
    )
 
  plot(moecalc(mchoice[[input$selectModel]], input$comPlotVarSelect))
  
}) 

output$ui_comPlot_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("Only allow numeric interactions and single factors"))
  
  
  list(
    uiOutput('comPlotVarSelect'),
    plotOutput('comPlotWin')
  )
})

# comMat_modal
output$comMatVarSelect <- renderUI({
  
  
  if (length(names(modelFactor()))!=0)
    selectInput('comMatVarSelect', label = "Variable(s) List:", 
                choices = names(modelFactor())
    )
  else
    selectInput('comMatVarSelect', label = "Variable(s) List:", 
                choices = c("No categorical variable(s) in the list"="")
    )
  
}) 

output$comMatWin <- renderPrint({
  
  validate(
    need(!is.null(input$comMatVarSelect),
         FALSE),
    need(input$comMatVarSelect != "", 
         "Only allow numeric interactions and single factors")
  )
  
  multicomp(moecalc(mchoice[[input$selectModel]], input$comMatVarSelect))
  
}) 

output$ui_comMat_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("Only allow numeric interactions and single factors"))
  
  
  list(
    uiOutput('comMatVarSelect'),
    verbatimTextOutput('comMatWin')
  )
})




# spm 
output$ui_spm_modal <- renderUI({
  
  plotOutput('spmWin')
  
})

output$spmWin <- renderPlot({
  
  if (input$selectModel=="-")
    return("")
  
  out <- mchoice[[input$selectModel]]
  
  gpairs(out$model)
  
})

# bplots


output$ui_bplots_modal <- renderUI({
  
  # write waiting script here to indicate the event is happening..
   list(
     div(class = "busy", style="z-index:9999;",
         p("Calculation in progress ..."),
         img(src="ajaxloaderq.gif")
     ),
    plotOutput('plotlm1'),
    plotOutput('plotlm2'),
    plotOutput('plotlm3'),
    plotOutput('plotlm4')
   )
  
})

output$plotlm1 <- renderPlot({
  
  
  # problem: always find the global one...
  
  if (input$selectModel!="-" && length(mchoice)!=0) {
    
    ## trying progress~!~!~!~!
#     progress <- shiny::Progress$new(session, min=1, max=15)
#     on.exit(progress$close())
#     
#     progress$set(message = 'Calculation in progress',
#                  detail = 'This may take a while...')
#     
#     for (i in 1:15) {
#       progress$set(value = i)
#       Sys.sleep(0.5)
#     }
    plotlm6(mchoice[[input$selectModel]], 1)
  } 

})



output$plotlm2 <- renderPlot({
  
  
  
  if (input$selectModel!="-"){
    
    plotlm6(mchoice[[input$selectModel]], 2)
  }
})

output$plotlm3 <- renderPlot({
  
  if (input$selectModel!="-")
    plotlm6(mchoice[[input$selectModel]], 3)
  
})

output$plotlm4 <- renderPlot({
  
  if (input$selectModel!="-")
    plotlm6(mchoice[[input$selectModel]], 4)
  
})


# prp_modal
output$prpVarSelect <- renderUI({
  
  choosen <- names(modelNum())[!names(modelNum()) %in% input$responseVar]
  if (length(choosen)!=0)
    selectInput('prpVarSelect', label = "Variable(s) List:", 
                choices = names(modelNum())[!names(modelNum()) %in% input$responseVar]
    )
  else
    selectInput('prpVarSelect', label = "Variable(s) List:", 
                choices = c("No numeric variable(s) in the list"="")
    )
  
  
  
}) 

output$prpWin <- renderPlot({
  
  validate(
    need(!is.null(input$prpVarSelect),
         FALSE),
    need(input$prpVarSelect != "", 
         "Only allow numeric variable(s)")  
  )
  
  
  partialResPlot(mchoice[[input$selectModel]], input$prpVarSelect)
  

}) 



output$ui_prp_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("Only allow numeric variable(s)"))
  
  if (!is.null(input$prpVarSelect) && input$prpVarSelect == "null")
    return(div("No numeric variable(s) in the list"))
  
  list(
    uiOutput('prpVarSelect'),
    plotOutput('prpWin')
  )
  
})

# nqq_modal
output$ui_nqq_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("at least a model!"))
  
  plotOutput('nqqWin')
  
})

output$nqqWin <- renderPlot({
  
  validate(
    need(input$selectModel != "", 
         FALSE),
    need(input$selectModel != "-", 
         "at least a model!")
  )
    
  plotlm6(mchoice[[input$selectModel]], 5)
  
})

# nch_modal
output$ui_nch_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("at least a model!"))
  
  plotOutput('nchWin')
  
})

output$nchWin <- renderPlot({
  
  validate(
    need(input$selectModel != "", 
         FALSE),
    need(input$selectModel != "-", 
         "at least a model!")
  )
  
  plotlm6(mchoice[[input$selectModel]], 6)
  
})

#ha_modal
output$ui_ha_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("at least a model!"))
  
  plotOutput('haWin')
  
})

output$haWin <- renderPlot({
  
  validate(
    need(input$selectModel != "", 
         FALSE),
    need(input$selectModel != "-", 
         "at least a model!")
  )
  
  histogramArray(mchoice[[input$selectModel]])
  
})


#qqpi_modal
output$ui_qqpi_modal <- renderUI({
  
  if (input$selectModel=="-")
    return(div("at least a model!"))
  
  plotOutput('qqpiWin')
  
})

output$qqpiWin <- renderPlot({
  
  validate(
    need(input$selectModel != "", 
         FALSE),
    need(input$selectModel != "-", 
         "at least a model!")
  )
    
  iNZightQQplot(mchoice[[input$selectModel]])
  
})
