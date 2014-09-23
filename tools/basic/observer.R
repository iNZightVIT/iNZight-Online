######### Observer for base module user control ##############
# optpar2 is the parameters list for iNZightPlot
# optpar3 is the parameter list using in the inzPlotDefaults()
# 
##############################################################

optpar2 <- reactiveValues(
  x = NULL ,
  y = NULL,
  varnames = list(xlab = NULL, ylab = NULL, 
                  g1 = NULL, g2 = NULL, 
                  by = NULL, prop.size = NULL), 
  g1 = NULL,
  g2 = NULL,
  g1.level = NULL,
  g2.level = NULL,
  by = NULL,
  prop.size = NULL, 
  main = NULL
)



paramS <- reactive({
  
  paramS <- reactiveValuesToList(optpar2)
  
  
  
  if (!is.null(paramS$x) && optpar2$varnames$x != " "){

    # This first one is testing paramS$x is a NULL to avoid the 
    # initialize generating x NULL value
    # The second one is check whether x is being choosen to be validate variable 
    
    validate(
      need(all(na.omit(paramS$x)!="' '"), "This Variable must be cleaning. All value is ' ' empty character")
    )
    validate(
      need(all(na.omit(paramS$x)!="\" \""), "This Variable must be cleaning. All value is \" \" empty character")
    )
    validate(
      need(all(na.omit(paramS$x)!=" "), "This Variable must be cleaning. All value is empty character")
    )
    
    
    if (gjudge() ==6) {
      ## if x and y are num then switch them
      temp <- list(x = NULL, y = NULL, 
                   varnames = list(x = " ", y = " "))
      temp$x <- paramS$y
      temp$y <- paramS$x
      temp$varnames$x <- paramS$varnames$y
      temp$varnames$y <- paramS$varnames$x
      paramS <- modifyList(paramS, temp)
    }
    paramS <- modifyList(reactiveValuesToList(optpar3), paramS)
  }
  else
    NULL
})

output$inzPlot <- renderPlot({
  
 
 if (!is.null(paramS()))   
  do.call("iNZightPlot", paramS())
  
})

#x 
observe({
  
  
  if (is.null(input$x))
    return()
  
  if (input$x != " "&& input$x %in% names(nowdata()))
    input.x <- nowdata()[, input$x]
  else
    input.x <- NULL
  
  optpar2$x <- input.x
  optpar2$varnames$x <- input$x
  
})



# y
observe({
  
  if (is.null(input$y))
    return()
  
  if (input$y != " "&& input$y %in% names(nowdata()))
    input.y <- nowdata()[, input$y]
  else
    input.y <- NULL
  
  optpar2$y <- input.y
  optpar2$varnames$y <- input$y
  
})


# g1
observe({
  
  if (is.null(input$A))
    return()
  
  if (input$A != " "&& input$A %in% names(nowdata()))
    input.A <- convert.to.factor(nowdata()[, input$A])
  else
    input.A <- NULL
  
  
  optpar2$g1 <- input.A
  optpar2$varnames$g1 <- input$A
})

# g2
observe({
  
  if (is.null(input$B))
    return()
  
  if (input$B != " "&& input$B %in% names(nowdata()))
    input.B <- convert.to.factor(nowdata()[, input$B])
  else
    input.B <- NULL
  
  
  optpar2$g2 <- input.B
  optpar2$varnames$g2 <- input$B
})

# g1, specified levels
observe({
  

  cache <- input$sb1.sl 
  if (is.null(cache) || cache == 0 )
    cache <- NULL
  
   
  optpar2$g1.level <- cache

})

# for every time g1 variables list change, clean up the g1.level
observe({
  
  input$A
  optpar2$g1.level <- NULL 
  updateSliderInput(session, inputId = "sb1.sl", value = 0)
  
})


# g2, specified levels
observe({
  
  cache <- input$sb2.sl 
  if (is.null(cache) || cache == 0)
    cache <- NULL
  
  
  if (!is.null(cache) && cache == (nlevels(check.g2.level())+1) )
    cache <- "_MULTI"
  
  optpar2$g2.level <- cache

})

# for every time g2 variables list change, clean up the g2.level
observe({
  
  input$B
  optpar2$g2.level <- NULL
  updateSliderInput(session, inputId = "sb2.sl", value = 0)
  
})

# colour by --- an example that new way is much more robust... The reason is the 
# colour by option is hidden and it is activiact until we get into the "Add to Plot" 
# module and then update the option to get optpar2$varname$by. Thus the plot is error
# when we switch data set and once get into "Add to Plot" module the plot work again
# because the ui is update. 
# For my new design, graphics is tracking the usable option not be " " and names in the 
# current data set otherwise return NULL. Then the shiny will notice optpar2$by is still
# NULL and it will not change the output. So we can have a robust ones here. And if we 
# get into "Add to Plot" module the colour by variable dropdown list update.
observe({
  
  if (is.null(input$BY))
    return()
  
  if (input$BY != " "&& input$BY %in% names(nowdata()))
    input.BY <- nowdata()[, input$BY]
  else
    input.BY <- NULL
  
  optpar2$by <- input.BY
  optpar2$varnames$by <- input$BY
  
  
})

# prop.size 
observe({
  
  if (is.null(input$rszprop))
    return()
  
  if (input$rszprop != " "&& input$rszprop %in% names(nowdata()))## Error in func() : object 'out' not found 
    input.rszprop <- nowdata()[, input$rszprop]
  else
    input.rszprop <- NULL
  
  
  optpar2$prop.size <- input.rszprop
  optpar2$varnames$prop.size <- input$rszprop
  
  
})

# mtitle 
observe({
  
  if (is.null(input$mtitle))
    return()
  
  dd = input$mtitle
  if (input$mtitle=="")  {
    # the special rule of shiny is javascript not NULL
    # the iNZightPlot require a NULL but the UI will only generate "" actually
    # the only possible shiny generate NULL is because 
    # the element is not generated ready when the function is running here this will generate NULL.
    # basic.R 252-253 rows, we on purposely set everytime create main title and 
    # xlab text input box default value is NULL to return the correct title.
    # Unlike other ui, I set value to be the corresponding optpar2$xxx, I called this as lock,
    # I won't lock the text input value here. 
    # By using lock, one can everytime create this ui and in the same time get the last time value,
    # I set NULL as everytime we create this object, then update the corresponding optpar2$xxx parameter,
    # and manually set the update. 
    # By doing this, one can avoid the bug(?) in text input that user fast speed or using space can
    # sometimes go into the endless loop.
    # compare textInput with other ui
    # lock: selectInput(..., selected = optpar2$xxx) ### no update function we need here
    # unlock: textInput(..., value = NULL) ### we do have update function here.  updateTextInput(..., value = optpar2$xxx)
    # I think the reason is because the shiny engine is using node.js which is asynchronous event query not responding too high
    # speed response.
    # By doing all of this, people can change the name of title or xlab,ylab and once they left
    # switch other radioButton options the user selection will keep in optpar$xxx there but if there 
    # they select radioButton of this panel this will generate a whloe new text input panel for
    # title and xlab which is default value NULL.
    optpar2$main <- NULL 
  }
  else{  
    optpar2$main <- dd
    updateTextInput(session, "mtitle", value = optpar2$main)
  }
})


# xlab 
observe({
  
  if (is.null(input$xlab))
    return()
  
  
  dd = input$xlab
  if (input$xlab=="")
    optpar2$xlab <- NULL
  else{
    
    optpar2$xlab <- dd
    updateTextInput(session, "xlab", value = optpar2$xlab)
    
  }
  
})

# ylab 
observe({
  
  if (is.null(input$ylab))
    return()
  
  dd = input$ylab
  if (input$ylab=="")
    optpar2$ylab <- NULL
  else{
    
    optpar2$xlab <- dd
    updateTextInput(session, "ylab", value = optpar2$ylab)
    
  }
})



# optpar3 is the parameter list using in the inzPlotDefaults()
optpar3 <- reactiveValues(col.pt         = "grey50",  #
                          alpha          = 1,  #
                          bg             = "white",  # 
                          col.line       = " ", #
                          jitter         = "", #
                          rugs           = "", #
                          trend          = NULL, #
                          smooth         = 0,  #
                          quant.smooth   = NULL,
                          LOE            = FALSE, #
                          join           = FALSE, #
                          lines.by       = FALSE,  #
                          col.trend      = list(linear = " ", #
                                                quadratic = " ", #
                                                cubic =" "), #
                          trend.by       = FALSE,  #
                          col.smooth     = " ", #
                          col.LOE        = "black", #
                          inference.type = NULL, #
                          bs.inference   = FALSE,#
                          szsym = as.numeric(1), #
                          tpsym = as.numeric(1) #
)

# we need a option parameter to storage our previous selection
# wondering if switching data set, we need extra condition control to avoid error
# optpar <- reactiveValues(
#   BY = " ",
#   bgcol = 'white',
#   szsym = as.numeric(1),
#   tpsym = as.numeric(1),
#   mtitle = " ",
#   xlab = " ",
#   tc.p1 = " ",
#   tc.p2 = " ",
#   tc.p3 = " ",
#   tc.pn = " ",
#   tb.cb = FALSE,
#   pn.sl = as.numeric(0.7),
#   xyl = " ",
#   jit = NULL,
#   rug = NULL,
#   p2l = NULL,
#   lb.cb = FALSE
# )

# bg
observe({
  
  if (is.null(input$bgcol)|| input$bgcol == " ")
    return()
  
  if (input$bgcol != " ")
    input.bgcol <- input$bgcol
  else
    input.bgcol <- "white"
  
  optpar3$bg <- input.bgcol
  
})

# szsym
observe({
  
  if (is.null(input$szsym))
    return()
  
  optpar3$cex.pt <- as.numeric(input$szsym)
  
})

# tpsym 
observe({
  
  if (is.null(input$tpsym))
    return()
 
  optpar3$alpha <- as.numeric(input$tpsym)
  
})


# tc.p.....
observe({
  
  # block NULL testing
  if (is.null(input$tc.p1) | is.null(input$tc.p2) | is.null(input$tc.p3) 
      | is.null(input$tc.pn) | is.null(input$pn.sl))
    return(NULL)
  
  
  # create original setting here
  origin <- list(trend = NULL, smooth = NULL, 
                 col.trend = list(linear = " ", quadratic = " ", cubic =" "),
                 col.smooth = " ", trend.by = FALSE)
  
  # create input vector here
  l1 <- input$tc.p1 != " "
  l2 <- input$tc.p2 != " "
  l3 <- input$tc.p3 != " "
  
  add.trend <- c("linear", "quadratic", "cubic")[c(l1,l2,l3)]
  addtrend <- NULL 
  if (length(add.trend) != 0)
    addtrend <- add.trend
  
  coltrend <- list(linear = input$tc.p1, quadratic = input$tc.p2, 
                   cubic = input$tc.p3)
  colsmooth <- ifelse(input$tc.pn != " ", input$tc.pn, " ")
  smoo <- NULL 
  if(colsmooth != " ") 
    smoo <- input$pn.sl
  
  
  optpar3$trend = addtrend
  optpar3$smooth = smoo
  optpar3$trend.by = input$tb.cb
  optpar3$col.trend = coltrend
  optpar3$col.smooth = colsmooth
  
  
  
})

# xyl
observe({
  
  if (is.null(input$xyl))
    return()
  
  optpar3$LOE = input$xyl != " "
  optpar3$col.LOE = input$xyl
  

})



# jit
observe({
  
  if (is.null(input$jit))
    return()
  
  d <- input$jit
  if (length(input$jit)==2) 
    d <- paste0(input$jit[1], input$jit[2])
  
  
  optpar3$jitter <- d

  
})

# rug
observe({
  
  if (is.null(input$rug))
    return()
  
  d <- input$rug
  if (length(input$rug)==2) 
    d <- paste0(input$rug[1], input$rug[2])
  
  optpar3$rugs <- d
  
})


#p2l
observe({
  
  # block NULL testing
  if (is.null(input$p2l))
    return()
  
  

  
  # create input vector here
  optpar3$join = input$p2l != " "
  optpar3$col.line = input$p2l
  optpar3$lines.by = input$lb.cb
  
  
  
  
})

buttonStage <- reactiveValues(click = FALSE)

# click button to change the button Stage
observe({
  input$GOInf
  isolate(buttonStage$click <- TRUE)
  
})


# click button to change the button Stage
observe({
  
  input$CutInf
  isolate(optpar3$inference.type <- NULL)
  isolate(optpar3$bs.inference <- FALSE)
  isolate(buttonStage$click <- FALSE)
})

# inference0: two num
# condition: bs.inference = TRUE, other lines has colour
observe({
  
  if (gjudge() == 6) {
    if(!is.null(input$GOInf) & isolate(buttonStage$click)){
      isolate(optpar3$bs.inference <- TRUE)
    }
  }
  
  
})


# inference1: mean/median/proportion inference type
observe({
  

  
  if (is.null(input$GOInf))
    return()
  
  if (is.null(input$intval))
    return()
  
  if (input$intval == "1")
    inference.type = "comp"
  
  if (input$intval == "2")
    inference.type = "conf"
  
  if (input$intval == "3")
    inference.type = c("comp","conf")
  
  
  if (gjudge() != 6) {
    if(!is.null(input$GOInf) & isolate(buttonStage$click)){
      isolate(optpar3$inference.type <- inference.type)
    }
  }
  
})

# inference1: mean/median/proportion Method
observe({
  
  
  
  if (is.null(input$GOInf))
    return()
  
  if (is.null(input$Meth))
    return()
  
  
  if (input$Meth == "TRUE")
    Meth = TRUE
  else
    Meth = FALSE
    
  
  if (gjudge() != 6) {
    if(!is.null(input$GOInf) & isolate(buttonStage$click)){
      isolate(optpar3$bs.inference <- Meth) 
    }
  }
  
})




output$sumadd <- renderPrint({
  
  if (is.null(optpar2$x))
    return(cat("Variable 1 have to be selected"))
  
  List <- modifyList(reactiveValuesToList(optpar2), reactiveValuesToList(optpar3))
    cat(do.call(iNZightPlots:::getPlotSummary, List), sep = "\n")
  
})

output$suminf <- renderPrint({
  
  if (is.null(optpar2$x))
    return(cat("Variable 1 have to be selected"))
  
  List <- modifyList(reactiveValuesToList(optpar2), reactiveValuesToList(optpar3))
  
  if (gjudge() == 6 )
    cat(do.call(iNZightPlots:::getPlotInference, List), sep = "\n")
  else if (isolate(buttonStage$click))
    cat(do.call(iNZightPlots:::getPlotInference, List), sep = "\n")
  else
    cat("click go Inference for your preference comparison")
  
})


observe({

  input$redefault
  
  
  
  updateSelectInput(session, "x", selected = " ")
  updateSelectInput(session, "y", selected = " ")
  updateSelectInput(session, "A", selected = " ")
  updateSelectInput(session, "B", selected = " ")
  
  
})