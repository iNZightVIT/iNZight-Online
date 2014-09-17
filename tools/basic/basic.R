# nowdata is a copy of current data



nowdata <- reactive({ 
  
  getdata()
})

  
output$var1.dd <- renderUI({
  
  selectInput("x", "Variable 1: ",
              c(" ",names(nowdata())),
              selectize = FALSE, selected = optpar2$varnames$x 
  )
  
})

output$var2.dd <- renderUI({
  
  selectInput("y", "Variable 2: ",
              c(" ",names(nowdata())),
              selectize = FALSE, selected = optpar2$varnames$y
  )
  
})




output$sb1.dd <- renderUI({
  
  selectInput("A", "subset by: ",
              c(" ", names(nowdata())),
              selectize = FALSE, selected = optpar2$varnames$g1
  )
  
})

output$sb2.dd <- renderUI({
  
  selectInput("B", "subset by: ",
              c(" ", names(nowdata())),
              selectize = FALSE, selected = optpar2$varnames$g2
  )
  
})

output$sb1_sl <- renderUI({
  
  if (!is.null(input$A) && input$A != " ")
    input.A <- convert.to.factor(nowdata()[, input$A])
  
  else
    input.A <- NULL
  
  if (is.null(input.A))
    return(NULL)
  
  xfact <- input.A
  
  sliderInput("sb1.sl", "Observations:", value= 0, step = 1,
              min = 0, max = nlevels(xfact), animate = TRUE)
  
})

## don't know this is a good idea...Because we need to change the max level + 1 as "_MULTI"
check.g2.level <- reactive({
  
  if ( !is.null(input$B) && input$B != " ")
    input.B <- convert.to.factor(nowdata()[, input$B])
  
  else
    input.B <- NULL
  
  input.B
})

output$sb2_sl <- renderUI({
  
  #if ( !is.null(input$B) && input$B != " ")
  #  input.B <- convert.to.factor(nowdata()[, input$B])
  
  #else
  #  input.B <- NULL
  
  if (is.null(check.g2.level()))
    return(NULL)
  
  
  input.B <- check.g2.level()
  
  xfact <- input.B
  
  sliderInput("sb2.sl", "Observations:", value= 0, step = 1,
              min = 0, max = nlevels(xfact)+1, animate = TRUE
  )
  
  
})



# some control; need a separated file to storage
Xclass <- reactive({
  
  if (is.null(optpar2$x))
    return(NULL)
  
  out <- class(optpar2$x)
  
  if (out == "integer")
    out <- "numeric"
  
  if (out == "character")
    out <- "factor"
  
  out
  
})

Yclass <- reactive({
  
  if (is.null(optpar2$y))
    return(NULL)
  
  out <- class(optpar2$y)
  
  if (out == "integer")
    out <- "numeric"
  
  if (out == "character")
    out <- "factor"
  
  out
  
})

gjudge <- reactive({
  
  obse <- c(Xclass(), Yclass())
  
  # 0: x,y null
  if (is.null(Xclass()) & is.null(Yclass()))
    return(0)
  
  # 1: x num, y null
  if (identical(obse, "numeric"))
    return(1)
  
  # 2: x fac, y null
  if (identical(obse, "factor"))
    return(2)
  
  # 3: x fac, y fac 
  if (identical(obse, c("factor","factor")))
    return(3)
  
  # 4: x fac, y num
  if (identical(obse, c("factor","numeric")))
    return(4)
  
  # 5: x num, y fac
  if (identical(obse, c("numeric","factor")))
    return(5)
  
  # 6: x num, y num
  if (identical(obse, c("numeric","numeric")))
    return(6)
  
  # 7: x,y has special class structure such as time series,etc..
  # suggested user to do its data manipulation
  id <- obse %in% c("numeric","factor")
  return(c("x wrong","y wrong")[!id])
  
})

output$basanno <- renderUI({

  
  if (gjudge() == 3)
    return(NULL)
  
  if (gjudge() == 6) {
    E <- selectInput("rszprop", "Resize proportional to: ",
                     c(" ",names(nowdata())), 
                     selectize = FALSE, selected = optpar2$varnames$prop.size)
    F <- textInput('ylab', "y-axis label:")
  }
  else{
    E <- NULL
    F <- NULL
  }
  
  if (!is.null(input$ADD) && input$ADD == '1')
    return(list(
      selectInput("BY", "colored by: ",
                  c(" ",names(nowdata())),
                  selectize = FALSE,
                  selected = optpar2$varnames$by
      ),
      E
    ))
  
  if (!is.null(input$ADD) && input$ADD == '2') {
    if (gjudge() !=2) { 
      cur.value1 <- as.numeric(optpar3$szsym)
      if (length(cur.value1)==0)
        cur.value1 <- 1
      
      cur.value2 <- as.numeric(optpar3$tpsym) 
      if (length(cur.value2)==0)
        cur.value2 <- 1
      
      return(list(
        selectInput('bgcol', 'Background colour:',
                    c('white', 'antiquewhite','azure3'),
                    selectize = FALSE,
                    selected = optpar3$bg
        ),
        
        sliderInput('szsym', 'Size of symbols',
                    min = 0.05,
                    max = 3.5,
                    value = cur.value1,
                    step = 0.05
        ),
        
        sliderInput('tpsym', 'Transparency of symbols',
                    min = 0.01,
                    max = 1,
                    value = cur.value2,
                    step = 0.01,
        )
      ))
    }
  
    if (gjudge() ==2 ){
      
      return(list(
        selectInput('bgcol', 'Background colour:',
                    c('white', 'antiquewhite','azure3'),
                    selectize = FALSE,
                    selected = optpar3$bg
        )
      ))
    }
  }  
  if (!is.null(input$ADD) && input$ADD == '3' ) 
    return(list(
      textInput('mtitle', "Main title:"),
      textInput('xlab', "x-axis label:"),
      F
    ))
  
  
  
})

output$numanno <- renderUI({
  
  # You have to use this way to create an show|hide ui.
  # anything using condition from dynamic ui should change to this way..
  # rather than using conditionalPanel method...
  
  
  if( is.null(optpar2$x) || is.null(optpar2$y) )
    return(NULL)
  
  # trend = c("linaer", "quadratic", "cubic")
  # smooth = NULL, 0.7 
  # x = y: loe = TRUE
  # jitter/rug = "x", "y", "xy"
  # join points: join = TRUE
  if (is.numeric(optpar2$x) & is.numeric(optpar2$y) ){
    radioButtons('num2', "Annotation:", 
                 c("Add trend curves" = 1,
                   "Add x = y line" = 2,
                   "Add jitter" = 3,
                   "Add rugs" = 4,
                   "Join points by lines" = 5
                 )
    )
  }  
  else
    NULL
  
})

output$atc.ui <- renderUI({
  
  ## a little difference with the iNZight design.. here use not " " and ! " " 
  ## to indicate whether user selected an option with different colour
  if (gjudge() ==6) {
    if (!is.null(input$num2) && input$num2 == 1){
      
      cur.value <- as.numeric(optpar3$smooth)
      if (is.null(optpar3$smooth) || cur.value==0)
        cur.value <- 0.7
      
      list(
        selectInput('tc.p1', "linear", 
                    c(" ", c("blue", "red", "green4", "magenta", 
                            "yellow", "pink", "grey", "orange")),
                    selectize = FALSE,
                    selected = optpar3$col.trend$linear
        ),
        
        selectInput('tc.p2', "quadratic", 
                    c(" ", c("blue", "red", "green4", "magenta", 
                            "yellow", "pink", "grey", "orange")),
                    selectize = FALSE,
                    selected = optpar3$col.trend$quadratic
        ),
        
        selectInput('tc.p3', "cubic", 
                    c(" ", c("blue", "red", "green4", "magenta", 
                            "yellow", "pink", "grey", "orange")),
                    selectize = FALSE,
                    selected = optpar3$col.trend$cubic
        ),
        
        selectInput('tc.pn', "Draw a smoother", 
                    c(" ", c("blue", "red", "green4", "magenta", 
                            "yellow", "magenta", "grey", "orange")),
                    selectize = FALSE,
                    selected = optpar3$col.smooth
        ),
        
        sliderInput('pn.sl', "Smoother control", min = 0.1, 
                    max = 1, value = cur.value, step = 0.1),
        
        checkboxInput('tb.cb', "for each level", value = optpar3$trend.by)
      )
    }
  }
  
})

output$axyl.ui <- renderUI({
  if (gjudge() == 6) {
    ## we use && here to provide if input$num is null stop to run the 
    ## the following, if it is not null, test num2 equal which option.
    if (!is.null(input$num2) && input$num2 == 2){
      selectInput('xyl', "Plot x=y line", 
                  c(" ", "black", "red", "blue", "green4",
                    "yellow", "pink", "grey", "orange"),
                  selectize = FALSE,
                  #selected = optpar$xyl)
                  selected = optpar3$col.LOE)
                 
      
    }  
  }
  
})

output$ajit.ui <- renderUI({
  
  if (gjudge() == 6) {
    if (!is.null(input$num2) && input$num2 == 3){
      checkboxGroupInput('jit', "Add jitter", 
                         c("Jitter x-variable" = "x", "Jitter y-variable" = "y"))
      # maybe we can change this to dropdown list
    }  
  }
  
})

output$arug.ui <- renderUI({
  if (gjudge() == 6) {
    if (!is.null(input$num2) && input$num2 == 4){
      checkboxGroupInput('rug', "Add rug", 
                         c("Add x-rug", "Add y-rug"))
                         
                         #selected = optpar$rug)
      
    } 
  }
})


output$ap2l.ui <- renderUI({
  if (gjudge() == 6) {
    if (!is.null(input$num2) && input$num2 == 5){
      list(
      selectInput('p2l', "Join points by lines", 
                  c(" ", "red", "black", "blue", "green4",
                    "yellow", "pink", "grey", "orange"),
                  selectize = FALSE,
                  selected = optpar3$col.line),
      checkboxInput("lb.cb", "For each level of colour factor", value = optpar3$lines.by)
      )
    } 
  }
})




output$addinfr.switch <- renderUI({
  

  if( !(gjudge() %in% c(1,4,5,6) )) {
    out <- radioButtons('chosparam', "Choose Parameter:", c("Proportions"= "proportion"), 
                        selected = "proportion"
    )
  }
  
  
  if (gjudge() %in% c(1,4,5)){
    
    out <- radioButtons('chosparam', "Choose Parameter:",
                        c("Medians" = "median", "Means" = "mean"),
                        selected = "mean")
    
  }
  
  if (gjudge() == 6){
    
    ## has problem
    ## Warning in if (input$chosparam == "Proportions") { :
    ## the condition has length > 1 and only the first element will be used
    #out <- checkboxGroupInput('chosparam', "Choose Line Type:",
    #             c("linear", "quadratic", "cubic", "smooth"),
    #             selected = " ")
    return(NULL)
    
  }
  
  out
  
})


output$addinfr.panel <- renderUI({
  if (gjudge() != 6) {
    radioButtons('intval', "Interval Type:",
                 choices = c("Comparison Intervals" = 1, 
                             "confidence Intervals" = 2,
                             "Comparison + Confidence Intervals" = 3),
                 selected = 3
    )
    
    
  }
})

output$addinfr.panel2 <- renderUI({
  
  if(gjudge() != 6){
    
    radioButtons('Meth', 'Methods to use:', 
                 choices = c("Bootstrap" = TRUE, "Normal Theory" = FALSE),
                 selected = FALSE
    )
    
  }
  
  
})


observe({
  ## need radioButtons "Mean/Median" to control 
  ## also need gjudge to control whether to use "Proportion" or "Mean/Median"
  ## In this case so far, unfortunely, if we update input$chosparam,
  ## and in the same time update input$intval and input$Meth,
  ## we fall into repeated updated....
  ## I try to use the intval and Meth to control the adding pattern to the 
  ## current plot...
  
  if (!is.null(input$chosparam)) {
    
    
    ## Mean
    if (input$chosparam == "mean"){
      updateRadioButtons(session, 'intval', "Interval Type:",
                         choices = c("Comparison Intervals" = 1,
                                     "Comparison + Confidence Intervals" = 3),
                         selected = 3
      )
      
      
      updateRadioButtons(session, 'Meth', 'Methods to use:', 
                         choices = c("Bootstrap" = TRUE, "Normal Theory" = FALSE),
                         selected = FALSE
      )
    }
    
    
    #Median
    if (input$chosparam == "median"){
      
      
      updateRadioButtons(session, 'intval', "Interval Type:",
                         c("Comparison Intervals" = 1),
                         selected = 1
      )
      updateRadioButtons(session, 'Meth', 'Methods to use:', 
                         c("Bootstrap" = TRUE, "Year 12" = FALSE),
                         selected = FALSE
      )
      
      
    }
    
    
    ## Proportion
    if (input$chosparam == "proportion"){
      
      
      updateRadioButtons(session, 'intval', "Interval Type:",
                         c("Comparison Intervals" = 1, 
                           "Confidence Intervals" = 2,
                           "Comparison + Confidence Intervals" = 3),
                         selected = 3
      )
      
      updateRadioButtons(session, 'Meth', 'Methods to use:', 
                         c("Bootstrap" = TRUE, "Normal Theory" = FALSE),
                         selected = FALSE
      )
      
    }
    
  }
  
})




A.nlevel <- reactive({
  if (!is.null(input$A) && input$A != " ")
    input.A <- convert.to.factor(nowdata()[, input$A])
  
  else
    input.A <- NULL
  
  if (is.null(input.A))
    return(NULL)
  
  xfact <- input.A
  nlevels(xfact)
  
})





