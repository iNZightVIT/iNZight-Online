output$home_side_and_main <- renderUI({
  list(
    includeCSS("www/style2.css"),
    # includeMathJax("www/js/MathJax.js"),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js")
      # tags$script(src = "js/MathJax.js?config=TeX-AMS-MML_HTMLorMML") 
      #tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
      #            type = 'text/javascript')
    ),
    
    fixedPage(
      column(width=9, offset = 1,
        includeMarkdown('tools/home/note.md'),
        #img(src="https://www.stat.auckland.ac.nz/images/people/chriswild.jpg",alt="C.W photo"),
        #p(a("Chris. Wild",href = "https://www.stat.auckland.ac.nz/showperson?firstname=Chris&surname=Wild")),
        em("This is its development home, not its permanent home."),
        p(),
        includeHTML('tools/home/HTMLobjects.Rhtml'),
        #a(href="https://www.stat.auckland.ac.nz/~wild/iNZight/",
        #  img(src="https://www.stat.auckland.ac.nz/~wild/iNZight/images/iNZhead.png",alt="iNZight",style="align:right")),
        p(style="color:red", "iNZight-Online V 1.0")
        #p(),
        #verbatimTextOutput('global.num')
        #a(href="https://www.stat.auckland.ac.nz/~wild/VIT/index.html",
        #  img(src="https://www.stat.auckland.ac.nz/~wild/VIT/images/VITHead.png",alt="iNZightVIT",style="align:right"))

      )
    )
  )
  
})

output$global.num <- renderPrint({
  
  cat("sessionVars")
  #print(sessionVars$username)
  
  cat("values")
  #print(names(values))
  
})
