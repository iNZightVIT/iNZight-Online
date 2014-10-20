output$home_side_and_main <- renderUI({

    authorInfo <- system("grep -i ^author: DESCRIPTION", intern = TRUE)
    author <- strsplit(authorInfo, ":")[[1]][2]
    
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
        #includeHTML('tools/home/HTMLobjects.Rhtml'),
        #a(href="https://www.stat.auckland.ac.nz/~wild/iNZight/",
        #  img(src="https://www.stat.auckland.ac.nz/~wild/iNZight/images/iNZhead.png",alt="iNZight",style="align:right")),
        p('"Home" is the initial landing page.'),
        p('Click the “Data” tab for the data-import page.'),
        p('Then click on either the “Basic” tab or the “Time Series” tab (as appropriate) to begin using iNZight Online for graphics and analysis.'),
        div(style="margin-bottom: 10px; padding-left: 10px; border-left: solid 5px #cccccc; color: #999999; font-size: 10pt;",
            'We are currently only telling a limited number of people where to find it to keep the initial load small while we monitor it. There is no documentation. We anticipate that that the iNZight users we tell about iNZight online will be able to figure out how it works.'),
        p(paste('Please report problems and send improvement suggestions to:',
                author)),
        p("Or, check the following links for more", strong("iNZight"), "project information."),
             
        p(style="color:red", paste0("iNZight-Online v",
          strsplit(system("grep -i ^version DESCRIPTION", intern = TRUE), ":")[[1]][2]))
        #p(),
        #verbatimTextOutput('global.num')
        #a(href="https://www.stat.auckland.ac.nz/~wild/VIT/index.html",
        #  img(src="https://www.stat.auckland.ac.nz/~wild/VIT/images/VITHead.png",alt="iNZightVIT",style="align:right"))

      )
    )
  )
  
})

output$global.num <- renderPrint({
  # Debugging stuff:
#  cat("sessionVars")
  #print(sessionVars$username)
  
#  cat("values")
  #print(names(values))
  
})
