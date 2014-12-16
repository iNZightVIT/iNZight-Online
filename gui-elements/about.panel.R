about.panel = function(){
  fixedPage(
    column(width=10, offset = 1,
           div(class="page-spacer",
               includeMarkdown('gui-elements/notes/about.intro.md'),
               HTML(paste("<center>",
                          p(class="version",paste("iNZight-Online version:",as.character(version),sep=" ")),
                          "</center>",sep=" ")))
  ))
}

