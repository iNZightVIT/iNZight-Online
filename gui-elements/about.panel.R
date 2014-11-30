about.panel = function(){
  fixedPage(
    column(width=10, offset = 1,
           div(class="page-spacer",
               includeMarkdown('gui-elements/notes/about.intro.txt'),
               HTML(paste("<center>",
                          p(class="version",paste("iNZight-Online version:",as.character(2.0),sep=" ")),
                          "</center>",sep=" ")))
  ))
}

