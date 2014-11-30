#only write if running on developer computer
#if(file.exists("./radiant")) {
   #list of all radiant files with time-stamps
   #dbox_remote <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
   #save(dbox_remote, file = "dbox_remote.rda")
   #options(shiny.reactlog=TRUE)
   #options(error = recover)
   #shiny.trace shows JSON packets transferred over websockets
  #options(shiny.trace = TRUE)
  #vimKeyBinding <- TRUE
#} else {
#  print(file.exists("./radiant"))
#  vimKeyBinding <- FALSE
#}

# testingRadiant set TRUE here, next time open the runApp(), you use the last time setting
testingRadiant <- FALSE 
#options(digits = 3)  # controlling digits, makes the prediction of TS failed.
# have to be greater than 4. The reason is 1997 year data is 4 digits, if 3,
# for some reason the data is numerical of 1997.5 for 1997M6, if digits is 3, this will
# force the output become 1998

# allowing anyfile size when run locally  
if(Sys.getenv('SHINY_PORT') == "") {
  # no limit to filesize locally
  # By default, Shiny limits file uploads to 5MB per file. 
  # You can modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.
  # R would increase the limit to 30MB.
  options(shiny.maxRequestSize=-1)
  running_local <<- TRUE
} else {
  options(shiny.maxRequestSize=-1)
  running_local <<- TRUE  # FALSE
}

values <- list()
state_list<- list()

setInitValues <- function() {
  # initialize state list and reactive values
  if(testingRadiant) {
    # load previous state for testing

  } else {

    state_list <<- list()
    values <<- reactiveValues()

    # initial plot height and width
    values$plotHeight <- 650
    values$plotWidth <- 650

    # Datasets can change over time (i.e. the changedata function). Therefore,
    # the data need to be a reactive value so the other reactive functions
    # and outputs that depend on these datasets will know when they are changed.
    # robj <- load("../base/data/data_init/diamonds.rda") 
    robj <- load("data/data_init/diamonds.rda") 
    df <- get(robj)
    values[["diamonds"]] <- df
    values[["diamonds_descr"]] <- attr(df,'description')
    values[["datasetlist"]] <- c("diamonds")
  }
}

# state_list initialization
# using in data/manage "output$uiDatasets" for 
state_init <- function(inputvar, init = "") {
  ifelse(is.null(state_list[[inputvar]]), return(init), return(state_list[[inputvar]]))
}

setInitValues()   # using a function here so it can also be called from state.R to reset the app


# main install happens through update.R 
options(repos = c(CRAN = "http://cran.rstudio.com"))
 #libs <- c("shiny", "knitr", "shinyAce", "car", "tools", "gridExtra", "markdown", "R.utils", "psych", 
  # "arm", "plyr", "reshape2", "vegan", "ggplot2", "lubridate", "wordcloud", "AlgDesign", "gpairs")

libs <- c(
          "knitr", 
          #"shinyAce", 
          #"car", 
          "tools", "gridExtra", 
          "MASS",
          "markdown", 
          #"R.utils", 
          "psych", 
          #"arm", 
          "plyr", "reshape2", 
          #"vegan", 
          "ggplot2", "lubridate", 
          #"wordcloud", "AlgDesign", 
          "gpairs", 
          "iNZightPlots", "iNZightTS", 
          "iNZightMR",
          "shiny", # the order of shiny should be after R.utils cause R.utils has validate()
          #"shinyIncubator",
          "grid")






# check if all packages in libs are available
available <- suppressWarnings(sapply(libs, require, character.only=TRUE))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
  install.packages(inst.libs, dependencies = TRUE)
  suppressWarnings(sapply(inst.libs, require, character.only=TRUE))
}

# binding for a text input that updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
  tagList(
    singleton(tags$head(tags$script(src = "js/returnTextInputBinding.js"))),
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
  )
}


# binding for a sortable list of variables or factor levels
# ?draw and drag
html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

# binding for a sortable list of variables or factor levels
returnOrder <- function(inputId, vars) {
  tagList(
    singleton(tags$head(tags$script(src = 'js/sort.js'))),
    singleton(includeCSS("www/sort.css")),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}

# function to render .Rmd files to html on-the-fly
# include the R markdown file, can be remove...

includeRmd <- function(path){
  # shiny:::dependsOnFile(path)
  contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
  # do not embed image or add css
  html <- knit2html(text = contents, fragment.only = TRUE, options = "", stylesheet = "www/empty.css")
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

# binding to a bootstrap popover, function by Joe Cheng https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'), 
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1], 
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

# adding the figures path to avoid making a copy of all figures in www/figures
#% most of them using in the helpPopup 
addResourcePath("figures", "tools/help/figures/")
# addResourcePath("www", "../base/www/")
# addResourcePath("tools", "../base/tools/")


# binding to a bootstrap modal
helpModal <- function(title, link, content) {
  ## title: popup window head title
  ## link: HTML id attribution
  ## content: things inside
  
  html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}


helpAndReport <- function(title, link, content) {
  # don't know purpose so far
  html <- sprintf("<div id='%sHelp' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <div>
                     <a title='Help' data-toggle='modal' href='#%sHelp' class='icon-question-sign alignleft'></a>
                     <a title='Report results' class='icon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></a>
                   </div> 
                   <div style='clear: both;'></div>
                   ", link, title, content, link, link, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

inclMD <- function(file) return(markdownToHTML(file, options = c(""), stylesheet="www/empty.css"))
# inclMD <- function(file) return(includeHTML(file))

is.Date <- function(x) is(x, "Date")



