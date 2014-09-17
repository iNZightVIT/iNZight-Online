########################################################################################
##
##
## fucntion to support functionality
## actionPbutton: actionButton with Primary class in Bootstrap
## actionRbutton: actionButton with Danger class in Bootstrap
## splitButton: button to design for dropdown menu in Bootstrap
## alerBlock: displaying alert message in Bootstrap
##
##
########################################################################################

actionPbutton <- function (inputId, label, icon = NULL, ...) 
{
  tags$button(id = inputId, type = "button", class = "btn action-button btn-primary", 
              list(icon, label))
}


actionRbutton <- function (inputId, label, icon = NULL, ...) 
{
  tags$button(id = inputId, type = "button", class = "btn action-button btn-danger", 
              list(icon, label))
}

splitButton <- function(buttonLabel, inputId1, label1, inputId2, label2) {
  div(class="btn-group",
      tags$a(class="btn btn-primary", href = "#", buttonLabel),
      tags$a(class="btn btn-primary dropdown-toggle", 'data-toggle'="dropdown",
             href="#",
             span(class="caret")
      ),
      tags$ul(class="dropdown-menu",
              tags$li(actionLink(tabindex="-1",href="#",inputId = inputId1, label=label1)),
              tags$li(actionLink(tabindex="-1",href="#",inputId = inputId2, label=label2))
      )
  )
}

alertBlock <- function(text,Info) {
  
  
  HTML(paste("<div class='", Info,
             "'><button type='button' class='close' data-dismiss='alert'>&times;</button>
  <strong>Warning!</strong>", text, "</div>"))
  
}