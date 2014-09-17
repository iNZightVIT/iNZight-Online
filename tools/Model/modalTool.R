### Tools for building modal window and dropDown list
### check model_ui.R for how they combine together for creating dropDown event.

toolList <- function(...) {
  
  tags$ul(class = "nav nav-pills", tagList(...))
  
}



dropdownPill <- function(..., label) {
  
  tags$li(class="dropdown",
          
          tags$a(class="dropdown-toggle", 'data-toggle'="dropdown", 
                 href="#", label, 
                 tags$b(class="caret")
          ),
          tags$ul(class="dropdown-menu", tagList(...))
  )
}

optionLink <- function(inputId, label, icon = NULL, ...) {
  
  tags$li(tags$a(id = inputId, href = "#", class = "action-button", 
                 # 'data-toggle'="pill",  # show blue pill
                 list(icon, label)))
}

optionModalLink <- function(inputId, hrefId= NULL, label, icon = NULL, ...) {
  
  tags$li(tags$a(id = inputId, href = hrefId, class = "action-button", 
                 # 'data-toggle'="pill",  # show blue pill
                 'data-toggle' = "modal",
                 list(icon, label)))
}

modalEvent <- function(id="myMODAL2", header = "Header", ...) {
  
  # ... can be any source returning HTML elements.
  
  div(id =id, class="modal hide fade", tabindex="-1", role="dialog",
      'aria-hidden' = "true", 
      div(class="modal-header", 
          tags$button(type="button", class="close",'data-dismiss'="modal",'aria-hidden'='true','x'),
          h3(header)
      ),
      div(class="modal-body", tagList(...))
  )
}