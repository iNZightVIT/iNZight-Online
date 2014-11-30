shinyServer(function(input, output, session) {

  ################################################################################
  #
  # Delete all imported files older than 1 day.
  #
  ################################################################################
  
  delete.old.files(1) #global.R
  
  ################################################################################
  #
  # Loading all the panels into memory.
  #
  ################################################################################
  
  source("gui-elements/current.data.R",local=T)
  source("gui-elements/about.panel.R",local=T)
  source("gui-elements/switch.data.panel.R",local=T)
  source("gui-elements/load.data.panel.R",local=T)
  source("gui-elements/remove.data.panel.R",local=T)
  source("gui-elements/transform.columns.R",local=T)
  source("gui-elements/reorder.levels.R",local=T)
  
  ################################################################################
  #
  # About - Start page
  #
  ################################################################################
  
  output$about.panel = renderUI({
    about.panel()
  })
  
  ################################################################################
  
  ################################################################################
  #
  # Current data - Presents the currently selected data to the user.
  #
  ################################################################################
  
  output$current.data = renderUI({
    current.data()
  })
  
  output$current = renderDataTable({
    input$selector
    data
  },options=list(lengthMenu = c(10, 30, 50), pageLength = 10, columns.defaultContent="NA",scrollX=T))
  
  ################################################################################
  
  ################################################################################
  #
  # Data -> Switch data
  #
  ################################################################################
  
  #loads and updates the switch data table Panel
  output$switch.data.panel = renderUI({
    input$selector
    if(!is.null(input$data_select)&!is.null(input$change_set)){
      isolate({
        if(input$change_set==1){
          new.data = load.data(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])
          data.name <<- new.data[[1]]
          new.data = new.data[[2]]
          data <<- new.data
        }
      })
    }
    switch.data.panel(input$data_select)
  })
  
  output$temp_table = renderDataTable({
    if(!is.null(input[[input$data_select]])){
      load.data(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
    }else{
      NULL
    }
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
  
  ################################################################################
  
  ################################################################################
  #
  # Data -> load data
  #
  ################################################################################
  
  output$load.data.panel = renderUI({
    input$selector
    if(!is.null(input$import_set)&&input$import_set==1){
      imported = T
    }
    if(imported){
      if(!file.exists("data/Imported")){
        dir.create("data/Imported",recursive=T)
      }
      name = change.file.ext(data.name,"RDS")
      saveRDS(data,file=paste0("data/Imported/",name))
    }
    imported <<- F
    load.data.panel()
  })
  
  output$filetable <- renderDataTable({
    input$selector
    if (is.null(input$files)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(!is.null(temp.data)&&temp.data%in%input$files[1,"name"]){
      return(NULL)
    }
    temp = load.data(fileID=input$files[1,"name"],path=input$files[1,"datapath"])
    data.name <<- temp[[1]]
    data <<- temp[[2]]
    temp.data <<- input$files[1,"name"]
    data
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
  
  ################################################################################
  
  ################################################################################
  #
  # Data -> remove data
  #
  ################################################################################
  
  output$remove.data.panel =renderUI({
    if(!is.null(input$remove_set)&&input$remove_set>0){
      if(!is.null(data.name)&&data.name==input$Importedremove){
        data.name = ""
        data = NULL;
      }
      files = list.files(path="data/Imported",pattern=input$Importedremove,full.names=T)
      for(f in files){
        if(file.exists(f)){
          unlink(f)
        }
      }
    }
    remove.data.panel()
  })

  output$removetable = renderDataTable({
    if(!is.null(input$Importedremove)){
      load.data(input$Importedremove)[[2]]
    }else{
      NULL
    }
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
  
  ################################################################################
  
  ################################################################################
  #
  # Modify data -> transform columns
  #
  ################################################################################
  
  output$table_part = renderDataTable({
    temp = NULL
    temp2  = as.data.frame(data[,input$select.columns])
    colnames(temp2) = colnames(data)[which(colnames(data)%in%input$select.columns)]
    if(!is.null(temp2)&&!is.null(input$select.columns)&&input$select.transform%in%"log"){
      temp = as.data.frame(cbind(temp2,log.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"add"){
      temp = as.data.frame(cbind(temp2,add.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"subtract"){
      temp = as.data.frame(cbind(temp2,subtract.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"multiply"){
      temp = as.data.frame(cbind(temp2,multiply.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"divide"){
      temp = as.data.frame(cbind(temp2,divide.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"root"){
      temp = as.data.frame(cbind(temp2,root.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"square"){
      temp = as.data.frame(cbind(temp2,square.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"abs"){
      temp = as.data.frame(cbind(temp2,abs.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"center"){
      temp = as.data.frame(cbind(temp2,center.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"standardize"){
      temp = as.data.frame(cbind(temp2,standardize.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"median split"){
      temp = as.data.frame(cbind(temp2,median.split.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"reverse-coding"){
      temp = as.data.frame(cbind(temp2,reverse.coding.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"copy"){
      temp = as.data.frame(cbind(temp2,copy.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"change sign"){
      temp = as.data.frame(cbind(temp2,change.sign.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%"change factor"){
      temp = as.data.frame(cbind(temp2,change.factor.transform(data[,input$select.columns],input$select.columns)))
    }else if(!is.null(input$select.columns)&input$select.transform%in%""){
      temp = data.frame(data[,input$select.columns])
    }
    if(!is.null(temp)&&ncol(temp)==1){
      colnames(temp) = input$select.columns
      temp = cbind(row=as.character(1:nrow(temp)),temp) 
    }else if(!is.null(temp)){
      temp = cbind(row=as.character(1:nrow(temp)),temp) 
    }
    temp
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
  
  output$status = renderText({
    input$transform
    isolate({
      if(!is.null(input$select.columns)&!input$select.transform%in%""){
        tryCatch({
          if(input$select.transform%in%"log"){
            temp = as.data.frame(log.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"add"){
            temp = as.data.frame(add.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"subtract"){
            temp = as.data.frame(subtract.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"multiply"){
            temp = as.data.frame(multiply.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"divide"){
            temp = as.data.frame(divide.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"root"){
            temp = as.data.frame(root.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"square"){
            temp = as.data.frame(square.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"abs"){
            temp = as.data.frame(abs.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"center"){
            temp = as.data.frame(center.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"standardize"){
            temp = as.data.frame(standardize.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"median split"){
            temp = as.data.frame(median.split.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"reverse-code"){
            temp = as.data.frame(reverse.code.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"copy"){
            temp = as.data.frame(copy.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"change sign"){
            temp = as.data.frame(change.sign.transform(data[,input$select.columns]))
          }else if(input$select.transform%in%"change factor"){
            temp = as.data.frame(change.factor.transform(data[,input$select.columns]))
          }else{
            temp=NULL
          }
          if(!is.null(temp)&&dim(temp)[1]>0&dim(temp)[2]>0){
            data <<- as.data.frame(cbind(data,temp))
            transform.text <<- paste("The transformation (",input$select.transform, ")of selected data was successful.") 
            paste("The transformation (",input$select.transform, ")of selected data was successful.") 
          }else{
            transform.text <<- ""
            ""
          }
        },
        error=function(cond) {print(cond)}, 
        warning=function(cond) {print(cond)}, 
        finally={})
      }else{
        transform.text
      }
    })
  })
  
  output$transform.columns =renderUI({
    input$selector
    input$transform
    transform.data.panel()
  })
  
  ################################################################################
  
  ################################################################################
  #
  # Reorder levels -> reorder the levels of a column of factors
  #
  ################################################################################
  
  selection.changed = observe({
    if(!is.null(input$select.column)){
      choices=""
      if(!""%in%input$select.column){
        if(is.factor(data[,input$select.column])){
          choices = levels(data[,input$select.column])
        }else{
          choices = levels(as.factor(data[,input$select.column]))
        }
      }
      updateSelectInput(session=session,inputId="select.item",selected="",choices=choices)
    }
  })

  button.pressed = observe({
    input$reorder
    updateSelectInput(session=session,inputId="select.item",selected="",choices="")
    updateSelectInput(session=session,inputId="select.column",selected="")
    isolate({
      items = input$select.item
      if(!is.null(items)&!is.null(input$select.column)){
        column = data[,input$select.column]
        if(length(items)<length(column)){
          not.in = sort(unique(data[,input$select.column])[which(!unique(data[,input$select.column])%in%items)])
          levels.new = c(items,not.in)
        }else{
          levels.new = c(items)
        }
        data[,input$select.column] <<- as.factor(data[,input$select.column])
        levels(data[,input$select.column]) <<- levels.new
      }
    })
  })

  output$maintext.reorder = renderPrint({
    text = ""
    if(!is.null(input$select.column)&&!""%in%input$select.column){
      print(table(data[,input$select.column]))
    }else{
      print("Select a column!")
    }
  })

  output$reorder.levels =renderUI({
    reorder.levels.panel(choices,selected)
  })
  
  ################################################################################

  ################################################################################
  #
  # On close (Not used in the moment)
  #
  ################################################################################
  
  # code to execute when session ends
  session$onSessionEnded(function() {
    # Also clean up the users directory file for this example
#     unlink(user.dir,recursive=T)
  })

  ################################################################################
})