require(shinyBS)
require(shiny)
require(ggplot2)
require(shinyjs)
require(e1071);
require(psy);
require(digest);
source("TABSIEHelpers.R")
source("graphHelper.R")

shinyServer(
  function(input, output, session){
    
    #The data package now reads in:
      #serverData - a list of data frames. Data Frame one should be the default. All data frames
                  ## should have the SAME columns. They should merely be subsets of the original.
      #serverDataDic - A list of definitions ofr the filters: E.g. c("Not filtered", "Filtered)
      #serverHash - the hash function for the server authentication. If you don't want an 
                  ## authentication screen make this = ""
    #REPLACED loading functions
        load("survSave.rdata")
    ##Since we have old .rdata files and we're putting a lot more "assumptions" on what the user will have in the 
    ##.rdata file I'll do some checks to make sure that the 3 expected files are either there or
    ##can be faked. (i.e. the only one that HAS to be there is serverData and it needs at least one data frame.)
    if(!exists("serverData")){
      createAlert(session, "systemError", "dError", content = "There is an error with the supplied data file. serverData does not exist! You have no data! Please upload a new .rdata package and try again.", title = "ERROR!", append = TRUE)
      pData = data.frame(factor(c("Alex", "Alfredo", "Dean", "Laura", "Margie")),
                         factor(c("Deputy Director", "Director", "Programmer", "Programmer", "Life Saver")),
                         c(3,NA, 1,2,4),
                         factor(c("Faded Charcoal","", "Faded Charcoal", "Red", "Yellow")));
      colnames(pData) = c("Name", "Role", "Cube", "Fav_Color")
      serverData = list(pData)
      serverDataDic = c("No Filter")
      serverHash = ""
    }else{
      if(!exists("serverDataDic")){
        serverDataDic = c("No Filter")
      }else{
        #if we do have filters loaded, add them to the select.
        updateSelectInput(session, inputId = "filter", choices = serverDataDic, selected = serverDataDic[1])
        updateSelectInput(session, inputId = "filterCon", choices = serverDataDic, selected = serverDataDic[1])
        toggle(id = "filterFlagDiv", anim= FALSE)
        toggle(id = "filterFlagDivCon", anim= FALSE)
      }
      if(!exists("serverHash")){
        serverHash = ""
      }
      pData = getpData(serverDataDic[1],serverDataDic,serverData);
    }
    dataDic = lapply(pData, class)
    valsFactor = names(dataDic[dataDic == "factor"])
    valsNumeric = names(dataDic[dataDic == "numeric" | dataDic == "integer" ])
    valsNonText = c(valsFactor, valsNumeric)
    #NOTE! ggplot will only apply a shape to the first 6 levels of a factor, thus we will only show
    #factors with 6 or fewer factor levels!
    valsShape = valsFactor[sapply(valsFactor, function(x) length(levels(pData[,x])))<=6]
    graphDivs = c("xOmitDiv", "barPlotDiv", "FNDiv", "pointDiv", "jitterDiv")
    constDivs = c("focusedPCADiv","constellationDiv")
    #session$sendCustomMessage(type = "bsAlertClose", "gError")
    if(serverHash == "") { # if there is no hash for this one, we skip the authentication screen.
      valAuth = TRUE;
      toggle(id = "AuthPage", anim= FALSE)
      toggle(id = "TABSIEApp",anim = FALSE)
    }
    valAuth = FALSE ## is the current session authenticated?
    authAttempts = 0 ## refuses authentication attempts after 10 attempts per session.
    
####### TITLE VIEWER  ######################
    output$TitleString <-renderUI({
      if(exists("serverTitle")){
        titlePanel(serverTitle)
      }
    })
    output$Statement <-renderUI({
      if(exists("serverStatement")){
        eval(serverStatement)
      }
    })
####### BUTTON PRESSES #####################
    observeEvent(input$clearTheme, {
      if (!valAuth) return;#break processing of not authorized.
      updateTextInput(session, "titleField", value = "")
      updateTextInput(session, "xLab", value = "")
      updateTextInput(session, "yLab", value = "")
      updateSliderInput(session, "textSize", value = 15)
      updateSliderInput(session, "xLabRotation", value = 0)
      updateSliderInput(session, "xLabHeight", value = 0)
    })
######## DIV BOX CONTROL for ADVANCED PANEL #################
    observe({
      if (!valAuth) return;#break processing of not authorized.
      validate(
        need(input$xVal, "")
      )
      shinyjs::onclick("toggleTheme", toggle(id = "themeDiv", anim= TRUE))
      shinyjs::onclick("togglePoint", {
        toggle(id = "pointAdvDiv", anim= TRUE)
        updateSelectInput(session, "pointColor", choices = c("No color", valsNonText))
        updateSelectInput(session, "pointShape", choices = c("No shape", valsShape))
        })
      shinyjs::onclick("toggleViolin", toggle(id = "violinDiv", anim= TRUE))
      shinyjs::onclick("toggleBox", toggle(id = "boxDiv", anim= TRUE))
    })
    
################ DIV BOX CONTROL for GRAPH PANEL ######################################
    observe({
      if (!valAuth) return;#break processing of not authorized.
      validate(
        need(input$xVal,""),
        need(input$yVal, "")
      )
      xf = (input$xVal %in% valsFactor)
      yf = (input$yVal %in% valsFactor)
      if(xf){
        if(yf){
          #both factors
          toggleOn = c("barPlotDiv", "xOmitDiv")
          toggleMaster(toggleOn, graphDivs)
        }else{
          #X is factor y is numeric
          toggleOn = c("xOmitDiv", "FNDiv")
          if(input$boxViolin == "Points"){
            toggleOn = c(toggleOn, "pointDiv")
            if(input$pointJitter){
              toggleOn = c(toggleOn, "jitterDiv")
            }
          }
          toggleMaster(toggleOn, graphDivs)
        }
      }else{
        #X is numeric
        if(yf){
          #Oops! X is numeric Y is a factor! 
          tmp = input$yVal
          ## Per Alex's request 7-27-2016 making this correction transparent to the user. 
          updateSelectInput(session, "yVal", selected = input$xVal)
          updateSelectInput(session, "xVal", selected = tmp)
          #This method will be recalled, so lets escape with a return.
          return()
        }else{
          #Y is also numeric!
          #activate the necessary divs: point and jitter
          if(input$pointJitter){
            toggleOn = c("pointDiv", "jitterDiv")
          }else{
            toggleOn = c("pointDiv")
          }
          toggleMaster(toggleOn, graphDivs)
        }
      }
    })
############### DIV BOX CONTROL FOR CONSTELLATION #################
    observe({
      if (!valAuth) return;#break processing of not authorized.
      if(input$focusedPCA){
        toggleOn = c("focusedPCADiv")
        toggleMaster(toggleOn, constDivs)
      }else{
        toggleOn = c("constellationDiv")
        toggleMaster(toggleOn, constDivs)
      }
    })
##############UI OUTPUTS####################################
#These are only here because in later TABSIE we'll allow users to upload their own datasets, so drop downs will need to
#be dynamically generated.
    output$xy <-renderUI({
      if (!valAuth) return;#break processing of not authorized.
      verticalLayout(
        selectInput("xVal", "X Value", valsNonText ),
        selectInput("yVal", "Y Value", valsNonText)
      )
    })
    output$PCAVariable <-renderUI({
      selectInput("constResponseVar","Response Variable", valsNonText)
    })
    output$summaryRegion <- renderUI({
      validate(
        need(input$xVal, warningRender),
        need(input$yVal, warningRender)
      )
      if(input$xVal %in% valsFactor & input$yVal %in% valsFactor){
        tableOutput("freqTable")
      }else{
        if(input$yVal %in% valsNumeric){
          verticalLayout(
            p("Summary"),
            tableOutput("summaryTable")#originally had the summary(lm()) after this, but I like just the summary table better- leaving the render table in case we decide to re-add it.
          )
        }
      }
    })
########## RENDER PLOT OUTPUT ################################
    output$visPlot <- renderPlot({
      if (!valAuth) return;#break processing of not authorized.
      validate(
        need(input$xVal, warningRender),
        need(input$yVal, warningRender)
      )
      #This is the easiest and most adaptable way to handle the filtering options.
      #We can expand it to an entire method later and just use pdata as the graphing
      #option instead of if this, pass a subset, just always pass pdata. or Plot-data
      #since this is pass by promise not changing the data only creates a pointer, otherwise
      #we were going to change the data anyway (unless the UI for a type of graph wasn't finished
      #rendering, but it's still better to have this here rather than in each if)
      pdata = getpData(input$filter, serverDataDic, serverData)
      p = NULL
      #if there is an alert message open, close it.
      session$sendCustomMessage(type = "bsAlertClose", "gError")
      if(input$xVal %in% valsFactor){
        if(input$yVal %in% valsFactor){
          #factor factor
          validate(need(input$barProportion, warningRender))
          if(input$barProportion == "Compare proportions") position = "fill"
          else position = "stack"
          p = runGGPLOTFF(pdata, input$xVal, input$yVal , omitNA_X = input$xOmit, omitNA_Y = input$yOmit, position = position)
        }else{
          #factor number
          validate(need(input$boxViolin, warningRender))
          switch(input$boxViolin,
                 "Points" = {p = getPointPlot(pdata, input, "FN")},
                 "Violin" = {p = getViolinPlot(pdata, input)},
                 "Box plot" = {p = getBoxPlot(pdata, input)})
        }
      }else{#else X is numeric (Note, the issue where Y is a factor is handled by the UI render hot swapping them.)
          p = getPointPlot(pdata, input, "NN")
      }
      p = addTheme(p, input)
      if(input$coordFlop){
        p + coord_flip()
      }else{
        p
      }
    })#end output$visPlot
    
    output$constellationPlot <- renderPlot({
      if (!valAuth) return;#break processing of not authorized.
      pdata = getpData(input$filterCon, serverDataDic, serverData)
      if(input$focusedPCA){
        validate(
          need(input$constResponseVar, warningRender)
        )
        pcawrap(pdata, respvar = input$constResponseVar,pca='f', contraction='Yes')
      }else{
        validate(
          need(input$constVSlider, warningRender),
          need(input$constHSlider, warningRender),
          need(input$constFSlider, warningRender)
        )
        pcawrap(pdata, nbsphere=1, back=T,v=input$constVSlider,
                h=input$constHSlider,f=input$constFSlider)
        
      }
    })#END PLOT constellationPlot
######### RENDER TABLES ###########################################    
    output$freqTable <- renderTable({#validation done before this is called, no need to repeat
      if (!valAuth) return;#break processing of not authorized.
      pdata = getpData(input$filter, serverDataDic, serverData)
      addmargins(table(pdata[,c(input$xVal,input$yVal)]))
    })
    
    output$summaryTable <- renderTable({
      if (!valAuth) return;#break processing of not authorized.
      pdata = getpData(input$filter, serverDataDic, serverData)
      if(input$xVal %in% valsFactor){
        as.table(sapply(split(pdata[,input$yVal],pdata[,input$xVal]),fpSummary))
      }else{
        as.table(sapply(pdata[,c(input$xVal,input$yVal)],fpSummary))
      }
    })
    
    output$lmTable <- renderTable({
      if (!valAuth) return;#break processing of not authorized.
      pdata = getpData(input$filter, serverDataDic, serverData)
      summary(lm(pdata[,input$yVal] ~ pdata[,input$xVal]))
    })
    
########## Authentication Reactive #########
    observeEvent(input$authButton,{
      ##processes authentication.
      session$sendCustomMessage(type = "bsAlertClose", "aError")
      if(authAttempts >= 10){ 
        createAlert(session, "authError", "aError", content = "Maximum authentications attempts reached.", title = "Warning", append = TRUE)
        return
      }
      authAttempts <<- authAttempts + 1
      if(digest(isolate(input$authPassword), algo = "sha512", ascii = TRUE) == serverHash){
        valAuth = TRUE
        toggle(id = "AuthPage", anim= TRUE)
        toggle(id = "TABSIEApp",anim = TRUE)
      }else{
        createAlert(session, "authError", "aError", content = "Invalid pin.", title = "Warning", append = FALSE)
      }
    })
########## PORTABLE R CODE#############    
    
    ### THIS CODE IS USED FOR PORTAL R SO THAT THE R SESSION ENDS WHEN THE BROWSER IS CLOSED!!
#      session$onSessionEnded(function() { 
#        stopApp()
#        q("no") 
#      })
  }
)
