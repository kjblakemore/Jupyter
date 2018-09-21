library(shiny)
library(data.table)
library(arules)
library(arulesViz)

shinyServer(function(input, output) {

  dataset <- eventReactive(input$load, {
    filename <- paste('./data/', input$dataset, '_features.csv', sep='')
    data <- read.csv(filename, na.strings=c(''), check.names=FALSE)
    data[] <- lapply(data, factor)  # all columns must be of type factor
    data
  })

  output$choose_columns <- renderUI({
    checkboxGroupInput("cols", "Choose variables:", choices  = colnames(dataset()), selected = colnames(dataset())[1:5])
  })

  output$choose_lhs <- renderUI({
    checkboxGroupInput("colsLHS", "Choose LHS variables:", choices  = input$cols, selected = input$cols[1])
  })

  output$choose_rhs <- renderUI({
    checkboxGroupInput("colsRHS", "Choose RHS variables:", choices  = input$cols, selected = input$cols[1])
  })

  rules <- eventReactive(input$generate, {
    filename <- paste('data/', input$dataset, '_features.csv', sep='')
    ds <- read.csv(filename, na.strings=c(''), check.names=FALSE)
    ds[] <- lapply(ds, factor)  # all columns must be of type factor
    tr <- as(ds[,input$cols], 'transactions')

    arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))

    if(input$rhsv=='Subset' & input$lhsv!='Subset'){
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(ds, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=rhs %in% varsR)

    } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(ds, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL)

    } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(ds, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(ds, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)

    } else {
      ar <- arAll
    }

    # Rule length
    nR <- ifelse(input$samp == 'All Rules', length(ar), input$nrule)
    ar <- sort(ar, by=input$sort)[1:nR]
  })

  ## Item Frequency Plot ##########################
  output$itemFreqPlot <- renderPlot({
    trans <- as(dataset(), 'transactions')
    itemFrequencyPlot(trans, support=.25, cex.names=0.8)
  }, height=800, width=800)

  ## Scatter Plot ##########################
  output$scatterPlot <- renderPlot({
    plot(rules(), method='scatterplot', engine='htmlwidget')
  }, height=800, width=800)

  ## Two-Key Scatter Plot ##########################
  output$twoKeyScatterPlot <- renderPlot({
    plot(rules(), method='two-key plot', engine='htmlwidget')
  }, height=800, width=800)

  ## Grouped Plot #########################
  output$groupedPlot <- renderPlot({
    plot(rules(), method='grouped', control=list(k=input$k))
  }, height=800, width=800)

  ## Graph Plot ##########################
  output$graphPlot <- renderPlot({
    plot(rules(), method='graph', engine='htmlwidget')
  }, height=800, width=800)

  ## Parallel Coordinates Plot ###################
  output$paracoordPlot <- renderPlot({
    plot(rules(), method='paracoord')
  }, height=800, width=800)

  ## Matrix Plot ###################
  output$matrixPlot <- renderPlot({
    plot(rules(), method='matrix', engine='htmlwidget', shading=c('lift'))
  }, height=800, width=800)

  ## 3D Matrix Plot ###################
  output$threeDMatrixPlot <- renderPlot({
    plot(rules(), method='matrix', engine='3d', shading=c('lift'))
  }, height=800, width=800)

  ## Rules Data Table ##########################
  output$rulesDataTable <- renderDataTable({
    p <- inspectDT(rules())
    htmlwidgets::saveWidget(p, "rules.html", selfcontained = FALSE)
    browseURL("rules.html")
  })

  ## Download data to csv ########################
  output$downloadData <- downloadHandler(
    filename = 'arules_data.csv',
    content = function(file) {
      write.table(as(rules(), 'data.frame'), file, row.names=FALSE, sep=',')
    })
})
