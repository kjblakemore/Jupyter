library(shiny)

vars <- 5
supp <- 0.1
conf <- 0.5

shinyUI(pageWithSidebar(
  headerPanel('Association Rules'),
  sidebarPanel(
    selectInput('dataset', 'Data Set:', c('titanic' = 'titanic',
                'hospital_readmission' = 'hospital_readmission',
                'PYDATAV3' = 'PYDATAV3',
                'PYDATAV4' = 'PYDATAV4')),
    actionButton('load', 'Load Data Set'), br(), br(),

    conditionalPanel(
      condition = "input.samp=='Sample'",
      numericInput("nrule", 'Number of Rules', 5), br()
    ),

    conditionalPanel(
      condition = "input.lhsv=='Subset'",
      uiOutput("choose_lhs"), br()
    ),

    conditionalPanel(
      condition = "input.rhsv=='Subset'",
      uiOutput("choose_rhs"), br()
    ),

    conditionalPanel(
      condition = "input.mytab=='grouped'",
      sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
    ),

    conditionalPanel(
      condition = "['itemFreq', 'scatter', 'twoKeyScatter', 'grouped', 'graph', 'paracoord', 'matrix', 'threeDMatrix', 'datatable'].indexOf(input.mytab) >= 0",
      radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
      uiOutput("choose_columns"), br(),
      # .00001 is used as the min for the sliders, because 0 causes an error in the apriori function
      sliderInput("supp", "Support:", min = 0.00001, max = 1, value = supp , step = 1/10000), br(),
      sliderInput("conf", "Confidence:", min = 0.00001, max = 1, value = conf , step = 1/10000), br(),
      selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
      numericInput("minL", "Min. items per set:", 2), br(),
      numericInput("maxL", "Max. items per set::", 3), br(),
      radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
      radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),

      actionButton('generate', 'Generate Association Rules'), br(), br(), br(),
      downloadButton('downloadData', 'Download Rules as CSV')
    )
  ),

  mainPanel(
    tabsetPanel(id='mytab',
                tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                tabPanel('Two-Key Scatter', value='twoKeyScatter', plotOutput("twoKeyScatterPlot", width='100%', height='100%')),
                tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                tabPanel('3D Matrix', value='threeDMatrix', plotOutput("threeDMatrixPlot", width='100%', height='100%')),
                tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
    )
  )
))
