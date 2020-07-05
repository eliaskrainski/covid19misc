ui <- fluidPage(
#  includeHTML("../GLOBAL/header.html"),
 # includeHTML("../GLOBAL/MathJax.html"),
  #includeCSS("../GLOBAL/style.css"),
  withMathJax(),
  titlePanel("Temporal visualization of COVID19 data"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "variables", 
        label = NULL, #"Variables to show", 
        choices = c('Cases' = 'cases', 
                    'Deaths' = 'deaths'), 
        selected = 'cases'),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c('PARANÁ - Brazil', 
                     'Brazil', 'NY - US', 'US')),
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Date (interval):',
        start = as.Date('2020-01-20'),
        end = Sys.Date(),
        format = "dd/mm/yy"),
      checkboxInput(
        inputId = 'showPoints',
        label = 'Show points',
        value = TRUE),
      radioButtons(
        inputId = "legend", 
        label = "Legend position", 
        choices = c('Right' = 'right', 
                    'Top' = 'top'), 
        selected = 'top'),
      radioButtons(
        inputId = "transf", 
        label = "Y-axis tranformation", 
        choices=c('none'='none', 
                  'sqrt'='sqrt', 
                  'log10'='log10'), 
        selected = 'log10'),
      actionButton(
        inputId="exit", 
        label="Exit"),
    width = 3),
    mainPanel(
      plotOutput("plot"),
      width = 7)
    )
) # fluidPage
