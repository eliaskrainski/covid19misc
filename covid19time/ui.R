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
        label = 'Variable (Total world)', 
        choices=lb.n, 
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
      checkboxGroupInput(
        inputId = "plots", 
        label = 'To show', 
        choices = pls, 
        selected = pls),
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
        label="Exit")),
    mainPanel(
      plotOutput("plot"))
    )
) # fluidPage
