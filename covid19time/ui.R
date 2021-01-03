ui <- fluidPage(
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
        selected = c('Curitiba(SM), PR - BR', 
                     'PR - BR', 
                     'Brasil', 'NY - US', 'US')),
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Date (interval):',
        start = as.Date('2020-01-20'),
        end = Sys.Date(),
        format = "dd/mm/yy"),
      checkboxGroupInput(
        inputId = "plots", 
        label = 'To show', 
        choices = allpls, 
        selected = allpls[c(1:2, 5)]),
      checkboxInput(
        inputId = 'showPoints',
        label = 'Show points',
        value = TRUE),
      radioButtons(
        inputId = "legend",
        label = "Legend (position)",
        choices = c('Right' = 'right',
                    'Top' = 'top'),
        selected = 'top'),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformation", 
	      choices=c('Nenhuma'='none', 
	                'sqrt'='sqrt', 
	                'log10'='log10'), 
        selected = 'log10'),
      actionButton(
        inputId="exit", 
        label="Exit")),
    mainPanel(
      plotOutput("plot"))
    ),
) # fluidPage
