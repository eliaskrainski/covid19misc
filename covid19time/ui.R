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
      checkboxInput(
        inputId = 'popDivide',
        label = "Per 1M inhabitants",
        value = TRUE),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c('US',##'Curitiba, PR - BR', 
		     ##'Curitiba(SM), PR - BR',
		     'Curitiba(SMB), PR - BR', 
         'Paraná - BR', 
         'Brasil', 'India')), 
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Date (interval):',
        start = as.Date('2020-11-01'),
        end = Sys.Date(),
        format = "dd/mm/yy"),
      checkboxGroupInput(
        inputId = "plots", 
        label = 'To show', 
        choices = allpls, 
        selected = allpls[c(1, 3, 5, 11)]),
      checkboxInput(
        inputId = 'showPoints',
        label = 'Show points',
        value = TRUE),
      radioButtons(
        inputId = "legend",
        label = "Legend position",
        choices = c('Right' = 'right',
                    'Top' = 'top'),
        selected = 'right'),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformation", 
	      choices=c('None'='none', 
	                'sqrt'='sqrt', 
	                'log10'='log10'), 
        selected = 'none'),
      numericInput('last',
                   'Mostrar Rt nos últimos "n" dias',
                    7),
      actionButton(
        inputId="exit", 
        label="Exit")),
    mainPanel(
      plotOutput("plot"),
      h4("Rt cases"),
      tableOutput("cRttab"),
      h4("Rt deaths"),
      tableOutput("dRttab")
    )
    )
) # fluidPage
