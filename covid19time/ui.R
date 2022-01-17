ui <- fluidPage(
  withMathJax(),
  titlePanel(paste("Temporal visualization of COVID19 related data including",
                   names(lb.n)[1], 'and', names(lb.n)[2], 'all over the world')),
  sidebarLayout(
    sidebarPanel(
      checkboxInput(
        inputId = 'popDivide',
        label = "Per 1M inhabitants",
        value = TRUE),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c("US", ##'Curitiba, PR - BR', 
		     ##'Curitiba(SM), PR - BR',
		     ##'Curitiba(SMB), PR - BR') 
         ##'Paraná - BR', 
         #'Brasil') 
         'United Kingdom')
         ), 
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Date (interval):',
        start = Sys.Date()-30,
        end = Sys.Date(),
        format = "dd/mm/yy"),
      checkboxGroupInput(
        inputId = "plots", 
        label = 'To show', 
        choices = allpls, 
        selected = allpls[c(1, 3)]),
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
                   labLastRt,
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
