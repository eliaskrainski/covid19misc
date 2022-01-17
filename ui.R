ui <- fluidPage(
  includeHTML("../GLOBAL/header.html"),
#  includeHTML("../GLOBAL/MathJax.html"),
#  includeCSS("../GLOBAL/style.css"),
  withMathJax(),
  titlePanel(paste("Visualização temporal de dados de COVID19,",
	     'incluindo', lb.n[1], 'e', lb.n[2], 'globais')),
  sidebarLayout(
    sidebarPanel(
      checkboxInput(
        inputId = 'popDivide',
        label = "Por 1M habitantes",
        value = TRUE),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c(##'Curitiba(SMB), PR - BR', 
                     'Paraná - BR', 
                     'Brazil')),
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Data (intervalo):',
        start = Sys.Date()-31,
        end = Sys.Date(),
        format = "dd/mm/yy", 
        language = 'pt'),
      checkboxGroupInput(
        inputId = "plots", 
        label = 'Para mostrar', 
        choices = allpls, 
        selected = allpls[c(1,3, 5:7)]),
      checkboxInput(
        inputId = 'showPoints',
        label = 'Mostrar pontos',
        value = TRUE),
      radioButtons(
        inputId = "legend",
        label = "Legenda (posição)",
        choices = c('À direita' = 'right',
                    'Acima' = 'top'),
        selected = 'right'),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformação", 
	      choices=c('Nenhuma'='none', 
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
    )),
#  includeHTML("../GLOBAL/footer.html")
) # fluidPage
