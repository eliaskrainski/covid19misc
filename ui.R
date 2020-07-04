ui <- fluidPage(
  includeHTML("../GLOBAL/header.html"),
  includeHTML("../GLOBAL/MathJax.html"),
  includeCSS("../GLOBAL/style.css"),
  withMathJax(),
  titlePanel("Visualização temporal de dados de COVID19"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "variables", 
        label = "Variáveis a visualizar", 
        choices = c('Casos' = 'cases', 
                    'Óbitos' = 'deaths'), 
        selected = 'cases'),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c('Curitiba, PARANÁ - Brazil', 
                     'PARANÁ - Brazil', 
                     'Brazil', 'NY - US', 'US')),
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Data (intervalo):',
        start = as.Date('2020-01-20'),
        end = Sys.Date(),
        format = "dd/mm/yy", 
        language = 'pt'),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformação", 
	      choices=c('Nenhuma'='none', 
	                'sqrt'='sqrt', 
	                'log10'='log10'), 
        selected = 'log10'),
      radioButtons(
        inputId = "legend", 
        label = "Legenda (posição)", 
        choices = c('À direita' = 'right', 
                    'Acima' = 'top'), 
        selected = 'right'),
      actionButton(
        inputId="exit", 
        label="Exit"),
    width = 3),
    mainPanel(
      plotOutput("plot"),
      width = 7)
    ),
  includeHTML("../GLOBAL/footer.html")
) # fluidPage
