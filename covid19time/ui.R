ui <- fluidPage(
#  includeHTML("../GLOBAL/header.html"),
 # includeHTML("../GLOBAL/MathJax.html"),
  #includeCSS("../GLOBAL/style.css"),
  withMathJax(),
  titlePanel("Temporal visualization of COVID19 data"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId="variables", 
                         label = "Variables to show", 
                         choices = c("Cases (dot)" = 'cases', 
                                     'Deaths (star)' = 'deaths'), 
                         selected = 'cases'),
      selectizeInput(inputId = "local",
                     label = "Local",
                     choices = locals,
                     multiple = TRUE, 
                     selected = 'NY - US'),
      dateRangeInput(inputId='dateRange',
                     label = 'Date (interval):',
                     start = as.Date('2020-01-20'),
                     end = Sys.Date(),
                     format = "dd/mm/yy"),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformation", 
	      choices=c('none'='none', 
	                'sqrt'='sqrt', 
##	                'log2'='log2', 
	                'log10'='log10'), 
	      selected = 'log10'),
      actionButton(inputId="exit", 
                   label="Exit"),
    width = 3),
    mainPanel(
      plotOutput("plot"),
      width = 7)
    )
) # fluidPage
