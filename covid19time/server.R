server <- function(input, output) {

    output$plot <- renderPlot({
        observe({
            if(input$exit > 0) {
                stopApp(NULL)
            }
        })

        if (FALSE) {
            
            input <- list(
                variables='cases',
                local=c('Curitiba(SM), PR - BR', 
                     'PR - Brasil', 
                     'Brasil', 'NY - US', 'US'),
                dateRange=c('01/01/20', '10/10/21'),
                plots=c("Daily counts",
                        "Reproduction number",
                        "Fatality rate (%)")[1:2],
                showPoints=TRUE,
                legend='top',
                transf='log10')
            
            source("global.R")
            
        }

##        stop(safeError('testing'))

        sdata <- dataPrepare(
          input$local)
        
        if (length(input$plots)<1)
          if (pt) {
            stop(safeError(
              'Favor selecionar pelo menos um gráfico a ser mostrado!'))
          } else {
            stop(safeError(
              'Please select at least one plot to be shown!'))
          }
        
        par(mfrow=c(length(input$plots), 1), 
            mar=c(0.5, 4.5, 0.5, 0.5), mgp=c(3.5, 0.5, 0))
        
        data2plot(d=sdata, 
                  variables=input$variables,
                  dateRange=input$dateRange, 
                  plots=input$plots,
                  showPoints=input$showPoints,
                  transf=input$transf, 
                  legpos=input$legend)

    })
    
}
