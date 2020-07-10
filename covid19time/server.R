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
                dateRange=c('01/01/20', '10/10/20'), 
                transf='log10',
                local='NY - US')
            source("global.R")
            
        }

        sdata <- prepareData(
          input$local)

        par(mfrow=c(2, 1), mar=c(0.5, 4.5, 0.5, 0.5), mgp=c(3.5, 0.5, 0))
        data2plot(d=sdata, 
                  variables=input$variables,
                  dateRange=input$dateRange, 
                  showPoints=input$showPoints,
                  transf=input$transf, 
                  legpos=input$legend)

        })
    
}
