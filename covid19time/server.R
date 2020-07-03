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

        par(mfrow=c(2, 1), mar=c(0, 3.5, 0, 0), mgp=c(2.5, 0.5, 0))
        data2plot(d=sdata, 
                  variables=input$variables,
                  dateRange=input$dateRange, 
                  transf=input$transf)
    ##    if (length(vv)==2)
      ##    legend('topleft', c('Cases', 'Deaths'), 
        ##         pch=c(19, 8), ncol = 2, bty='n')    
        
        })
    
}
