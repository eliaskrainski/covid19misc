server <- function(input, output) {
    output$plot <- renderPlot({
        observe({
            if(input$exit > 0) {
                stopApp(NULL)
            }
        })
        sdata <- prepareData(
          selectData(input), 
          input$transf)
        xlm <- as.Date(input$dateRange, 
                       '%d/%m/%y')
        if (diff(xlm)<3) 
          stop(safeError('Too narrow time window!'))
        if (length(input$variables)<1)
          stop(safeError(
            'Please select one variable to show!'))
        vv <- pmatch(
          input$variables, 
          c('cases', 'deaths'))
        par(mfrow=c(2, 1), mar=c(2, 4, 0, 0), c(3, 0.5, 0))
        data2plot(d=sdata, 
                  v=vv,
                  scol=rep(1:ncol(sdata[[2]]), 2), 
                  xlim=xlm, 
                  transf=input$transf)
    ##    if (length(vv)==2)
      ##    legend('topleft', c('Cases', 'Deaths'), 
        ##         pch=c(19, 8), ncol = 2, bty='n')    
        
        })
    
}
