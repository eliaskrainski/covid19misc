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
                popDivide=TRUE,
                local=c('US', 'Curitiba(SMB), PR - BR', 
                        'Paraná - Brasil', 
                        'Brasil',
                        'India'),
                dateRange=c('01/01/20', '10/10/21'),
                plots=allpls[c(1,3)],
                ##c("Daily counts",
                  ##      "Reproduction number",
                    ##    "Fatality rate (%)",
                      ##  "retail_and_recreation",
                        ##"grocery_and_pharmacy", "parks",
                        ##'workplaces', 'residential')[c(1:2, 4:5)],
                showPoints=TRUE,
                legend='top',
                transf='log10')
            
            source("global.R")
            
        }

##        stop(safeError('testing'))

        if (length(input$plots)<1)
          if (pt) {
            stop(safeError(
              'Favor selecionar pelo menos um gráfico a ser mostrado!'))
          } else {
            stop(safeError(
              'Please select at least one plot to be shown!'))
          }

        
        sdata <- dataPrepare(input$local)
        
        data2plot(d=sdata, 
                  popDivide=input$popDivide,
                  variables=input$variables,
                  dateRange=input$dateRange, 
                  plots=input$plots,
                  showPoints=input$showPoints,
                  transf=input$transf, 
                  legpos=input$legend)


    })
    
    output$cRttab <- renderTable({
        sdata <- dataPrepare(input$local)
        nd <- as.integer(input$last)
        itt <- rev(tail(1:length(sdata$x), nd))
        rtt <- data.frame(sapply(1:length(input$local), function(l)
            paste0(
                sprintf("%0.2f", sdata$Rt[itt,l,1]), ' (',
                sprintf("%0.2f", sdata$Rtlow[itt,l,1]), '-',
                sprintf("%0.2f", sdata$Rtupp[itt,l,1]), ')')))
        colnames(rtt) <- input$local
        rtt <- data.frame(Date=rev(as.character(tail(sdata$x, nd))), rtt)
        colnames(rtt)[-1] <- input$local
        rtt
    })

    output$dRttab <- renderTable({
        sdata <- dataPrepare(input$local)
        nd <- as.integer(input$last)
        itt <- rev(tail(1:length(sdata$x), nd))
        rtt <- data.frame(sapply(1:length(input$local), function(l)
            paste0(
                sprintf("%0.2f", sdata$Rt[itt,l,2]), ' (',
                sprintf("%0.2f", sdata$Rtlow[itt,l,2]), '-',
                sprintf("%0.2f", sdata$Rtupp[itt,l,2]), ')')))
        colnames(rtt) <- input$local
        rtt <- data.frame(Date=rev(as.character(tail(sdata$x, nd))), rtt)
        colnames(rtt)[-1] <- input$local
        rtt
    })
}
