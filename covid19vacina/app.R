
library(shiny)
library(shinydashboard)

load('RData/w2pop.RData')
load('RData/v2tab.RData')

attdate <- attr(v2tab, 'update')

locl <- locll <- dimnames(w2pop)[[1]]
locl[c(1+5+27+1):length(locll)] <- 
  substring(locll[(1+5+27+1):length(locll)], 9)

vaclab <- c('AZ', 'Coronavac', 'Janssen', 'Pfizer')

pyramid2plot <- function(slocal, 
                         svac) {
  
  il <- which(locl==slocal)
  iv <- pmatch(svac, vaclab)
  
  pop <- w2pop[il,, ]
  vac <- apply(v2tab[il,,,,,], 2:5, sum)
  vac <- apply(vac[,,iv,,drop=FALSE], c(1,2,4), sum)

  cF <- rgb(1.0, 3:0/5, 3:0/8)
  cM <- rgb(3:0/10, 3:0/4, 1.0)
  
  xMax <- max(pop)
  
  x0 <- pretty(c(0,xMax),4)
  
  xl <- list(x=c(-rev(x0[-1]), x0)) 
  xl$l <- ifelse(xl$x==0, '', paste0(abs(round(xl$x/1e3)), 'K'))
  
  par(mfrow=c(1,1), mar=c(5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
  barplot(-pop[,1], names.arg='', horiz=TRUE, space=0, axes=FALSE,
          border='transparent', col=cF[1],
          xlim=c(-1,1)*xMax) 
  barplot(pop[,2], horiz=TRUE, space=0, add=TRUE,
          border='transparent', col=cM[1], axes=FALSE)
  axis(1, xl$x, xl$l)

  for(k in 1:2) {
    barplot(-vac[,1,k], horiz=TRUE, space=0, add=TRUE,
            border='transparent', ##'black')[(k>1)+1],
            col=cF[1+k], axes=FALSE)
    barplot(vac[,2,k],  horiz=TRUE, space=0, add=TRUE,
            border='transparent', ##'black')[(k>1)+1],
            col=cM[1+k], axes=FALSE)
  }

  abline(v=xl$x, h=2*(0:10), lty=2, col=gray(0.3,0.5))
  text(rep(par()$usr[2]*0.95, 5), 
       4*(0:4), 20*(0:4), cex=1.2, xpd=TRUE)
  text(par()$usr[2]*0.95, 2, 'Idade', srt=90, cex=1.2)
  mtext(paste('Atualizado em',
              format(attdate, '%d de %B de %Y')),
        1, 2, adj=0, cex=0.9)
  mtext(paste0('População: Projeções feitas pelo departamento de demografia da UFRN'),
        1, 3, adj=0, cex=0.9)
  mtext('Vacinação: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao', 1, 4, adj=0, cex=0.9)
  mtext('@eliaskrainski', 1, 4, adj=1, cex=1.25)
}

ui <- fluidPage(
  withMathJax(),
  titlePanel("Vacinação contra COVID19."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "local",
        label = "Local",
        choices = locl, 
        selected = 'Brasil',
        selectize = TRUE,
        multiple = FALSE), 
      checkboxGroupInput(
        inputId = "vaccine",
        label = "Vacina",
        choices = vaclab,
        selected = vaclab,
        inline = TRUE),
      actionButton(
        inputId="exit", 
        label="Exit")
    ),
    mainPanel(
      plotOutput("pyramid"),
      h4("Doses de cada vacina."),
      tableOutput("vdose")
    ) ## end mainPanel
  ) ## end sidebarLayout
) ## fluidPage

server <- function(input, output) { 

    output$pyramid <- renderPlot({
      observe({
        if(input$exit > 0) {
          stopApp(NULL)
        }
      })
      
      if (length(input$local)<1)
        if (pt) {
          stop(safeError(
            'Favor selecionar pelo menos um local!'))
        } else {
          stop(safeError(
            'Please select at least one local!'))
        }

      if (length(input$vaccine)<1)
        if (pt) {
          stop(safeError(
            'Favor selecionar pelo menos uma vacina!'))
        } else {
          stop(safeError(
            'Please select at least one vaccine!'))
        }
      
      pyramid2plot(input$local, 
                   input$vaccine)
      
    })
    
    output$vdose <- renderTable({
      il <- which(locl==input$local)
      iv <- pmatch(input$vaccine, vaclab)
      tab <- apply(v2tab[il,,,,iv,, drop=FALSE], 5:6, sum)
      colnames(tab) <- paste('Dose', colnames(tab))
      tab <- addmargins(tab)
      storage.mode(tab) <- 'integer'
      rownames(tab) <- gsub('Sum', 'Total', rownames(tab))
      colnames(tab) <- gsub('Sum', 'Total', colnames(tab))
      return(tab)
    }, rownames=TRUE)
    
}

shinyApp(ui, server)
