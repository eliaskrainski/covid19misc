library(shiny)

load('RData/w2pop.RData')
load('RData/v2tab.RData')

dataupdate <- attr(v2tab, 'dataupdate')
attdate <- attr(v2tab, 'update')

locl <- locll <- dimnames(w2pop)[[1]]
locl[c(1+5+27+1):length(locll)] <- 
  substring(locll[(1+5+27+1):length(locll)], 9)

vaclab <- c('AZ', 'Coronavac', 'Janssen', 'Pfizer')
cF <- rgb(1.0, 4:0/5, 4:0/8)
cM <- rgb(4:0/10, 4:0/4, 1.0)

pt <- length(grep('MESSAGES=pt', Sys.getlocale())>0)

if(pt) {
  mainlabel <- 'Vacinação contra COVID19 no Brasil, regiões, estados e municípios.'
  Vlabel <- 'Vacina'
  Plabel <- 'Divide pela População (%)'
  Ptime <- 'Evolução por semana epidemiológica'
  genderlabs <- c('Mulheres', 'Homens')
  agelabel <- 'Idade'
  yVlabel <- '# doses' 
  Fnotes <- c(paste('Dados atualizados pelo MS em',
              format(dataupdate, '%d de %B de %Y')), 
              'População: Projeções feitas pelo departamento de demografia da UFRN', 
              'Vacinação: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao')
  Ttitle <- 'Doses de cada vacina:'
  T2title <- 'População (Lepp/PPGDem/UFRN, "https://doi.org/10.31406/relap2020.v14.i1.n26.6">) e doses (MS) por faixa etária e sexo:'
} else {
  mainlabel <- 'Vaccine against COVID19 in Brazil, regions, states and municipalities.'
  Vlabel <- 'Vaccine'
  Plabel <- 'Divide by population (%)'
  Ptime <- 'Evolution by epidemiological week.'
  genderlabs <- c('Women', 'Men')
  agelabel <- 'Age'
  yVlabel <- '# doses per week'
  Fnotes <- c(paste('Last update',
                    format(attdate, '%B %d, %Y')), 
              'Population data: Projections made by the department of demography of the UFRN', 
              'Vaccine data: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao')
  Ttitle <- 'Doses by each vaccine:'
  T2title <- 'Population (Lepp/PPGDem/UFRN, "https://doi.org/10.31406/relap2020.v14.i1.n26.6") and doses (MS) by age group and gender:'
}

prettyDateLabel <- function(x, format=NULL) {
  xl <- list(x=x)
  if(!is.null(format)) {
    xl$l <- format(xl$x, format)
    return(xl)
  }
  nd <- as.integer(difftime(x[2], x[1], units='days'))
  xl$l <- format(xl$x, '%b,%d')
  if(nd>40) {
    xl$l <- format(xl$x, '%b,%d')
  }
  if(nd>400) {
    xl$l <- format(xl$x, '%y,%b')
  }
  return(xl)
}

xKMlabel <- function(x, digits=2) {
  l <- x
  ii <- which(abs(x)>=1e3)
  if(length(ii)>0)
    l[ii] <- paste0(round(x[ii]/1e3, digits=digits), 'K')
  ii <- which(abs(x)>=1e6)
  if(length(ii)>0)
    l[ii] <- paste0(round(x[ii]/1e6, digits=digits), 'M')
  return(l)
}

prettyLabel <- function(x, sign=FALSE) {
  if(sign) {
    xl <- list(x=c(-rev(x), x)) 
  } else {
    xl <- list(x=x) 
  }
  xl$l <- xKMlabel(abs(xl$x))
  return(xl)
}

pyramid2plot <- function(slocal, 
                         svac, 
                         ppop) {
  
  il <- which(locl==slocal)
  iv <- pmatch(svac, vaclab)
  
  pop <- w2pop[il,, ]
  npop <- apply(pop, 2, sum)
  
  vac <- apply(v2tab[il,,,,,], 2:5, sum)
  vac <- apply(vac[,,iv,,drop=FALSE], c(1,2,4), sum)
  nvac <- apply(vac, 2:3, sum)

  xMax <- max(pop)
  
  xl <- prettyLabel(
    x=pretty(c(0,xMax),4)[-1], 
    sign=TRUE)
  
  par(mfrow=c(1,ppop+1), mar=c(5, 0, 0, 0), 
      mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
  
  barplot(-pop[,1], names.arg='', horiz=TRUE, space=0, axes=FALSE,
          border='transparent', col=cF[1],
          xlim=c(-1.05,1.05)*xMax) 
  barplot(pop[,2], horiz=TRUE, space=0, add=TRUE,
          border='transparent', col=cM[1], axes=FALSE)
  axis(1, xl$x, xl$l)
  abline(v=xl$x, h=2*(0:10), lty=2, col=gray(0.3,0.5))
  text(rep(par()$usr[2]*0.95, 4), 4*(1:4), 20*(1:4), cex=1.2, xpd=TRUE)
  text(par()$usr[2]*0.95, 2, agelabel, srt=90, cex=1.2, xpd=TRUE)
  n3s <- cbind(npop, nvac)
  n3l <- xKMlabel(n3s) 
  for(k in 1:2)
    legend(xMax*c(-0.99, 0.9)[k], par()$usr[4], 
           paste(c('Pop', dimnames(vac)[[3]]), n3l[k,]), 
           bg='white', box.col='transparent',
           border='transparent', xjust=k-1,
           fill=rbind(cF, cM)[k,], 
           cex=1.00, title=genderlabs[k])
  
  for(k in 1:3) {
    barplot(-vac[,1,k], horiz=TRUE, space=0, add=TRUE,
            border='transparent', ##'black')[(k>1)+1],
            col=cF[1+k], axes=FALSE)
    barplot(vac[,2,k],  horiz=TRUE, space=0, add=TRUE,
            border='transparent', ##'black')[(k>1)+1],
            col=cM[1+k], axes=FALSE)
  }
  for(k in 1:3)
    mtext(Fnotes[k], 1, 1+k, adj=0, cex=0.9)
  
  if(ppop){
    for(k in 1:3) {
      vplot <- new('numeric', -vac[,1,k]/pop[,1])
      barplot(vplot, 
              horiz=TRUE, space=0, add=(k>1), ylab='',
              border='transparent', ##'black')[(k>1)+1],
              col=cF[1+k], axes=FALSE, xlim=c(-1,1))
      vplot <- new('numeric', vac[,2,k]/pop[,2])
      barplot(vplot,  horiz=TRUE, space=0, add=TRUE,
              border='transparent', ##'black')[(k>1)+1],
              col=cM[1+k], axes=FALSE, ylab='')
    }
    axis(1, c(-4:-1, 1:4)/5, 
         paste0(abs(round(c(-4:-1, 1:4)/.05)), '%'))
    abline(v=c(-5:-1/5, 1:5/5), h=2*(0:10), lty=2, col=gray(0.3,0.5))
    p3s <- nvac/npop
    p3l <- matrix(paste0(round(100*p3s, 1), '%'), 2) 
    for(k in 1:2)
      legend(c(-0.95, 0.99)[k], 5, 
             paste(c('D1', 'D2', 'D3'), p3l[k,]), 
             bg='white', box.col='transparent',
             border='transparent', xjust=k-1,
             fill=rbind(cF, cM)[k,], 
             cex=1.00, title=genderlabs[k])
  } 
  mtext('@eliaskrainski', 1, 2, adj=0.97, cex=1.25)
  
}

timeplot <- function(slocal, svac, ppop, doplot) {

  il <- which(locl==slocal)
  iv <- pmatch(svac, vaclab)
  
  pop <- w2pop[il,, ]
  npop <- apply(pop, 2, sum)

  vac <- apply(v2tab[il,,,,,], c(1,3:5), sum)
  vac <- apply(vac, 2:4, cumsum)

  nt <- dim(vac)[1]
  x <- as.Date('2021-01-03') + 7*(1:nt) -7

  xl <- prettyDateLabel(pretty(x,7))
  
  col4d <- list(
    d1=rgb(c(0.3,1.0,0.0,0.7), 
           c(0.5,0.4,1.0,0.9), 
           c(1.0,0.2,0.0,0.8)), 
    d2=rgb(c(0.1,1.0,0.3,0.5), 
           c(0.3,0.1,1.0,0.8), 
           c(1.0,0.0,0.3,0.7)),
    d3=rgb(c(0.1,1.0,0.2,0.3), 
           c(0.3,0.1,1.0,0.6), 
           c(1.0,0.0,0.2,0.5)))
  
  nv <- length(iv)

  par(mfrow=c(1,3), mar=c(2, 4, 0, 0), 
      mgp=c(3,0.5,0), las=1)
  
  dpop <- c(1,1)
  if(ppop) dpop <- npop

  for(d in 1:3) {
    avacf <- apply(vac[,1,,d][,iv,drop=FALSE],1,cumsum)/dpop[1]
    avacm <- apply(vac[,2,,d][,iv,drop=FALSE],1,cumsum)/dpop[2]
    if(nv==1) {
      avacf <- matrix(avacf, 1)
      avacm <- matrix(avacm, 1)
    }

    ylm <- c(-1, 1)*max(avacf, avacm)
    if(ppop) {
      yl <- list(x=pretty(c(0, ylm[2]), 4)[-1])
      yl$x <- c(-rev(yl$x), yl$x)
      yl$l <- paste0(abs(round(100*yl$x)), '%')
      yVlabel  <- paste('%', substring(yVlabel, 3))
    } else {
      yl <- prettyLabel(
        x=pretty(c(0, ylm[2]), 4)[-1], 
        sign=TRUE)
    } 
    ylm <- range(yl$x)
    plot(x, vac[,1,iv[1],1], ylim=ylm, 
         type='n', axes=FALSE, xlab='', 
         ylab=paste(genderlabs[2], '      ', 
                    yVlabel, '       ', genderlabs[1]))
    
    polygon(c(x, rev(x), x[1]), 
            c(avacf[nv,,drop=FALSE], 
              rev(-avacm[nv,,drop=FALSE]), 
              avacf[nv,1,drop=FALSE]), 
            col=col4d[[d]][iv[nv]], 
            border='transparent')
    if(nv>1) {
      for(v in (nv-1):1) {
        polygon(c(x, rev(x), x[1]), 
                c(avacf[v,,drop=FALSE], 
                  rev(-avacm[v,,drop=FALSE]), 
                  avacf[v,1,drop=FALSE]), 
                col=col4d[[d]][iv[v]], 
                border='transparent')
      }
    }
    axis(1, xl$x, xl$l)
    axis(2, yl$x, yl$l)
    legend('topleft', vaclab[iv], fill=col4d[[d]][iv], 
           bty='n', border='transparent', 
           title=paste('Dose', c(1, '2')[d]))
  }
  
}

makeTable <- function(slocal, svac, ppop) {
  il <- which(locl==slocal)
  iv <- pmatch(svac, vaclab)
  tab <- apply(v2tab[il,,,,iv,, drop=FALSE], 5:6, sum)
  tab <- addmargins(tab)
  storage.mode(tab) <- 'integer'
  if(pt) {
    rownames(tab) <- gsub('Sum', 'Total', rownames(tab))
    colnames(tab) <- gsub('Sum', 'Total', colnames(tab))
  }
  if(ppop) {
    tab <- as.data.frame(cbind(tab, 100*tab/sum(w2pop[il,, ])))
    colnames(tab)[5:8] <- paste0(colnames(tab)[5:8], '(%Pop)')
    for(j in 1:4)
      storage.mode(tab[,j]) <- 'integer'
  }
  return(tab)
}

makeTable2 <- function(slocal, svac, ppop) {
  il <- which(locl==slocal)
  iv <- pmatch(svac, vaclab)
  tab <- as.data.frame(cbind(
    w2pop[il,,],
    apply(v2tab[il,,,,iv,1, drop=FALSE], c(3,4), sum),
    apply(v2tab[il,,,,iv,2, drop=FALSE], c(3,4), sum),
    apply(v2tab[il,,,,iv,3, drop=FALSE], c(3,4), sum)))
  rownames(tab) <- gsub(
      'X', '', 
      gsub('.a.', ' a ', 
           gsub('90.', '90+', rownames(tab), 
                fixed=TRUE), fixed=TRUE), fixed=TRUE)
  colnames(tab) <- paste0(
      rep(c('Pop', paste0('D',1:3)), each=2),
      rep(c('F', 'M'), 3))
  if(ppop) {
    tab <- as.data.frame(cbind(tab, tab[,3:8]))
    for(j in 1:2) {
      tab[, 8+j] <- 100*tab[,2+j]/tab[,j]
      tab[, 10+j] <- 100*tab[,4+j]/tab[,j]
      tab[, 12+j] <- 100*tab[,6+j]/tab[,j]
    }
    colnames(tab)[9:14] <- paste0(
        substr(colnames(tab)[3:8], 1, 2), '%',
        substring(colnames(tab)[3:8], 3))
  }
  for(j in 1:8)
    storage.mode(tab[,j]) <- 'integer'
  return(tab)
}

ui <- fluidPage(
  includeHTML("../GLOBAL/header.html"), 
  withMathJax(), 
  titlePanel(mainlabel),
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
        label = Vlabel,
        choices = vaclab,
        selected = vaclab,
        inline = TRUE),
      checkboxInput(
        inputId = "ppop",
        label = Plabel,
        value = FALSE),
      actionButton(
        inputId="exit", 
        label="Exit")
    ),
    mainPanel(
      plotOutput("pyramid"),
      plotOutput("time"),
      h4(Ttitle),
      tableOutput("vdose"),
      h4(T2title),
      tableOutput("adose")
    ) ## end mainPanel
  ) ## end sidebarLayout
) ## fluidPage

server <- function(input, output) { 

  runInside <- function() {
    observe({
      if(input$exit > 0) {
        stopApp(NULL)
      }
    })
  
    if (is.null(input$vaccine)) {
      if (pt) {
        stop(safeError(
          'Favor selecionar pelo menos uma vacina!'))
      } else {
        stop(safeError(
          'Please select at least one vaccine!'))
      }
    }
  }

  output$pyramid <- renderPlot({
    runInside()
    pyramid2plot(input$local, 
                 input$vaccine, 
                 input$ppop)
    
  })
    
  output$time <- renderPlot({
    runInside()
    timeplot(input$local, 
             input$vaccine, 
             input$ppop)
  })
  
  output$vdose <- renderTable({
    runInside()
    makeTable(input$local, input$vaccine, input$ppop)
  }, rownames=TRUE)
  
  output$adose <- renderTable({
    runInside()
    makeTable2(input$local, input$vaccine, input$ppop)
  }, rownames=TRUE)
  
}

shinyApp(ui, server)
