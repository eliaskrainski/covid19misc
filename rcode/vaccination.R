
if (FALSE)
    setwd('..')

if (FALSE)
    download.file(
        paste0('https://raw.githubusercontent.com/',
               'owid/covid-19-data/master/public/',
               'data/vaccinations/vaccinations.csv'),
        'data/vaccinations.csv')

system('ls -lh data/vaccinations.csv')
system('wc data/vaccinations.csv')

vd <- read.csv('data/vaccinations.csv')
dim(vd)

head(vd,1)
names(vd)

vd$date <- as.Date(as.character(vd$date))
vd$daily_vaccinations_per_hundred <-
    100*vd$daily_vaccinations_per_million/1e6

if(FALSE)
    with(vd[(vd$date<as.Date('2021-01-01')) & 
            (vd$location=='World'), ],
         plot(date, total_vaccinations))

if(FALSE)
    with(vd[(vd$location=='China'), ],
         plot(date, daily_vaccinations, main='China', pch=19))

if(FALSE)
    with(vd[(vd$location=='China'), ],
         plot(date, total_vaccinations, main='China', pch=19))

library(ggplot2)

if(FALSE)
    ggplot(vd[vd$date<as.Date('2021-01-01'), ]) +
        geom_point(aes(x=date, y=total_vaccinations,
                       group=location, col=location)) +
        scale_y_sqrt()

vd[vd$location=='United States' &
   (Sys.Date()-vd$date)<4, ]
vd[vd$location=='China' &
   (Sys.Date()-vd$date)<4, ]
vd[vd$location=='Denmark' &
   (Sys.Date()-vd$date)<4, ]
vd[vd$location=='Norway' &
   (Sys.Date()-vd$date)<4, ]
vd[vd$location=='Sweden' &
   (Sys.Date()-vd$date)<4, ]

pfully.c <- tapply(vd$people_fully_vaccinated_per_hundred,
                   vd$location, max, na.rm=TRUE)
tail(sort(pfully.c), 10)

vbr <- vd[vd$location=='Brazil', ]
vbr[(Sys.Date()-vbr$date)<9, c(3:8)]
100*220000/213e6

if (FALSE)
    with(vbr,
         plot(date, daily_vaccinations, pch=19,
              xlab='', type='o')) 
if (FALSE)
    with(vd[vd$location=='Chile',],
         plot(date, daily_vaccinations_per_hundred, pch=19,
              xlab='', type='o')) 
if (FALSE)
    with(vd[vd$location=='United States',],
         plot(date, daily_vaccinations_per_hundred, pch=19,
              xlab='', type='o')) 

if (FALSE)
    with(vd[vd$location=='Brazil',],
         plot(date, daily_vaccinations_per_hundred, pch=19,
              xlab='', type='o')) 

if (FALSE)
    with(vd[vd$location=='Brazil',],
         plot(date, people_fully_vaccinated_per_hundred, pch=19,
              xlab='', type='o', bty='n'))
abline(h=pretty(par()$usr[3:4]), lty=2, col=gray(0.5,0.5))

tabpp1 <- addmargins(outer(
    c(NVac=.4, Vac=.6),
    c(NInf=.4, Infe=.6)))*100
dimnames(tabpp1)[[1]][3] <- dimnames(tabpp1)[[2]][3] <- 'Tot'
tabpp1

n <- 1e6
prop.table(table(rbinom(n, 1, 0.6),
                 rbinom(n, 1, 0.6)))

xx <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1))
pp <- plogis(qlogis(0.6)+xx)

par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(1.5,0.5,0), las=1)
plot(pp, xlab='P fully vax', ylab='P infected', pch=19, bty='n')

bb <- apply(ppc, 2, function(p) rbinom(n, 1, p))
prop.table(table(bb[,1], bb[,2]))

mcor <- matrix(c(1, -0.9, -0.9, 1), 2)
mcor

xxc <- xx%*%chol(mcor)
cor(xxc)

plot(xxc)

ppc <- plogis(qlogis(0.6)+xxc)

par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(1.5,0.5,0), las=1)
plot(ppc, xlab='P fully vax', ylab='P infected', pch=19, bty='n')

bbc <- apply(ppc, 2, function(p) rbinom(n, 1, p))
ppc2tab <- addmargins(100*prop.table(table(bbc[,1], bbc[,2])))
dimnames(ppc2tab) <- dimnames(tabpp1)

round(ppc2tab, 2)

tcond <- tabpp1
tcond[1:2, 1] <- c(10, 30)
tcond[1:2, 2] <- c(30, 30)
tcond
tcond[2,1:2]/tcond[3,1:2]

gctr <- c('World', 'Asia', 'Upper middle income', 'High income',
          'North America', 'Europe', 'Lower middle income',
          'European Union', 'South America', 'Africa', 'Low income')
subctr <- c('England', 'Scotland', 'Wales') 

ntot <- tapply(vd$total_vaccinations, vd$location, max, na.rm=TRUE)
ontot <- sort(ntot, decreasing=TRUE)
head(ontot, 10)

ptvacc <- tapply(vd$total_vaccinations_per_hundred,
                 vd$location, max, na.rm=TRUE)
optvacc <- sort(ptvacc, decreasing=TRUE)
head(optvacc, 10)

summary(vd$date[vd$date>(Sys.Date()-7)])

dvacc <- tapply(vd$daily_vaccinations[vd$date>(Sys.Date()-7)],
                vd$location[vd$date>(Sys.Date()-7)], max, na.rm=TRUE)
odvacc <- sort(dvacc, decreasing=TRUE)
head(odvacc, 20)

pdvacc <- tapply(vd$total_vaccinations_per_hundred[vd$date>(Sys.Date()-7)],
                 vd$location[vd$date>(Sys.Date()-7)], max, na.rm=TRUE)
opdvacc <- sort(pdvacc, decreasing=TRUE)
head(opdvacc, 10)

lcsel <- list(n=head(setdiff(names(ontot), c(gctr, subctr)), 8),
              nd=head(setdiff(names(odvacc), c(gctr, subctr)), 8),
              p=head(setdiff(names(optvacc), c(gctr, subctr)), 8),
              pd=head(setdiff(names(opdvacc), c(gctr, subctr)), 8))
lcsel

###clsel <- c('blue4', 'red1', 'orange', 'blue1', 'magenta', 'green3')
ns <- 7
csel <- palette.colors(ns)
###csel <- c(blues9[-1]##rainbow(ns); csel <- rev(c(tail(csel,2), head(csel, length(csel)-2)))

plot(1:ns, pch=19, cex=2, col=csel)

xl <- list(x=pretty(vd$date, 10))

vplots <- c('total_vaccinations', 'daily_vaccinations',
            'total_vaccinations_per_hundred',
            'daily_vaccinations_per_hundred')

for(k in 1:4) {
    png(paste0('figures/selected_countries_',
               names(lcsel)[k], '.png'), 2100, 1800, res=300)
    ctsel <- lcsel[[k]]
    par(mfrow=c(2,2), mar=c(2,3,0,0), mgp=c(2,0.5,0))
    for (v in 1:4) {
        ds <- vd[vd$location %in% ctsel, c('location', 'date', vplots[v])]
        if(v<3) {
            ds$y <- sqrt(ds[,3])
            par(mar=c(0,3,0,0))
        } else {
            ds$y <- ds[,3]
            par(mar=c(2,3,0,0))
        }
        plot(ds[c('date', vplots[v])],
             ylim=range(ds$y, na.rm=TRUE), 
             type='n', xlab='', axes=FALSE)
        if(v>2) {
            axis(1, xl$x, format(xl$x, '%b,%d'))
            axis(2)
        } else {
            axis(2, pretty(par()$usr[3:4], 10),
                 pretty(par()$usr[3:4], 10)^2)
        }
        for(j in 1:length(ctsel))
            lines(ds$date[ds$location==ctsel[j]], 
                  ds$y[ds$location==ctsel[j]], 
                  pch=19, col=csel[j], lwd=2)
        abline(h=pretty(par()$usr[3:4], 7),
               v=pretty(par()$usr[1:2], 15),
               lty=2, col=gray(0.5,0.5))
        if(vplots[v]==vplots[k])
            legend('topleft', ctsel, col=csel,
                   lty=1, lwd=2, bg=gray(0.95),
                   title=vplots[v], cex=0.7)
    }
    dev.off()    
}

if(FALSE)
    system('eog figures/selected_countries_n.png &')

if(FALSE) {
    
    names(vd)

    ctsel <- c('Israel', 'United Arab Emirates', 'Chile', 'United Kingdom', 
             'United States', 'European Union', 'Saudi Arabia', 'Brazil', 'China')
    clsel <- c('blue4', 'green4', 'orange', 'red1', 
               'blue1', 'magenta', 'red4', 'green2', 'cyan')

    par(mfcol=c(2,2), mar=c(2,3,0,0), mgp=c(2,0.5,0))
    for (v in c('daily_vaccinations', 'daily_vaccinations_per_hundred',
                'total_vaccinations', 'total_vaccinations_per_hundred')) {
        gt <- length(grep('hundred', v))+1
        plot(vd[vd$location==ctsel[1], c('date', v)],
             ylim=c(c(50,0)[gt] + 
                    range(vd[vd$location%in%ctsel, v], na.rm=TRUE)),
             type='n', xlab='', log=c('y', '')[gt], axes=FALSE)
        axis(1, xl$x, format(xl$x, '%b,%d'))
        axis(2)
        for(j in 1:length(ctsel))
            lines(vd[vd$location==ctsel[j], c('date', v)],
                  pch=19, col=clsel[j], lwd=2)
        if(gt==1) {
            abline(h=10^pretty(par()$usr[3:4], 7),
                   v=pretty(par()$usr[1:2], 15),
                   lty=2, col=gray(0.5,0.5))
        } else {
            abline(h=pretty(par()$usr[3:4], 7),
               v=pretty(par()$usr[1:2], 15),
               lty=2, col=gray(0.5,0.5))
        }
    }
    legend('topleft', ctsel, col=clsel, lty=1, lwd=2, bg=gray(0.95))

}
    
if (FALSE)
    with(vd[vd$location=='United States',],
         plot(date, people_vaccinated, pch=19,
              xlab='')) 

if (FALSE)
    with(vbr, 
         plot(date, people_vaccinated_per_hundred, 
              xlab='', pch=19, type='o'))

if (FALSE)
    with(vd[vd$location=='Chile', ],
         plot(date, people_vaccinated_per_hundred, 
              xlab='', pch=19, type='o'))

if (FALSE) {

    csel1 <- 'Saudi Arabia' ##'United Arab Emirates'
    iloc <- (vd$location==csel1)
    vd[((Sys.Date()-vd$date)<5) & iloc, c(3:8)]
    iiloc <- which(iloc)

    iip <- which.max(vd$daily_vaccinations[iiloc])
    pop <- 1e4*vd$daily_vaccinations[iiloc[iip]]/
        vd$daily_vaccinations_per_million[iiloc[iip]]
    pop

    par(mfrow=c(1,2), mar=c(3,3,.5,2), mgp=c(1.5,0.5,0))
    with(vd[vd$location==csel1, ],
         plot(date, daily_vaccinations, 
              xlab='', pch=19, type='o'))
    axis(4, pretty(par()$usr[3:4]),
         format(pretty(par()$usr[3:4])/pop, dig=2))
    with(vd[vd$location==csel1, ],
         plot(date, total_vaccinations, 
              xlab='', pch=19, type='o'))
    axis(4, pretty(par()$usr[3:4]),
         format(pretty(par()$usr[3:4])/pop, dig=2))

}

if(FALSE){

    nmax.o <- max(vbr$total_vaccinations, na.rm=TRUE)
    vmax.o <- max(vbr$daily_vaccinations, na.rm=TRUE)
    dtarg <- as.Date('2021-10-01')
    (np <- as.integer(dtarg-Sys.Date())+1)
    nvtarg <- 1.2e6
    dv.pl <- vmax.o*1.05 + (nvtarg-vmax.o*1.05)*
        (seq(1, 100, length=trunc(np/2))^0.75)/(100^0.75)
    dv.pl
    dv.pl[(trunc(np/2)+1):np] <- nvtarg
    dv.pl
    
    summary(dv.pl)
    sum(dv.pl)
    v.p <- cumsum(dv.pl) + nmax.o

    xmin <- min(vbr$date, na.rm=TRUE)
    xmin
    xl <- list(x=pretty(c(xmin, dtarg)))
    xl$l <- format(pretty(c(xmin, dtarg)), '%b,%d')
    
    yl <- list(y=4e5*(0:8))
    yl$l <- c('0', paste(yl$y[yl$y<1e6][-1]/1e3, 'K'),
              paste(yl$y[yl$y>=1e6]/1e6, 'M'))
##               l=c(0, paste0(200*(1:4), 'mil'),
  ##                 paste0(c(1,1.2), 'milhão')))
    yl
    yl2 <- list(y=seq(0, 200, 20)*1e6)
    yl2$l <- c('0', paste(yl2$y[-1]/1e6, 'M'))
    yl2
    
    csel <- c('Israel', 'United Arab Emirates',
              'United Kingdom', 'United States',
              'Chile', 'European Union')
    psel <- c(9e6, 9.771e6, 6.665e7, 313e6, 18.95e6, 4.477e8)
    ccol <- c('magenta', 'green4', 'red4', 'blue', 'orange', 'cyan')
    
    png('figures/vaccina-dia-projecao.png', 900, 600)
    par(mfrow=c(2,2), mar=c(0,4.5,0,0), mgp=c(3.3,0.5,0), las=1)
    with(vbr, 
         plot(date, daily_vaccinations, axes=FALSE,
              xlim=c(xmin, dtarg), ylim=c(0, nvtarg*3), 
              xlab='', ylab='D O S E S    P O R    D I A', pch=19))
    abline(h=pretty(par()$usr[3:4], 10),
           lty=2, col=gray(.5,.5))
    axis(2, yl$y, yl$l, las=1)
    polygon(Sys.Date() + c(1:np, np:1, 1), 
            c(dv.pl*.9, rev(dv.pl)*1.1, dv.pl[1]*.9), 
            border='transparent', col=gray(.7,.5))
    lines(Sys.Date()+1:np, dv.pl, lwd=3, lty=2)
    legend('topleft', c('realizado', 'projeção'), cex=1.5,
           title='Brasil', ##' aplicadas de vacina anti-covid19',
           pch=c(19,0), pt.cex=c(2,0), 
           border='transparent',
           lwd=c(0,2), lty=2,
           fill=c('transparent', gray(.7,.5)), bty='n')
    polygon(Sys.Date() + c(1:np, np:1, 1), 
            c(dv.pl*.9, rev(dv.pl)*1.1, dv.pl[1]*.9), 
            border='transparent', col=gray(.7,.5))
    for (cc in 1:length(csel))  
        with(vd[vd$location==csel[cc], ],
             lines(date, daily_vaccinations,
                   col=ccol[cc], lwd=3))
###    text(dtarg-12, 1.20e6, 'Butantã+Fiocruz\n=1.2Milhião/dia', cex=1.5)
###    mtext('Doses(D)', 2, line=0.05, at=par()$usr[4]*.95)
    par(mar=c(0,4.5,0,4.5))
    with(vbr,
         plot(date, 100*daily_vaccinations/213e6, axes=FALSE,
              xlim=c(xmin, dtarg), ylim=c(0, 300*nvtarg)/2e8, 
              xlab='', ylab='', pch=19))
    abline(h=pretty(par()$usr[3:4], 10),
           lty=2, col=gray(.5,.5))
    polygon(Sys.Date() + c(1:np, np:1, 1), 
            c(dv.pl*.9, rev(dv.pl)*1.1, dv.pl[1]*.9)/213e4, 
            border='transparent', col=gray(.7,.5))
    lines(Sys.Date()+1:np, dv.pl/213e4, lty=2)
    a <- axis(2, las=1)
    axis(4, a, a/2, las=1) 
    for (cc in 1:length(csel))
        with(vd[vd$location==csel[cc], ],
             lines(date, 100*daily_vaccinations/psel[cc],
                   col=ccol[cc], lwd=3)) 
    legend('topright', csel, col=ccol, lty=1, bty='n', cex=1.3, lwd=3)
###    mtext('Doses/\nPop.', 2, line=0.05, at=par()$usr[4]*.95)
###    mtext('% Pop.\nVacinada\n(aprox.)', 4, line=0.05, at=par()$usr[4]*.925)
    mtext('Doses diárias / População (%)', 2, 2.3, las=3)
    mtext('% pessoas vacinadas por dia (aprox.)', 4, 2.3, las=3,
          at=0.7*mean(par()$usr[3:4]))
    par(mar=c(2,4.5,0,0))
    with(vbr, 
         plot(date, total_vaccinations,
              pch=19, axes=FALSE, 
              xlim=c(xmin, dtarg), ylim=c(0, 2e8), 
              ylab='T O T A L    D E    D O S E S    A P L I C A D A S'))
    abline(h=pretty(par()$usr[3:4], 10),
           lty=2, col=gray(.5,.5))
    polygon(Sys.Date() + c(1:np, np:1, 1),
            nmax.o+c(cumsum(dv.pl*.9), rev(cumsum(dv.pl*1.1)), dv.pl[1]*.91), 
            border='transparent', col=gray(.7,.5))
    lines(Sys.Date()+1:np, v.p, lty=2)
    for (cc in 1:length(csel))  
        with(vd[vd$location==csel[cc], ],
             lines(date, total_vaccinations,
                   col=ccol[cc], lwd=3))
    axis(1, xl$x, xl$l)
    axis(2, yl2$y, yl2$l, las=1)
    abline(h=pretty(par()$usr[3:4], 10),
           lty=2, col=gray(.5,.5))
    par(mar=c(2,4.5,0,4.5))
    with(vbr, 
         plot(date, 100*total_vaccinations/213e6, pch=19,
              xlim=c(xmin, dtarg),
              ylab='',
              ylim=c(0, 3*100*7e7/2e8), axes=FALSE))    
    abline(h=pretty(par()$usr[3:4], 10),
           lty=2, col=gray(.5,.5))
    polygon(Sys.Date() + c(1:np, np:1, 1),
            100*(nmax.o+c(cumsum(dv.pl*.9), rev(cumsum(dv.pl*1.1)), dv.pl[1]*.91))/213e6, 
            border='transparent', col=gray(.7,.5))
    lines(Sys.Date()+1:np, 100*v.p/213e6, lty=2)
    for (cc in 1:length(csel))  
        with(vd[vd$location==csel[cc], ],
             lines(date, 100*total_vaccinations/psel[cc],
                   col=ccol[cc], lwd=3))
    a <- axis(2, las=1)
    axis(4, a, a/2, las=1)
    axis(1, xl$x, xl$l)
    mtext('total vacinas aplicadas / população (%)', 2, 2.3, las=3)
    mtext('% total população vacinada (aprox.)', 4, 2.3, las=3)
    dev.off()

    if(FALSE)
        system('eog figures/vaccina-dia-projecao.png &')

}

plotxy2 <- function(x, y1, y2,
                    xlab, ylab1, ylab2,
                    c1=1, c2=2, xlim=range(x, na.rm=TRUE), 
                    xl=NULL, yl1=NULL, yl2=NULL,
                    add=FALSE, r1=NULL, r2=NULL,
                    ...) {
    if (add) {
        if(any(c(is.null(r1[1:2]), is.null(r2[1:2]))))
            stop("'r1[1:2]' and 'r2[1:2]' should be without NULL!")
    } else {
        if(any(is.null(r1[1:2]))) 
            r1 <- range(y1, na.rm=TRUE)
        if(any(is.null(r2[1:2])))
            r2 <- range(y2, na.rm=TRUE)
    }
    u1 <- (y1 - r1[1])/diff(r1)
    u2 <- (y2 - r2[1])/diff(r2)
    if(is.null(xl)) {
        xl <- list(x=pretty(xlim, 15))
        xl$l <- format(xl$x, '%b,%d')
    }
    if (is.null(yl1)) {
        yl1 <- list(y=pretty(r1, 15))
        yl1$l <- format(yl1$y)
    }
    if (is.null(yl2)) {
        yl2 <- list(y=pretty(r2, 15))
        yl2$l <- format(yl2$y)
    }
    if (add) {
        points(x, u1, col=c1, lty=2, ...)
    } else {
        plot(x, u1, col=c1, axes=FALSE,
             xlim=xlim, ylim=0:1,  
             xlab=xlab, ylab=ylab1, lty=2, ...)
    }
    points(x, u2, col=c2, ...)
    if (!add) { 
        axis(1, xl$x, xl$l)
        axis(2, (yl1$y-r1[1])/diff(r1), yl1$l)
        axis(4, (yl2$y-r2[1])/diff(r2), yl2$l)
        mtext(ylab2, 4, line=par()$mgp[1], las=3)
    }
    return(invisible())
}

dcc <- t(matrix(c('Israel', 'United Arab Emirates',
                  'United Kingdom', 'United States',
                  'European Union', 'Brazil'), nrow=2))
dcc

png('figures/vaccination3pairs.png', 700, 500)
par(mfrow=c(3,1), mar=c(2,4,.5,4), mgp=c(3,.5,0), las=1)
for (j in 1:nrow(dcc)) { 
    tmp <- vd[vd$location==dcc[j, 1], ]
    with(tmp, 
         plotxy2(date, daily_vaccinations_per_hundred,
                 total_vaccinations_per_hundred, 
                 xlab='', ylab1='Diaria', ylab2='Acumulada',
                 main='', xlim=range(vd$date),
                 pch=19, type='o'))
    v <- c(mean(tmp$daily_vaccinations_per_hundred, na.rm=TRUE), 
           max(tmp$total_vaccinations_per_hundred, na.rm=TRUE))
    legend('topleft',
           paste0(c('Diaria', 'Acumulada'), ': ',
                  format(v, digits=2), '%'),
           col=1:2, lty=1, lwd=1, pch=19, title=dcc[j,1])
    (r1a <- range(tmp$daily_vaccinations_per_hundred, na.rm=TRUE))
    (r2a <- range(tmp$total_vaccinations_per_hundred, na.rm=TRUE))
    tmp <- vd[vd$location==dcc[j, 2], ]
    with(tmp, 
         plotxy2(date, daily_vaccinations_per_hundred,
                 total_vaccinations_per_hundred, 
                 c1=3, c2=4, type='o', pch=19, lwd=3, 
                 add=TRUE, r1=r1a, r2=r2a))
    v <- c(mean(tmp$daily_vaccinations_per_hundred, na.rm=TRUE), 
           max(tmp$total_vaccinations_per_hundred, na.rm=TRUE))
    legend('left',
           paste0(c('Diaria', 'Acumulada'), ': ',
                  format(v, digits=2), '%'),
           col=3:4, lty=1, lwd=3, pch=19, title=dcc[j,2])
}
dev.off()
if(FALSE)
    system("eog figures/vaccination3pairs.png &")

100*(200e6/150)/213e6

tail(tmp, 10)

names(tmp)
jj <- c(3:5, 8, 10, 13)
names(vbr)[jj]

plot(vbr$people_vaccinated/vbr$total_vaccinations)

nvac <- max(vbr$total_vaccinations, na.rm=TRUE)
tvac <- 210e6
c(nvac, tvac-nvac, nvac)

nn <- c(nrow(vbr), 180)
c(nvac, tvac)/nn
(nvday <- tvac/nn[2])

pbr <- data.frame(date=Sys.Date()+c(0,180),
                  total_vaccinations=c(nvac+nvday, tvac), 
                  people_vaccinated=c((nvac+nvday)*0.8, tvac*0.55)) 
pbr$people_vaccinated_per_hundred <- 100*pbr$people_vaccinated/213e6
pbr$daily_vaccinations_per_hundred <- 100*c(nvday, nvday)/213e6

##tail(rbind(vbr[, jj], pbr))

par(mfrow=c(1,1), mar=c(2,5,1,5), mgp=c(4,0.5,0), las=1)
with(vbr, ##rbind(vbr[,jj], pbr),
     plotxy2(date, daily_vaccinations_per_hundred,
             people_vaccinated_per_hundred,
             xlim=range(vbr$date, pbr$date),
             r1=range(vbr$daily_vaccinations_per_hundred,
                      pbr$daily_vaccinations_per_hundred, na.rm=TRUE),
             r2=range(vbr$people_vaccinated_per_hundred,
                      pbr$people_vaccinated_per_hundred),
             xlab='', ylab1='Diaria', ylab2='Acumulada',
             pch=19, type='o', main='Brazil'))
legend('topleft',
       c('Diaria', 'Acumulada'), 
       col=1:2, lty=1, lwd=1, pch=19)

(r1b <- range(rbind(vbr[,jj], pbr)$daily_vaccinations_per_hundred, na.rm=TRUE))
yl1b <- list(n=pretty(r1b*213e6/100))
yl1b$y <- (pretty(r1b*213e6/100)-r1b[1]*213e6/100)/diff(r1b*213e6/100)
yl1b$l <- c(yl1b$n[1], paste0(yl1b$n[(yl1b$n>0) & (yl1b$n<1e6)]/1e3, 'K'),
            paste0(yl1b$n[yl1b$n>=1e6]/1e6, 'M'))
axis(2, yl1b$y, yl1b$l, line=1.5, lwd=0, las=1) 

(r2b <- range(rbind(vbr[,jj], pbr)$people_vaccinated_per_hundred, na.rm=TRUE))
yl2b <- list(n=pretty(r2b*213e6/100))
yl2b$y <- (pretty(r2b*213e6/100)-r1b[1]*213e6/100)/diff(r2b*213e6/100)
yl2b$l <- c(yl2b$n[1], paste0(yl2b$n[yl2b$n>=1e6]/1e6, 'M'))
axis(4, yl2b$y, yl2b$l, line=1.5, lwd=0, las=1) 

vvj <- c(tail(vbr$daily_vaccinations_per_hundred,1),
         round(tail(vbr$daily_vaccinations,1)/1e3))
text(max(vbr$date), (vvj[1]-r1b[1])/diff(r1b),
     paste0('Velocidade atual:\n',
            vvj[2], 'K dia (',
            round(100*vvj[2]*1e3/213e6, 2), '%)'))
text(as.Date('2021-03-15'), 1.0,
     'Velocidade alvo:\n~1.2M dia (0.56%)')

vd[(vd$location=='Saudi Arabia') &
   (Sys.Date()-vd$date)<9, c(3:8)]
vd[(vd$location=='Saudi Arabia') &
   (Sys.Date()-vd$date)<9, ]
if (FALSE)
    with(vd[vd$location=='Saudi Arabia', ],
         plot(date, daily_vaccinations, pch=19)) 

vars <- names(vd)[c(4, 8, 9, 13)]

t(apply(vd[vars], 2, summary))

ts0 <- lapply(vd[vars], tapply,
              vd[c('location', 'date')],
              mean)
sapply(ts0, dim)

tsmax <- sapply(ts0, apply, 1, function(x) {
    x <- tail(x, 14)
    x <- x[complete.cases(x)]
    if (length(x)>1)
        return(mean(x))
    return(NA)
})
dim(tsmax)
###tsmax[,4] <- rowMeans(ts0[[4]], na.rm=TRUE)

head(tsmax,2)

o1 <- order(tsmax[,3]*(tsmax[,1]>2e7), decreasing=TRUE)
which(names(tsmax[o1, 1])=='Brazil')

head(tsmax[o1,],20)

vd$location <- factor(
    vd$location,
    c('Brazil', setdiff(rownames(tsmax)[o1], 'Brazil')))

(nsel <- pmin(which(names(tsmax[o1, 1])=='Brazil'), 12))

library(ggplot2)

g0 <- ggplot(vd[vd$location %in%
                rownames(tsmax[head(o1, nsel),]), ]) 
##    scale_fill_discrete(breaks=rownames(tsmax[head(o1, nsel),]))
g1 <- g0 +
    geom_line(aes(x=date, total_vaccinations_per_hundred,
                  col=location))
g2 <- g0 +
    geom_line(aes(x=date, people_vaccinated_per_hundred,
                  col=location))
g3 <- g0 +
    geom_line(aes(x=date, people_fully_vaccinated_per_hundred,
                  col=location))
g4 <- g0 +
    geom_line(aes(x=date, daily_vaccinations_per_hundred,
                  col=location))

library(gridExtra)

png('figures/vaccination-time-countries.png', 1000, 700)
grid.arrange(g1, g2, g3, g4)
dev.off()
if (FALSE)
    system('eog figures/vaccination-time-countries.png &')

csel <- c('United Kingdom', 'Israel', 'United Arab Emirates', 
          'Brazil', 'Chile', 'Saudi Arabia', 'United States')
vdcsel <- vd[which(vd$location %in% csel), ]
vdcsel$País <- factor(vdcsel$location, csel)
table(vdcsel$País)
vdcsel$total_vaccinations_per_hundred[
           vdcsel$total_vaccinations_per_hundred>100] <- 100

ocsel <- names(rev(sort(tapply(
    vdcsel$total_vaccinations_per_hundred,
    vdcsel$location, max, na.rm=TRUE))))
ocsel

tapply(vdcsel$daily_vaccinations_per_hundred,
       vdcsel$País, range, na.rm=TRUE)

gs0 <- ggplot(vdcsel) 
gs1 <- gs0 +
    geom_point(aes(x=date, y=daily_vaccinations_per_hundred,
                   group=País, color=País)) + 
    xlab('') + ylab('VELOCIDADE\n% vacinados por dia') +
    scale_colour_hue(breaks=ocsel) 
gs2 <- gs0 + 
    geom_point(aes(x=date, y=total_vaccinations_per_hundred,
                   group=País, color=País)) + 
    xlab('') + ylab('ACUMULADO\n% com pelo menos 1 dose') +
    scale_colour_hue(breaks=ocsel) +
    ylim(c(0,100)) 

png('figures/vacinacaoBRtops.png', 700, 500)
grid.arrange(gs1, gs2)
dev.off()

if(FALSE)
    system('eog figures/vacinacaoBRtops.png &')

nsel <- 10
isel <- head(o1, nsel)
datel <- Sys.Date()-c(50,0)

if (nsel>12) {
    u2rgb <- function(u, alpha=1, a=0, b=1, p=1) {
        u <- u^p
        ur <- a + (b-a)*u
        ub <- 1-u
        ub <- a + (b-a)*ub
        ug <- 1-2*abs(u-0.5)
        ug <- a + (b-a)*ug
        rgb(ur, ug, ub)
    }
    n2rgb <- function(n, alpha=1, a=1, b=1, p=1) {
        u <- seq(0.5, n, 1)/n
        u2rgb(u, alpha, a, b, p)
    }    
    cols <- n2rgb(nsel, alpha=0.7, a=0, b=0.7)
} else {
    library(RColorBrewer)
    cols <- brewer.pal(nsel, 'Paired')
}

fac <- c(100, 100, 100, 1e6)
ylm <- c(1, 1, 1,
         max(ts0[[4]][head(isel,nsel),]/fac[4], na.rm=TRUE))
nams <- c('Doses / populacion',
          'People vaccinated / population',
          'Complete vaccined / population',
          'Daily doses / population')

png(paste0('figures/vaccinateds', nsel, '.png'),
    1000, 1000)
par(mfrow=c(2,2), mar=c(2,2,0,0), mgp=c(2,0.5,0), las=1)
for (k in 1:4) {
    plot(as.Date(colnames(ts0[[k]])), 
         ts0[[k]][isel[1], ]/(fac[k] * ylm[k]),
         type='n', 
         xlim=range(datel)+c(0, 15),
         ylim=c(0, 1),
         xlab='', ylab='',
         axes=FALSE)
    axis(1, pretty(datel, 10),
         format(pretty(datel, 10), '%b,%d'))
    axis(2, 0:10/10, format(10*0:10*ylm[k], digits=2), las=1)
    for (j in 1:nsel) {
        lines(as.Date(colnames(ts0[[k]])),
              ts0[[k]][isel[j], ]/(fac[k]*ylm[k]),
              type='o', pch=j,
              col=cols[j])
        if (is.na(tsmax[isel[j], k])) {
            text(rep(max(datel), 10),
                 1*(0.5+nsel-j)/nsel, 
                 paste0(rownames(ts0[[k]])[isel[j]], ' NA'),
                 adj=0, col=gray(.7, .7))
        } else {
            text(rep(max(datel), 10),
                 1*(0.5+nsel-j)/nsel, 
                 paste0(rownames(ts0[[k]])[isel[j]], ': ',
                        format(100*tsmax[isel[j], k]/fac[k], digits=2), '%'),
            adj=0, col=cols[j])
        }
    }
    text(mean(datel), .95, nams[k], cex=2)
    abline(h=1:10/10, lty=2, col=gray(.7, .7))
}
dev.off()
if (FALSE)
    system(paste0('eog figures/vaccinateds', nsel, '.png &'))
