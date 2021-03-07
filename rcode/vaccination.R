
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

vd[(vd$location=='Brazil') &
   (Sys.Date()-vd$date)<3, c(3:8)]
100*220000/213e6
if (FALSE)
    with(vd[vd$location=='Brazil', ],
         plot(date, daily_vaccinations, pch=19,
              xlab='')) 
if (FALSE)
    with(vd[vd$location=='Brazil', ],
         plot(date, people_vaccinated_per_hundred, 
              xlab='', pch=19, type='o'))
if (FALSE)
    with(vd[vd$location=='Chile', ],
         plot(date, people_vaccinated_per_hundred, 
              xlab='', pch=19, type='o'))

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
                  format(v, digits=1), '%'),
           col=1:2, lty=1, lwd=1, pch=19, title=dcc[j,1])
    (r1a <- range(tmp$daily_vaccinations_per_hundred, na.rm=TRUE))
    (r2a <- range(tmp$total_vaccinations_per_hundred, na.rm=TRUE))
    tmp <- vd[vd$location==dcc[j, 2], ]
    with(tmp, 
         plotxy2(date, daily_vaccinations_per_hundred,
                 total_vaccinations_per_hundred, 
                 c1=3, c2=4, type='o', pch=19, lwd=2, 
                 add=TRUE, r1=r1a, r2=r2a))
    v <- c(mean(tmp$daily_vaccinations_per_hundred, na.rm=TRUE), 
           max(tmp$total_vaccinations_per_hundred, na.rm=TRUE))
    legend('left',
           paste0(c('Diaria', 'Acumulada'), ': ',
                  format(v, digits=1), '%'),
           col=3:4, lty=1, lwd=2, pch=19, title=dcc[j,2])
}
dev.off()
if(FALSE)
    system("eog figures/vaccination3pairs.png &")

100*(200e6/150)/213e6

tail(tmp, 10)

names(tmp)
vbr <- vd[vd$location=='Brazil', ]
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

tail(rbind(vbr[, jj], pbr))

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

vars <- names(vd)[9:12]

t(apply(vd[vars], 2, summary))

ts0 <- lapply(vd[vars], tapply,
              vd[c('location', 'date')],
              mean)
sapply(ts0, dim)

tsmax <- sapply(ts0, apply, 1, function(x) {
    x <- tail(x, 10)
    x <- x[complete.cases(x)]
    if (length(x)>1)
        return(max(x))
    return(NA)
})
tsmax[,4] <- rowMeans(ts0[[4]], na.rm=TRUE)

o1 <- order(tsmax[,1], decreasing=TRUE)
which(names(tsmax[o1, 1])=='Brazil')

vd$location <- factor(
    vd$location,
    rownames(tsmax)[o1])

(nsel <- pmin(which(names(tsmax[o1, 1])=='Brazil'), 60))

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

csel <- c('Israel', 'United Arab Emirates', 'United Kingdom', 
          'United States', 'Brazil', 'Chile', 'Saudi Arabia')
table(as.character(vd[vd$location%in%csel, ]$location))

ggplot(vd[vd$location%in%csel, ]) + 
    geom_point(aes(x=date, y=total_vaccinations_per_hundred,
                   group=location, color=location)) +
    scale_y_sqrt()

nsel <- 20
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
