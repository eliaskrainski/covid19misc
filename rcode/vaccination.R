
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

vd$location <- factor(
    vd$location,
    rownames(tsmax)[o1])

nsel <- 15

vd$daily_vaccinations_per_hundred <-
    100*vd$daily_vaccinations_per_million/1e6

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

png('figures/vaccination-time-countries.png', 700, 700)
grid.arrange(g1, g2, g3, g4)
dev.off()
if (FALSE)
    system('eog figures/vaccination-time-countries.png &')

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
ylm <- c(1, 1, 1, max(ts0[[4]]/fac[4], na.rm=TRUE))
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
         format(pretty(datel, 10), '%d, %b'))
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
