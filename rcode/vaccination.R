
if (FALSE)
    setwd('..')

if (FALSE)
    download.file(
        paste0('https://raw.githubusercontent.com/',
               'owid/covid-19-data/master/public/',
               'data/vaccinations/vaccinations.csv'),
        'data/vaccinations.csv')

vd <- read.csv('data/vaccinations.csv')
dim(vd)

vd[1,]

vd$date <- as.Date(as.character(vd$date))

vts <- with(vd, 
            tapply(total_vaccinations_per_hundred,
                   list(location, date), mean, na.rm=TRUE))
dts <- with(vd, 
            tapply(people_vaccinated_per_hundred,
                   list(location, date), mean, na.rm=TRUE))

dim(vts)

maxv <- apply(vts, 1, max, na.rm=TRUE)
maxd <- apply(dts, 1, max, na.rm=TRUE)
summary(maxv)
summary(maxd)

ov <- order(maxv, decreasing=TRUE)
od <- order(maxd, decreasing=TRUE)

nsel <- 30
iv.sel <- head(ov, nsel)
id.sel <- head(od, nsel)
datel <- Sys.Date()-c(50,0)

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

png('figures/vaccinateds.png', 700, 700)
par(mfrow=c(2,1), mar=c(2,2,0,0), mgp=c(2,0.5,0), las=1)
plot(as.Date(colnames(vts)),
     sqrt(vts[iv.sel[1], ]/100),
     type='n', pch=8, 
     xlim=range(datel)+c(0, 15),
     ylim=0:1,
     xlab='', ylab='',
     axes=FALSE)
axis(1, pretty(datel, 10),
     format(pretty(datel, 10), '%d, %b'))
axis(2, 0:10/10, 10*0:10, las=1)
for (j in 1:nsel) {
    lines(as.Date(colnames(vts)),
          sqrt(vts[iv.sel[j], ]/100),
          type='o', pch=8,
          col=cols[j])
    text(rep(max(datel), 10),
         (0.5+nsel-j)/nsel, 
         paste0(rownames(vts)[iv.sel[j]], ': ',
                maxv[iv.sel[j]], '%'),
         adj=0, col=cols[j])
}
text(mean(datel), .95, 'Doses / population', cex=2)
abline(h=1:10/10, lty=2, col=gray(.7, .7))
plot(as.Date(colnames(vts)),
     sqrt(dts[id.sel[1], ]/100),
     type='n', pch=8, 
     xlim=range(datel)+c(0, 15),
     ylim=0:1,
     xlab='', ylab='',
     axes=FALSE)
axis(1, pretty(datel, 10),
     format(pretty(datel, 10), '%d, %b'))
axis(2, 0:10/10, 10*0:10, las=1)
for (j in 1:nsel) {
    lines(as.Date(colnames(dts)),
          sqrt(dts[id.sel[j], ]/100),
          type='o', pch=8,
          col=cols[j])
    text(rep(max(datel), 10),
         (0.5 + nsel -j)/nsel, 
         paste0(rownames(dts)[id.sel[j]], ': ',
                maxd[id.sel[j]], '%'),
         adj=0, col=cols[j])
}
abline(h=1:10/10, lty=2, col=gray(.7, .7))
text(mean(datel), .95, 'People vaccinated / population', cex=2)
dev.off()
if (FALSE)
    system('eog figures/vaccinateds.png &')
