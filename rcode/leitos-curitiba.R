
if (FALSE)
    setwd('..')

library(RCurl)

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/'
u1 <- 'https://mid.curitiba.pr.gov.br/dadosabertos/'

f.end <- '_Censo_Covid19_-_Base_de_Dados.csv'

Date0 <- as.Date('2020-07-02')
DateN <- Sys.Date()

if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/CensoCovid19')) dir.create('data/CensoCovid19')

lfl <- 'data/leitosCuritiba.csv'

addDateData <- function(date, create=FALSE, verbose=TRUE) {
    fl <- paste0('CensoCovid19/', date, f.end)
    loc.fl <- paste0('data/', fl)
    if(verbose) cat('file :', fl, '\n')
    if(!file.exists(loc.fl))
        if (url.exists(paste0(u0, fl))) {
            download.file(paste0(u0, fl), loc.fl, quiet=!verbose)
        } else {
            if(url.exists(paste0(u1, fl))) {
                download.file(paste0(u1, fl), loc.fl, quiet=!verbose)
            }
        }
    if(file.exists(loc.fl)) {
        skip <- substr(readLines(loc.fl,1), 1, 8)!='Hospital'
        tmp <- read.csv2(loc.fl, skip=skip+0, encoding='latin1')
        if(create) 
            cat(paste(c('Data', colnames(tmp)), collapse=';'),
                '\n', sep="", file=lfl)
        if(nrow(tmp)>0)
            for(k in 1:nrow(tmp)) {
                s <- sum(unlist(tmp[k, 4:7]), na.rm=TRUE)
                if(s>0)
                    cat(as.character(date), ";",
                        paste(tmp[k, ], collapse=';'), '\n', sep="", 
                        file=lfl, append=TRUE)
            }
    }
    return(invisible())
}

if(!file.exists(lfl)) {
    addDateData(Date0, create=TRUE)
    for(k in 1:difftime(DateN, Date0, units='days'))
        addDateData(Date0+k, create=FALSE, verbose=TRUE)
}

if(file.exists(lfl)) {
    leitos <- read.csv2(lfl)
    mdate <- max(as.Date(leitos$Data), na.rm=TRUE)
    dn <- as.integer(difftime(DateN, mdate, units='days'))
    cat(as.character(mdate), dn, '\n')
    if(dn>0) {
        for(k in 1:dn)
            addDateData(mdate+k, create=FALSE, verbose=TRUE)
    }
    leitos <- read.csv2(lfl)
}

str(leitos)

range(as.Date(leitos$Data), na.rm=TRUE)

xnams <- tail(colnames(leitos), 4)
fnams <- c('Perfil.da.vaga', 'Tipo.de.Leito') 

nl.t <- sapply(leitos[, 5:8], tapply, leitos[,'Data'], sum, na.rm=TRUE)
str(nl.t)
summary(nl.t)

tail(nl.t,10)

nuti.t <- sapply(leitos[which(leitos$Tipo=='UTI'), 5:8], tapply,
                 leitos[which(leitos$Tipo=='UTI'), 'Data'], sum, na.rm=TRUE)
summary(nuti.t)

tail(nuti.t,10)

ddates <- as.Date(rownames(nl.t))
i0 <- tail(1:length(ddates),Inf)

load('data/wdl.RData')

wdl[[1]][grep('Curitiba', wdl[[1]]$City), 1:7]
(iic <- which(wdl[[1]]$City=='Curitiba(SMB)'))

accMax <- function(x) {
    x.ori <- x
    i.ok <- which(!is.na(x))
    x <- x.ori[i.ok] 
    d <- diff(x)
    dn <- which(d<0)
    while(length(dn)>0) {
        for (j in dn) {
            a <- x[j] 
            x[j] <- x[j+1]
            x[j+1] <- a
        }
        d <- diff(x)
        dn <- which(d<0)
    }
    x.ori[i.ok] <- x
    return(x.ori) 
}

wcwb <- sapply(wdl, function(d)
    diff(accMax(c(0, unlist(d[iic, -(1:6)])))))

summary(wcwb)

wdlD <- as.Date(colnames(wdl[[1]])[-(1:6)], 'X%Y%m%d')
i1 <- which(wdlD>=ddates[i0[1]] &
            wdlD<=ddates[i0[length(i0)]])
str(i0)
str(i1)

range(ddates[i0])
range(wdlD[i1])

summary(wdlD)
range(ddates)

t0 <- seq(1, nrow(wcwb), length=round(nrow(wcwb)/14))
dim(bb <- splines:::bs(1:nrow(wcwb), knots=t0))
bb <- bb[, which(colSums(bb)>0)]
bb[,2] <- bb[,1] + bb[,2]
bb[, ncol(bb)-1] <- bb[, ncol(bb)-1] +bb[,ncol(bb)]
bb <- bb[, 2:(ncol(bb)-1)]

swcwb <- apply(wcwb, 2, function(y) {
    r <- y
    ii <- which(complete.cases(y))
    b <- glm.fit(bb[ii,], y[ii], family=poisson())$coef
    r[ii] <- exp(bb %*% b)
    return(r)
})
summary(swcwb)

xl <- list(x=pretty(c(ddates[i0], wdlD), 15))
xl$l <- format(xl$x, '%b/%y')
xl$l <- gsub('01', '1', xl$l)

png('figures/leitosCuritiba.png', 1500, 1500, res=150)
par(mfrow=c(3,1), mar=c(1,4,0.5,1), mgp=c(3,0.5,0))
plot(wdlD[i1], wcwb[i1, 1], las=1, pch=19, bty='n',##axes=FALSE,
     ylab='Numero de leitos', xlab='', cex=0.5)
lines(wdlD, swcwb[,1], col=1, lwd=2)
abline(v=pretty(par()$usr[1:2], 20),
       h=pretty(par()$usr[3:4], 20),
       lty=2, col=gray(.5,.5))
legend('topleft',
       c('Casos'), pch=19, lty=1,
       bg=gray(0.95))
legend('top', '', title='Curitiba',
       bg=gray(0.95), bty='n', cex=2)
plot(ddates[i0], nl.t[i0, 1], las=1, pch=19, 
     ylim=ylm, bty='n', ##axes=FALSE,
     ylab='Numero de leitos', xlab='', cex=0.5)
for (j in 2:3)
    points(ddates[i0], nl.t[i0,j], col=j, pch=19, cex=0.5)
points(ddates[i0], nuti.t[i0,2], col=6, pch=19, cex=0.5)
abline(v=pretty(par()$usr[1:2], 20),
       h=pretty(par()$usr[3:4], 20),
       lty=2, col=gray(.5,.5))
legend('topleft',
       c('Total', 'Ocupados', 'Livres', 'UTI'),
       pch=19, col=c(1:3,6), ncol=1,
       bg=gray(0.95), title='Leitos COVID')
text(rep(ddates[tail(i0,1)], 3)+14,
     c(nl.t[tail(i0,1), 1:2], nuti.t[tail(i0,1),2]),
     c(nl.t[tail(i0,1), 1:2], nuti.t[tail(i0,1),2]),
     col=c(1, 2, 6), srt=45, xpd=TRUE, cex=1.2)
par(mar=c(1.5,4,0.5,1))
plot(wdlD, wcwb[,2], col=2, cex=0.5, pch=19, 
     bty='n', las=1, xlab='', ylab='Número de óbitos')
lines(wdlD, swcwb[,2], lwd=2, col=2)
abline(v=pretty(par()$usr[1:2], 15),
       h=pretty(par()$usr[3:4], 15),
       lty=2, col=gray(.5,.5))
legend('topleft',
       c('Óbitos'), pch=19, lty=1, lwd=2, col=2,
       bg=gray(0.95))
dev.off()

if (FALSE)
    system("eog figures/leitosCuritiba.png &")

data.frame(utis=tail(nuti.t[,2]),
           total=tail(nl.t[,2]))

