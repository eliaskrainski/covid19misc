
if (FALSE)
    setwd('..')

library(RCurl)

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/'
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
        if (url.exists(paste0(u0, fl)))
            download.file(paste0(u0, fl), loc.fl, quiet=!verbose)
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
i0 <- 1:length(ddates)

xl <- list(x=pretty(ddates[i0], 15))
xl$l <- format(xl$x, '%b%Y')
xl$l <- gsub('01', '1', xl$l)

png('figures/leitosCuritiba.png', 1500, 1500, res=150)
par(mfrow=c(1,1), mar=c(4, 3, 0.5, 0.5), mgp=c(2,0.5,0))
plot(ddates[i0], nl.t[, 1], las=1, pch=19, 
     ylim=range(nl.t, na.rm=TRUE), axes=FALSE,
     ylab='Numero de leitos', xlab='', cex=0.5)
axis(1, xl$x, xl$l, las=3)
axis(2, pretty(c(0, max(nl.t, na.rm=TRUE)), 10), las=1)
for (j in 2:3)
    points(ddates[i0], nl.t[,j], col=j, pch=19, cex=0.5)
points(ddates[i0], nuti.t[,2], col=6, pch=19, cex=0.5)
abline(v=pretty(ddates,10),
       h=100*(0:15), lty=2, col=gray(.5,.5))
legend('topleft', c('Total', 'Ocupados', 'Livres', 'UTI'),
       pch=19, col=c(1:3,6), ncol=1,
       bg=gray(0.95), title='Leitos Curitiba')
dev.off()

if (FALSE)
    system("eog figures/leitosCuritiba.png &")
