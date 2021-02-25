
if (FALSE)
    setwd('..')

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/'

nt <- as.integer(difftime(Sys.Date(),
                          as.Date('2020-07-02'),
                          units='days'))
nt

ddates <- as.Date('2020-07-02')+0:nt
tail(ddates)

f.end <- '_Censo_Covid19_-_Base_de_Dados.csv'

library(RCurl)

ldl0 <- lapply(ddates, function(d) {
    fl <- paste0('CensoCovid19/', d, f.end)
    if (!file.exists(fl))
        if (url.exists(paste0(u0, fl)))
            download.file(paste0(u0, fl), fl)
    if (file.exists(fl)) {
        return(read.csv2(fl, skip=1))
    } else {
        return(NULL)
    }
})

table(sapply(ldl0, class))

i0 <- which(!sapply(ldl0, is.null))[-1]
if (length(i0)>0) {
    ldl <- ldl0[i0]
} else {
    ldl <- ldl0
}

table(sapply(ldl, ncol))
table(sapply(ldl, nrow))

xnams <- tail(colnames(ldl[[1]]), 4)
fnams <- c('Perfil.da.vaga', 'Tipo.de.Leito') 

sapply(ldl[[1]][xnams], tapply,
       ldl[[1]][fnams], sum)


sapply(ldl[[1]][xnams[2]], tapply,
       ldl[[1]][fnams], sum)


nl.t <- sapply(ldl, function(d) {
    if (nrow(d)>0)
        return(colSums(d[, 4:7], na.rm=TRUE))
    return(rep(NA, 4))
})

tail(ddates)
tail(ddates[i0])

png('figures/leitosCuritiba.png', 500, 500)
par(mfrow=c(1,1), mar=c(2, 3, 0.5, 0.5), mgp=c(2,0.5,0))
plot(ddates[i0], nl.t[1, ], las=1, pch=19, 
     ylim=range(nl.t, na.rm=TRUE), ##axes=FALSE,
     ylab='Numero de leitos', xlab='')
for (j in 2:3)
    points(ddates[i0], nl.t[j,], col=j, pch=19)
legend('left', c('Total', 'Ocupados', 'Livres'),
       lty=1, col=1:4, ncol=1,
       bty='n', bg=gray(.9))
##axis(1, 
  ##   format(pretty(ddates,10), '%m,%d'),
    ## lty=2, col=gray(.5,.5))
abline(v=pretty(ddates,10),
       h=50*(0:20), lty=2, col=gray(.5,.5))
dev.off()
if (FALSE)
    system("eog figures/leitosCuritiba.png &")
