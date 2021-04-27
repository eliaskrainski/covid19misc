if(FALSE)
    setwd('..')

source('rcode/ocommon.R')

library(readr)

ww <- fwf_positions(c(6, 50, 83, 92),
                    c(7, 65, 83, 94),
                    c('uf', 'w', 'sexo', 'idade'))
colcl <- do.call('cols', list('i', 'd', 'i', 'i'))

if(FALSE) {

    fls4 <- paste0('~/dados/pnadc/PNADC_0', 1:4, '2020.txt')
    names(fls4) <- paste0('Trim', 1:4)

    ld <- lapply(fls4,
                 read_fwf, ww, colcl)

    round(t(sapply(ld, function(x)
        c(sum(x$w),
          tapply(x$w, cut(x$idade, c(0,14,18,25,40, 60,Inf), right=FALSE), sum))/1e3)))
    
}

fld <- '~/dados/pnadc/PNADC_042020.txt'
dat <- read_fwf(fld, ww, colcl)

round(tapply(dat$w, dat$sexo, sum)/1e6, 2)

b.i <- c(0:115,Inf)
m.i <- 0:115

b5i <- 5*c(0:18,Inf)
m5i <- 5*0:18+2.5
l5i <- paste0('[', b5i[1:19],
              rep(c('-',''), c(18,1)),
              c(b5i[2:19], '+'), ')')
l5i

table(cut(dat$idade, b5i, right=FALSE))
tapply(dat$w, cut(dat$idade, b5i, right=FALSE), sum)

nFHi.o <- tapply(
    dat$w,
    list(Idade=factor(m.i[findInterval(dat$idade, b.i)], m.i),
         Sexo=factor(dat$sexo, 2:1, c('Fem', 'Masc'))), sum)
tail(nFHi.o,3)

dimnames(nFHi.o)[[1]][dim(nFHi.o)[1]] <-
    paste0(tail(dimnames(nFHi.o)[[1]], 1), '+')

source('rcode/functions.R')
bbb <- bs3f(m.i, seq(m.i[1], tail(m.i,1), 5))

nFHi <- apply(nFHi.o, 2, function(y) {
    y[is.na(y)] <- 0
    y <- round(y)
    ff <- glm.fit(bbb, y, family=poisson())
    y1 <- exp(bbb %*% ff$coeff)
    round(y1*sum(y)/sum(y1))
})

c(sum(nFHi.o, na.rm=TRUE), sum(nFHi))/1e3
tail(nFHi,3)


if(FALSE) {
    par(mar=c(3,0,0,0), mgp=c(2,1,0))
    barplot(-nFHi[,1], col=2, xlim=c(-1,1)*max(nFHi), space=0, 
            names.arg='', border='transparent', horiz=TRUE)
    barplot(nFHi[,2], col=4, add=TRUE, space=0, 
            names.arg='', border='transparent', horiz=TRUE)
}    

if(FALSE)
    write.csv(round(nFHi),
              file='data/estPNADCpopBR202004IdadeSexo.csv')


### faixas etarias por UFs
table(m5i[i <- findInterval(dat$idade, b5i)])
t5cl <- tapply(dat$w, list(i=l5i[i], sexo=dat$sexo, uf=dat$uf), sum)

t5cl[1:2, , 1:2]

apply(t5cl, 2, sum)

flPis <- 'data/estimativaPopulacaoUF202004SexoFaixa5a.csv'

if(FALSE) {

    cat('FaixaEtaria;UF;Fem;Masc\n', file=flPis)
    for(u in 1:dim(t5cl)[3])
        write.table(data.frame(UF=dimnames(t5cl)[[3]][u],
                               round(t5cl[,2:1,u])),
                    sep=';', file=flPis, append=TRUE,
                    row.names=TRUE, col.names=FALSE)

}

cF <- rgb(1.0,c(0.3,0),c(0.1,0),0.5)
cM <- rgb(c(0.1,0),c(0.5,0.1),1.0,0.5)

uf[1,]
ufi <- pmatch(dimnames(t5cl)[[3]], rownames(uf))
ufi

longd <- read.csv2(flPis)
head(longd)

source('rcode/functions.R')

bbi <- bs3f(1:dim(t5cl)[1], seq(1, dim(t5cl)[1], 3))
str(bbi)

spop <- lapply(1:27, function(u) {
    y1 <- round(t5cl[,1,u])
    f1 <- glm.fit(bbi, y1, family=poisson())
    y2 <- round(t5cl[,2,u])
    f2 <- glm.fit(bbi, y2, family=poisson())
    p1 <- exp(drop(bbi %*% f1$coeff))
    p2 <- exp(drop(bbi %*% f2$coeff))
    cbind(p1 * sum(y1)/sum(p1),
          p2 * sum(y2)/sum(p2))
})

sum(sapply(spop, sum))
sum(t5cl)


t5clbr <- aggregate(
    data.frame(nFHi.o),
    list(i5=findInterval(1:nrow(nFHi.o)-0.5, b5i)), sum, na.rm=TRUE)
str(t5clbr)

str(spopbr <- sapply(t5clbr[,2:3], function(y) {
    y[is.na(y)] <- 0
    y <- round(y)
    ff <- glm.fit(bbi, y, family=poisson())
    y1 <- exp(bbi %*% ff$coeff)
    round(y1*sum(y)/sum(y1))
}))

png('figures/piramidesUF202004.png', 1000, 700)
par(mfrow=c(4,7), mar=c(0,0,0,0), xaxs='i', yaxs='i')
for(u in 1:27) {
    xr <- c(-1,1)*max(t5cl[,,u])
    barplot(-t5cl[,1,u], names.arg='', xlim=xr,
            horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1])
    barplot(t5cl[,2,u], add=TRUE, names.arg='',
            horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cM[1])
    lines(-spop[[u]][,1], 1:19-0.5, lwd=2)
    lines(spop[[u]][,2], 1:19-0.5, lwd=2)
    text(rep(0,8), seq(2, 16, 2), paste(10*(1:8)))
    legend('topleft', uf$State[ufi[u]], bty='n')
}
xr <- c(-1,1)*max(t5clbr)
barplot(-t5clbr[,2], names.arg='', xlim=xr,
        horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cF[1])
barplot(t5clbr[,3], add=TRUE, names.arg='',
        horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cM[1])
lines(-spopbr[,1], 1:19-0.5, lwd=2)
lines(spopbr[,2], 1:19-0.5, lwd=2)
text(rep(0,8), seq(2, 16, 2), paste(10*(1:8)))
legend('topleft', 'Brasil', bty='n')
dev.off()

if(FALSE)
    system('eog figures/piramidesUF202004.png &')


