if(FALSE)
    setwd('..')

source('rcode/ocommon.R')

library(readr)

ww <- fwf_positions(c(6, 8, 10, 50, 83, 92),
                    c(7, 9, 11, 65, 83, 94),
                    c('uf', 'cap', 'rg', 'w', 'sexo', 'idade'))
colcl <- do.call('cols', list('i', 'i', 'i', 'd', 'i', 'i'))

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

source('rcode/define_idade_faixas.R')

table(cut(dat$idade, bKi, right=FALSE))
tapply(dat$w, cut(dat$idade, bKi, right=FALSE), sum)

nFHi.o <- tapply(
    dat$w,
    list(Idade=factor(lk1i[findInterval(dat$idade, b1i)], lk1i),
         Sexo=factor(dat$sexo, 2:1, c('Fem', 'Masc'))), sum)
tail(nFHi.o,3)

source('rcode/functions.R')
bbb <- bs3f(m1i, seq(m1i[1], tail(m1i,1), 3*k1i))

nFHi <- apply(nFHi.o, 2, function(y) {
    y[is.na(y)] <- 0
    y <- round(y)
    ff <- glm.fit(bbb, y, family=poisson())
    y1 <- exp(bbb %*% ff$coeff)
    round(y1*sum(y)/sum(y1))
})
rownames(nFHi) <- rownames(nFHi.o)

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
    write.csv(data.frame(Faixa=rownames(nFHi), round(nFHi)),
              file='data/estPNADCpopBR202004IdadeSexo2.csv',
              row.names=FALSE)


### faixas etarias por UFs
table(mKi[i <- findInterval(dat$idade, bKi)])
tKcl <- tapply(dat$w, list(i=mKi[i], sexo=dat$sexo, uf=dat$uf), sum)

dimnames(tKcl)

n12c <- round(apply(tKcl, 3, function(m) colSums(m[1:3, ], na.rm=TRUE)))
n12c
round(100*n12c[1,]/n12c[2,]) ### indice de masculinidade ate 15 anos, cada UF
n12o <- round(apply(tKcl, 3, function(m) colSums(m[-2:0+nrow(m), ], na.rm=TRUE)))
n12o
round(100*n12o[1,]/n12o[2,]) ### indice de masculinidade apos 90 anos, cada UF

apply(tKcl, 2, sum, na.rm=TRUE)

uf[1,]
ufi <- pmatch(dimnames(tKcl)[[3]], rownames(uf))
ufi

bbi <- bs3f(1:dim(tKcl)[1], seq(1, dim(tKcl)[1], 3))
str(bbi)

apply(tKcl,2,sum,na.rm=T)

spop <- lapply(1:27, function(u) {
    y1 <- round(tKcl[,1,u])
    y1[is.na(y1)] <- 0L 
    f1 <- glm.fit(bbi, y1, family=poisson())
    y2 <- round(tKcl[,2,u])
    y2[is.na(y2)] <- 0L
    f2 <- glm.fit(bbi, y2, family=poisson())
    p1 <- exp(drop(bbi %*% f1$coeff))
    p2 <- exp(drop(bbi %*% f2$coeff))
    cbind(M=p1 * sum(y1)/sum(p1),
          F=p2 * sum(y2)/sum(p2))
})
names(spop) <- dimnames(tKcl)[[3]][1:length(spop)]

n12c.s <- round(sapply(spop, function(x) colSums(x[1:3,])))
round(100*n12c.s[1,]/n12c.s[2,]) ### indice de masculinidade ate 15 anos, cada UF
n12o.s <- round(sapply(spop, function(x) colSums(x[-2:0+nrow(x),])))
round(100*n12o.s[1,]/n12o.s[2,]) ### indice de masculinidade apos 90 anos, cada UF

if(FALSE) {
    
    barplot(100*rbind(n12c[1,]/n12c[2,],
                      n12c.s[1,]/n12c.s[2,],
                      n12o[1,]/n12o[2,],
                      n12o.s[1,]/n12o.s[2,]), beside=TRUE)
    abline(h=100, col=2)

}
    
sapply(spop,colSums)

c(n0=sum(sapply(spop, sum)),
  nS=sum(tKcl, na.rm=TRUE))

mKi
round(sapply(spop, function(x) c(sum(x[1:2,]), sum(x[-1:0+nrow(x),])))/1e3)

cbind(o=round(tKcl[,,1]), s=round(spop[[1]]))

flPis <- paste0('data/estimativaPopulacaoUF202004SexoFaixa', K, 'a.csv')

if(FALSE) {
    
    cat('FaixaEtaria;UF;Fem;Masc\n', file=flPis)
    for(u in 1:27)
        write.table(data.frame(
            Faixa=lKi,
            UF=dimnames(tKcl)[[3]][u],
            round(spop[[u]][,2:1])),
            sep=';', file=flPis, append=TRUE,
            row.names=FALSE, col.names=FALSE)

}

longd <- read.csv2(flPis)
head(longd,3)
tail(longd,3)
sapply(longd[,3:4], sum)

tKclbr <- data.frame(tapply(dat$w, list(i=mKi[i], sexo=dat$sexo), sum))
str(tKclbr)

str(spopbr <- sapply(tKclbr, function(y) {
    y[is.na(y)] <- 0
    y <- round(y)
    ff <- glm.fit(bbi, y, family=poisson())
    y1 <- exp(bbi %*% ff$coeff)
    round(y1*sum(y)/sum(y1))
}))

cF <- rgb(1.0,c(0.3,0),c(0.1,0),0.5)
cM <- rgb(c(0.1,0),c(0.5,0.1),1.0,0.5)

if(FALSE) {

    jp <- c(-2,-4,-6,-8); names(jp) <- paste0(9:6, '0+')
    sapply(jp, function(n) colSums(spopbr[n:0+nrow(spopbr),]))/colSums(spopbr)

    p.olds <- lapply(jp, function(n) 
        sapply(spop, function(x) colSums(x[n:0+nrow(x),])/colSums(x)))
    
    par(mfrow=c(2,2), mar=c(3,3,0,0), mgp=c(1.5,0.5,0), las=1)
    for(j in 1:4) {
        barplot(p.olds[[j]][2:1,], beside=TRUE, col=c(cF[2], cM[2]))
        legend('topleft', c("Fem", "Masc"),
               fill=c(cF[2], cM[2]), title=names(p.olds)[j])
    }    

}

if(FALSE) {

    flK <- paste0('figures/piramidesUF202004', K, 'a.png')
    png(flK, 1000, 700)
    par(mfrow=c(4,7), mar=c(0,0,0,0), xaxs='i', yaxs='i')
    for(u in 1:27) {
        xr <- c(-1,1)*max(tKcl[,,u], na.rm=TRUE)
        barplot(-tKcl[,1,u], names.arg='', xlim=xr,
                horiz=TRUE, space=0, axes=FALSE,
                border='transparent', col=cF[1])
        barplot(tKcl[,2,u], add=TRUE, names.arg='',
                horiz=TRUE, space=0, axes=FALSE,
                border='transparent', col=cM[1])
        lines(-spop[[u]][,1], 1:nKi-0.5, lwd=2, col=gray(0.3,0.5))
        lines(spop[[u]][,2], 1:nKi-0.5, lwd=2, col=gray(0.3,0.5))
        text(par()$usr[2]*0.95, seq(4, nKi-3, 4), paste(K*seq(4, nKi-3, 4)))
        legend('topleft', uf$State[ufi[u]], bty='n')
    }
    xr <- c(-1,1)*max(tKclbr)
    barplot(-tKclbr[,1], names.arg='', xlim=xr,
            horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1])
    barplot(tKclbr[,2], add=TRUE, names.arg='',
            horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cM[1])
    lines(-spopbr[,1], 1:nKi-0.5, lwd=2, col=gray(0.3,0.5))
    lines(spopbr[,2], 1:nKi-0.5, lwd=2, col=gray(0.3,0.5))
    text(par()$usr[2]*0.95, seq(2, 8, 1)*2, paste(10*(2:8)))
    legend('topleft', 'Brasil', bty='n')
    dev.off()
    
    if(FALSE)
        system(paste('eog', flK, '&'))

}



