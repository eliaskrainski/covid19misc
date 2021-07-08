if(FALSE)
    setwd('..')

load('data/tiKdsu.RData')
ls()

dim(t.iKdsu)
dimnames(t.iKdsu)

apply(t.iKdsu, 4, sum)

### consider the population data
p21i <- read.csv('data/estPNADCpopBR202101IdadeSexo2.csv')
tail(p21i)

source('rcode/define_idade_faixas.R')
ls()

dlevels <- c('    1ª Dose', '    2ª Dose')
slevels <- c('F', 'M')

ftab1 <- function(dv) {
        table(factor(findInterval(dv[,1], b1i),
                     0:51, m1i), 
              dv[,2], dv[,5])
    }
    ftab2 <- function(dv) 
        table(dv[, 3], dv[, 4], dv[,5])
        return(r)

rfls <- dir('RData/')
rfls

robjs <- substr(rfls, 1, 5)
head(robjs)

ufs <- substr(robjs, 4, 5)
ufs

for (k in 1:27) {
    obj <- robjs[k]
    load(paste0('RData/', obj, '.RData'))
    tmp <- get(obj)
    tmp <- tmp[(tmp[,2] %in% slevels) &
                   (tmp[,5] %in% dlevels), ]
        dim(tmp)

    }

### create array with
### age (1st dim),
### gender (2nd dim) and
### which data: pop, dose 1 and dose 2 (3nd dim)
nv24 <- array(NA, c(nrow(p21i),2,4))
dimnames(nv24) <- 
    list(Age=p21i[,1],
         Gender=colnames(p21i)[2:3],
         Data=c('Pop', 'D1', 'D2', 'D2t'))

nv24[,,1] <- as.matrix(p21i[,2:3])

n.Ids$iClass <- pmatch(
    n.Ids$idade1, p21i[,1],
    duplicates.ok=TRUE)
n.Ids <- n.Ids[!is.na(n.Ids$iClass)]

n.Ids2$iClass <- pmatch(
    n.Ids2$idade1, p21i[,1],
    duplicates.ok=TRUE)
n.Ids2 <- n.Ids2[!is.na(n.Ids2$iClass)]


nv24[n.Ids[vacina_descricao_dose==dlevels[1] &
           paciente_enumsexobiologico=='F']$iClass,1,2] <-
    n.Ids[vacina_descricao_dose==dlevels[1] &
          paciente_enumsexobiologico=='F']$N
nv24[n.Ids[vacina_descricao_dose==dlevels[1] &
           paciente_enumsexobiologico=='M']$iClass,2,2] <-
    n.Ids[vacina_descricao_dose==dlevels[1] &
          paciente_enumsexobiologico=='M']$N

nv24[n.Ids[vacina_descricao_dose==dlevels[2] &
           paciente_enumsexobiologico=='F']$iClass,1,3] <-
    n.Ids[vacina_descricao_dose==dlevels[2] &
          paciente_enumsexobiologico=='F']$N
nv24[n.Ids[vacina_descricao_dose==dlevels[2] &
           paciente_enumsexobiologico=='M']$iClass,2,3] <-
    n.Ids[vacina_descricao_dose==dlevels[2] &
          paciente_enumsexobiologico=='M']$N

nv24[n.Ids2[vacina_descricao_dose==dlevels[2] &
            paciente_enumsexobiologico=='F']$iClass,1,4] <-
    n.Ids2[vacina_descricao_dose==dlevels[2] &
           paciente_enumsexobiologico=='F']$N
nv24[n.Ids2[vacina_descricao_dose==dlevels[2] &
            paciente_enumsexobiologico=='M']$iClass,2,4] <-
    n.Ids2[vacina_descricao_dose==dlevels[2] &
           paciente_enumsexobiologico=='M']$N

apply(nv24, 2, colSums)/1e6

cbind(F=apply(nv24[,1,], 2, tail),
      M=apply(nv24[,2,], 2, tail))


### define colors for the pyramid plot
par(mfrow=c(1,1), mar=c(2,2,1,1), mgp=c(1.5,0.5,0), xaxs='r', yaxs='r')
cF <- rgb(1.0, 3:0/5, 3:0/8)
cM <- rgb(3:0/10, 3:0/4, 1.0)
plot(c(1:4, 1:4), rep(1:2, each=4),
     xlim=c(0.5, 4.5), ylim=c(0.5, 2.5), 
     col=c(cF, cM), pch=19, cex=10)

if(FALSE) {

    ylm <- c(0, 3e5)
    yl2 <- list(x=pretty(ylm, 10))
    yl2$l <- ifelse(
        yl2$x<1e6, ifelse(yl2$x<1, '0',
                      paste0(format(yl2$x/1e3, digits=1), 'K')),
        paste0(format(yl2$x/1e6, digits=1), 'M'))
    yl2

    par(mfrow=c(1,2), mar=c(3,3,0,0), mgp=c(2,0.5,0))
    plot(m1i, nv24[,1,1], col=cF[1], xlim=c(90, 101),
         type='n', ylim=ylm, axes=FALSE,
         xlab='Idade', ylab='Frequencia para faixas etárias de 2 em 2 anos')
    for(j in 1:3)
        lines(m1i, nv24[,1,j], col=cF[j], lwd=3, lty=j)
    for(j in 1:3)
        lines(m1i, nv24[,2,j], col=cM[j], lwd=3, lty=j)
    axis(1)
    axis(2, yl2$x, yl2$l)
    abline(v=seq(80, 100, 2), h=yl2$x, lty=3, col=gray(0.5,0.5))
    legend('top', c('Pop', 'D1', 'D2'), title='Mulheres', col=cF, lty=1:3, lwd=3, bg='white')
    legend('topright', c('Pop', 'D1', 'D2'), title='Homens', col=cM, lty=1:3, lwd=3, bg='white')    
    plot(m1i, nv24[,1,1], col=cF[1], xlim=c(90, 101),
         type='n', ylim=c(0,105),
         xlab='Idade', ylab='% de doses aplicadas faixas etárias de 2 em 2 anos')
    for(j in 2:3)
        lines(m1i, 100*nv24[,1,j]/nv24[,1,1], col=cF[j], lwd=3, lty=j)
    for(j in 2:3)
        lines(m1i, 100*nv24[,2,j]/nv24[,2,1], col=cM[j], lwd=3, lty=j)
    abline(v=seq(80, 100, 2), h=20*(0:5), lty=3, col=gray(0.5,0.5))
    legend('bottomleft', c('D1', 'D2'), title='Mulheres', col=cF[-1], lty=2:3, lwd=3, bg='white')
    legend('bottomright', c('D1', 'D2'), title='Homens', col=cM[-1], lty=2:3, lwd=3, bg='white')

}

### define axis labels 
xl <- list(x=seq(-3e6, 3e6, 5e5))
xl$l <- paste0(
    abs(xl$x/ifelse(abs(xl$x)>=1e6,1e6,1e3)), 
    rep(c('M', 'K', '', 'K', 'M'),
        table(findInterval(
            xl$x,c(-Inf,-1e6+1,-1e3+1,0-1e-5,1e3-1,1e6-1,Inf)))))
xl

### make the plot
png('figures/vacinados_pyramid_idade_sexo_Pop.png', 2500, 2000, res=300)
par(mfrow=c(1,1), mar=c(5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
barplot(-nv24[,1,1], names.arg='', horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cF[1],
        xlim=c(-1,1)*max(nv24[,,1]))
barplot(nv24[,2,1], horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[1], axes=FALSE)
axis(1, xl$x, xl$l)
for(k in 2:4) {
    barplot(-nv24[,1,k], horiz=TRUE, space=0, add=TRUE,
            border=c('transparent', 'black')[(k>3)+1],
            col=cF[k], axes=FALSE)
    barplot(nv24[,2,k],  horiz=TRUE, space=0, add=TRUE,
            border=c('transparent', 'black')[(k>3)+1],
            col=cM[k], axes=FALSE)
}
abline(v=xl$x, h=5*(0:10), lty=2, col=gray(0.7,0.5))
legend('topleft',
       paste0(c('Pop. 2021', 'Dose 1', 'Dose 2', 'D2+14dias'), ": ", 
              sprintf("%2.1f", 
                      c(sum(p21i$Fem), n.dg[paciente_enumsexobiologico=='F']$N,
                        n.dg2[paciente_enumsexobiologico=='F']$N)/1e6),
              'M'), bg='white', box.col='transparent',
       border=rep(c('transparent', 'black'), c(3,1)),
       fill=cF, cex=1.00, title='Mulheres')
legend('topright',
       paste0(c('Pop. 2021', 'Dose 1', 'Dose 2', 'D2+14dias'), ": ", 
              sprintf("%2.1f",
                      c(sum(p21i$Masc), n.dg[paciente_enumsexobiologico=='M']$N,
                        n.dg2[paciente_enumsexobiologico=='M']$N)/1e6),
              'M'), bg='white', box.col='transparent',
       border=rep(c('transparent', 'black'), c(3,1)),
       fill=cM, cex=1.00, title='Homens')
text(rep(3.35,5)*1e6, ##rep(c(1.6,1.65,1.6,1.4,1,0.8,0.4)*1e6, c(1,3,1,1,1,1,1)),
     10*(0:4), 20*(0:4), cex=0.7, xpd=TRUE)
text(3.35e6, 4, 'Idade', srt=90, cex=0.7)
mtext(paste('Atualizado em',
            format(Sys.Date()-1*(as.integer(format(Sys.time(), '%H'))<17), '%d de %B de %Y')),
      1, 2, adj=0, cex=0.9)
mtext(paste0('Fonte 1: https://www.ibge.gov.br/estatisticas/sociais/trabalho/',
             '17270-pnad-continua.html',
             ' (expansão amostral + suavização)'), 1, 3, adj=0, cex=0.9)
mtext('Fonte 2: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao', 1, 4, adj=0, cex=0.9)
mtext('@eliaskrainski', 1, 4, adj=1, cex=1.25)
dev.off()

if(FALSE)
    system('eog figures/vacinados_pyramid_idade_sexo_Pop.png &')

p2v <- nv24[,,2:3]/nv24[,,c(1,1)]
dim(p2v)

n2g <- apply(nv24, 3, colSums)
n2g
sum(n2g[,2:3])

png('figures/vacinados_pyramid_idade_sexo_Pop_prop.png', 2500, 2000, res=300)
par(mar=c(5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
barplot(-p2v[,1,1], names.arg='', horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cF[1],
        xlim=c(-1,1), ylim=c(9,dim(p2v)[1]))
barplot(p2v[,2,1], horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[1], axes=FALSE)
barplot(-p2v[,1,2], horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[2], axes=FALSE)
barplot(p2v[,2,2],  horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[2], axes=FALSE)
barplot(-p2v[,1,3], horiz=TRUE, space=0, add=TRUE,
        col=cF[3], axes=FALSE)
barplot(p2v[,2,3],  horiz=TRUE, space=0, add=TRUE,
        col=cM[3], axes=FALSE)
axis(1, seq(-.8, .8, 0.2), paste0(abs(seq(-80, 80, 20)), '%'))
abline(h=5*(2:10), v=seq(-0.8,0.8,0.2), col=gray(0.5,0.5), lty=2)
legend(-0.7, 25, ##'bottomleft', 
       paste0(c('Dose 1', 'Dose 2', 'D2+14dias'), ": ", 
              sprintf("%2.1f", 100*n2g[1,-1]/n2g[1,1]), '%'), 
       bg='white', box.col='transparent',
       fill=cF, border='transparent', cex=1.00, title='Mulheres')
legend(0.3, 25, ##'bottomright',
       paste0(c('Dose 1', 'Dose 2', 'D2+14dias'), ": ", 
              sprintf("%2.1f", 100*n2g[2,-1]/n2g[2,1]),'%'), 
       bg='white', box.col='transparent',
       fill=cM, border='transparent', cex=1.00, title='Homens')
text(rep(0.95,9)*1, ##rep(c(1.6,1.65,1.6,1.4,1,0.8,0.4)*1e6, c(1,3,1,1,1,1,1)),
     5*(2:10), 10*(2:10), cex=0.7, xpd=TRUE)
text(0.95, 12, 'Idade', srt=90, cex=0.7)
mtext(paste('Atualizado em',
            format(Sys.Date()-1*(as.integer(format(Sys.time(), '%H'))<17), '%d de %B de %Y')),
      1, 2, adj=0, cex=0.9)
mtext(paste0('Fonte 1: https://www.ibge.gov.br/estatisticas/sociais/trabalho/',
             '17270-pnad-continua.html',
             ' (expansão amostral + suavização)'), 1, 3, adj=0, cex=0.8)
mtext('Fonte 2: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao', 1, 4, adj=0, cex=0.8)
mtext('@eliaskrainski', 1, 4, adj=1, cex=1.00)
dev.off()

if(FALSE)
    system('eog figures/vacinados_pyramid_idade_sexo_Pop_prop.png &')

source('rcode/ocommon.R')
head(uf,3)

n.u <- dvbr[UF %in% rownames(uf), .N, UF]
n.u

uds <- c('UF', ds)
n.uds <- dvbr[UF %in% rownames(uf) &
              vacina_descricao_dose %in% dlevels[1:2] &
              paciente_enumsexobiologico %in% c('F', 'M'),.N,uds]
n.uds <- setorder(n.uds, vacina_descricao_dose)

n.uds$nleg <- ifelse(n.uds$N<1e6, paste0(format(n.uds$N/1e3, digits=1), 'K'),
                     paste0(format(n.uds$N/1e6, digits=1), 'M'))
n.uds

K
ius.p <- read.csv2(paste0('data/estimativaPopulacaoUF202101SexoFaixa', K, 'a.csv'))
head(ius.p)
tail(ius.p)

png('figures/pyramidsUFs.png', 2000, 2500, res=200)
par(mfrow=c(9,3), mar=c(1.5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
for(i in 1:27) {
    u <- dimnames(t.iKdsu)$UF[i]
    p.sel <- ius.p[ius.p$UF==u,]
    xM <- max(p.sel$Fem, p.sel$Masc)
    barplot(-p.sel$Fem, names.arg='', horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1],
            xlim=c(-1,1)*xM, ylim=c(0,nrow(p.sel)+1.5))
    barplot(p.sel$Masc, horiz=TRUE, space=0, add=TRUE, axes=FALSE,
            border='transparent', col=cM[1])
    xl <- list(x=pretty(c(0,0.6*xM), 3))
    xl$l <- round(ifelse(xl$x<1e6, xl$x/1e3, xl$x/1e6))
    xl$l <- paste0(xl$l,
                   ifelse(xl$x==0, '',
                   ifelse(xl$x<1e6, 'K', 'M')))
    axis(1, c(-rev(xl$x), xl$x), c(rev(xl$l), xl$l))
    for(k in 1:2) {
        barplot(-as.integer(t.iKdsu[,1,k,i]), horiz=TRUE, space=0, add=TRUE,
                border='transparent', col=cF[k+1], axes=FALSE)
        barplot(as.integer(t.iKdsu[,2,k,i]), horiz=TRUE, space=0, add=TRUE,
                border='transparent', col=cM[k+1], axes=FALSE)
    }
    text(par()$usr[2]*0.95, seq(4, nKi-2, 4), K*seq(4, nKi-2, 4), cex=0.7)
    N1n <- sum(p.sel$Fem)
    if(N1n>=1e6) {
        llN1 <- paste0('Pop Fem: ', format(N1n/1e6, digits=2), 'M')
    } else {
        llN1 <- paste0('Pop Fem: ', format(N1n/1e3, digits=2), 'K')
    }
    N2n <- sum(p.sel$Masc)
    if(N2n>=1e6) {
        llN2 <- paste0('Pop Masc: ', format(N2n/1e6, digits=2), 'M')
    } else {
        llN2 <- paste0('Pop Masc: ', format(N2n/1e3, digits=2), 'K')
    }
    llab1 <- n.uds[UF==u & paciente_enumsexobiologico=='F']$nleg
    llab2 <- n.uds[UF==u & paciente_enumsexobiologico=='M']$nleg
    nn1 <- n.uds[UF==u & paciente_enumsexobiologico=='F']$N
    nn2 <- n.uds[UF==u & paciente_enumsexobiologico=='M']$N
    legend('top', '', title=uf$State[rownames(uf)==u], bty='n')
    legend('topleft',
           paste0('D', 1:2, ': ', llab1, "(",
                  format(100*nn1/N1n, dig=1), "%)"),
           fill=cF[-1], bty='n', border='transparent')
    text(par()$usr[1]*0.3, 1, llN1)
    legend(par()$usr[2]*0.2, par()$usr[4], ##'topright',
           paste0('D', 1:2, ': ', llab2, "(",
                  format(100*nn2/N2n, dig=1), "%)"), 
           fill=cM[-1], bty='n', border='transparent')
    text(par()$usr[2]*0.3, 1, llN2)
}
dev.off()

if(FALSE)
    system('eog figures/pyramidsUFs.png &')

xlp <- seq(-0.75, 0.75, 0.25)
xlp

png('figures/pyramidsUFs_prop.png', 2000, 2500, res=200)
par(mfrow=c(9,3), mar=c(0, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
for(i in 1:27) {
    u <- dimnames(t.iKdsu)$UF[i]
    p.sel <- ius.p[ius.p$UF==u,]
    pF1 <- t.iKdsu[,1,1,i]/p.sel$Fem
    pF2 <- t.iKdsu[,1,2,i]/p.sel$Fem
    pM1 <- t.iKdsu[,2,1,i]/p.sel$Masc
    pM2 <- t.iKdsu[,2,2,i]/p.sel$Masc
    xM <- max(pF1, pM1, 1)
    if(i>24)
        par(mar=c(1.5,0,0,0))
    barplot(-pF1, names.arg='', horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1],
            xlim=c(-1,1), ylim=c(0,nrow(p.sel)+1.5))
    barplot(pM1, names.arg='', horiz=TRUE, space=0, add=TRUE, axes=FALSE,
            border='transparent', col=cM[1])
    barplot(-pF2, names.arg='', horiz=TRUE, space=0, add=TRUE, axes=FALSE,
            border='transparent', col=cF[2])
    barplot(pM2, names.arg='', horiz=TRUE, space=0, add=TRUE, axes=FALSE,
            border='transparent', col=cM[2])
    if(i>24)
        axis(1, xlp, paste0(abs(xlp*100), '%'))
    segments(xlp, rep(0,length(xlp)),
             xlp, rep(nKi-1,length(xlp)),
             col=gray(0.5,0.5), lty=2)
    segments(rep(-0.9, 10), seq(2, nKi-2, 2), 
             rep(0.9,10), seq(2, nKi-2, 2), 
             col=gray(0.5,0.5), lty=2)
    N1n <- sum(p.sel$Fem)
    N2n <- sum(p.sel$Masc)
    llab1 <- n.uds[UF==u & paciente_enumsexobiologico=='F']$nleg
    llab2 <- n.uds[UF==u & paciente_enumsexobiologico=='M']$nleg
    nn1 <- n.uds[UF==u & paciente_enumsexobiologico=='F']$N
    nn2 <- n.uds[UF==u & paciente_enumsexobiologico=='M']$N
    legend('top', '', title=uf$State[rownames(uf)==u], bty='n')
    legend(0.2, 2*K, cex=0.9, ##'bottomleft',
           paste0('D', 1:2, ': ', llab1, "(",
                  format(100*nn1/N1n, dig=1), "%)"),
           fill=cF, border='transparent',
           bg=gray(0.9,0.5), box.col='transparent')
    legend(-0.9, 2*K, cex=0.9, ## 'bottomright',
           paste0('D', 1:2, ': ', llab2, "(",
                  format(100*nn2/N2n, dig=1), "%)"), 
           fill=cM, border='transparent',
           bg=gray(0.9,0.5), box.col='transparent')
    if(i%in%seq(1,27,3)) {
  ##      text(-0.95, 1.3, 'Idade', srt=90, cex=1.05)
        text(rep(-0.95, 3), seq(4, nKi-1, 4), K*seq(4, nKi-1, 4))
    }
    if(i%in%seq(3,27,3)) {
##        text(0.95, 1.3, 'Idade', srt=90, cex=1.05)
        text(rep(0.95, 3), seq(4, nKi-1, 4), K*seq(4, nKi-1, 4))
    }
}
dev.off()

if(FALSE)
    system('eog figures/pyramidsUFs_prop.png &')
