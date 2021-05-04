if(FALSE)
    setwd('..')

system.time(source('rcode/dados-vacinacao-brasil-uf.R'))

n.t <- tapply(tMunDate$N, substr(tMunDate$vacina_dataAplicacao,1,10),sum)

tail(n.t,21)/1e6

plot(as.Date(names(n.t)), n.t, xlab='', ylab='',
     xlim=Sys.Date()-c(120,0), type='o')

### consider the population data
p21i <- read.csv('data/estPNADCpopBR202004IdadeSexo.csv')
tail(p21i,3)

### define colors for the pyramid plot 
cF <- rgb(1.0,c(0.5,.3,0),c(0.4,0.2,0))
cM <- rgb(c(0.4,0.2,0),c(0.7,0.5,0.1),1.0)

### define axis labels 
xl <- list(x=seq(-1.5e6, 1.5e6, 5e5)) 
xl$l <- paste(c(1.5, 1, 500, 0, 500, 1, 1.5),
              rep(c('Milhão', 'mil', '', 'mil', 'Milhão'),
                  c(2,1,1,1,2)))

### make the plot
png('figures/vacinados_pyramid_idade_sexo_Pop.png', 2500, 2000, res=300)
par(mar=c(5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
barplot(-p21i$Fem, names.arg='', horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cF[1],
        xlim=c(-1,1)*max(p21i$Fem, p21i$Masc), 
        ylim=c(-0.2, 105))
barplot(p21i$Masc, horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[1], axes=FALSE)
axis(1, xl$x, xl$l)
barplot(-n.Ids[vacina_descricao_dose==dlevels[1] &
                 paciente_enumSexoBiologico=='F']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[2], axes=FALSE)
barplot(-n.Ids[vacina_descricao_dose==dlevels[2] &
                 paciente_enumSexoBiologico=='F']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[3], axes=FALSE)
barplot(n.Ids[vacina_descricao_dose==dlevels[1] &
                paciente_enumSexoBiologico=='M']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[2], axes=FALSE)
barplot(n.Ids[vacina_descricao_dose==dlevels[2] &
                 paciente_enumSexoBiologico=='M']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[3], axes=FALSE)
abline(v=seq(-10, 10, 1)*2e5,
       h=10*(0:10), lty=2, col=gray(0.7,0.5))
axis(1, seq(-10,10,1)*2e5, labels=FALSE)
legend(-1.5e6, 105, ##'topleft',
       paste0(c('Pop. 2020', 'Dose 1', 'Dose 2'), ": ", 
              sprintf("%2.1f", 
                      c(sum(p21i$Fem), n.dg[paciente_enumSexoBiologico=='F']$N)/1e6),
              'M'), bg='white', box.col='transparent',
       fill=cF, border='transparent', cex=1.00, title='Mulheres')
legend(5e5, 105, ##'topright',
       paste0(c('Pop. 2020', 'Dose 1', 'Dose 2'), ": ", 
              sprintf("%2.1f",
                      c(sum(p21i$Masc), n.dg[paciente_enumSexoBiologico=='M']$N)/1e6),
              'M'), bg='white', box.col='transparent',
       fill=cM, border='transparent', cex=1.00, title='Homens')
text(rep(1.65,11)*1e6, ##rep(c(1.6,1.65,1.6,1.4,1,0.8,0.4)*1e6, c(1,3,1,1,1,1,1)),
     10*(0:10), 10*(0:10), cex=0.7, xpd=TRUE)
text(1.7e6, 4, 'Idade', srt=90, cex=0.7)
mtext(paste('Atualizado em',
            format(Sys.Date()-1*(as.integer(format(Sys.time(), '%H'))<17), '%d de %B de %Y')),
      1, 2, adj=0, cex=0.7)
mtext(paste0('Fonte 1: https://www.ibge.gov.br/estatisticas/sociais/trabalho/',
             '17270-pnad-continua.html',
             ' (expansão amostral + suavização)'), 1, 3, adj=0, cex=0.7)
mtext('Fonte 2: https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao', 1, 4, adj=0, cex=0.7)
mtext('@eliaskrainski', 1, 4, adj=1, cex=1.00)
dev.off()

if(FALSE)
    system('eog figures/vacinados_pyramid_idade_sexo_Pop.png &')

source('rcode/ocommon.R')
uf

n.u <- dvbr[UF %in% rownames(uf), .N, UF]
n.u

uds <- c('UF', ds)
n.uds <- dvbr[UF %in% rownames(uf) &
              vacina_descricao_dose %in% dlevels[1:2] &
              paciente_enumSexoBiologico %in% c('F', 'M'),.N,uds]

n.uds <- setorder(n.uds, vacina_descricao_dose)
n.uds$nleg <- n.uds$N/ifelse(n.uds$N>=1e6, 1e6, 1e3)
n.uds$nleg <- paste0(format(n.uds$nleg, digits=2),
                     c('M', 'K')[(n.uds$N<1e6)+1])
n.uds

ius.p <- read.csv2('data/estimativaPopulacaoUF202004SexoFaixa5a.csv')
head(ius.p)

png('figures/pyramidsUFs.png', 2000, 2500, res=200)
par(mfrow=c(9,3), mar=c(1.5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
for(i in 1:27) {
    u <- dimnames(t.i5dsu)$UF[i]
    p.sel <- ius.p[ius.p$UF==u,]
    xM <- max(p.sel$Fem, p.sel$Masc)
    barplot(-p.sel$Fem, names.arg='', horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1],  xlim=c(-1,1)*xM, ylim=c(0,23))
    barplot(p.sel$Masc, horiz=TRUE, space=0, add=TRUE, axes=FALSE,
            border='transparent', col=cM[1])
    xl <- list(x=pretty(c(0,0.6*xM), 3))
    xl$l <- round(ifelse(xl$x<1e6, xl$x/1e3, xl$x/1e6))
    xl$l <- paste0(xl$l,
                   ifelse(xl$x==0, '',
                   ifelse(xl$x<1e6, 'K', 'M')))
    axis(1, c(-rev(xl$x), xl$x), c(rev(xl$l), xl$l))
    barplot(-as.integer(t.i5dsu[,1,1,i]), horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cF[2], axes=FALSE)
    barplot(-as.integer(t.i5dsu[,1,2,i]), horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cF[3], axes=FALSE)
    barplot(as.integer(t.i5dsu[,2,1,i]), horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cM[2], axes=FALSE)
    barplot(as.integer(t.i5dsu[,2,2,i]), horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cM[3], axes=FALSE)
    text(rep(0, 4), 4*(1:4), 20*(1:4), cex=0.7)
    N1n <- sum(p.sel$Fem)
    if(llN1>=1e6) {
        llN1 <- paste0('Pop Fem: ', format(N1n/1e6, digits=2), 'M')
    } else {
        llN1 <- paste0('Pop Fem: ', format(N1n/1e3, digits=2), 'K')
    }
    N2n <- sum(p.sel$Masc)
    if(llN2>=1e6) {
        llN2 <- paste0('Pop Masc: ', format(N2n/1e6, digits=2), 'M')
    } else {
        llN2 <- paste0('Pop Masc: ', format(N2n/1e3, digits=2), 'K')
    }
    llab1 <- n.uds[UF==u & paciente_enumSexoBiologico=='F']$nleg
    llab2 <- n.uds[UF==u & paciente_enumSexoBiologico=='M']$nleg
    nn1 <- n.uds[UF==u & paciente_enumSexoBiologico=='F']$N
    nn2 <- n.uds[UF==u & paciente_enumSexoBiologico=='M']$N
    legend('top', '', title=uf$State[rownames(uf)==u], bty='n')
    legend('topleft',
           c(llN1, paste0('D', 1:2, ': ', llab1, "(",
                          format(100*nn1/N1n, dig=1), "%)")),
           fill=cF, bty='n', border='transparent')
    legend('topright',
           c(llN2, paste0('D', 1:2, ': ', llab2, "(",
                          format(100*nn2/N2n, dig=1), "%)")), 
           fill=cM, bty='n', border='transparent')
}
dev.off()

if(FALSE)
    system('eog figures/pyramidsUFs.png &')

