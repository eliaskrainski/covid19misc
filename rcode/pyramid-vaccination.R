if(FALSE)
    setwd('..')

source('rcode/dados-vacinacao-brasil-uf.R')

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
barplot(-n.ids[vacina_descricao_dose==dlevels[1] &
                 paciente_enumSexoBiologico=='F']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[2], axes=FALSE)
barplot(-n.ids[vacina_descricao_dose==dlevels[2] &
                 paciente_enumSexoBiologico=='F']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[3], axes=FALSE)
barplot(n.ids[vacina_descricao_dose==dlevels[1] &
                paciente_enumSexoBiologico=='M']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[2], axes=FALSE)
barplot(n.ids[vacina_descricao_dose==dlevels[2] &
                 paciente_enumSexoBiologico=='M']$N,
        horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[3], axes=FALSE)
text(rep(0, 9), 10*(1:9), 10*(1:9), cex=0.7)
text(-5e3, 4, 'Idade', srt=90, cex=0.7)
legend('topleft',
       paste0(c('Pop. 2020', 'Dose 1', 'Dose 2'), ": ", 
              sprintf("%2.2f", 
                      c(sum(p21i$Fem), n.dg[paciente_enumSexoBiologico=='F']$N)/1e6),
              ' Milhões'), 
       fill=cF, bty='n', border='transparent', cex=1.00, title='Mulheres')
legend('topright',
       paste0(c('Pop. 2020', 'Dose 1', 'Dose 2'), ": ", 
              sprintf("%2.2f",
                      c(sum(p21i$Masc), n.dg[paciente_enumSexoBiologico=='M']$N)/1e6),
              ' Milhões'), 
       fill=cM, bty='n', border='transparent', cex=1.00, title='Homens')
mtext(paste('Atualizado em ',
            format(Sys.Date(), '%d de %B de %Y')),
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
n.uds

ius.p <- read.csv2('data/estimativaPopulacaoUF202004SexoFaixa5a.csv')
head(ius.p)


png('figures/pyramidsUFs.png', 1800, 2400, res=200)
par(mfrow=c(9,3), mar=c(1.5, 0, 0, 0), mgp=c(1.5,0.5,0), xaxs='i', yaxs='i')
for(i in order(rownames(uf))) {
    u <- rownames(uf)[i] 
    p.sel <- ius.p[ius.p$UF==u,]
    xM <- max(p.sel$Fem, p.sel$Masc)
    barplot(-p.sel$Fem, names.arg='', horiz=TRUE, space=0, axes=FALSE,
            border='transparent', col=cF[1],  xlim=c(-1,1)*xM, ylim=c(0,23))
    barplot(p.sel$Masc, horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cM[1], axes=FALSE)
    xl <- pretty(c(0,0.6*xM), 3)
    axis(1, c(-rev(xl), xl), round(c(rev(xl), xl)/1000))
    barplot(-n.i5dsu[vacina_descricao_dose==dlevels[1] &
                     paciente_enumSexoBiologico=='F' & UF==u]$N,
            horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cF[2], axes=FALSE)
    barplot(-n.i5dsu[vacina_descricao_dose==dlevels[2] &
                     paciente_enumSexoBiologico=='F' & UF==u]$N,
            horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cF[3], axes=FALSE)
    barplot(n.i5dsu[vacina_descricao_dose==dlevels[1] &
                    paciente_enumSexoBiologico=='M' & UF==u]$N,
            horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cM[2], axes=FALSE)
    barplot(n.i5dsu[vacina_descricao_dose==dlevels[2] &
                    paciente_enumSexoBiologico=='M' & UF==u]$N,
            horiz=TRUE, space=0, add=TRUE,
            border='transparent', col=cM[3], axes=FALSE)
    text(rep(0, 9), 2*(1:9), 10*(1:9), cex=0.7)
    llab1 <- n.uds[UF==u & paciente_enumSexoBiologico=='F']
    llab2 <- n.uds[UF==u & paciente_enumSexoBiologico=='M']
    legend('top', '', title=uf$State[i], bty='n')
    legend('topleft',
           c(paste(format(sum(p.sel$Fem)/1e6, digits=3), 'Milhões'), 
             paste0('D', 1:2, ': ', 
                    format(llab1$N/1e3, digits=1), 'mil')),
           fill=cF, bty='n', border='transparent')
    legend('topright',
           c(paste(format(sum(p.sel$Masc)/1e6, digits=3), 'Milhões'), 
             paste0('D', 1:2, ': ', 
                    format(llab2$N/1e3, digits=1), 'mil')),
           fill=cM, bty='n', border='transparent')
}
dev.off()

if(FALSE)
    system('eog figures/pyramidsUFs.png &')
