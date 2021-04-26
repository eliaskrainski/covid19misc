
if(FALSE)
    setwd('..')

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

### automatic download function
brvac.uf <- function(d=FALSE, uf=NULL) { 
    url0 <- paste0('https://opendatasus.saude.gov.br/',
                   'dataset/covid-19-vacinacao/resource/',
                   'ef3bd0b8-b605-474b-9ae5-c97390c197a8')
### non elegant way to get the file names... 
    d0 <- readLines(url0)
    urls <- grep('c000.csv', d0, value=TRUE)[-1]
    ufls <- substr(urls, 14, 155)
    if(!is.null(uf)) {
        ufls <- ufls[substr(ufls, 84, 85) %in% uf]
    }
    lfls <- gsub('/', '_', substring(ufls, 84), fixed=TRUE)
### download if asked and do not exists locally
    if(d) {
        for(j in 1:length(lfls)) {
            fl.j <- paste0('data/', lfls[j])
            if(!file.exists(fl.j))
                download.file(ufls[j], fl.j)
        }
    } else {  ### return the local filename
        return(paste0('data/', lfls))
    }
}

options(timeout=60*10) ### to work with bad internet...
brvac.uf(TRUE) ## download each UF file

### get each UF local file: 'data/UF_....csv"
fls <- brvac.uf(d=FALSE)

### look at the first lines from one of the files
(tmp <- read.csv2(fls[1], nrow=2))

### desired variables 
xsel <- c(###'paciente_id',
          'paciente_idade',
          'paciente_enumSexoBiologico',
          'paciente_endereco_coIbgeMunicipio',
          'vacina_dataAplicacao',
          'vacina_descricao_dose') ###, 'vacina_nome')

jj <- pmatch(xsel, colnames(tmp))
jj

library(data.table)

### read and rbind each UF data
dvbr <- Reduce('rbind',
               lapply(fls, fread, select=jj))

### look at the first lines
head(dvbr, 3)

### tabulate by gender and dose
ds <- c('vacina_descricao_dose',
        'paciente_enumSexoBiologico')
n.dg <- dvbr[, .N, by=ds]
n.dg

### dose levels, for later use
dlevels <- sort(unique(n.dg$vacina_descricao_dose))
dlevels

### select desired levels and order it for later use
n.dg <- setorder(
    n.dg[vacina_descricao_dose %in% dlevels[1:2] &
         paciente_enumSexoBiologico %in% c('F', 'M')],
    vacina_descricao_dose,
    paciente_enumSexoBiologico)
n.dg

### tabulate by age, dose and gender
ids <- c('paciente_idade', ds)
n.ids <- dvbr[
    vacina_descricao_dose %in% dlevels[1:2] &
    paciente_enumSexoBiologico %in% c('F', 'M'),
    .N,by=ids]

### set it inder of age, for later use
n.ids <- setorder(
    n.ids,
    paciente_idade,
    vacina_descricao_dose,
    paciente_enumSexoBiologico)
n.ids

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


tMunDate <- dvbr[,.N,by=c('paciente_endereco_coIbgeMunicipio',
                          'vacina_dataAplicacao',
                          'vacina_descricao_dose')]

str(tMunDate)

dMunDateDose <- as.data.frame(tMunDate)

sum(dMunDateDose$N)

table(dMunDateDose$vacina_descricao_dose)

save('dMunDateDose',
     file='data/dMunDateDose.RData',
     compress='xz')

tapply(tMunDate$N, tMunDate$vacina_des, sum)

tapply(tMunDate$N,
       substr(tMunDate$paciente_endereco, 1, 2),
       sum)

tMunDate[1:20, c(2,3,4)]
