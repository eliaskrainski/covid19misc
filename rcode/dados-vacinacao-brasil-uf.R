
if(FALSE)
    setwd('..')

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

source('rcode/ocommon.R')

url0 <- paste0('https://s3-sa-east-1.amazonaws.com/',
               'ckan.saude.gov.br/PNI/vacina/uf/2021-04-19/uf=')
fl0 <- 'part-00000-51e5ff2c-513f-4d37-8a45-5ee2c98cb342.c000.csv'

if(FALSE) {
    
    options(timeout=60*60)

    for (u in uf$UF) {
        cat(u, '...')
        download.file(paste0(url0, u, '/', fl0),
                      paste0('data/', u, fl0))
        cat('\n')
    }

}

fls <- paste0('data/', uf$UF, fl0)
tail(fls, 10)

### look at the first lines 
(tmp <- read.csv2(fls[1], nrow=2))

### desired variables 
xsel <- c(###'paciente_id',
          'paciente_idade',
          'paciente_enumSexoBiologico',
          'paciente_endereco_coIbgeMunicipio',
          'vacina_dataAplicacao',
          'vacina_descricao_dose', 'vacina_nome')

jj <- pmatch(xsel, colnames(tmp))
jj

library(data.table)

t1 <- Sys.time()
dvbr <- Reduce('rbind', lapply(fls, fread, select=jj))
###dvbr <- lapply(fls[1:5], fread, select=jj)
Sys.time()-t1

dim(dvbr)
head(dvbr, 3)
table(substr(dvbr$paciente_endereco_coIbgeMunicipio,1,2))

t1 <- Sys.time()
dvbr <- setorder(dvbr, vacina_dataAplicacao) 
Sys.time()-t1

head(dvbr, 3)

t1 <- Sys.time()
dt.d <- dvbr[, .N, by=vacina_descricao_dose]
Sys.time()-t1

dt.d

t1 <- Sys.time()
dt.v <- dvbr[, .N, by=vacina_nome]
Sys.time()-t1

dt.v

gc(reset=TRUE)

t1 <- Sys.time()
dt.sx <- dvbr[, .N, by=paciente_enumSexoBiologico]
Sys.time()-t1

dt.sx

t1 <- Sys.time()
dt.dv <- dvbr[,.N,by=c('vacina_nome', 'vacina_descricao_dose')]
Sys.time()-t1

dt.dv

t1 <- Sys.time()
dvbr[, .(mean=mean(paciente_idade, na.rm=TRUE),
         min=min(paciente_idade, na.rm=TRUE),
         max=max(paciente_idade, na.rm=TRUE))]
Sys.time()-t1

gc(reset=TRUE)

t1 <- Sys.time()
dvbr <- dvbr[(paciente_enumSexoBiologico=='F' |
              paciente_enumSexoBiologico=='M') & 
             (vacina_descricao_dose=='    1ª Dose') |
             (vacina_descricao_dose=='    2ª Dose')]
Sys.time()-t1

nrow(dvbr)
sum(dt.dv$N)

t1 <- Sys.time()
dt.i.d.s <- dvbr[,.N,by=c('paciente_idade',
                        'vacina_descricao_dose',
                        'paciente_enumSexoBiologico')]
Sys.time()-t1

dt.i.d.s <- setorder(dt.i.d.s, paciente_idade) 
dt.i.d.s

cF <- rgb(1.0,c(0.3,0),c(0.1,0),0.5)
cM <- rgb(c(0.1,0),c(0.5,0.1),1.0,0.5)
##plot(1:4, col=c(cF, cM), pch=19, cex=5)

t1 <- Sys.time()
dvbr1F <- dvbr[paciente_enumSexoBiologico=='F' &
               vacina_descricao_dose=='    1ª Dose']
dvbr1M <- dvbr[paciente_enumSexoBiologico=='M' &
               vacina_descricao_dose=='    1ª Dose']
Sys.time()-t1

t1 <- Sys.time()
hFi <- hist(dvbr1F$paciente_idade,
            0:222-0.5, plot=FALSE)
Sys.time()-t1

t1 <- Sys.time()
hMi <- hist(dvbr1M$paciente_idade,
            0:222-0.5, plot=FALSE)
Sys.time()-t1

gc(reset=TRUE)

dt.d
c(sum(hFi$counts), sum(hMi$counts),
  sum(hFi$counts) + sum(hMi$counts))
c(sum(hFi$counts), sum(hMi$counts))/nrow(dvbr)
    
if(FALSE) {
    
    png('figures/vacinados_histogram_idade_sexo.png', 900, 500)
    par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(1.7,0.5,0))
    plot(hFi, col=cF[1], border='transparent', xlim=c(0,105),
         main='', xlab='Idade')
    plot(hMi, col=cM[1], add=TRUE, border='transparent')
    legend('topleft', c('Feminino', 'Masculino'),
           fill=c(cF[1], cM[1]), cex=1.5)
    dev.off()
    
    if(FALSE)
        system('eog figures/vacinados_histogram_idade_sexo.png &')

}

p21i <- read.csv('data/estPNADCpopBR202004IdadeSexo.csv')
tail(p21i,3)

w90i <- exp(-(1:29)/3.3); w90i <- w90i/sum(w90i)
c(p21i$Fem[87:90], 
  round(w90i*p21i$Fem[90+1]))

pFi <- c(p21i$Fem[1:90], round(p21i$Fem[91]*w90i))
pMi <- c(p21i$Masc[1:90], round(p21i$Masc[91]*w90i))

HFi <- hFi
HFi$counts <- pFi
HMi <- hMi
HMi$counts <- pMi
    
if(FALSE) {

    png('figures/vacinados_histogram_idade_sexo_Pop.png', 900, 500)
    par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(1.7,0.5,0))
    plot(HFi, col=cF[1], border='transparent', 
         main='', xlab='Idade', xlim=c(0,105))
    plot(hFi, col=cF[2], add=TRUE, border='transparent')
    plot(HMi, col=cM[1], add=TRUE, border='transparent')
    plot(hMi, col=cM[2], add=TRUE, border='transparent')
    legend('topright', paste(c('Fem', 'Masc'), rep(c('Pop', 'Vac'), each=2)),
           fill=c(cF, cM)[c(1,3,2,4)], cex=1.5)
    dev.off()
    
}

if(FALSE)
    system('eog figures/vacinados_histogram_idade_sexo_Pop.png &')

png('figures/vacinados_pyramid_idade_sexo_Pop.png', 700, 700)
par(mar=c(2, 0, 0, 0))
barplot(-pFi, names.arg='', horiz=TRUE, space=0, axes=FALSE,
        border='transparent', col=cF[1],
        xlim=c(-1,1)*max(HFi$counts, HMi$counts), ylim=c(-0.5,105.5))
barplot(pMi, horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[1], axes=FALSE)
axis(1, pretty(par()$usr[1:2], 7),
     paste0(abs(pretty(par()$usr[1:2], 7)/1e3), 'K'))
barplot(-hFi$counts, horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cF[2], axes=FALSE)
barplot(hMi$counts, horiz=TRUE, space=0, add=TRUE,
        border='transparent', col=cM[2], axes=FALSE)
text(rep(0, 10), 10*(0:9), 10*(0:9))
legend('topleft',
       paste0(c('Pop. est. 2021', 'Vacinadas'), ": ", 
              sprintf("%2.2f",
                      c(sum(HFi$counts), sum(hFi$counts))/1e6), 'M'), 
       fill=cF, bty='n', box.col='transparent', cex=1.5)
legend('topright',
       paste0(c('Pop. est. 2021', 'Vacinados'), ": ", 
              sprintf("%2.2f",
                      c(sum(HMi$counts), sum(hMi$counts))/1e6), 'M'), 
       fill=cM, bty='n', box.col='transparent', cex=1.5)
dev.off()

if(FALSE)
    system('eog figures/vacinados_pyramid_idade_sexo_Pop.png &')

if(FALSE) {
    
    t1 <- Sys.time()
    tDate <- dvbr[,.N,by=vacina_dataAplicacao]
    Sys.time()-t1
    
    wD <- Sys.time()-60*60*24*c(90,0)
    
    plot(tDate, xlim=wD,
         xlab='', ylab='', type='h', axes=FALSE)
    axis(1, pretty(wD, 7), 
         format(pretty(wD, 7), '%b,%d'))

}


head(dvbr,3)

t1 <- Sys.time()
tMunDate <- dvbr[,.N,by=c('paciente_endereco_coIbgeMunicipio',
                          'vacina_dataAplicacao',
                          'vacina_descricao_dose')]
Sys.time()-t1

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
