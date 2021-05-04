
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

dvbr$Idade <- factor(
    findInterval(dvbr$paciente_idade, c(0:115,Inf)-1e-5),
    1:116, 0:115)

### tabulate by age, dose and gender
Ids <- c('Idade', ds)
n.Ids <- dvbr[
    vacina_descricao_dose %in% dlevels[1:2] &
    paciente_enumSexoBiologico %in% c('F', 'M'),
    .N,by=Ids]

### set it order of age, for later use
n.Ids <- setorder(
    n.Ids,
    Idade, 
    vacina_descricao_dose,
    paciente_enumSexoBiologico)
n.Ids

tMunDate <- dvbr[,.N,by=c('paciente_endereco_coIbgeMunicipio',
                          'vacina_dataAplicacao',
                          'vacina_descricao_dose')]

str(tMunDate)

dMunDateDose <- as.data.frame(tMunDate)

save('dMunDateDose',
     file='data/dMunDateDose.RData',
     compress='xz')


b5i <- c(seq(0, 100, 5), Inf)
dvbr$i5 <- findInterval(dvbr$paciente_idade, b5i-1e-3)

dvbr$UF <- substr(dvbr$paciente_endereco_coIbgeMunicipio,1,2)

t1 <- Sys.time()
i5dsu <- c('i5', ds, 'UF')
n.i5dsu <- dvbr[
    vacina_descricao_dose %in% dlevels[1:2] &
    paciente_enumSexoBiologico %in% c('F', 'M'),
    .N,by=i5dsu]
n.i5dsu <- setorder(
    n.i5dsu,
    i5, 
    vacina_descricao_dose,
    paciente_enumSexoBiologico)
Sys.time()-t1

t1 <- Sys.time()
t.i5dsu <- with(dvbr[vacina_descricao_dose %in% dlevels[1:2] &
                     paciente_enumSexoBiologico %in% c('F', 'M')],
                table(i5, paciente_enumSexoBiologico,
                      vacina_descricao_dose, UF))
Sys.time()-t1
dim(t.i5dsu)
dimnames(t.i5dsu)

