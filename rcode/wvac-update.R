
if(FALSE)
    setwd('..')

load('data/dMunDateDose.RData')
ls()

dim(dMunDateDose)
head(dMunDateDose)

sum(dMunDateDose$N)

tapply(dMunDateDose$N, dMunDateDose$vacina_descricao_dose, sum)

tapply(dMunDateDose$N, dMunDateDose$UF, sum)

Date <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
alldates <- gsub('-', '', as.character(Date))

dMunDateDose$fdate <- factor(
    gsub('-', '', substr(dMunDateDose$vacina_dataAplicacao, 1, 10)), 
    alldates)

head(dMunDateDose)
tail(dMunDateDose)

head(dMunDateDose$vacina_dataAplicacao, 20)
sum(dMunDateDose$N[is.na(dMunDateDose$fdate)])

dlab <- sort(unique(dMunDateDose$vacina_descricao_dose))
names(dlab) <- gsub(' ', '', gsub('ª', '', dlab))
dlab

p19mun <- read.csv2('data/populacao2019municipio.csv', skip=3)
head(p19mun,3)
p19mun[-5:0+5572,]

mcod6 <- sort(substr(unique(p19mun[2:5571,1]), 1, 6))

dMunDateDose$cod6 <- factor(dMunDateDose$paciente_endereco_coIbgeMunicipio, mcod6)
sum(is.na(dMunDateDose$cod6))
sum(dMunDateDose$N[is.na(dMunDateDose$cod6)])

head(dMunDateDose[is.na(dMunDateDose$cod6), ])
tail(dMunDateDose[is.na(dMunDateDose$cod6), ])

table(iisel <- !is.na(dMunDateDose$cod6) & !is.na(dMunDateDose$fdate))

wvac <- lapply(dlab, function(d) {
    with(dMunDateDose[iisel & (dMunDateDose$vacina_descricao_dose==d), ],
         tapply(N, list(mun=paciente_endereco_coIbgeMunicipio, date=fdate), sum))
})

str(wvac)

imu <- pmatch(rownames(wvac[[1]]), p19mun[,1])
mu.uf <- sapply(imu, function(i) {
    nch <- nchar(p19mun[i, 2])
    c(substr(p19mun[i, 2], 1, nch-4),
      substr(p19mun[i, 2], nch-2, nch-1))
})
head(t(mu.uf))

attr(wvac, 'City') <- mu.uf[1,]
attr(wvac, 'Province') <- mu.uf[2,]
attr(wvac, 'Country') <- rep('BR', ncol(mu.uf))

save(wvac,
     file='data/wvac.RData',
     compress='xz')

