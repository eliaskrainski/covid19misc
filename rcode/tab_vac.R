
if(FALSE)
    setwd('..')

t0 <- Sys.time()

load('data/w2pop.RData')

loc.full.names <- dimnames(w2pop)[[1]]

locc <- substr(dimnames(w2pop)[[1]], 1, 6)
locc[1:(1+5+27)] <- dimnames(w2pop)[[1]][1:(1+5+27)]
head(locc, 40)
length(unique(head(locc, 40)))

locl <- substring(dimnames(w2pop)[[1]], 9)
locl[1:(1+5+27)] <- dimnames(w2pop)[[1]][1:(1+5+27)]
head(locl, 40)
tail(locl)

dvac2tabf <- function(part, verbose=FALSE) {
    
    rfl <- paste0('RData/dv_', sprintf('%05d', part), '.RData')
    attach(rfl) # safer and will warn about masked objects w/ same name in .GlobalEnv
    if(verbose) cat('data loaded: dim =', dim(dvpart), '\n')
    
    dl0 <- unique(dvpart$vacina_descricao_dose)
    dl <- rep('3', length(dl0))
    dl[intersect(grep('1', dl0), grep('ose', dl0))] <- '1'
    dl[intersect(grep('1', dl0), grep('OSE', dl0))] <- '1'
    dl[intersect(grep('2', dl0), grep('ose', dl0))] <- '2'
    dl[intersect(grep('2', dl0), grep('OSE', dl0))] <- '2'
    dl[grep('icial', dl0)] <- '1'
    dl[grep('ICIAL', dl0)] <- '1'
    dl[grep('nica', dl0)] <- '1'
    dl[grep('NICA', dl0)] <- '1'
    dl[dl0%in%c('Dose', 'Dose ', 'DOSE', 'DOSE ')] <- '1'
    dl[union(grep('Re', dl0), grep('RE', dl0))] <- '3'
    
    dose <- factor(factor(dvpart$vacina_descricao_dose,
                          dl0, paste0('D', dl)),
                   paste0('D', 1:3))
    if(verbose>9) cat("'dose' created\n")
    
    if(verbose>999) {
        print(addmargins(table(dvpart$vacina_descricao_dose, dose)))
        print(addmargins(table(dvpart$vacina_nome, dose)))
    }
    
    dvpart$vacina_descricao_dose <- dose
    rm(dose)
    gc(reset=TRUE)
    
    wMax <- (as.integer(difftime(
        Sys.Date(),
        as.Date('2021-01-03'), units='days')) %/% 7)+1
    
    ddate <- as.Date(dvpart$vacina_dataAplicacao)
    dvpart$vacina_dataAplicacao <-
        factor((as.integer(difftime(
                   ddate,
                   as.Date('2021-01-03'), units='days')) %/% 7)+1,
               3:wMax)
    maxdate <- max(ddate)
    rm(ddate)
    
    if(verbose>9) cat("epidemiological week created\n")
    
    if(verbose>999)
        print(table(dvpart$vacina_dataAplicacao))
    
    dvpart$paciente_idade <-
        cut(dvpart$paciente_idade, c(0, 12, 60, Inf), right=FALSE)
    
    if(verbose>9) cat("age group created\n")
    
    if(verbose>999)
        print(table(dvpart$paciente_idade))
    
    dvpart$paciente_endereco_coIbgeMunicipio <-
        factor(dvpart$paciente_endereco_coIbgeMunicipio,
               locc, locc)
    
    if(verbose>9) cat("local cleared\n")
    
    if(verbose>999)
        print(str(table(dvpart$paciente_endereco_coIbgeMunicipio)))
    
    tab <- table(dvpart[c(3,4,1,6)])

    if(verbose) cat('dim(tab) =', dim(tab), '\n')
    
    attr(tab, 'dataupdate') <- maxdate
    
    detach()
    return(tab)
    
}

t1 <- Sys.time()
vac2tab <- dvac2tabf(0, 10000)
cat('part_00000 : ', Sys.time()-t1, '\n')

cat("'dim(vac2tab)' =", dim(vac2tab), '\n')
print(str(dimnames(vac2tab)))

dataup <- attr(vac2tab, 'dataupdate')
for(u in 1:9) {
    tt <- Sys.time()
    cat('part', u, '... ')
    b <- dvac2tabf(u, 1)
    d2 <- attr(b, 'dataupdate')
    vac2tab <- vac2tab + b
    dataup <- as.character(max(dataup, d2))
    cat(' data update on',
        dataup, ##as.character(as.Date(dataup, origin='1960-01-01')),
        'cpu time: ')
    cat(Sys.time()-tt)
    cat(' done!\n')
}
attr(vac2tab, 'dataupdate') <- dataup
    
sum(vac2tab)

round(addmargins(apply(vac2tab, 3:4, sum)/1e6), 1)

round(addmargins(apply(vac2tab, 3:4, sum)/213e4), 1)

ii.mun <- (1+5+27+1):dim(vac2tab)[1]
length(ii.mun)

vac2tab[1,,,,,] <- apply(vac2tab[(1+5+27+1):dim(vac2tab)[1],,,,,], 2:4, sum)

sum(vac2tab[1,,,,,])

for(i in 1:5) {
    ii <- which(substr(loc.full.names[ii.mun], 1, 1)==i)
    vac2tab[1+i,,,,,] <- apply(vac2tab[1+5+27+ii,,,,,], 2:4, sum)
}
    
uftb <- structure(list(
    STATE = c("RONDÔNIA", "ACRE", "AMAZONAS", "RORAIMA", "PARÁ", "AMAPÁ",
              "TOCANTINS", "MARANHÃO", "PIAUÍ", "CEARÁ", "RIO GRANDE DO NORTE",
              "PARAÍBA", "PERNAMBUCO", "ALAGOAS", "SERGIPE", "BAHIA",
              "MINAS GERAIS", "ESPÍRITO SANTO", "RIO DE JANEIRO", 
              "SÃO PAULO", "PARANÁ", "SANTA CATARINA", "RIO GRANDE DO SUL", 
              "MATO GROSSO DO SUL", "MATO GROSSO", "GOIÁS", "DISTRITO FEDERAL"),
    State = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá",
              "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
              "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia",
              "Minas Gerais", "Espírito Santo", "Rio de Janeiro",
              "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul",
              "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"),
    UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI",
           "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES",
           "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")),
    row.names = c("11", "12", "13", "14", "15", "16", "17", "21", "22",
                  "23", "24", "25", "26", "27", "28", "29", "31", "32",
                  "33", "35", "41", "42", "43", "50", "51", "52", "53"),
    class = "data.frame")

i2i.uf <- pmatch(loc.full.names[(1+5+1):(1+5+27)], uftb$STATE)

for(i in 1:27) {
    ii <- which(substr(loc.full.names, 1, 2)==rownames(uftb)[i2i.uf[i]])
    vac2tab[6+i,,,,,] <- apply(vac2tab[ii,,,,,,drop=FALSE], 2:4, sum)
}

print(c(br=sum(vac2tab[1,,,,,]),
        rg=sum(vac2tab[2:6,,,,,]),
        uf=sum(vac2tab[c(1+5+1):(1+5+27),,,,,]),
        muns=sum(vac2tab[ii.mun,,,,,]))/1e6)

dimnames(vac2tab)[[1]] <- locl


attr(vac2tab, 'updated') <- Sys.time()

save(list='vac2tab',
     file='data/vac2tab.RData')

print(Sys.time() -t0)
