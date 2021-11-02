
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

load(file='data/v2tab.RData')

attdvtime <- difftime(
    Sys.time(),
    attr(v2tab, 'updated'), units='hours')
attdvtime

if(floor(attdvtime)>19) {
    
    dv2tab <- function(uf='PR', verbose=FALSE) {
        
        rfl <- paste0('RData/dv_', uf, '.RData')
        attach(rfl) # safer and will warn about masked objects w/ same name in .GlobalEnv
        if(verbose) cat('data loaded: dim =', dim(ufdv), '\n')
        
	vl <- unique(ufdv$vacina_nome)
	ll <- rep('AZ', length(vl))
	ll[grep('Coronav', vl)] <- 'Coronavac'
        ll[grep('CORONAV', vl)] <- 'Coronavac'
	ll[grep('fizer', vl)] <- 'Pfizer'
	ll[grep('FIZER', vl)] <- 'Pfizer' 
	ll[grep('anssen', vl)] <- 'Janssen'
	ll[grep('ANSSEN', vl)] <- 'Janssen'

	vacina <- factor(ufdv$vacina_nome, vl, ll)

        if(verbose) cat("'vacina' created\n")
        
        if(verbose>999) {
            print(table(ufdv$vacina_nome, vacina))
            print(table(vacina, ufdv$vacina_descricao_dose))
        }
        
        ufdv$vacina_nome <- vacina
        rm(vacina)
        gc(reset=TRUE)
       
        dl0 <- unique(ufdv$vacina_descricao_dose)
        dl <- rep('R', length(dl0))
	dl[intersect(grep('1', dl0), grep('ose', dl0))] <- '1'
	dl[intersect(grep('1', dl0), grep('OSE', dl0))] <- '1'
	dl[intersect(grep('2', dl0), grep('ose', dl0))] <- '2/u'
	dl[intersect(grep('2', dl0), grep('OSE', dl0))] <- '2/u'
	dl[grep('icial', dl0)] <- '1'
	dl[grep('ICIAL', dl0)] <- '1'
	dl[grep('nica', dl0)] <- '2/u'
	dl[grep('NICA', dl0)] <- '2/u'
	dl[dl0%in%c('Dose', 'Dose ', 'DOSE', 'DOSE ')] <- '2/u'
	dl[union(grep('Re', dl0), grep('RE', dl0))] <- 'R'

        dose <- factor(ufdv$vacina_descricao_dose, dl0, dl)
        if(verbose) cat("'dose' created\n")

        if(verbose>999) {
            print(table(ufdv$vacina_descricao_dose, dose))
            print(table(ufdv$vacina_nome, dose))
	}

        ufdv$vacina_descricao_dose <- dose
        rm(dose)
        gc(reset=TRUE)

        wMax <- (as.integer(difftime(
            Sys.Date(),
            as.Date('2021-01-03'), units='days')) %/% 7)+1
        
        ufdv$vacina_dataaplicacao <-
            factor((as.integer(difftime(
                       as.Date(ufdv$vacina_dataaplicacao),
                       as.Date('2021-01-03'), units='days')) %/% 7)+1,
                   3:wMax)
        
        if(verbose) cat("epidemiological week created\n")
        
        if(verbose>999)
            print(table(ufdv$vacina_dataaplicacao))
        
        ufdv$paciente_idade <-
            cut(ufdv$paciente_idade, 5*c(0:18, Inf), right=FALSE)
        
        if(verbose) cat("age group created\n")
        
        if(verbose>999)
            print(table(ufdv$paciente_idade))
        
        if(verbose>999)
            print(table(ufdv$paciente_enumsexobiologico))
        
        ufdv$paciente_enumsexobiologico <-
            factor(ufdv$paciente_enumsexobiologico, c('F', 'M'))
        
        if(verbose) cat("gender cleared\n")
        
        if(verbose>999)
            print(table(ufdv$paciente_enumsexobiologico))
        
        ufdv$paciente_endereco_coibgemunicipio <-
            factor(ufdv$paciente_endereco_coibgemunicipio,
                   locc, locc)
        
        if(verbose) cat("local cleared\n")
        
        if(verbose>999)
            print(str(table(ufdv$paciente_endereco_coibgemunicipio)))
        
        tab <- table(ufdv[c(3,4,1,2,6,5)])
        detach()
        return(tab)
        
    }

    t1 <- Sys.time()
    v2tab <- dv2tab('SP', 10000)
    cat('SP : ', Sys.time()-t1, '\n')

    cat("'dim(v2tab)' =", dim(v2tab), '\n')
    
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
    
    for(u in setdiff(uftb$UF, 'SP')) {
        tt <- Sys.time()
        cat('uf = ', u, ' ... ')
        v2tab <- v2tab + dv2tab(u, FALSE)
        cat(Sys.time()-tt)
        cat(' done!\n')
    }
    
    sum(v2tab)
    
    round(addmargins(apply(v2tab, 5:6, sum)/1e6), 1)
    
    round(addmargins(apply(v2tab, 5:6, sum)/212e4), 1)

    ii.mun <- (1+5+27+1):dim(v2tab)[1]
    length(ii.mun)
    
    v2tab[1,,,,,] <- apply(v2tab[(1+5+27+1):dim(v2tab)[1],,,,,], 2:6, sum)
    
    sum(v2tab[1,,,,,])
    
    for(i in 1:5) {
        ii <- which(substr(loc.full.names[ii.mun], 1, 1)==i)
        v2tab[1+i,,,,,] <- apply(v2tab[1+5+27+ii,,,,,], 2:6, sum)
    }
    
    i2i.uf <- pmatch(loc.full.names[(1+5+1):(1+5+27)], uftb$STATE)
    
    for(i in 1:27) {
        ii <- which(substr(loc.full.names, 1, 2)==rownames(uftb)[i2i.uf[i]])
    ##    cat(loc.full.names[6+i],
      ##      length(ii), sum(v2tab[ii,,,,,])/1e6, '\n')
        v2tab[6+i,,,,,] <- apply(v2tab[ii,,,,,,drop=FALSE], 2:6, sum)
    }
    
    print(c(br=sum(v2tab[1,,,,,]),
            rg=sum(v2tab[2:6,,,,,]),
            uf=sum(v2tab[c(1+5+1):(1+5+27),,,,,]),
            muns=sum(v2tab[ii.mun,,,,,]))/1e6)
    
    dimnames(v2tab)[[1]] <- locl
    
    attr(v2tab, 'updated') <- Sys.time()
    
    save(list='v2tab',
         file='data/v2tab.RData')
    
}

print(Sys.time() -t0)
