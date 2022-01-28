
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

if(TRUE) {##if(floor(attdvtime)>19) {
    
    dv2tab <- function(part, verbose=FALSE) {
        
        rfl <- paste0('RData/dv_', sprintf('%05d', part), '.RData')
        attach(rfl) # safer and will warn about masked objects w/ same name in .GlobalEnv
        if(verbose) cat('data loaded: dim =', dim(dvpart), '\n')
        
	vl <- unique(dvpart$vacina_nome)
	ll <- rep('AZ', length(vl))
	ll[grep('Coronav', vl)] <- 'Coronavac'
        ll[grep('CORONAV', vl)] <- 'Coronavac'
	ll[grep('fizer', vl)] <- 'Pfizer'
	ll[grep('FIZER', vl)] <- 'Pfizer' 
	ll[grep('anssen', vl)] <- 'Janssen'
	ll[grep('ANSSEN', vl)] <- 'Janssen'

	vaclab <- c('AZ', 'Coronavac', 'Janssen', 'Pfizer')
        vacina <- factor(factor(dvpart$vacina_nome, vl, ll), vaclab)

        if(verbose) cat("'vacina' created\n")
        
        if(verbose>999) {
            print(table(dvpart$vacina_nome, vacina))
            print(table(vacina, dvpart$vacina_descricao_dose))
        }
        
        dvpart$vacina_nome <- vacina
        rm(vacina)
        gc(reset=TRUE)
       
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

        dose <- factor(factor(dvpart$vacina_descricao_dose, dl0, paste0('D', dl)),
                       paste0('D', 1:3))
        if(verbose) cat("'dose' created\n")

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
        
        if(verbose) cat("epidemiological week created\n")
        
        if(verbose>999)
            print(table(dvpart$vacina_dataAplicacao))
        
        dvpart$paciente_idade <-
            cut(dvpart$paciente_idade, 5*c(0:18, Inf), right=FALSE)
        
        if(verbose) cat("age group created\n")
        
        if(verbose>999)
            print(table(dvpart$paciente_idade))
        
        if(verbose>999)
            print(table(dvpart$paciente_enumSexoBiologico))
        
        dvpart$paciente_enumSexoBiologico <-
            factor(dvpart$paciente_enumSexoBiologico, c('F', 'M'))
        
        if(verbose) cat("gender cleared\n")
        
        if(verbose>999)
            print(table(dvpart$paciente_enumSexoBiologico))
        
        dvpart$paciente_endereco_coIbgeMunicipio <-
            factor(dvpart$paciente_endereco_coIbgeMunicipio,
                   locc, locc)
        
        if(verbose) cat("local cleared\n")
        
        if(verbose>999)
            print(str(table(dvpart$paciente_endereco_coIbgeMunicipio)))
        
        tab <- table(dvpart[c(3,4,1,2,6,5)])
    
        attr(tab, 'dataupdate') <- maxdate
        
        detach()
        return(tab)
        
    }

    t1 <- Sys.time()
    v2tab <- dv2tab(0, 10000)
    cat('part_00000 : ', Sys.time()-t1, '\n')

    cat("'dim(v2tab)' =", dim(v2tab), '\n')
    print(tail(dimnames(v2tab),4))
    

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

    dataup <- attr(v2tab, 'dataupdate')
    for(u in 1:9) {
        tt <- Sys.time()
        cat('part', u, '... ')
        b <- dv2tab(u, FALSE)
        d2 <- attr(b, 'dataupdate')
        v2tab <- v2tab + b
        dataup <- max(dataup, d2)
        cat(' data update on',
            as.Date(dataup, origin='1960-01-01'),
            'cpu time: ')
        cat(Sys.time()-tt)
        cat(' done!\n')
    }
    attr(v2tab, 'dataupdate') <- dataup
    
    sum(v2tab)
    
    round(addmargins(apply(v2tab, 5:6, sum)/1e6), 1)
    
    round(addmargins(apply(v2tab, 5:6, sum)/213e4), 1)

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

    if(FALSE) {

        vtr <- apply(v2tab[1:(1+5+27),,,,,], c(1,6),sum)
        names(dimnames(vtr)) <- c('local', 'dose')
        print(vtr)
        
        print(round(100*vtr/
                    apply(w2pop[1:(1+5+27),,], 1, sum), 2))
                
    }
    
    attr(v2tab, 'updated') <- Sys.time()
    
    save(list='v2tab',
         file='data/v2tab.RData')
    
}

print(Sys.time() -t0)
