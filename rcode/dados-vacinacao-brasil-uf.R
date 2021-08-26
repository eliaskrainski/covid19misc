
if(FALSE)
    setwd('..')

t00 <- Sys.time()

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

### colnames of the files
fl1 <- system('ls data/vacinacao/*', TRUE)[1]
fl1

library(data.table)
tmp <- fread(fl1, nrows=0)
colnames(tmp)

### desired variables 
xsel <- colnames(tmp)[c(3, 5, 8, 28, 29, 31)] 
jj <- pmatch(xsel, colnames(tmp))
jj

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
            fl.j <- paste0('data/vacinacao/vac_', lfls[j])
            if(!file.exists(fl.j))
                download.file(ufls[j], fl.j)
        }
    } 
    return(paste0('data/vacinacao/vac_', lfls))
}

if(!any(ls()=='dupdate'))
    dupdate <- TRUE 
if(FALSE)
    dupdate <- FALSE

##if((!dupdate) & (length(system('ls RData'))<27))
  ## dupdate <- TRUE

if(dupdate) {
    
    options(timeout=60*300) ### to work with bad internet...
### download each UF local file and retrieve the local file names: 'data/UF_....csv"
    fls <- brvac.uf(TRUE)

    ufs <- substr(fls, 20, 21)
    ufs
    
    t1a <- Sys.time()
    for (k in 1:length(fls)) {
        t1 <- Sys.time()
        robj <- paste0('dv_', ufs[k])
        assign(robj,
               fread(fls[k], select=jj))
        save(list=robj,
             file=paste0('RData/', robj, '.RData'))
        print(Sys.time()-t1)
    }
    print(Sys.time()-t1a)

} else {

##    system.time(load('data/dvbr.RData'))
    ##  xsel <- colnames(dvbr)

    robjs <- dir('RData/')
    robjs

    library(data.table)

    mdd <- c('paciente_endereco_coibgemunicipio',
             'vacina_dataaplicacao', 
             'vacina_descricao_dose')

    cat('cod6;Date;Dose;N\n',
        file='data/tMunDateN.csv')

    rr <- lapply(robjs, function(rfl, verbose=TRUE) {
        cat('loading', rfl, '... ')
        attach(paste0('RData/', rfl)) ## safer and will warn about masked objects w/ same name in .GlobalEnv
        ufdv <- get(substr(rfl, 1, 5))
        detach()
        if(verbose) cat('loaded ', dim(ufdv), '')  
        tMunDate <- ufdv[,.N,by=mdd]
        if(verbose) cat('write ... ')
        write.table(tMunDate,
                    file='data/tMunDateN.csv',
                    append=TRUE,
                    sep=';',
                    row.names=FALSE,
                    col.names=FALSE,
                    quote=FALSE)
        if(verbose) cat('ok\n')
        return(NULL)
    })
    
    system.time(tmd <- read.csv2('data/tMunDateN.csv'))
    dim(tmd)

    head(tmd)
    tail(tmd)
    
    class(dv_SP)
    system.time(dv_SP <- as.data.frame(dv_SP))

    system.time(loc <- factor(dv_SP$paciente_endereco_coibgemunicipio, cmu))
    table(is.na(loc))

    system.time(ndose0 <- table(dv_SP$vacina_descricao_dose))
    ndose0
    
    system.time(dose <- rep('D2/u', length(loc)))
    system.time(dose[substr(dv_SP$vacina_descricao_dose,1,1)=='1'] <- 'D1')
    system.time(ndose <- table(dose))

    ndose0
    ndose

    table(is.na(dose))

    Date01 <- c(as.Date('20200121', '%Y%m%d'), Sys.Date())
    Date <- as.Date(dv_SP$vacina_dataaplicacao)
    Date[Date<Date01[1]] <- NA
    Date[Date>Date01[2]] <- NA
    table(is.na(Date))

    Date0 <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
    fdate <- factor(as.character(Date), as.character(Date0))
    
    table(dv_SP$paciente_endereco_coibgemunicipio[is.na(loc)])
    iout <- is.na(loc) |  
    
    udose <- unique(dv_SP$vacina_descricao_dose)
    dose[which(dv_SP$vacina_descricao_dose%in%
                   c("1ª Dose", "Dose Inicial "), 'D1', 'D2/U')
    table(dose, dv_SP$vacina_descricao_dose)
    
    ls()

    system.time(dvbr <- Reduce('rbind', lapply(robjs, function(x) {
        obj <- substr(x, 1, 5)
        cat('loading', obj, '... ')
        load(paste0('RData/', x))
        cat('OK!\n')
        return(get(obj))
    })))

    cat("saving 'dvbr' ... ")
    ts1 <- Sys.time()
    save(list='dvbr',
         file='data/dvbr.RData')
    cat('done in', Sys.time()-ts1, '\n')

    source('rcode/define_idade_faixas.R')
    ls()

### tabulate by gender and dose
    ds <- xsel[c(5,2)]
    n.dg <- dvbr[, .N, by=ds]
    n.dg
    
    n.doses <- tapply(n.dg$N, n.dg$vacina_descricao_dose, sum)
    n.doses <- sort(n.doses, decreasing=TRUE)
    n.doses

### dose levels, for later use
    dlevels <- names(n.doses)
    dlevels
    
### select desired levels and order it for later use
    n.dg <- setorder(
        n.dg[vacina_descricao_dose %in% dlevels[1:2] &
             paciente_enumsexobiologico %in% c('F', 'M')],
        vacina_descricao_dose,
        paciente_enumsexobiologico)
    n.dg

    iineg.i <- which(dvbr$paciente_idade<0)
    print(c('nIdade<0'=length(iineg.i)))
    dvbr$paciente_idade[iineg.i] <- NA
    
    dvbr$idade1 <- factor(
        lk1i[findInterval(dvbr$paciente_idade, b1i-1e-5)], lk1i)
    dvbr$idadeK <- factor(
        lKi[findInterval(dvbr$paciente_idade, bKi-1e-5)], lKi)

    dvbr

    table(dvbr$vacina_descricao_dose)

### tabulate by age, dose and gender
    Ids <- c('idade1', ds)
    n.Ids <- dvbr[
        vacina_descricao_dose %in% dlevels[1:2] &
        paciente_enumsexobiologico %in% c('F', 'M'),
        .N,by=Ids]
    
    dd <- as.integer(difftime(
        Sys.Date(),
        dvbr$vacina_dataaplicacao, units='days'))
    summary(dd)
    
    summary(dvbr$vacina_dataaplicacao)
    sum(dvbr$vacina_dataaplicacao<as.Date('2020-01-01'))
    sum(dvbr$vacina_dataaplicacao<as.Date('2020-05-01'))
    dd0 <- as.integer(difftime(
        Sys.Date(), as.Date('2020-01-01'), units='days'))
    dd0
    sum(dd>dd0, na.rm=TRUE)
    
    dd[dd>dd0] <- NA
    table(is.na(dd))
    summary(dd)
    hdd <- hist(dd, plot=FALSE)
    
    n.dg2 <- dvbr[dd>14 &
                  vacina_descricao_dose %in% dlevels[2] &
                  paciente_enumsexobiologico %in% c('F', 'M'),
                  .N, by=ds]
    n.dg2
    
### select desired levels and order it for later use
    n.dg2 <- setorder(
        n.dg2,
        vacina_descricao_dose,
        paciente_enumsexobiologico)
    n.dg2
    
    n.Ids2 <- dvbr[dd>14 &
                   vacina_descricao_dose %in% dlevels[2] &
                   paciente_enumsexobiologico %in% c('F', 'M'),
                   .N,by=Ids]
    
    sum(n.Ids$N)
    sum(n.Ids2$N)
    
    mdd <- c('paciente_endereco_coibgemunicipio',
             'vacina_dataaplicacao', 
             'vacina_descricao_dose')
    tMunDate <- dvbr[,.N,by=mdd]
    
    str(tMunDate)
    
    dMunDateDose <- as.data.frame(tMunDate)
    
    system.time(save(
        'dMunDateDose',
        file='data/dMunDateDose.RData',
        compress='xz'))
    
    rm(tMunDate, dMunDateDose)
    gc(reset=TRUE)
    
    dvbr$UF <- substr(dvbr$paciente_endereco_coibgemunicipio,1,2)

    if(FALSE) {
        
        t1 <- Sys.time()
        iKdsu <- c('idadeK', ds, 'UF')
        n.iKdsu <- dvbr[
            vacina_descricao_dose %in% dlevels[1:2] &
            paciente_enumsexobiologico %in% c('F', 'M'),
            .N,by=iKdsu]
        Sys.time()-t1
        
    }
    
    t1 <- Sys.time()
    t.iKdsu <- with(dvbr[vacina_descricao_dose %in% dlevels[1:2] &
                         paciente_enumsexobiologico %in% c('F', 'M')],
                    table(idadeK, paciente_enumsexobiologico,
                          vacina_descricao_dose, UF))
    Sys.time()-t1
    
    dim(t.iKdsu)
    dimnames(t.iKdsu)
    
    system.time(save(
        list='t.iKdsu',
        file='data/tiKdsu.RData'))
    
}

print(Sys.time() - t00)
    
