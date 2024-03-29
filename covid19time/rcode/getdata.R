if (FALSE)
    setwd('..')

if(!any(ls()=='dupdate'))
    dupdate <- TRUE

if(dupdate)
    options(timeout=60*30)

owid.f <- function(d=TRUE) {
    download.file(
        paste0('https://raw.githubusercontent.com/',
               'owid/covid-19-data/master/public/',
               'data/vaccinations/vaccinations.csv'),
        'data/vaccinations.csv')
}

cssegis.f <- function(d=TRUE) {
### download the cssegis data (the famous one) 
### The base url for the data source 
    url0.csse <- paste0(
        'https://raw.githubusercontent.com/',
        'CSSEGISandData/COVID-19/master/',
        'csse_covid_19_data/csse_covid_19_time_series/',
        'time_series_covid19_')
    
### The root names of the three derired  data files
    vnames <- c('confirmed', 'deaths')###, 'recovered')
    
### (may or) may not download the files again
    if (d)
        for (fl in vnames)
            download.file(paste0(url0.csse, fl, '_global.csv'),
                          paste0('data/', fl, '_global.csv'))
    return(invisible())
}

uss.f <- function(d=TRUE) {
### USdata
    us.fl <- 'data/us-states.csv'
    us.url <- paste0(
        'https://raw.githubusercontent.com/nytimes/',
        'covid-19-data/master/',
        'us-states.csv')
    if (d)
        download.file(us.url, us.fl)
    return(invisible())
}

usc.f <- function(d=TRUE) {
### US counties data
    if (d)
        download.file(
            'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv',
            'data/us-counties.csv')
    return(invisible())
}

### the official data from the Brazilian Health Ministry
###  is at https://covid.saude.gov.br/
brms.f <- function(d=TRUE) {
    res.url <-
        httr::GET("https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral",
                  httr::add_headers("X-Parse-Application-Id" =
                                        "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"))
    url <- httr::content(res.url)$results[[1]]$arquivo$url 
    if (d) {
        system('rm HIST_*.csv')
        if (substring(url, nchar(url)-2)=='.7z') {
            download.file(url, 'brms.7z')
            system('7z e brms.7z')
            system('rm brms.7z')
        }
        if (substring(url, nchar(url)-2)=='zip') {
            download.file(url, 'brms.zip')
            unzip('brms.zip')
            system('rm brms.zip')
        } 
        if (substring(url, nchar(url)-2)=='rar') {
            download.file(url, 'brms.rar')
            system('unrar e brms.rar')
            system('rm brms.rar')
        }
        hpfls <- system('ls HIST_PAINEL*.csv', TRUE)
        if(length(hpfls)>0) {
            system('rm data/HIST_*')
            for(hpfl in hpfls)
                system(paste0('mv ', hpfl, ' data/', hpfl))
        }
        if (substring(url, nchar(url)-2)=='csv') {
            download.file(url, 'data/HIST_PAINEL_COVIDBR.csv')
        } 
    } else return(url)
    return(invisible())
}

### time series by municipalities
### area available at brazil.io
brio.f <- function(d=FALSE) {
    url.br <- paste0('https://data.brasil.io/',
                     'dataset/covid19/caso.csv.gz')
    
    if (d)
        try(download.file(
            url.br, 'data/caso.csv.gv'), TRUE)
    return(invisible())
}

### data from the braziliam deaths registry
brio.oc.f <- function(d=FALSE) {
    url.c <- 'https://data.brasil.io/dataset/covid19/obito_cartorio.csv.gz'
    if(d)
        try(download.file(
            url.c, 'data/obito_cartorio.csv.gv'), TRUE)
    return(invisible())
}

wcota.f <- function(d=TRUE) {
### Brazilian data put together by Wesley Cota
    
    urlwc <- 'https://raw.githubusercontent.com/wcota/covid19br/master/'
    urlwc <- 'https://github.com/wcota/covid19br/raw/master/'
    wc.fl <- 'cases-brazil-cities-time.csv'
    wc.fl <- 'cases-brazil-cities-time.csv.gz'
    
    if (d) 
        download.file(paste0(urlwc, wc.fl),
                      paste0('data/', wc.fl))
    return(invisible())
}

fnd.f <- function(d=FALSE) {
### DEST-UFMG data
    if (d) 
        download.file(paste0(
            'https://github.com/dest-ufmg/',
            'covid19repo/blob/master/data/',
            'cities.rds?raw=true'),
            'data/cities.rds')
    return(invisible())
}

sesa.f <- function(d=FALSE) {
    
    if (d) {
        ldate <- Sys.Date()
        ldate
        
        CAP <- 0
        
        options(show.error.messages = FALSE)
        
        repeat {
            
            yyyy <- format(ldate, '%Y')
            mm <- format(ldate, '%m')
            dd <- format(ldate, '%d')
            
            fldt <- paste0(yyyy, '-', mm, '/informe_epidemiologico_',
                           dd, '_', mm, '_', yyyy, '_g')
            if (CAP>0)
                fldt <- toupper(fldt)
            
            sesa.fl <- paste0('http://www.saude.pr.gov.br/sites/default/',
                              'arquivos_restritos/files/documento/',
                              fldt, 'eral.csv')
            sesa.fl
            
            tmp <- try(download.file(
                sesa.fl, 'data/sesa-pr-geral.csv'))
            if (class(tmp)=='try-error') {
                if (CAP>1) {
                    ldate <- ldate-1
                    CAP <- 0
                } else {
                    CAP <- CAP + 1
                }
            } else {
                break
            }
            
        }
        options(show.error.messages = TRUE)
        
    }
    
    ## https://www.saude.pr.gov.br/sites/default/arquivos_restritos/files/documento/2020-11/informe_epidemiologico_08_11_geral.csv
    ## https://www.saude.pr.gov.br/sites/default/arquivos_restritos/files/documento/2020-11/informe_epidemiologico_08_11_obitos_casos_municipio.csv
    
    if (FALSE) {

        library(data.table)
        system.time(ses <- fread('data/sesa-pr-geral.csv'))
        head(ses)

        summary(ses$Data <- as.Date(ses$DATA_CONFIRMACAO_DIVULGACAO))
        
        table(factor(ses$OBITO, c('Não', 'NÃO', '', 'Sim', 'SIM'),
                     rep(c('n', 's'), c(3,2))), ses$OBITO)
        table(factor(ses$OBITO, c('Não', 'NÃO', '', 'Sim', 'SIM'),
                     rep(c('n', 's'), c(3,2))), ses$STATUS)
        
    }
    
    return(invisible())
}

gmob.f <- function(d=TRUE) {
    if (d) {
### mobility data from Google
        
        mfl <- 'Global_Mobility_Report.csv'
        ##system(paste0('wget https://www.gstatic.com/covid19/mobility/',
        ##            mfl, ' -O data/', mfl))
        download.file(paste0('https://www.gstatic.com/covid19/mobility/',mfl),
                      paste0('data/', mfl))
        
    }
    return(invisible())
}

amob.f <- function(d=TRUE) {
    ## tip from
    ## https://kieranhealy.org/blog/archives/2020/05/23/get-apples-mobility-data/
    get_apple_target <- function(cdn_url = "https://covid19-static.cdn-apple.com",
                                 json_file = "covid19-mobility-data/current/v3/index.json") {
        tf <- tempfile(fileext = ".json")
        curl::curl_download(paste0(cdn_url, "/", json_file), tf)
        json_data <- jsonlite::fromJSON(tf)
        paste0(cdn_url, json_data$basePath, json_data$regions$`en-us`$csvPath)
    }
    
    aurl <- get_apple_target()
    amfl <- tail(strsplit(aurl, '/')[[1]], 1)
    
    if (d) {    
        ##system(paste0('wget ', aurl, ' -O data/', amfl))
        download.file(aurl, paste0('data/', amfl)) 
    } else return(aurl)
    return(invisible())
}


gus.f <- function(d=TRUE) {
    cssegis.f(d)
    uss.f(d)
    usc.f(d)
}

others.f <- function() {
    gus.f(TRUE)
    wcota.f(TRUE)
    brms.f(TRUE)
    amob.f(TRUE)
}

if(dupdate) {

owid.f(TRUE)

    if(TRUE) {

        if(Sys.info()['nodename']=='pataxo') {
            others.f()
            gmob.f()
            sesa.f(TRUE)
        } else {
            gus.f(TRUE)
        }
        
    } else {
    
    library(parallel)
    (ncores <- as.integer(detectCores()/2))

    if (ncores<39) {
        cat('1\n')
        system.time(mclapply(list(
            others='others.f()',
            google='gmob.f()'), 
            function(x)
                ##        eval(str2lang(x))
                eval(parse(text=x)), 
            mc.cores = ncores))
    } else {
        if (ncores<69) {
            cat('2\n')
            system.time(mclapply(list(
                gus='gus.f()',
                brms='brms.f()',    
                ##brio='brio.f()',
                ##wcota=wcota.f(),
                ##fnd='fnd.f()',
                ##sesa='sesa.f()')
                apple='amob.f()',
                google='gmob.f()'), 
                function(x)
                    ##        eval(str2lang(x))
                    eval(parse(text=x)), 
            mc.cores = ncores))
        } else {
            cat('3\n')
            system.time(mclapply(list(
                global='cssegis.f()',
                uss='uss.f()',
                usc='usc.f()',
                brms=brms.f(),
                ##brio='brio.f()',
                ##wcota='wcota.f()',
                ##fnd='fnd.f()',
                ##sesa='sesa.f()')
                apple='amob.f()',
                google='gmob.f()'),
                function(x)
                    ##        eval(str2lang(x))
                    eval(parse(text=x)),
                mc.cores = ncores))
        }
      }	
    }	
}	

##lapply(list('uss.f()'), function(x) eval(call(x)[[1]]))
##lapply(list('uss.f()'), function(x) eval(parse(text=x)))

