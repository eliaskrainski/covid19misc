
if (FALSE)
    setwd('..')

ufl <- paste0('http://dadosabertos.c3sl.ufpr.br/curitiba/CasosCovid19/',
              Sys.Date(),
              '_Casos_Covid_19_-_Base_de_Dados.csv')
ufl

library(RCurl)
if (url.exists(ufl))
    download.file(ufl, 'data/cwb_covid.csv')

dcwb <- read.csv2('data/cwb_covid.csv', encoding='latin1')
head(dcwb)

dcwb$date <- as.Date(dcwb$DATA.INC, '%d/%m/%Y')
summary(dcwb$date)

if (!any(ls()=='alldates'))
    alldates <- gsub('-', '', sort(unique(dcwb$date)))
dcwb$fdate <- factor(gsub('-', '', 
                          dcwb$date,
                          fixed=TRUE), alldates)
tail(dcwb)

t3 <- table(dcwb$EVOLU, dcwb$fdate)
str(t3)

t3[, -5:0+ncol(t3)]

(jj <- pmatch(paste0('202011', 14:18), 
              colnames(t3)))
t3[1, jj] <- c(715, 750, 758, 879, 914)-t3[2,jj]
t3[2, jj] <- c(6, 5, 5, 13, 11)
t3[3, jj] <- c(52084-sum(t3[3, 1:(min(jj)-1)]),
               200, 52438-52084-200,
               52704-52084, 53342-52704)

t3[, -5:0+ncol(t3)]

t3a <- apply(t3, 1, cumsum)
tail(t3a)
tail(rowSums(t3a))

wcwb <- list(cumsum(colSums(t3)),
             cumsum(t3[2,]))
str(wcwb)

##plot(diff(c(0,wcwb[[1]])))

print(sapply(wcwb, function(x) tail(x)))

