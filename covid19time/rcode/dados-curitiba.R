
if (FALSE)
    setwd('..')

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/CasosCovid19/'

k <- 0
repeat {
    ufl <- paste0(u0,  Sys.Date() + k,
                  '_Casos_Covid_19_-_Base_de_Dados.csv')
    if (class(try(download.file(ufl, 'data/casosCuritibaSM.csv'),
                  TRUE))=='try-error') {
        k <- k-1
    } else {
        break
    }
}

dcwb <- read.csv2('data/casosCuritibaSM.csv', encoding='latin1')
head(dcwb)

dcwb$date <- as.Date(dcwb[,2], '%d/%m/%Y')
summary(dcwb$date)

if (!any(ls()=='alldates'))
    alldates <- gsub('-', '', seq(as.Date('20200122', '%Y%m%d'),
                    Sys.Date(), 1))

dcwb$fdate <- factor(gsub('-', '', 
                          dcwb$date,
                          fixed=TRUE), alldates)
tail(dcwb)

t3 <- table(dcwb$EVOLU, dcwb$fdate)
str(t3)

t3[, -5:0+ncol(t3)]

(jj <- pmatch(paste0('202011', 14:19), 
              colnames(t3)))
t3[1, jj] <- c(715, 750, 758, 879, 914, 1381)-t3[2,jj]
t3[2, jj] <- c(6, 5, 5, 13, 11, 9)
t3[3, jj] <- c(52084-sum(t3[3, 1:(min(jj)-1)]),
               200, 52438-52084-200,
               52704-52084, 53342-52704,
               54013-52704)

t3[, -5:0+ncol(t3)]

t3a <- apply(t3, 1, cumsum)
tail(t3a)
tail(rowSums(t3a))

wcwb <- list(cumsum(colSums(t3)),
             cumsum(t3[2,]))
str(wcwb)

##plot(diff(c(0,wcwb[[1]])))

print(sapply(wcwb, function(x) tail(x)))

