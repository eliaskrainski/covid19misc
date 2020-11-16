
if (FALSE)
    setwd("..")

system.time(lsesa <- read.csv2('data/sesa-pr-geral.csv'))

dim(lsesa)
head(lsesa)

str(lsesa)

ddate <- lapply(
    lsesa[, grep('DATA', names(lsesa))], function(x) 
        as.Date(x, '%d/%m/%y'))

sapply(ddate, summary)

names(ddate)
lapply(ddate[-1], function(x)
    summary(as.numeric(difftime(x, ddate[[1]], units='days'))))


t(as.data.frame(lapply(ddate, range, na.rm=TRUE)))

icwb <- lsesa[, c('IBGE_RES_PR', 'IBGE_ATEND_PR')]=='4106902'

addmargins(table(Residencia=icwb[,1],
                 Atendimento=icwb[,2]))

lsesa$cmun <- factor(
    lsesa$IBGE_RES_PR,
    setdiff(unique(lsesa$IBGE_RES_PR), '9999999'))

lsesa$acmun <- factor(
    lsesa$IBGE_ATEND_PR,
    setdiff(unique(lsesa$IBGE_ATEND_PR), '9999999'))

if (!any(ls()=='alldates'))
    alldates <- gsub(
        '-', '', sort(unique(
                     ddate$DATA_CONFIRMACAO_DIVULGACAO)))

lsesa$fdate <- factor(
    gsub('-', '', ddate$DATA_CONFIRMACAO_DIVULGACAO),
    alldates)

table(lsesa$OBITO)

system.time(wco.pr <- list(
                table(lsesa[, c('cmun', 'fdate')]),
                table(lsesa[lsesa$OBITO%in%c('Sim', 'SIM'),
                            c('cmun', 'fdate')])
    ))

if (FALSE) {

    system.time(wco.pr.a <- list(
                    table(lsesa[, c('acmun', 'fdate')]),
                    table(lsesa[lsesa$OBITO%in%c('Sim', 'SIM'),
                                c('acmun', 'fdate')])
                ))

    iic <- which(rownames(wco.pr[[1]])=='4106902')
    iic.a <- which(rownames(wco.pr.a[[1]])=='4106902')
    c(iic, iic.a)

    sapply(wco.pr, dim)
    sapply(wco.pr.a, dim)

    x0 <- as.Date(colnames(wco.pr[[1]]), '%Y%m%d')

    library(lubridate)
    x0w <- epiweek(x0)

    n2cwb <- rbind(colSums(t3), t3[2,])
    
    par(mfrow=c(2,2), mar=c(3,4,0,0), mgp=c(3,0.5,0), las=1)
    for (k in 1:2) {
        plot(x0+0.3, wco.pr[[k]][iic,], type='h',
             ylim=range(wco.pr[[k]][iic,],
                        wco.pr.a[[k]][iic.a, ]),
             xlab='', ylab='Casos confirmados (novos)')
        points(x0-0.3, wco.pr.a[[k]][iic.a,], type='h', col=2)
        points(as.Date('2020-01-01') + 7*unique(x0w)-7, 
               tapply(wco.pr[[k]][iic,], x0w, sum)/7, pch=19)
        points(as.Date('2020-01-01') + 7*unique(x0w)-7, 
           tapply(wco.pr.a[[k]][iic.a,], x0w, sum)/7, pch=19, col=2)
        if (FALSE) {
            points(as.Date(colnames(t3)),
                   n2cwb[k,], col=4, type='h')
            points(as.Date('2020-01-01') +
                   7*unique(epiweek(as.Date(colnames(t3))))-7,
                   tapply(n2cwb[k,],
                          epiweek(as.Date(colnames(t3))), mean),
                   col=4, pch=19)
        }
        legend('topleft',
               c('Residentes em Curitiba (SESA)',
                 'Atendimento em Curitiba (SESA)',
                 'Casos confirmados em Curitiba (SM)'),
               pch=19, col=c(1:2, 4), bty='n',
               title='Média semanal (epidemiológica)')
        
        plot(x0+0.3, cumsum(wco.pr[[k]][iic,]), type='l',
             ylim=c(0.7, max(sum(wco.pr[[k]][iic,]),
                             sum(wco.pr.a[[k]][iic.a, ]),
                             sum(n2cwb[k,]))),
             xlab='', ylab='Casos confirmados (acumulados)')
        lines(x0-0.3, cumsum(wco.pr.a[[k]][iic.a,]), col=2)
        if (FALSE)
            lines(as.Date(colnames(t3)),
              cumsum(n2cwb[k,]), col=4)
        
        legend('topleft',
               c('Residentes em Curitiba (SESA)',
                 'Atendimento em Curitiba (SESA)',
                 'Casos confirmados em Curitiba (SM)'),
               lty=1, col=c(1:2, 4), bty='n')
    }

}


sapply(wco.pr, sum)
table(lsesa$OBITO[is.na(lsesa$cmun)])

sapply(wco.pr, dim)

system.time(waco.pr <- lapply(lapply(
                wco.pr, apply, 1, cumsum), t))
str(waco.pr)

waco.pr[[1]][1:5, 1:5]
waco.pr[[1]][-3:0+nrow(waco.pr[[1]]),
             -3:0+ncol(waco.pr[[1]])]

sapply(waco.pr, function(m)
    colSums(m[, -3:0+ncol(m)]))


if (FALSE) {

    x0 <- as.Date(colnames(wco.pr[[1]]), '%Y%m%d')

    par(mar=c(3,4,0.5,0.5))
    plot(x0,
         colSums(wco.pr[[1]])+0.35, ylim=c(1, 4e3),
         log='y', las=1, xlab='', ylab='', type='o')
    lines(x0,
         wco.pr[[1]][which(rownames(wco.pr[[1]])%in%'4106902'),]+0.35,
         type='o', pch=8, col=4)

}
