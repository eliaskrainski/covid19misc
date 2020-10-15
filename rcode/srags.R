if (FALSE)
    setwd('..')

options(width=70)

source('rcode/load-srags-all.R')

t(sapply(sragsl, dim))

ny <- length(sragsl)

n.gI.y <- sapply(sragsl, function(x) table(x$gIdade))
n.gI.y

round(addmargins(n.gI.y,1)/1e3)

### tabelas por grupo de idade, evolução e classificação 
tab.gI.evol <- lapply(sragsl, function(x)
    table(x$gIdade, x$Evolução, x$Classificação))

sapply(tab.gI.evol, apply, 3, sum)
sapply(tab.gI.evol, apply, 2, sum)

apply(tab.gI.evol[[1]], 3, colSums)

op.gI.evol <- lapply(tab.gI.evol, function(m)
    apply(m, 3, function(x) x[,2]/rowSums(x)))

cores4 <- c('blue', 'red', gray(c(0.5, 0.7)))
coresc <- rgb(0:5/5, 1-2*abs(0:5/5-0.5), 5:0/5)
coresc2 <- rgb(3:8/8, 1-1.8*abs(0:5/5-0.5), 8:3/8)
coresy <- rgb(1:12/12, 1-2*abs(1:12/12-0.5), 12:1/12)
cyshad <- rgb(1:12/12, 1-2*abs(1:12/12-.5), 12:1/12, 0.3)

par(mfrow=c(3,4), mar=c(3,3,0.15,0.15), mgp=c(2,0.5,0))
for (y in 1:ny)
    barplot(apply(tab.gI.evol[[y]], 3, colSums),
            beside=TRUE, col=cores4)
legend('topleft', levels(sragsl[[ny]]$Evolução),
       fill=cores4, bty='n')

y.class <- sapply(sragsl, function(x)
    table(x$Classificação))
colSums(y.class)/sapply(sragsl, nrow)

oy.class <- sapply(sragsl, function(x)
    table(x$Classificação[x$Evolução=='Óbito']))

y.evol <- sapply(sragsl, function(x)
    table(x$Evolução))
y.evol
round(1e4*y.evol[2,]/colSums(y.evol))

barplot(oy.class/y.class,
        beside=TRUE, col=coresc)

par(mfrow=c(1,1), mar=c(3,4,0.15,0.15), mgp=c(2,0.5,0))
barplot(y.class, beside=TRUE, col=coresc, las=1)
abline(h=pretty(par()$usr[3:4]), lty=2, col=gray(0.5,0.3))
legend('topleft', lab.class.fin, 
       fill=coresc, bty='n')

png('figures/mortalidade-srags-ano-Classificação.png', 700, 500)
par(mfrow=c(2,2), mar=c(3,3,0.15,0.15), mgp=c(2,0.5,0))
for (d in c(2:3, 6,7)) {
    plot(mid, op.gI.evol[[1]][,1], las=1, ylim=c(0,0.7),
         xlab='Idade', type='n', 
         ylab='Proporção de SRAGs que evoluiram a óbito')
    for (a in 1:ny) {
        if ((d==5) & (a==ny)) {
            y <- tab.gI.evol[[a]][,2,d]
            n <- rowSums(tab.gI.evol[[a]][,,d])
            polygon(c(mid, rev(mid), mid[1]),
                    c(qbeta(0.025, y, n-y),
                      rev(qbeta(0.975, y, n-y)),
                      qbeta(0.025, y[1], n[1]-y[1])),
                    col=cyshad[a], border=cyshad[a])
        }
        lines(mid, op.gI.evol[[a]][,d],
              col=coresy[a], lwd=2, lty=3-2*(a==ny))
    }    
    abline(h=0:10/10, lty=2, col=gray(0.7, 0.5))
    n.d.y <- sapply(tab.gI.evol, apply, 3, sum)[d,]
    n.d.y <- sapply(tab.gI.evol, apply, 3, sum)[d,]
    legend('topleft',
           paste0(2008+1:ny, ' : ', n.d.y)[n.d.y>0], 
           col=coresy[n.d.y>0], ncol=3, bty='n',
           title=lab.class.fin[d], 
           lty=rep(c(3,1), c(ny-1, 1)), lwd=4, bg=gray(0.99))
}
dev.off()
if (FALSE)
    system('eog figures/mortalidade-srags-ano-Classificação.png &')

addmargins(tab.gI.evol[[ny]][,,4])

c20.sem <- table(
    sragsl[[ny]]$Classificação,
    sragsl[[ny]]$SEM_NOT)

png('figures/semana-notifica-classificação.png', width=700, height=400)
par(mar=c(3,3,0,0), mgp=c(2,0.5,0))
barplot(c20.sem+0.5, beside=TRUE, col=coresc, las=1, ##log='y',
        xlab='Semana de Notificação',
        legend.text=lab.class.fin, args.legend=list(x='topleft'))
dev.off()
if (FALSE)
    system('eog figures/semana-notifica-classificação.png &')

tab.gI.class <- table(sragsl[[ny]]$gIdade, sragsl[[ny]]$Classificação)
tab.gI.class

p.gI.class.1 <- prop.table(tab.gI.class,1)

png('figures/classificação-gIdade2020.png', width=700, height=400)
par(mar=c(3,3,0,0), mgp=c(2,0.5,0))
plot(mid, p.gI.class.1[,5], type='n',
     ylim=c(0, max(p.gI.class.1)), las=1,
     xlab='Idade',
     ylab='Proporção de classificação do PCR')
for (j in 1:5)
    lines(mid, p.gI.class.1[, j], col=coresc[j], lwd=3)
legend('topright', lab.class.fin, col=coresc, ncol=3, lwd=3, bty='n')
abline(h=0:10/10, lty=2, col=gray(0.5, 0.3))
dev.off()
if (FALSE)
    system('eog figures/classificação-gIdade2020.png &')

png('figures/classificação-gIdade.png', 800, 800)
par(mfrow=c(3,3), mar=c(3,3,0,0), mgp=c(1.5,0.5,0))
for (a in 1:ny) {
    n <- table(sragsl[[a]]$gIdade, sragsl[[a]]$Classificação)
    p <- prop.table(n, 1)
    plot(mid, p[,1], ylim=0:1, las=1, type='n',
         xlab='Idade',
         ylab='Proporção de classificação do PCR')
    for (j in 1:5)
        lines(mid, p[, j], col=coresc[j], lwd=4)
    legend('topright', title=paste(2008+a),
           paste(lab.class.fin, ":", colSums(n)),
           col=coresc, cex=1.0,
           ncol=2, lwd=5, bty='n')
    abline(h=0:10/10, lty=2, col=gray(0.5, 0.3))
}
dev.off()
if (FALSE)
    system('eog figures/classificação-gIdade.png &')


if (FALSE) { ### random stuff
dt.int <- lapply(sragsl, function(x)
    as.Date(x$DT_INTERNA, '%d/%m/%Y'))
as.Date('1970-01-01') + sapply(dt.int, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.int, max, na.rm=TRUE)
sapply(1:length(dt.int), function(i)
    length(which(dt.int[[i]]>as.Date(paste0(2008+i,'-12-31')))))
if (FALSE) {  
    for (i in 1:length(dt.int)) 
        dt.int[[i]][
            which(dt.int[[i]]>
                  as.Date(paste0(2008+i,'-12-31')))] <- NA
}

dt.enc <- lapply(sragsl, function(x)
    as.Date(x$DT_ENCERRA, '%d/%m/%Y'))
as.Date('1970-01-01') + sapply(dt.enc, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.enc, max, na.rm=TRUE)
if (FALSE) {
    sragsl[[5]][order(dt.enc[[5]])[1:5],]
    dt.enc[[5]][which(dt.enc[[5]]=='2001-04-20')] <-
        as.Date('2017-04-20')
}
sapply(1:length(dt.enc), function(i)
    length(which(dt.enc[[i]]>as.Date(paste0(2008+i,'-12-31')))))
sapply(1:length(dt.enc), function(i)
    length(which(dt.enc[[i]]>as.Date(paste0(2008+i+1,'-12-31')))))
if (FALSE) {  
    for (i in 1:length(dt.enc)) 
        dt.enc[[i]][
            which(dt.enc[[i]]>
                  as.Date(paste0(2008+i+1,'-12-31')))] <- NA
}

dt.obi <- c(lapply(sragsl[1:6], function(x)
    as.Date(x$DT_OBITO, '%d/%m/%Y')), 
    lapply(sragsl[7:ny], function(x)
        ifelse(x$EVOLUCAO==2,
               as.Date(x$DT_EVOLUCA, '%d/%m/%Y'), NA)))

as.Date('1970-01-01') + sapply(dt.obi, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.obi, max, na.rm=TRUE)


dtF <- function(a, b)
    as.integer(difftime(a, b, units='days'))

t.int <- mapply(dtF, dt.enc, dt.int)

round(sapply(t.int, summary))

par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(2, 0.5, 0))
for (j in 1:length(t.int)) {
    hist(t.int[[j]], breaks=c(-Inf, 0:51, Inf), 
         xlim=c(0, 50), col=gray(0.7),
         main=paste('Ano', names(t.int)[j],
                    ':', length(t.int[[j]]), 'Internações'), 
         xlab='', ylab='')
    m <- median(t.int[[j]], na.rm=TRUE)
    abline(v=m, col=2, lwd=4)
    text(m+2, par()$usr[4]-0.2*diff(par()$usr[3:4]),
         format(m, digits=4))
}


### fix the 'SEM_NOT' column to 'yyyyww'
for (j in 1:ny)
    if (all(nchar(sprintf("%02d", as.integer(sragsl[[j]]$SEM_NOT)))==2))
        sragsl[[j]]$SEM_NOT <- paste0(
            2008+j, sprintf("%02d", as.integer(sragsl[[j]]$SEM_NOT)))


parana <- TRUE
if (parana) {
    sel.d <- lapply(sragsl, function(x) {
        j <- which(names(x)=='CO_MUN_NOT')
        if (length(j)==0) {
            ii <- which(substr(x$ID_MUNICIP, 1, 2)=='41')
        } else {
            ii <- which(substr(x[,j], 1, 2)=='41')
        }
        return(x[ii, ])
    })
} else {
    sel.d <- sragsl 
}

if (FALSE) {
    curitiba <- FALSE#TRUE
    if (curitiba) {
        sel.d <- lapply(sragsl, function(x) {
            j <- which(names(x)=='CO_MUN_NOT')
        if (length(j)==0) {
            ii <- which(substr(x$ID_MUNICIP, 1, 6)=='410690')
        } else {
            ii <- which(x[,j]=='410690')
        }
            return(x[ii, ])
        })
    } else {
        sel.d <- sragsl 
    }
}

### Resultados de PCR, por ano - Brasil
sapply(sragsl, function(d) 
    table(factor(d$CLASSI_FIN, c(1:5, 9), l.class.fin)))
### Resultados de PCR, por ano - Paraná
sapply(sel.d, function(d) 
    table(factor(d$CLASSI_FIN, c(1:5, 9), l.class.fin)))

table(sel.d[[ny]]$Classificação,
            sel.d[[ny]]$Evolução)

### labels for the leves of the PCR_RESUL variable 
lpcrres <- c('Detectável', 'N detectável', 'Inconclusivo',
             'Não realizado', 'Ag. resultado', 'Ignorado')
sel.d[[ny]]$resPCR <-
    factor(sel.d[[ny]]$PCR_RESUL, c(1:5,9), lpcrres)

t2 <- table(sel.d[[ny]]$resPCR,
      as.character(sel.d[[ny]]$Classificação))
t2
colnames(t2) <- c('COVID', 'Influ', 'NEspec', 'OEtiol',  'OResp')
rownames(t2) <- c('Detectável', 'NDetect', 'Inconclus', 'NRealiz', 'AgResult', 'Ignorado')
t2

tco <- table(as.character(sel.d[[ny]]$Classificação),
             sel.d[[ny]]$Evolução)
tco
rownames(

pcr20 <- table(
    sel.d[[ny]]$resPCR, substr(sel.d[[ny]]$SEM_NOT, 5, 6))
pcr20

wc.freq <- table(
    substr(unlist(lapply(sel.d, function(d) d$SEM_NOT)), 5, 6),
    unlist(lapply(sel.d, function(d)
        factor(d$CLASSI_FIN, c(1:5, 9), l.class.fin))), 
    substr(unlist(lapply(sel.d, function(d) d$SEM_NOT)), 1, 4))

dim(wc.freq)
dimnames(wc.freq)

### soma no ano
apply(wc.freq, 3, apply, 2, sum)

### stats for previous years 
prev.y.stats <- lapply(1:6, function(j)
    apply(wc.freq[,j,1:7], 1, summary, na.rm=TRUE))
str(prev.y.stats)

### define labels for the log10 scale
log(yl0 <- c(1, 3), 10)
yl <- list(l=c(yl0, 10*yl0, 100*yl0, 1000*yl0, 1e4*yl0))
yl$y <- log(yl$l, 10)
yl$ll <- gsub('000000', '1M', as.character(yl$l))
yl$ll <- gsub('00000', '100K', yl$ll)
yl$ll <- gsub('0000', '0K', yl$ll)
yl$ll <- gsub('000', 'K', yl$ll)
yl

### define colors 
cores0 <- c('red4', 'blue', 'green4', 'black', 'orange', 'gray')
cores <- c(cores0[1:5], rep(NA,3), cores0[6])
length(cores)

par(mfcol=c(2,1), mar=c(3,3,0,0), mgp=c(2,0.5,0))
for (log10 in FALSE) { ##c(FALSE, TRUE)) {
    par(mar=c(1.5,3,0,0))
    if (log10) {
        plot(log(1+wc.freq[,1,1], 10), axes=FALSE,
             ylim=range(log(1+wc.freq, 10)),
             ylab='Número de SRAGs por semana', type='n')
    } else {
        plot(wc.freq[,1,1], axes=FALSE,
             ylim=range(wc.freq),
             ylab='Número de SRAGs por semana', type='n')
    }
    yy <- c(prev.y.stats[[1]][1, ],
            rev(prev.y.stats[[1]][6, ]),
            prev.y.stats[[1]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(1, 0.5, 0.5, 0.5))
    yy <- c(prev.y.stats[[2]][1, ],
            rev(prev.y.stats[[2]][6, ]),
            prev.y.stats[[2]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(0.5, 0.5, 1, 0.5))
    yy <- c(prev.y.stats[[3]][1, ],
            rev(prev.y.stats[[3]][6, ]),
            prev.y.stats[[3]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(0.5, 1, 0.5, 0.5))
    yy <- c(prev.y.stats[[4]][1, ],
            rev(prev.y.stats[[4]][6, ]),
            prev.y.stats[[4]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=gray(0.7, 0.5))
    for (j in 1:5) {
        yy <- wc.freq[1:23, j, ny]
        if (log10)
            yy <- log(ifelse(yy<0.5, 0.5, yy), 10) 
        lines(yy, col=cores[j], lwd=3)
    }
    axis(1)
    if (log10) {
        axis(2, yl$y, yl$ll, las=1) 
        abline(h=yl$y, lty=2, col=gray(0.5, 0.5))
    } else {
        axis(2)
        abline(h=pretty(par()$usr[3:4]), col=gray(0.5, 0.5))
    }
    legend('topright', title='Resultado de teste', lwd=3,
           l.class.fin[1:5], col=cores0[1:5], lty=1, bty='n')
    legend('topleft', c('anos anteriores', '2020'),
           lty=c(NA,1), fill=c(gray(0.7,0.5), 0),
           border=c(gray(0.7,0.5), 0), bty='n')
    par(mar=c(3,3,0,0))
    if (log10) {
        plot(log(ifelse(pcr20[1, ]==0, 0.5, pcr20[1, ]), 10), 
             xlab='Semana epidemiológica', axes=FALSE, xlim=c(1,53), 
             ylab='Número de SRAGs por semana', type='n',
         ylim=log(range(pcr20+0.5, na.rm=TRUE), 10))
        axis(1); axis(2, yl$y, yl$ll, las=1)
        abline(h=yl$y, lty=2, col=gray(0.5, 0.5))
    } else {
        plot(pcr20[1, ], type='n', axes=FALSE, 
             xlab='Semana epidemiológica', 
         ylab='Número de SRAGs por semana', 
         xlim=c(1,53), ylim=range(pcr20, na.rm=TRUE)) 
        axis(1); axis(2) 
        abline(h=pretty(par()$usr[3:4]), col=gray(0.5, 0.5))
    }
    for (j in 1:nrow(pcr20)) {
        yy <- pcr20[j, ]
        if (log10)
            yy <- log(ifelse(yy==0, 0.5, yy), 10)
        lines(yy, col=cores0[j], lwd=3)
    }
    legend('right', rownames(pcr20), col=cores0, lty=1, lwd=3,
           title='Result. PCR', bty='n')
}


}
