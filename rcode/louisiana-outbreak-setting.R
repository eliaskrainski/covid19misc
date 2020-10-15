
if (FALSE)
    setwd("..")

louo <- read.csv2("data/louisiana-outbreaks-setting.csv")
head(louo,2)

louo$perOutbreak <- louo$CASES / louo$NUMBER.OF.OUTBREAKS
louo[,1] <- gsub('Grand Total', 'Average',
                 gsub(' ', '', as.character(louo[,1]),
                      fixed=TRUE))
louo

olouo <- louo[rev(order(louo$perOutbreak)),]

k <- nrow(olouo)
cols <- rgb(k:1/k, 1-2*abs(1:k-k/2)/k, 1:k/k)
cols[olouo[,1]=='Average'] <- 'cyan'

png('figures/louisiana-outbreaks-setting.png', 700, 500)
par(mfrow=c(1,1), mar=c(1.5,10.3,2,0), mgp=c(0,0.1,0))
b <- barplot(olouo$perOutbreak, col=cols, xlim=c(0,51),
             axes=FALSE, las=1, names.arg=olouo[,1], horiz=TRUE)
par(mgp=c(1,0.5,0))
axis(3, 5*(0:7), las=1)
abline(v=5*(0:8), col=gray(0.3,0.3), lty=2)
text(olouo$perOutbreak+1.5, b, 
     paste0(olouo$CASES, ':', olouo$NUMBER.OF.OUTBREAKS))
legend(10, b[k-5], 'CASES : OUTBREAKS',
       bg=gray(0.99), box.col=gray(0.99))
legend(40, b[k]+2, 'Outbreak size',
       bg=gray(0.99), box.col=gray(0.99))
im <- which(olouo[,1]=='Average')
legend(14, b[im]+1, 
       paste0('<--- Average of ',
              format(olouo$perOutbreak[im], dig=2),
              ' cases per outbreak'),
       bg=gray(0.99), box.col=gray(0.99))
symbols(rep(35, k-1)+2*log(olouo$CASES[-im]), b[-im],
        .5+sqrt(olouo$CASES[-im]/100), inches=FALSE,
        bg=cols[-im], add=TRUE)
text(rep(35, k-1)+2*log(olouo$CASES[-im]), b[-im],
     paste(olouo$CASES[-im]))
mtext(paste('Font: https://ldh.la.gov/index.cfm/page/3997',
            '(accessed on Aug, 18 2020)'),
      side=1, line=0)
dev.off()
if (FALSE)
    system('eog figures/louisiana-outbreaks-setting.png &')
