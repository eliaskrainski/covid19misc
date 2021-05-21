k1i <- 2
(b1i <- c(seq(0, 100, k1i), Inf))
nk1i <- length(b1i)-1
m1i <- b1i[1:nk1i]+k1i/2
lk1i <- paste0('[', b1i[1:nk1i],
               rep(c('-',''), c(nk1i-1,1)),
               c(b1i[2:nk1i], '+'),
               rep(c(')', ''), c(nk1i-1,1)))
lk1i

K <- 5
(bKi <- c(seq(0, 100, K),Inf))
nKi <- length(bKi)-1
(mKi <- K*1:nKi-K/2)
lKi <- paste0('[', bKi[1:nKi],
              rep(c('-',''), c(nKi-1,1)),
              c(bKi[2:nKi], '+'),
              rep(c(')', ''), c(nKi-1,1)))
lKi
