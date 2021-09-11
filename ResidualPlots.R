library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
head(d[,c(1,4,5,7)])
#standardize
d$A <- standardize(d$MedianAgeMarriage)
d$M <- standardize(d$Marriage)
d$D <- standardize(d$Divorce)
head(d[,c(1,14,15,16)])

par(mfrow=c(2,2))

model1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

set.seed(10)

prior <- extract.prior( model1 )
mu <- link( model1 , post=prior , data=list( A=c(-2,2) ) )
plot( D ~ A, data = d , xlim=c(-2,2) , ylim=c(-2,2), 
      xlab = 'Median Age standardized', ylab = 'Divorce rate standardized', col=rangi2)
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )
mtext('Prior')

post <- extract.samples(model1,30)
plot(d$D ~ d$A,
     xlim = range(d$A), ylim = range(d$D),
     col = rangi2,
     xlab = 'Median Age standardized', ylab = 'Divorce rate standardized')
for(i in 1:dim(post)[1]){
  curve(post$a[i] + post$bA[i]*(x - mean(d$A)),
        col=col.alpha('black',0.3), add = T)
}
mtext('Posterior')

model2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

set.seed(10)


prior <- extract.prior( model2 )
mu <- link( model2 , post=prior , data=list( M=c(-2,2) ) )
plot( D ~ M, data = d , xlim=c(-2,2) , ylim=c(-2,2), 
      xlab = 'Marriage rate standardized', ylab = 'Divorce rate standardized', col=rangi2)
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )
mtext('Prior')

post <- extract.samples(model2,30)
plot(d$D ~ d$M,
     xlim = range(d$A), ylim = range(d$D),
     col = rangi2,
     xlab = 'Marriage rate standardized', ylab = 'Divorce rate standardized')
for(i in 1:dim(post)[1]){
  curve(post$a[i] + post$bM[i]*(x - mean(d$M)),
        col=col.alpha('black',0.3), add = T)
}
mtext('Posterior')

model3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp( 1 )
  ) , data = d )


par(mfrow = c(1,1))
plot(coeftab(model3,model2,model1), pars = c("bM", 'bA'))
#######RESDIUAL
par(mfrow = c(1,2))

model4 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

mu <- link(model4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$M - mu_mean
plot(d$D ~ mu_resid, col = col.alpha(rangi2, 1),
     ylab = 'Divorce rate standardized',
     xlab = 'Marriage rate conditioned on age of marriage')
abline(v = 0)
abline(b = cor(mu_resid, d$D), a= 0)

model5 <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0,0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

mu <- link(model5)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$A - mu_mean
plot(d$D ~ mu_resid, col = col.alpha(rangi2, 1),
     ylab = 'Divorce rate standardized',
     xlab = 'Age of marriage conditioned on marriage rate')
abline(v = 0)
abline(b = cor(mu_resid, d$D), a= 0)


