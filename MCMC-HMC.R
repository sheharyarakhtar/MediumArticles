library(rethinking)


#Metropolis Algorithm
par(mfrow = c(1,1))
num_weeks <- 1e4
positions <- rep(0,num_weeks)
current<-10
for(i in 1:num_weeks){
  ##record current position
  positions[i] <- current
  
  ##flip coin to generate a proposal
  proposal <- current + sample(c(-1,1), size = 1)
  
  ##now make sure he loops around the archipelago
  if(proposal < 1) proposal <- 10
  if(proposal >10) proposal <- 1
  
  #move?
  prob_move <- proposal/current
  current <- ifelse(runif(1)<prob_move, proposal, current)
}
simplehist(positions, xlab = 'Island', ylab = 'f')

barplot(prop.table(table(positions)), ylim = c(0,.2))



##Sampling from high dimensional PD
D <- 1000
T <- 1e3
Y <- rmvnorm(T, rep(0,D),diag(D))
rad_dist <- function(Y) sqrt(sum(Y^2))

Rd <- sapply(1:T, function(i) rad_dist(Y[i,]))
# dens(Rd, xlim = c(0,34), ylim = c(0,1), xlab = 'Radial distance from the mode', ylab = "")
dens(Rd, add= T,show.zero = T, xlim = c(0,34), ylim = c(0,1))

text(locator(), labels = c('1','10','100','1000'))




##HMC simulation

#U needs to return the negative log probability

U <- function( q , a=0 , b=1 , k=0 , d=1 ) {
  muy <- q[1]
  mux <- q[2]
  U <- sum( dnorm(y,muy,1,log=TRUE) ) + sum( dnorm(x,mux,1,log=TRUE) ) +
    dnorm(muy,a,b,log=TRUE) + dnorm(mux,k,d,log=TRUE)
  return( -U )
}

# gradient function
# need vector of partial derivatives of U with respect to vector q
U_gradient <- function( q , a=0 , b=1 , k=0 , d=1 ) {
  muy <- q[1]
  mux <- q[2]
  G1 <- sum( y - muy ) + (a - muy)/b^2 #dU/dmuy
  G2 <- sum( x - mux ) + (k - mux)/d^2 #dU/dmux
  return( c( -G1 , -G2 ) ) # negative bc energy is neg-log-prob
}
# test data
set.seed(7)
y <- rnorm(50)
x <- rnorm(50)
x <- as.numeric(scale(x))
y <- as.numeric(scale(y))

library(shape)
Q <- list()
Q$q <- c(-0.1,0.2)
pr <- 0.3
plot( NULL , ylab="muy" , xlab="mux" , xlim=c(-pr,pr) , ylim=c(-pr,pr) )
step <- 0.03
L <- 11 # 0.03/28 for U-turns --- 11 for working example
n_samples <- 5
path_col <- col.alpha("black",0.5)
points( Q$q[1] , Q$q[2] , pch=4 , col="black" )
for ( i in 1:n_samples ) {
  Q <- HMC2( U , U_gradient , step , L , Q$q )
  if ( n_samples < 15 ) {
    for ( j in 1:L ) {
      K0 <- sum(Q$ptraj[j,]^2)/2 # kinetic energy
      lines( Q$traj[j:(j+1),1] , Q$traj[j:(j+1),2] , col=path_col , lwd=1+2*K0 )
    }
    points( Q$traj[1:L+1,] , pch=16 , col="white" , cex=0.35 )
    Arrows( Q$traj[L,1] , Q$traj[L,2] , Q$traj[L+1,1] , Q$traj[L+1,2] ,
            arr.length=0.35 , arr.adj = 0.7 )
    text( Q$traj[L+1,1] , Q$traj[L+1,2] , i , cex=0.8 , pos=4 , offset=0.4 )
  }
  points( Q$traj[L+1,1] , Q$traj[L+1,2] , pch=ifelse( Q$accept==1 , 16 , 1 ) ,
          col=ifelse( abs(Q$dH)>0.1 , "red" , "black" ) )
}
text(locator(), labels = c("samples = 5
      leapfrog steps = 11
      step size = 0.03"))
