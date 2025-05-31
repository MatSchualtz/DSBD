a=1;b=30
x=50; n=100
theta <- seq(0,1,length.out=100)

plot(theta,dbeta(theta,a,b),
     col='gray',lwd=2,
     type='l',
     ylim=c(0,5))
abline(v=(x+a)/(a+b+n),lty=2)
lines(theta,dbinom(x,n,theta),
      lwd=2,
      type='l')
      
lines(theta,dbeta(theta,a+x,b+n-x),
      col='tomato',lwd=2,
      type='l')

(x+a)/(a+b+n)
