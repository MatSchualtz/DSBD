a = 8; b=8
x=0; n=10

theta <- seq(0,1, length.out= 100)

plot(theta, dbeta(theta,a,b),
     col='gray', lwd = 2, type = 'l',
     ylim=c(0,5))

abline(v=(x+a) /(a+b+n), lty = 2)

lines(theta, dbinom(x,n,theta),
      col='black', lwd=2, type='l')

lines(theta, dbeta(theta, a+x, b+n-x),
      col='tomato', lwd=2, type='l')

#n = amostra
# x = Quantidade de sucesso
# a e b = conhecimento a priori