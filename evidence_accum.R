# Acumulación de evidencia

# Inferencia bayesiana recursiva para el caso en el que el estado del mundo es 
# estático y discreto (en particular, binario). 

# Esta es una implementacion del ejemplo del libro "Bayesian perception: an 
# introduction" por Wei Ji Ma, Konrad Kording y Daniel Goldreich.
# En este caso, el estado del mundo no cambia a lo largo del tiempo, pero el
# sujeto va incorporando nueva información. El ejemplo del libro trata de 
# identificar la dirección general en la que se mueve un conjunto de puntos.

# Por el análisis realizado en el libro, se tiene que la log posterior evoluciona
# como la log likelihood. Además, se supone que el logaritmo del cociente de
# likelihoods tiene distribución normal. Más precisamente, en el ejemplo,
# se considera que la media es 0.5 y el desvío estándar 2

loglike_mu = 0.5
loglike_sigma = 2

# Se consideran los intervalos temporales:
N = 15

# El sujeto decide por Izquierda si la variable de decisión (d) es mayor o igual 
# que 4.6 y por Derecha si es menor o igual que -4.6 . Al comenzar, el sujeto
# asume que el movimiento en cualquier dirección es equiprobable. Por lo tanto,
# comienza con d = 0

upper_bound = 4.6
lower_bound = -4.6

d = 0
d_vals = c(0, rep(NaN, N))

# Se simula el trial del sujeto:
t = 2
while (-4.6 < d & d < 4.6 & t <= N + 1){
  d = d + rnorm(1, loglike_mu, loglike_sigma)
  d_vals[t] = d
  t = t + 1
}
plot(seq(0, N), d_vals, ylim = c(-5.6, 5.6), xaxt='n', xaxs='i', type='o',
     col = 'gray28', lwd = 2.5, pch = 19, xlab = 't', ylab = '', axes = FALSE,
     main = 'Evolución temporal del "log posterior ratio"')
abline(h = c(upper_bound, lower_bound), col=c('firebrick1', 'dodgerblue2'),
       lwd = 2, lty=4)
axis(1, at = seq(0, N))
axis(2, at = c(lower_bound, 0, upper_bound))

# También podemos simular varios trials y graficar el tiempo de respuesta según
# la decisión: 
nsims = 1000
t_vals = rep(NaN, nsims)
decision = rep(NaN, nsims)

for (j in 1:nsims){
  
  d = 0

  # Se simula el trial del sujeto:
  t = 2
  while (-4.6 < d & d < 4.6 & t - 1 <= N ){
    d = d + rnorm(1, loglike_mu, loglike_sigma)
    d_vals[t] = d
    t = t + 1
  }
  t_vals[j] = t - 2
  decision[j] = ifelse( d < 4.6 & d > -4.6, NA, ifelse( d >= 4.6, 0, 1))
}

utvals = sort(unique(t_vals))
ls = list()

for (dec in c(0,1)){
  dec_tvals = t_vals[which(decision == dec)]
  decutvals = sort(unique(dec_tvals))
  t_freq = rep(0, length(utvals))
  i = 1
  for (t in decutvals){
    t_freq[i] = sum(dec_tvals == t) / length(dec_tvals)
    i = i + 1
  }
  if (dec == 0){
    ls[[1]] = c(0, t_freq) * 100
  }else{
    ls[[2]] = c(0, t_freq) * 100
  }
}

names(ls) = c('izqfreq', 'derfreq')
plot(c(0,utvals), ls$izqfreq, type='l', xaxt = 'n', yaxs='i',
     xaxs ='i', col = 'royalblue2', lwd = 2, bty = 'L',
     ylab = 'Frecuencia (%)', xlab = 'Tiempo de respuesta',
     main = 'Frecuencia de los tiempos de respuesta',
     ylim = c(0, max(c(ls$izqfreq, ls$derfreq))))
axis(1, at = seq(0, max(t_vals)))
lines(c(0, utvals), ls$derfreq, type='l', col = 'orangered1', lwd = 2)
labels = c('Izquierda', 'Derecha')
legend('topright', inset=0.05, labels, lwd=2, lty = c(1,1), bty='n', cex=0.9,
       col = c('royalblue2', 'orangered1'))
