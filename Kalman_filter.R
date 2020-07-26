# Kalman filter

# Inferencia bayesiana recursiva para el caso en el que el estado del mundo es 
# dinámico, continuo y unidimensional. 

# Esta es una implementacion del ejemplo del libro "Bayesian perception: an 
# introduction" por Wei Ji Ma, Konrad Kording y Daniel Goldreich. 
# En este ejemplo, el sujeto extiende su brazo con los ojos cerrados e 
# intenta determinar la posición de su mano con respecto a un eje de referencia:
# una línea imaginaria recta que se extiende desde su hombro hacia adelante.

# Se considera que el modelo generativo tiene un dinámica lineal con ruido. 
# También se supone que la función de likelihood es gaussiana con desvío 
# estándar 1.
sigma_obs = 1

# Definimos el parámetro A de la dinámica:
A = 0.9

# Definimos el desvío estándar del ruido en la dinámica:
sigma_eta = 1

# Establecemos la cantidad de pasos temporales
N = 3

# Definimos las observaciones
obs = c(4,6,4)

# Definimos los parámetros de la distribución inicial (podemos pensarla como
# la distribución posterior en t=0)

prev_post_mu = 0
prev_post_sigma = sqrt(2)
par(mfrow = c(3,1))

for (i in 1:N){
  
  # Introducimos la información sobre la dinámica del sistema
  prior_mu = prev_post_mu * A
  prior_sigma = sqrt((A*prev_post_sigma)**2 + sigma_eta**2)
  print('')
  cat('prior_mu: ', prior_mu, ' | prior_sigma: ', prior_sigma)

  # Aplicamos la Regla de Bayes. Para eso, primero calculamos el peso W:
  W = prior_sigma**2 / (sigma_obs**2 + prior_sigma**2)
  cat(' W: ', W)
  # Calculamos la media de la multiplicación de las gaussianas de lalikelihood
  # y de la prior
  post_mu = prior_mu * (1-W) + W * obs[i]
  post_sigma = 1 / ((1 / sigma_obs**2 + 1 / prior_sigma**2))
  cat(' post_mu: ', post_mu, ' | post_sigma: ', post_sigma)

  # Graficamos
  x = seq(-10, 10, length=500)
  yprev = dnorm(x, prev_post_mu, prev_post_sigma)
  yprior = dnorm(x, prior_mu, prior_sigma)
  ylike = dnorm(x, obs[i], sigma_obs)
  ypost = dnorm(x, post_mu, post_sigma)
  labels = c(paste('Posterior a tiempo ', i-1), 'Prior', 'Likelihood', 'Posterior')
  plot(x, yprev, type='l', ylab='', xlab='', main=paste('t = ', i), bty='L',
       yaxs = 'i', col = alpha('coral',0.9), lwd=3, lty='dotted', ylim=c(0,1))
  lines(x, yprior, col = 'red', lwd=3)
  lines(x, ylike, col = 'darkolivegreen4', lwd=3)
  lines(x, ypost, col = "dodgerblue3", lwd=3)
  legend('topleft', inset=0.05, labels, lwd=3, lty = c(3,1,1,1), bty='n', cex=0.9,
         col = c('coral', 'red', 'darkolivegreen4', "dodgerblue3"))

  prev_post_mu = post_mu
  prev_post_sigma = post_sigma
}



