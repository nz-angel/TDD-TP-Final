# Hidden Markov Models

# Inferencia bayesiana recursiva para el caso en el que el estado del mundo es 
# dinámico y discreto (en particular, binario). 

# Esta es una implementacion del ejemplo del libro "Bayesian perception: an 
# introduction" por Wei Ji Ma, Konrad Kording y Daniel Goldreich. 
# En este ejemplo se el sujeto trata de determinar si un animal, que ve a lo 
# lejos, se está moviendo a izquierda (I) o a derecha (D).

# A priori, la probabilidad de que se esté moviendo en cada dirección es la 
# misma: p(I) = p(D) = 0.5

# Se supone que la percepción visual tiene ruido: hay un 20% de probabilidades
# de observar la dirección contraria a la que el animal realmente se está 
# moviendo. Este ruido es constante en el tiempo.

# Dinámica: en promedio, la dirección cambia cada 5 pasos temporales.

# Aprovechamos la representación matricial para llevar a cabo la recursión
# de una manera sencilla.

# Consideramos la cantidad de pasos temporales:

N = 6

# Definimos la matriz que representa la dinamica del sistema
M = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2, ncol = 2)

# Generamos las observaciones del individuo: 0 representa a la izquierda 
# y 1 a la derecha.
obs = c(0,0,0,1,1,1)
observations = ifelse(obs == 1, 'D', 'I')
print(observations)


# Definimos la matriz de Likelihood
L = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2, ncol = 2)

# Inicializamos el vector de probabilidades a priori (que podemos considerar 
# probabilidades posterior del tiempo 0)

p_post = c(0.5, 0.5)

# Iteramos para cada paso temporal:
par(pty="s", mfrow=c(2,3))
for (t in 1:N){
  
  # Se aplica la dinámica para tener la probabilidad prior a tiempo t
  p_before = M %*% p_post
  
  # Aplicamos la Regla de Bayes para calcular la probabilidad posterior a tiempo t
  p_post = p_before * L[,obs[t]+1] / sum(p_before * L[,obs[t]+1]) 
  
  x = c(0,1)
  y = p_post
  
  #R Graficamos la probabilidad posterior a tiempo t
  
  title = paste('Posterior a tiempo', t)
  plot(x, y, type='h', xlim=c(-0.5, 1.5), ylim=c(0,1), lwd=3, xaxt='n', main=title, xlab='', ylab='p', )
  axis(1, at=c(0,1), labels=c('I', 'D'))
  points(x,y,pch=16,cex=2.5,col="red")
}
