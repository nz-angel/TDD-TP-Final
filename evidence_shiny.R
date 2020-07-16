library(shiny)

simulation = function(N_max, lmu, lsig, nsims){

  upper_bound = 4.6
  lower_bound = -4.6
  
  par(mfrow = c(2,1))
  cols = c('dodgerblue3', 'darkorchid3', 'indianred2', 'seagreen3', 'burlywood2')
  # Se simulan algunos trials:
  for (j in 1:5){
    d = 0
    d_vals = c(0, rep(NaN, N_max))
    t = 2
    while (-4.6 < d & d < 4.6 & t <= N_max + 1){
      d = d + rnorm(1, lmu, lsig)
      d_vals[t] = d
      t = t + 1
    }
    if (j==1){
      plot(seq(0, N_max), d_vals, ylim = c(-5.6, 5.6), xaxt='n', xaxs='i', type='o',
           col = cols[1], lwd = 2, pch = 19, xlab = 't', ylab = '', 
           axes = FALSE, lty=2,
           main = 'Evolución temporal del "log posterior ratio" 
           para distintos trials')
      abline(h = c(upper_bound, lower_bound), 
             col=c('royalblue2', 'orangered1'),
             lwd = 2, lty=4)
      axis(1, at = seq(0, N_max))
      axis(2, at = c(lower_bound, 0, upper_bound))
    }else{
      lines(seq(0,N_max), d_vals, col = cols[j], type = 'o', 
            lwd = 2, lty = 2, pch=19)
    }
  }
  
  # Ahora podemos simular varios trials y graficar el tiempo de respuesta según
  # la decisión: 
  t_vals = rep(NaN, nsims)
  decision = rep(NaN, nsims)
  
  for (j in 1:nsims){
    
    d = 0
    
    # Se simula el trial del sujeto:
    t = 2
    while (-4.6 < d & d < 4.6 & t - 1 <= N_max ){
      d = d + rnorm(1, lmu, lsig)
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
}

ui = fluidPage(
  h2(style="text-align:center", 'Inferencia bayesiana recursiva: 
     acumulación de evidencia'),
  h4(style="text-align:center", 'Teoría de toma de decisiones - 
     Primer cuatrimestre 2020 - Nazareno Faillace'),
  
  sidebarLayout(
    
    sidebarPanel(
      p(style="text-align:justify", 'En este caso, el estado del mundo no cambia
      a lo largo del tiempo, pero el sujeto va incorporando nueva información. 
      En este experimento, el sujeto intenta identificar la dirección general en
        la que se mueve un conjunto de puntos (izquierda o derecha). 
        Al comenzar el experimento, el sujeto supone que ambas direcciones 
        son equiprobables. En cada paso temporal, utiliza como prior a la 
        posterior del paso temporal anterior. Utilizando la función de 
        likelihood, aplica la regla de Bayes para obtener la posterior del paso
        temporal actual.'),
      p(style="text-align:justify", 'A continuación se pueden simular varios
        trials de un mismo sujeto. En el gráfico superior se observa la 
        evolución del logaritmo del cociente de posteriors en el tiempo. Si 
        dicho cociente (variable de decisión) supera 4.6, el sujeto decide
        que la dirección es la izquierda; si es menor que -4.6, decide por la 
        derecha. En el gráfico inferior se observa la frecuencia de los 
        tiempos de respuesta para cada una de las opciones.'),
      
      
      numericInput('tsteps', 'Numero máximo de pasos de tiempo', 
                  value=10,min = 10, max = 200, step = 1),
      
      numericInput('sims', 'Cantidad de simulaciones',
                   value=1000, min=200, max=2000, step=50),
      
      sliderInput('likesig', HTML('Desvío estándar de la función de likelihood'), 
                  value=2, min = 0.1, max=4.6, step=0.1),
      
      numericInput('likemu', HTML('Media de la función de likelihood'),
                   value=0.5, min=-4, max=4, step=0.25),
      
    ),
    mainPanel(
      plotOutput('plots'),
    )
  ),
  
  
)

server = function(input, output){
  output$plots = renderPlot({
    simulation(input$tsteps, input$likemu, input$likesig, input$sims)
  }, height=800)
  

}

shinyApp(ui = ui, server = server)