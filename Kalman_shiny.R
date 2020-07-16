library(shiny)

Kalman_filter = function(N, sigma_obs, A){

  # Definimos el desvío estándar del ruido en la dinámica:
  sigma_eta = 1

  # Generamos las observaciones
  obs = rep(NaN, N)
  obs[1] = runif(1, 0, 6)
  for (i in 2:N){
    obs[i] = A * obs[i-1] + rnorm(1)
  }
  
  # Creamos una matriz para guardar los parámetros de cada paso temporal
  mat_post = matrix(rep(NaN, (N+1)*2), nrow = 2, ncol = N+1)
  mat_prior = matrix(rep(NaN, N*2), nrow = 2, ncol = N)
  mat_like = matrix(c(obs, rep(sigma_obs, N)), nrow = 2, ncol = N, byrow=TRUE)
  
  # Definimos los parámetros de la distribución inicial (podemos pensarla como
  # la distribución posterior en t=0)
  
  prev_post_mu = 0
  prev_post_sigma = sqrt(2)
  mat_post[, 1] = c(prev_post_mu, prev_post_sigma)
  
  for (i in 1:N){
    
    # Introducimos la información sobre la dinámica del sistema
    prior_mu = prev_post_mu * A
    prior_sigma = sqrt((A*prev_post_sigma)**2 + sigma_eta**2)
    mat_prior[, i] = c(prior_mu, prior_sigma)

    # Aplicamos la Regla de Bayes. Para eso, primero calculamos el peso W:
    W = prior_sigma**2 / (sigma_obs**2 + prior_sigma**2)

    # Calculamos la media de la multiplicación de las gaussianas de lalikelihood
    # y de la prior
    post_mu = prior_mu * (1-W) + W * obs[i]
    post_sigma = 1 / ((1 / sigma_obs**2 + 1 / prior_sigma**2))
    mat_post[, i+1] = c(post_mu, post_sigma)
    
    prev_post_mu = post_mu
    prev_post_sigma = post_sigma
  }
  
  ls = list(obs, mat_prior, mat_like, mat_post)
  names(ls) = c('observaciones', 'prior', 'like', 'post')

  return(ls)

}

ui = fluidPage(
  h2(style="text-align:center", 'Inferencia bayesiana recursiva: 
     filtro de Kalman'),
  h4(style="text-align:center", 'Teoría de toma de decisiones - 
     Primer cuatrimestre 2020 - Nazareno Faillace'),
  
  sidebarLayout(
    
    sidebarPanel(
      p(style="text-align:justify",'Con esta aplicación se simula un ejemplo de 
      inferencia bayesiana recursiva cuando el estado del mundo es dinámico y 
      continuo. En el experimento, el sujeto extiende su brazo con los ojos 
      cerrados e intenta determinar la posición de su mano con respecto a un eje
      de referencia: una línea imaginaria recta que se extiende desde su hombro 
      hacia adelante. Se analiza el caso unidimensional, por lo que el sujeto 
      intenta determinar la altura de su mano con respecto al eje de referencia'),
      p(style="text-align:justify", 'Se considera que el modelo generativo tiene
        una dinámica lineal con ruido: ', HTML('s<sub>n+1</sub> = As<sub>n</sub> + &eta;'),
        'donde ', HTML('<em>s<sub>n</sub></em> es el estado del mundo a tiempo 
                       n, A es constante y &eta; proviene de una distribución N(0,1).'),
        'El sujeto conoce la dinámica y la aplica a la creencia posterior del
        intervalo de tiempo anterior para obtener la prior del tiempo actual.'),
      p(style="text-align:justify", 'Se supone también que la función de likelihood
        es gaussiana y que la probabilidad prior tiene distribución normal. A
        continuación se puede simular trial especificando los intervalos de
        tiempo, el desvío estándar de la función de likelihood y el coeficiente
        de la dinámica.'),

      sliderInput('tsteps', 'Cantidad de espacios temporales', 
                  value=3,min = 2, max = 10, step = 1),
      
      sliderInput('noise', HTML('Desvío estándar de la función de likelihood'), 
                  value=1, min = 0.5, max=1.5, step=0.25),
      
      numericInput('coef', HTML('Coeficiente de la dinámica (<em>A</em>)'),
                   value=0.9, min=0.5, max=1, step=0.05),
      
      uiOutput('time')
    ),
    mainPanel(
      htmlOutput('obs'),
      plotOutput('plots'),
    )
  ),
  
  
)

server = function(input, output){
  recursion = reactive({
    Kalman_filter(input$tsteps, input$noise, input$coef)
  })
  
  output$time = renderUI({
    nsteps = input$tsteps
    sliderInput('time2', 't', 
                value=1, min=1, max=nsteps, step=1,
                animate = animationOptions(interval = 500, loop = FALSE))
  })
  
  output$obs = renderText({
    req(input$time2)
    s = paste('<h4 style="text-align:center">','Observación: ')
    obs = recursion()$observaciones
    for (d in 1:length(obs)){
      if (d == input$time2){
        s = paste(s, '<b>',round(obs[d],1), '</b>')
      } else {
        s = paste(s, '<span style="color: #A9A9A9">', round(obs[d], 1), '</span>')
      }
    }
    s = paste(s, '</h4>')
    s
  })
  
  output$plots = renderPlot({
    req(input$time2)
    prev_post = recursion()$post[,input$time2]
    prior = recursion()$prior[,input$time2]
    likelihood = recursion()$like[,input$time2]
    posterior = recursion()$post[,input$time2 + 1]
    
    prev_post_mu = prev_post[1]
    prev_post_sigma = prev_post[2]
    
    prior_mu = prior[1]
    prior_sigma = prior[2]
    
    like_mu = likelihood[1]
    like_sigma = likelihood[2]
    
    post_mu = posterior[1]
    post_sigma = posterior[2]
    
    x = seq(-10, 10, length=1000)
    yprev = dnorm(x, prev_post_mu, prev_post_sigma)
    yprior = dnorm(x, prior_mu, prior_sigma)
    ylike = dnorm(x, like_mu, like_sigma)
    ypost = dnorm(x, post_mu, post_sigma)
    labels = c(paste('Posterior a tiempo ', input$time2-1), 'Prior', 'Likelihood', 'Posterior')
    plot(x, yprev, type='l', ylab='', xlab='', main=paste('t = ', input$time2), bty='L',
         yaxs = 'i', col = 'coral', lwd=3, lty='dotted', ylim=c(0,1))
    lines(x, yprior, col = 'red', lwd=3)
    lines(x, ylike, col = 'darkolivegreen4', lwd=3)
    lines(x, ypost, col = "dodgerblue3", lwd=3)
    legend('topleft', inset=0.05, labels, lwd=3, lty = c(3,1,1,1), bty='n', cex=0.9,
           col = c('coral', 'red', 'darkolivegreen4', "dodgerblue3"))
    
    
  }, height=800)
  
  
}

shinyApp(ui = ui, server = server)