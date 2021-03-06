library(shiny)
library(ggplot2)
library(gridExtra)

recursive_HMM = function(N, noise, din){
  # Esta funcion generaliza la implementada en HMM.R : permite elegir cuantos
  # intervalos temporales se van a considerar, cuánto es el ruido de la
  # percepcion y cual es la dinamica del estado del mundo.
  
  # Se define la din?mica del sistema
  M = matrix(c(1-din, din, din, 1-din), nrow = 2, ncol = 2)
  
  # Se simulan las observaciones
  obs = rep(NaN, N)
  obs[1] = rbinom(1, 1, 0.5)
  for (i in 2:N){
    if (obs[i-1] == 0){
      obs[i] = rbinom(1, 1, 0.2)
    }
    else{
      obs[i] = rbinom(1, 1, 0.8)
    }
  }
  observations = ifelse(obs == 1, 'D', 'I')

  # Se define likelihood
  L = matrix(c(1-noise, noise, noise, 1-noise), nrow = 2, ncol = 2)
  
  # Definimos las matrices donde vamos a guardar los daos para graficar
  mat_prev_post = matrix(rep(NaN, N*2), nrow=2, ncol=N)
  mat_prior = matrix(rep(NaN, N*2), nrow=2, ncol=N)
  mat_likelihood = matrix(rep(NaN, N*2), nrow=2, ncol=N)
  mat_posterior = matrix(rep(NaN, N*2), nrow=2, ncol=N)
  
  # Inicializamos el vector de probabilidades a priori (que podemos considerar 
  # probabilidades posterior del tiempo 0)
  
  p_post = c(0.5, 0.5)
  
  # Iteramos para cada paso temporal:
  for (t in 1:N){
    
    mat_prev_post[,t] = p_post
    
    # Se aplica la dinámica para tener la probabilidad prior a tiempo t
    p_before = M %*% p_post
    mat_prior[, t] = p_before
    
    mat_likelihood[, t] = L[,obs[t]+1]
    
    # Aplicamos la Regla de Bayes para calcular la probabilidad posterior a tiempo t
    p_post = p_before * L[,obs[t]+1] / sum(p_before * L[,obs[t]+1])
    mat_posterior[, t] = p_post
  }
  
  ls = list(observations, mat_prev_post, mat_prior, mat_likelihood, mat_posterior)
  names(ls) = c('observations', 'prev_post', 'prior', 'like', 'post')
  
  return(ls)
}

ui = fluidPage(
  h2(style="text-align:center", 'Inferencia bayesiana recursiva: 
     modelo oculto de Markov'),
  h4(style="text-align:center", 'Teoría de toma de decisiones - 
     Primer cuatrimestre 2020 - Nazareno Faillace'),
  
  sidebarLayout(
    
    sidebarPanel(
      p(style="text-align:justify",'Con esta aplicación se simula un ejemplo de 
      inferencia bayesiana recursiva cuando el estado del mundo es dinámico y 
      binario. El experimento consiste en observar un animal a lo lejos y 
      determinar su dirección de movimiento (izquierda o derecha).'),
      p(style="text-align:justify", 'Como suele ocurrir, la percepción del 
      sujeto puede introducir ruido, por lo que hay cierta probabilidad', 
      em(HTML('p<sub>c</sub>')), 'de que el individuo observe la dirección 
      errónea. Por otro lado, el sujeto conoce la dinámica: entre cada intervalo
      de tiempo: sabe que hay una probabilidad',  em(HTML('p<sub>d</sub>')), 
      'de que el animal cambie de dirección. Por eso, en cada intervalo de 
      tiempo ajusta con esta información su creencia posterior en en el 
      intervalo de tiempo previo para dar lugar a su creencia a priori actual.'),
      p(style="text-align:justify", 'A continuacion, se pueden ajustar los 
        valores de', em(HTML('p<sub>c</sub>')),'y de', em(HTML('p<sub>d</sub>')),
        '. Se puede observar, manualmente o con una animación, cómo evolucionan 
        en el tiempo las creencias del sujeto. En la parte superior figura lo 
        que observa en cada intervalo de tiempo.'),
      
      
      sliderInput('tsteps', 'Cantidad de espacios temporales', 
                  value=6,min = 2, max = 30, step = 1),
      
      sliderInput('noise', HTML('Ruido en la percepcion (p<sub>c</sub>)'), 
                  value=0.25, min = 0.05, max=0.95, step=0.05),
      
      numericInput('pancake', HTML('Probabilidad de que el animal cambie de 
                                   direccion (p<sub>d</sub>)'),
                   value=0.2, min=0, max=1, step=0.05),
      
      uiOutput('time')
    ),
    mainPanel(
      htmlOutput('obs'),
      plotOutput('plots', height="auto"),
    )
  ),
  
  
)

server = function(input, output){
  recursion = reactive({
    recursive_HMM(input$tsteps, input$noise, input$pancake)
    })
  
  output$time = renderUI({
    nsteps = input$tsteps
    sliderInput('time2', 't', 
                value=1, min=1, max=nsteps, step=1,
                animate = animationOptions(interval = 500, loop = FALSE))
    })
  
  output$obs = renderText({
    req(input$time2)
    s = paste('<h4 style="text-align:center">','Observacion: ')
    obs = recursion()$observations
    for (d in 1:length(obs)){
      if (d == input$time2){
        s = paste(s, '<b>',obs[d], '</b>')
      } else {
        s = paste(s, '<span style="color: #A9A9A9">', obs[d], '</span>')
      }
    }
    s = paste(s, '</h4>')
    s
    })
  
  output$plots = renderPlot({
    req(input$time2)
    prev_post = recursion()$prev_post[,input$time2]
    prior = recursion()$prior[,input$time2]
    likelihood = recursion()$like[,input$time2]
    posterior = recursion()$post[,input$time2]
    
    par(pty="s", mfrow=c(2,2))
    
    x = c(0,1)
    df = data.frame(direccion=c('I', 'D'), p=prev_post)
    title = paste('Posterior a tiempo', input$time2 - 1)
    prev_plt = ggplot(data=df, aes(x=direccion, y=p)) +
      geom_bar(stat="identity", fill="mediumpurple")+
      theme_minimal() + ylim(0,1) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 22))
    
    df = data.frame(direccion=c('I', 'D'), p=prior)
    title = paste('Prior a tiempo', input$time2)
    prior_plt = ggplot(data=df, aes(x=direccion, y=p)) +
      geom_bar(stat="identity", fill="mediumpurple")+
      theme_minimal() + ylim(0,1) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 22))

    df = data.frame(direccion=c('I', 'D'), p=likelihood)
    title = paste('Likelihood a tiempo', input$time2)
    like_plt = ggplot(data=df, aes(x=direccion, y=p)) +
      geom_bar(stat="identity", fill="mediumpurple")+
      theme_minimal() + ylim(0,1) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 22))
    
    df = data.frame(direccion=c('I', 'D'), p=posterior)
    title = paste('Posterior a tiempo', input$time2)
    post_plt = ggplot(data=df, aes(x=direccion, y=p)) +
      geom_bar(stat="identity", fill="tomato")+
      theme_minimal() + ylim(0,1) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 22))
    
    grid.arrange(prev_plt, prior_plt, like_plt, post_plt, ncol=2, nrow=2)

    }, height=800)

  
}

shinyApp(ui = ui, server = server)