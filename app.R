# app.R
library(shiny)
library(tidyverse)
library(patchwork)

extrair_metricas_sports2d <- function(df, nome = "Arquivo", tempo_col = "...2", pos_x = "Z1", pos_y = "X2",
                                      fps = 120, limiar_inicio = 0.5, delta_t_retro = 0.5,
                                      margem_final = 0.5, suavizacao_spar = 0.9) {
  delta_t <- 1 / fps
  df$tempo <- df[[tempo_col]]
  df$x <- df[[pos_x]]
  df$y <- df[[pos_y]]
  df <- df[order(df$tempo), ]
  df$delta_x <- c(NA, diff(df$x))
  df$delta_y <- c(NA, diff(df$y))
  df$velocidade <- sqrt(df$delta_x^2 + df$delta_y^2) / delta_t
  df <- df %>% drop_na(tempo, velocidade) %>% dplyr::filter(is.finite(tempo), is.finite(velocidade))
  spline_fit <- smooth.spline(df$tempo, df$velocidade, spar = suavizacao_spar)
  df$vel_suave <- predict(spline_fit, df$tempo)$y
  tempo_inicio_raw <- df$tempo[which(df$vel_suave >= limiar_inicio)[1]]
  tempo_inicio <- max(tempo_inicio_raw - delta_t_retro, min(df$tempo, na.rm = TRUE))
  vmax <- max(df$vel_suave, na.rm = TRUE)
  tempo_fim <- df$tempo[which(df$vel_suave <= (vmax - margem_final) & df$tempo > df$tempo[which.max(df$vel_suave)])[1]]
  df <- df %>% dplyr::filter(tempo >= tempo_inicio, tempo <= tempo_fim) %>% drop_na(vel_suave)
  df <- df %>% mutate(tempo_relativo = tempo - min(tempo))
  df$aceleracao <- c(NA, diff(df$vel_suave) / diff(df$tempo))
  vmax <- max(df$vel_suave, na.rm = TRUE)
  t_vmax <- df$tempo_relativo[which.max(df$vel_suave)]
  v_media <- mean(df$vel_suave, na.rm = TRUE)
  t_90 <- df$tempo_relativo[which(df$vel_suave >= 0.9 * vmax)][1]
  pico_acel <- max(df$aceleracao, na.rm = TRUE)
  t_pico_acel <- df$tempo_relativo[which.max(df$aceleracao)]
  acel_media <- mean(df$aceleracao[df$tempo_relativo <= t_90], na.rm = TRUE)
  tempo_total <- max(df$tempo_relativo) - min(df$tempo_relativo)
  list(
    df = df %>% mutate(arquivo = nome),
    resultados = tibble(
      arquivo = nome,
      tempo_inicio = round(tempo_inicio, 3),
      tempo_fim = round(tempo_fim, 3),
      tempo_total = round(tempo_total, 3),
      vmax = round(vmax, 2),
      tempo_vmax = round(t_vmax, 3),
      tempo_90pct_vmax = round(t_90, 3),
      vel_media = round(v_media, 2),
      acel_pico = round(pico_acel, 2),
      acel_media = round(acel_media, 2)
    ),
    t_vmax = t_vmax,
    vmax = vmax,
    t_pico_acel = t_pico_acel,
    pico_acel = pico_acel
  )
}

ui <- navbarPage("Análise Sprint 10m - Sports2D",
                 tabPanel("Comparar 3 Arquivos",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("arquivo1", "Arquivo 1 (.trc)", accept = ".trc"),
                              fileInput("arquivo2", "Arquivo 2 (.trc)", accept = ".trc"),
                              fileInput("arquivo3", "Arquivo 3 (.trc)", accept = ".trc"),
                              numericInput("fps", "FPS", value = 120),
                              sliderInput("spar", "Suavização", min = 0.1, max = 1, value = 0.9, step = 0.05),
                              numericInput("limiar_inicio", "Limiar início (m/s)", value = 0.5),
                              numericInput("delta_t_retro", "Tempo antes do início (s)", value = 0.5),
                              numericInput("margem_final", "Margem fim (m/s)", value = 0.5)
                            ),
                            mainPanel(
                              plotOutput("grafico_vel"),
                              plotOutput("grafico_acel"),
                              tableOutput("tabela_resultados")
                            )
                          )
                 ),
                 tabPanel("Autores",
                          fluidPage(
                            h3("Autores"),
                            HTML("<p><b>Vitor Bertoli Nascimento, PhD</b><br>
           E-mail: bertolinascimento.vitor@gmail.com</p>"),
                            br(),
                            h4("Alunos"),
                            tags$ul(
                              tags$li("Andryws Generoso Farinhuka"),
                              tags$li("João de Abreu Costa"),
                              tags$li("João Vitor Marquardt Lass Camargo")
                            )
                          )
                 )
)


server <- function(input, output) {
  analises <- reactive({
    req(input$arquivo1, input$arquivo2, input$arquivo3)
    df1 <- read_delim(input$arquivo1$datapath, delim = "\t", escape_double = FALSE, trim_ws = TRUE, skip = 4)
    df2 <- read_delim(input$arquivo2$datapath, delim = "\t", escape_double = FALSE, trim_ws = TRUE, skip = 4)
    df3 <- read_delim(input$arquivo3$datapath, delim = "\t", escape_double = FALSE, trim_ws = TRUE, skip = 4)
    a1 <- extrair_metricas_sports2d(df1, nome = input$arquivo1$name, fps = input$fps, suavizacao_spar = input$spar,
                                    limiar_inicio = input$limiar_inicio, delta_t_retro = input$delta_t_retro,
                                    margem_final = input$margem_final)
    a2 <- extrair_metricas_sports2d(df2, nome = input$arquivo2$name, fps = input$fps, suavizacao_spar = input$spar,
                                    limiar_inicio = input$limiar_inicio, delta_t_retro = input$delta_t_retro,
                                    margem_final = input$margem_final)
    a3 <- extrair_metricas_sports2d(df3, nome = input$arquivo3$name, fps = input$fps, suavizacao_spar = input$spar,
                                    limiar_inicio = input$limiar_inicio, delta_t_retro = input$delta_t_retro,
                                    margem_final = input$margem_final)
    list(
      df = bind_rows(a1$df, a2$df, a3$df),
      resultados = bind_rows(a1$resultados, a2$resultados, a3$resultados),
      marcadores = list(a1 = a1, a2 = a2, a3 = a3)
    )
  })
  
  output$grafico_vel <- renderPlot({
    dados <- analises()
    df <- dados$df
    pontos <- tibble(
      tempo_relativo = c(dados$marcadores$a1$t_vmax, dados$marcadores$a2$t_vmax, dados$marcadores$a3$t_vmax),
      vel = c(dados$marcadores$a1$vmax, dados$marcadores$a2$vmax, dados$marcadores$a3$vmax),
      arquivo = c(dados$marcadores$a1$resultados$arquivo, dados$marcadores$a2$resultados$arquivo, dados$marcadores$a3$resultados$arquivo)
    )
    ggplot(df, aes(x = tempo_relativo, y = vel_suave, color = arquivo)) +
      geom_line(size = 1) +
      geom_point(data = pontos, aes(x = tempo_relativo, y = vel, color = arquivo), shape = 16, size = 3, inherit.aes = FALSE) +
      labs(
        title = "Comparativo de Velocidade",
        x = "Tempo (s)", y = "Velocidade (m/s)"
      ) +
      theme_minimal()
  })
  
  output$grafico_acel <- renderPlot({
    dados <- analises()
    df <- dados$df %>% drop_na(aceleracao)
    pontos <- tibble(
      tempo_relativo = c(dados$marcadores$a1$t_pico_acel, dados$marcadores$a2$t_pico_acel, dados$marcadores$a3$t_pico_acel),
      acel = c(dados$marcadores$a1$pico_acel, dados$marcadores$a2$pico_acel, dados$marcadores$a3$pico_acel),
      arquivo = c(dados$marcadores$a1$resultados$arquivo, dados$marcadores$a2$resultados$arquivo, dados$marcadores$a3$resultados$arquivo)
    )
    ggplot(df, aes(x = tempo_relativo, y = aceleracao, color = arquivo)) +
      geom_line(size = 1) +
      geom_point(data = pontos, aes(x = tempo_relativo, y = acel, color = arquivo), shape = 17, size = 3, inherit.aes = FALSE) +
      labs(
        title = "Comparativo de Aceleração",
        x = "Tempo (s)", y = "Aceleração (m/s²)"
      ) +
      theme_minimal()
  })
  
  output$tabela_resultados <- renderTable({
    analises()$resultados
  })
}

shinyApp(ui = ui, server = server)
