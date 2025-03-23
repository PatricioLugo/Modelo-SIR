library(shiny)
library(mwshiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(deSolve)
library(plotly)
library(tidyr)

# ----UI----
ui <- dashboardPage(
  dashboardHeader(title = "Modelo SIR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("COVID-19", tabName = "covid_tab", icon = icon("virus-covid")),
      menuItem("Modelo SIR", tabName = "inicio", icon = icon("chart-simple")),
      menuItem("Modelo SIR Ec Dif", tabName = "sir_estandar_tab", icon = icon("chart-line")),
      menuItem("Modelo SIR VA", tabName = "sir_estandar_va_tab", icon = icon("subscript")),
      menuItem("R0", tabName = "r0_tab", icon = icon("shield-virus")),
      menuItem("Vacunación", tabName = "vacunacion_tab", icon = icon("syringe")),
      menuItem("Síntomas", tabName = "sintomas_tab", icon = icon("mask-face")),
      menuItem("Modelo SEIRSD", tabName = "seirs_intro_tab", icon = icon("cube")),
      menuItem("SEIRSD Ec Dif", tabName = "seirs_ecdif_tab", icon = icon("equals")),
      menuItem("SEIRSD VA", tabName = "seirs_va_tab", icon = icon("circle-xmark")),
      menuItem("Referencias", tabName = "referencias_tab", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabsetPanel(
      
      id = "page",
      type = "hidden",
      
      tabPanelBody("landing-page",
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "pagina_bienvenida.css")),
                   div(
                     class = "landing-page-container",
                     div(class = "flip-container1",
                         img(src = "S.png", height = "200px", width = "220px", class = "flip-image1"),
                         img(src = "I.png", height = "200px", width = "220px", class = "flip-image2"),
                         img(src = "R.png", height = "200px", width = "220px", class = "flip-image3")
                     ),
                     div(
                       class = "landing-page-content",
                       h1("MODELACIÓN DE EPIDEMIAS"),
                       br(),
                       actionButton("close-landing-page", "COMENZAR") 
                     )
                   )
      ),
      
      tabPanelBody("content",
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "aesth_SIR.css")),
                   tabItems(
                     tabItem(tabName = "inicio",
                             h2("Modelo SIR"),
                             fluidRow(
                               column(width = 6,
                                      tags$article(
                                        class = "intro_SIR sistema_ecuaciones",
                                        withMathJax(
                                          p("El", strong("Modelo SIR"),"es uno de los modelos matemáticos más populares a la hora de traducir
                                                     el comportamiento (y específicamente a la propagación) de una epidemia infecciosa"),
                                          p("El modelo divide a la población en 3 grupos:"),
                                          tags$ul(
                                            tags$li("Susceptibles"),
                                            tags$li("Infectados"),
                                            tags$li("Recuperados")
                                          ),
                                          p("Donde los", strong("susceptibles"), "son las personas en posibilidad de infectarse, los", strong("infectados"),
                                            "son las personas enfermas con el virus y los", strong("recuperados"), "son las personas que estuvieron infectadas
                                            y se han aliviado."),
                                          p("Así pues, estos compartimientos poblacionales son modelados mediante un sistema de ecuaciones diferenciales,
                                            que describe la cantidad de entradas y salidas en términos de la población para cada grupo:"),
                                          p("$$\\frac{dS}{dt} = \\mu N-\\beta I \\frac{S}{N}-\\mu S$$"),
                                          p("$$\\frac{dI}{dt} = \\beta I \\frac{S}{N}-\\gamma I - \\mu I$$"),
                                          p("$$\\frac{dR}{dt} = \\gamma I - \\mu R$$")
                                        )
                                      )
                               ),
                               column(width = 6,
                                      tags$img(src = "Diagrama_SIR.png", height = "540px", width = "960px")
                               )
                             )
                     ),
                     tabItem(tabName = "covid_tab",
                             h2("Pandemia de COVID-19"),
                             tags$br(),
                             fluidRow(
                               column(width = 6,
                                 box(
                                   title ="La Pandemia del COVID-19", width = NULL, solidHeader = TRUE, status = "primary",
                                   tags$article(
                                     p("El COVID-19 es un tipo de coronavirus que proviene de la misma familia genética de los virus SARS-CoV 
                                       y MERS-CoV, este apareció por primera vez en diciembre de 2019 eh la ciudad de Wuhan en china y comenzó 
                                       a ser reportado en enero de 2020 como un tipo de neumonía desconocida."),
                                     p("El número de infectados aumentó a casi 10,000 en tan solo un mes, de los cuales 1, 527 eran casos graves. 
                                       Se encontró que todas las personas infectadas tenían algún tipo de vínculo con el mercado de Huanan, donde 
                                       se vendían animales salvajes para su consumo."),
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6,
                                     tags$article(
                                       p("El virus pertenece a los coronavirus, una familia de virus ARN monocatenarios de sentido positivo. 
                                         Su mecanismo patogénico involucra la unión de la proteína S (spike) al receptor ACE2 en células huésped, 
                                         especialmente en las del epitelio respiratorio.")
                                     )
                                   ),
                                   column(width = 6,
                                          div(class = "flip-container2",
                                              img(src = "virus.png", height = "260px", width = "260px", class = "virus_imagen"))
                                   )
                                 )
                               ),
                               column(width = 6,
                                 box(
                                   title ="El COVID-19 en México", width = NULL, solidHeader = TRUE, status = "primary",
                                   tags$article(
                                     p("El primer caso de COVID en méxico fue el 27 de Febrero de 2020, el crecimiento de este fue increíble, pues
                                       en tan solo dos meses el número de casos era de 19,224 y 1,859 fallecidos, casi 10% del total de infectados."),
                                     p("La mayoría de los casos de México fueron el la Ciudad de México, y hubo mayor contagio por parte de los
                                       hombres que de las mujeres, y los fallecidos presentaban al menos una comorbilidad."),
                                     p("En México se presentaron las variantes Alpha, Beta, Gamma, Delta y Omicron, las cuales se fueron volviendo 
                                       menos mortales pero más contagiosas.")
                                   )
                                 )
                               )
                             )
                     ),
                     tabItem(tabName = "sir_estandar_tab",
                             h2("Modelo SIR con Ecuaciones Diferenciales"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                 box(
                                   title = "Parámetros del Modelo", width = NULL, solidHeader = TRUE, status = "primary",
                                   helpText("Tasa de natalidad/mortalidad"),
                                   withMathJax(
                                     numericInput("mu_input", "\\(\\mu\\)", value = 0.007, step = .01)
                                   ),
                                   helpText("Tasa de infección"),
                                   withMathJax(
                                     numericInput("beta_input","\\(\\beta\\)", value = 3/14, step = 0.5)
                                   ),
                                   helpText("Tasa de recuperación"),
                                   withMathJax(
                                     numericInput("gamma_input", "\\(\\gamma\\)", value = 1/14, step = 1)
                                   )
                                 )
                               ),
                               column(width = 8,
                                 plotlyOutput("grafico_SIR_estandar")
                               )
                             )
                     ),
                     tabItem(tabName = "sir_estandar_va_tab",
                             h2("Modelo SIR con Variables Aleatorias"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                 box(
                                   title = "Parámetros del Modelo", width = NULL, solidHeader = TRUE, status = "primary",
                                   helpText("Tiempo (días"),
                                   numericInput("t_input", "t", value = 100, step = 5),
                                   helpText("Probabilidad de infección de los susceptibles"),
                                   sliderInput("pI_estandar_input", "pI", min = 0, max = 1, value = 0.14),
                                   helpText("Probabilidad de los infectados de ser removidos"),
                                   sliderInput("pR_estandar_input","pR", min = 0, max = 1, value = 0.22)
                                 )
                               ),
                               column(width = 8,
                                 plotlyOutput("grafico_SIR_estandar_va")
                               )
                             )
                     ),
                     tabItem(tabName = "r0_tab",
                             h2("R0 en una epidemia"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                 tags$article(
                                   withMathJax(
                                     p("El número que representa la cantidad esperada
                                       de casos secundarios dado un primer infectado se le denomina el", strong("número
                                       básico de reproducción"), "y se representa como \\(R_0\\)."),
                                     p("A su vez, \\(R_0\\) es igual a la tasa de infección (\\(\\beta\\)) entre la suma
                                       de la tasa de natalidad/mortalidad (\\(\\mu\\)) y la tasa de recuperación (\\(\\gamma\\)):"),
                                     p("$$R_0 = \\frac{\\beta}{\\mu + \\gamma}$$"),
                                     p(),
                                     p("Así pues, es importante considerar que un número básico de reproducción menor significará una trayectoria más
                                       plana del grupo de los infectados.")
                                   )
                                 )
                               ),
                               column(width = 8,
                                 box(
                                   title = "Impacto de R0 en los Infectados", width = NULL, solidHeader = TRUE, status = "danger",
                                   numericInput("r0_input", "\\(R_0\\)", value = 1, step = 0.25),
                                   plotlyOutput("grafico_r0")
                                 )
                               )
                             )
                     ),
                     tabItem(tabName = "vacunacion_tab",
                             h2("Impacto de la vacunación en una epidemia"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                 tags$article(
                                   withMathJax(
                                     p("Las vacunas son preparaciones farmacéuticas que contienen versiones debilitadas, 
                                       atenuadas o inactivadas de microorganismos causantes de enfermedades. Su propósito
                                       es entrenar al sistema inmunológico para que genere anticuerpos y active su 
                                       respuesta defensiva, almacenando información que le permite combatir futuras 
                                       infecciones causadas por virus, bacterias o parásitos. (Instituto Nacional
                                       de las Personas Adultas Mayores, 2024)"),
                                     p("La vacunación podemos representarla como una proporción que afecta directamente a
                                       la cantidad de", strong("susceptibles iniciales:")),
                                     p("$$S_0 = N(1 - p)$$")
                                   )
                                 )
                               ),
                               column(width = 8,
                                 box(
                                   title = "Impacto de la Vacunación en los Infectados", width = NULL, solidHeader = TRUE, status  = "danger",
                                   sliderInput("vacunacion_input", "p", min = 0, max = 1, value = 0.5, step = 0.05),
                                   plotlyOutput("grafica_vacunacion")
                                 )
                               )
                             )
                     ),
                     tabItem(tabName = "sintomas_tab",
                             h2("División de los Infectados en Base a sus Síntomas"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                 box(
                                   title = "Parámetros", width = NULL, solidHeader = TRUE, status = "primary",
                                   helpText("Tiempo (días)"),
                                   numericInput("t_sintomas_input", "t", value = 100, step = 5),
                                   helpText("Probabilidades de ser infectados"),
                                   sliderInput("pI_low_input", "Síntomas Leves", min = 0, max = 1, value = 0.1, step = 0.01),
                                   sliderInput("pI_med_input", "Síntomas Medios", min = 0, max = 1, value = 0.14, step = 0.01),
                                   sliderInput("pI_high_input", "Síntomas Graves", min = 0, max = 1, value = 0.18, step = 0.01),
                                   helpText("Probabilidades de ser removidos"),
                                   sliderInput("pR_low_input", "Síntomas Leves", min = 0, max = 1, value = 0.25, step = 0.01),
                                   sliderInput("pR_med_input", "Síntomas Medios", min = 0, max = 1, value = 0.22, step = 0.01),
                                   sliderInput("pR_high_input", "Síntomas Graves", min = 0, max = 1, value = 0.18, step = 0.01),
                                 )
                               ),
                               column(width = 8,
                                 plotlyOutput("grafico_sintomas")
                               )
                             )
                     ),
                     tabItem(tabName = "seirs_intro_tab",
                             h2("Modelo SEIRSD"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 6,
                                    tags$article(
                                      withMathJax(
                                        p("El", strong("Modelo SEIRSD"), "es una propuesta alternativa al modelo SIR, que incluye
                                          varios atributos adicionales:"),
                                        tags$ul(
                                          tags$li("Expuestos: nuevo compartimiento poblacional"),
                                          tags$li("Los recuperados/removidos pueden volver a convertirse en susceptibles"),
                                          tags$li("Dead: nuevo compartimiento poblacional que represental la cantidad de muertos durante la epidemia")
                                        ),
                                        p("Así pues, estos compartimientos poblacionales son modelados mediante un sistema de ecuaciones 
                                          diferenciales, que describe la cantidad de entradas y salidas en términos de la 
                                          población para cada grupo:"),
                                        p("$$\\frac{dS}{dt} = \\mu N-\\beta I \\frac{S}{N}+ \\omega R -\\mu S$$"),
                                        p("$$\\frac{dE}{dt} = \\beta I \\frac{S}{N} - \\sigma E - \\mu E$$"),
                                        p("$$\\frac{dI}{dt} = \\sigma E - \\gamma I - \\alpha I - \\mu I$$"),
                                        p("$$\\frac{dR}{dt} = \\gamma I - \\omega R - \\mu R$$"),
                                        p("$$\\frac{dD}{dt} = \\alpha I + \\mu (S + E + I + R)$$")
                                      )
                                    )
                               ),
                               column(width = 6,
                                    tags$img(src = "Diagrama_SEIRSD.png", height = "412px", width = "678.5px")
                               )
                             )
                     ),
                     tabItem(tabName = "seirs_ecdif_tab",
                             h2("Modelo SEIRSD con Ecuaciones Diferenciales"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 4,
                                    box(
                                      title = "Parámetros del Modelo", width = NULL, solidHeader = TRUE, status = "primary",
                                      helpText("Tiempo (días)"),
                                      numericInput("t_ecdif_input", "t", value = 365, step = 10),
                                      helpText("Tasas de cambio"),
                                      withMathJax(
                                      numericInput("beta_ecdif_input", "\\(\\beta\\)", value = 3/14, step = 0.01)
                                      ),
                                      withMathJax(
                                        numericInput("sigma_ecdif_input", "\\(\\sigma\\)", value = 1/2, step = 0.01)
                                      ),
                                      withMathJax(
                                        numericInput("gamma_ecdif_input", "\\(\\gamma\\)", value = 1/14, step = 0.01)
                                      ),
                                      withMathJax(
                                        numericInput("omega_ecdif_input", "\\(\\omega\\)", value = 1/90, step = 0.01)
                                      ),
                                      withMathJax(
                                        numericInput("mu_ecdif_input", "\\(\\mu\\)", value = 1 / (76 * 365), step = 0.01)
                                      ),
                                      withMathJax(
                                        numericInput("alpha_ecdif_input", "\\(\\alpha\\)", value = 0.075/14, step = 0.01)
                                      )
                                    )
                               ),
                               column(width = 8,
                                    plotlyOutput("grafico_seirsd_ecdif")
                               )
                             )
                     ),
                     tabItem(tabName = "seirs_va_tab",
                             h2("Modelo SEIRSD con Variables Aleatorias"),
                             tags$br(),
                             tags$br(),
                             fluidRow(
                               column(width = 5,
                                      box(
                                        title = "Parámetros del Modelo", width = NULL, solidHeader = TRUE, status = "primary",
                                        helpText("Tiempo (días)"),
                                        numericInput("t_va_input", "t", value = 365, step = 10),
                                        helpText("Número de expuestos iniciales"),
                                        withMathJax(
                                          sliderInput("E0_input", "\\(E_0\\)", min = 0, max = 100, value = 1, step = 5)
                                        ),
                                        helpText("Probabilidad de que los susceptibles se conviertan en expuestos"),
                                        sliderInput("pE_input", "pE", min = 0, max = 1, value = 0.3, step = 0.1),
                                        helpText("Probabilidad de que los recuperados pierdan inmunidad"),
                                        numericInput("pL_input", "pL", min = 0, max = 1, value = 1/180, step = 0.001),
                                      ),
                                      box(
                                        title = "Gravedad de síntomas", width  = NULL, solidHeader = TRUE, status = "danger",
                                        helpText("Probabilidades de ser infectados"),
                                        sliderInput("pI_low_va_input", "Síntomas Leves", min = 0, max = 1, value = 0.333, step = 0.01),
                                        sliderInput("pI_med_va_input", "Síntomas Medios", min = 0, max = 1, value = 0.333, step = 0.01),
                                        sliderInput("pI_high_va_input", "Síntomas Graves", min = 0, max = 1, value = 0.333, step = 0.01),
                                      )
                               ),
                               column(width = 7,
                                      plotlyOutput("grafico_seirsd_va"),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      plotlyOutput("grafico_seirsd_sintomas")
                               )
                             )
                     ),
                     tabItem(tabName = "referencias_tab",
                        h2("Referencias"),
                        tags$br(),
                        tags$br(),
                        tags$article(
                          p("Bjørnstad, O. N., Shea, K., Krzywinski, M., & Altman, N. (2020a). Modeling infectious epidemics. Nature Methods, 17(5), 455-456. https://doi.org/10.1038/s41592-020-0822-z"),
                          p("Bjørnstad, O. N., Shea, K., Krzywinski, M., & Altman, N. (2020). The SEIRS model for infectious disease dynamics. En Nature. Nature Methods. https://www.nature.com/articles/s41592-020-0856-2.pdf"),
                          p("De Salud, H. (n.d.). Variantes del virus SARS-CoV2. gob.mx. https://www.gob.mx/promosalud/articulos/variantes-del-virus-sars-cov2-282523?idiom=es"),
                          p("De las Personas Adultas Mayores, I. N. (s.f.). La importancia de la vacunación como medida para la prevención de . . . gob.mx. https://www.gob.mx/inapam/articulos/la-importancia-de-la-vacunacion-como-medida-para-la-prevencion-de-enfermedades?idiom=es"),
                          p("Conversation, I. M. /. *. (2022, December 15). Los 7 tipos de coronavirus que infectan humanos. www.nationalgeographic.com.es. https://www.nationalgeographic.com.es/ciencia/siete-tipos-coronavirus-que-infectan-humanos_15353"),
                          p("Roberts, M. (2021, January 16). Covid-19: ¿qué tan preocupantes son las variantes de coronavirus de Reino Unido, Sudáfrica y Brasil? BBC News Mundo. https://www.bbc.com/mundo/noticias-55689478"),
                          p("Rodríguez, H. (2022, August 8). Confirmado, la pandemia de COVID-19 surgió en el mercado de Huanan, en Wuhan. www.nationalgeographic.com.es. https://www.nationalgeographic.com.es/ciencia/todas-pistas-conducen-a-wuhan-epicentro-pandemia-covid-19_18644"),
                          p("Suárez, V., Quezada, M. S., Ruiz, S. O., & De Jesús, E. R. (2020). Epidemiología de COVID-19 en México: del 27 de febrero al 30 de abril de 2020. Revista Clínica Española, 220(8), 463–471. https://doi.org/10.1016/j.rce.2020.05.007")
                        )
                     )
                   )
      )
    )
  )
)


# ----LÓGICA DEL SERVIDOR----
server <- function(input, output, session) {
  
  observe({
    req(input$gamma_ecdif_input)
    new_alpha_value <- 0.01 * input$gamma_ecdif_input
    updateNumericInput(session, "alpha_ecdif_input", value = new_alpha_value)
  })
  
  observeEvent(input$`close-landing-page`, {
    updateTabsetPanel(session, "page", "content")
  })
  
  output$grafico_seirsd_sintomas <- renderPlotly({
    
      t <- 1:input$t_va_input
      N <- 100    
      
      S <- numeric(length(t)) 
      E <- numeric(length(t))
      I <- numeric(length(t))
      I_low <- numeric(length(t))
      I_med <- numeric(length(t))
      I_high <- numeric(length(t))
      R <- numeric(length(t))
      D <- numeric(length(t))
      
      pI_low <- input$pI_low_va_input
      pI_med <- input$pI_med_va_input
      pI_high <- input$pI_low_va_input
      
      pR_low <- 0.15  
      pR_med <- 0.10  
      pR_high <- 0.05
      
      pD_low <- 0.001  
      pD_med <- 0.03  
      pD_high <- 0.10
      
      pE <- input$pE_input
      pL <- input$pL_input
      
      E[1] <- input$E0_input
      I_low[1] <- 1
      I_med[1] <- 0
      I_high[1] <- 0
      I[1] <- I_low[1] + I_med[1] + I_high[1]
      R[1] <- 0
      D[1] <- 0
      S[1] <- N - E[1] - I[1] - R[1] - D[1]
      
      for (i in 1:(length(t) - 1)) {
        I[i] <- I_low[i] + I_med[i] + I_high[i]
        
        if (I[i] > 0) {
          p_combined <- min(0.99, (pI_low * I_low[i] + pI_med * I_med[i] + pI_high * I_high[i]) / N)
        } else {
          p_combined <- 0
        }
        
        exposed <- rbinom(1, S[i], p_combined)
        
        new_infected <- rbinom(1, E[i], pE)
        
        new_inf_low <- rbinom(1, new_infected, 0.7)  
        new_inf_med <- rbinom(1, new_infected - new_inf_low, 0.8)
        new_inf_high <- new_infected - new_inf_low - new_inf_med 
        
        deaths_low <- rbinom(1, I_low[i], pD_low)
        deaths_med <- rbinom(1, I_med[i], pD_med)
        deaths_high <- rbinom(1, I_high[i], pD_high)
        total_deaths <- deaths_low + deaths_med + deaths_high
        
        recovered_low <- rbinom(1, I_low[i] - deaths_low, pR_low)
        recovered_med <- rbinom(1, I_med[i] - deaths_med, pR_med)
        recovered_high <- rbinom(1, I_high[i] - deaths_high, pR_high)
        total_recovered <- recovered_low + recovered_med + recovered_high
        
        loss_immunity <- rbinom(1, R[i], pL)
        
        S[i + 1] <- S[i] - exposed + loss_immunity
        E[i + 1] <- E[i] + exposed - new_infected
        
        I_low[i + 1] <- I_low[i] + new_inf_low - deaths_low - recovered_low
        I_med[i + 1] <- I_med[i] + new_inf_med - deaths_med - recovered_med
        I_high[i + 1] <- I_high[i] + new_inf_high - deaths_high - recovered_high
        
        I[i + 1] <- I_low[i + 1] + I_med[i + 1] + I_high[i + 1]
        R[i + 1] <- R[i] + total_recovered - loss_immunity
        D[i + 1] <- D[i] + total_deaths
      }
      
      
      miOutput <- data.frame(
        t = t,
        I_bajo = I_low,
        I_med = I_med,
        I_alto = I_high,
        I = I
      )
      
      output_long <- pivot_longer(miOutput, 
                                  cols = c(I_bajo, I_med, I_alto, I),
                                  names_to = "Gravedad", 
                                  values_to = "Proporción")
      
      output_long$Compartimiento <- factor(output_long$Gravedad, 
                                           levels = c("I", "I_bajo", "I_med", "I_alto"))
      
      p <- ggplot(output_long, aes(x = t, y = Proporción, color = Gravedad)) +
        geom_line(linewidth = 1.2) +
        ggtitle("Gravedad de los infectados") +
        labs(x = "Tiempo (días)", y  = "Porcentaje de la Población", color = "Gravedad") +
        scale_color_manual(values = c("I" = "black", "I_bajo" = "darkgoldenrod2", 
                                      "I_med" = "darkorange1", "I_alto" = "firebrick2")) +
        theme_stata() +
        theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "#21295C"),
          plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
          axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
          axis.text.x = element_text(color = "#bcd2e2"),
          axis.text.y = element_text(color = "#bcd2e2")
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Times New Roman", size = 14)
          ),
          paper_bgcolor = "#21295C",
          plot_bgcolor = "transparent"
        ) %>%
        config(displayModeBar = FALSE)
  })
  
  output$grafico_seirsd_va <- renderPlotly({

    t <- 1:input$t_va_input
    N <- 100    
    
    S <- numeric(length(t)) 
    E <- numeric(length(t))
    I <- numeric(length(t))
    I_low <- numeric(length(t))
    I_med <- numeric(length(t))
    I_high <- numeric(length(t))
    R <- numeric(length(t))
    D <- numeric(length(t))
    
    pI_low <- input$pI_low_va_input
    pI_med <- input$pI_med_va_input
    pI_high <- input$pI_low_va_input
    
    pR_low <- 0.15  
    pR_med <- 0.10  
    pR_high <- 0.05
    
    pD_low <- 0.001/14  
    pD_med <- 0.05/14  
    pD_high <- 0.10/14
    
    pE <- input$pE_input
    pL <- input$pL_input
    
    E[1] <- input$E0_input
    I_low[1] <- 1
    I_med[1] <- 0
    I_high[1] <- 0
    I[1] <- I_low[1] + I_med[1] + I_high[1]
    R[1] <- 0
    D[1] <- 0
    S[1] <- N - E[1] - I[1] - R[1] - D[1]
    
    for (i in 1:(length(t) - 1)) {
      I[i] <- I_low[i] + I_med[i] + I_high[i]
      
      if (I[i] > 0) {
        p_combined <- min(0.99, (pI_low * I_low[i] + pI_med * I_med[i] + pI_high * I_high[i]) / N)
      } else {
        p_combined <- 0
      }
      
      exposed <- rbinom(1, S[i], p_combined)
      
      new_infected <- rbinom(1, E[i], pE)
      
      new_inf_low <- rbinom(1, new_infected, 0.7)  
      new_inf_med <- rbinom(1, new_infected - new_inf_low, 0.8)
      new_inf_high <- new_infected - new_inf_low - new_inf_med 
      
      deaths_low <- rbinom(1, I_low[i], pD_low)
      deaths_med <- rbinom(1, I_med[i], pD_med)
      deaths_high <- rbinom(1, I_high[i], pD_high)
      total_deaths <- deaths_low + deaths_med + deaths_high
      
      recovered_low <- rbinom(1, I_low[i] - deaths_low, pR_low)
      recovered_med <- rbinom(1, I_med[i] - deaths_med, pR_med)
      recovered_high <- rbinom(1, I_high[i] - deaths_high, pR_high)
      total_recovered <- recovered_low + recovered_med + recovered_high
      
      loss_immunity <- rbinom(1, R[i], pL)
      
      S[i + 1] <- S[i] - exposed + loss_immunity
      E[i + 1] <- E[i] + exposed - new_infected
      
      I_low[i + 1] <- I_low[i] + new_inf_low - deaths_low - recovered_low
      I_med[i + 1] <- I_med[i] + new_inf_med - deaths_med - recovered_med
      I_high[i + 1] <- I_high[i] + new_inf_high - deaths_high - recovered_high
      
      I[i + 1] <- I_low[i + 1] + I_med[i + 1] + I_high[i + 1]
      R[i + 1] <- R[i] + total_recovered - loss_immunity
      D[i + 1] <- D[i] + total_deaths
    }
    
    
    miOutput <- data.frame(
      t = t,
      S = S,
      E = E,
      I = I,
      R = R,
      D = D
    )
    
    output_long <- pivot_longer(miOutput, 
                                cols = c(S, E, I, R, D),
                                names_to = "Compartimiento", 
                                values_to = "Proporción")
    
    output_long$Compartimiento <- factor(output_long$Compartimiento, 
                                         levels = c("S", "E", "I", "R", "D"))
    
    p <- ggplot(output_long, aes(x = t, y = Proporción, color = Compartimiento)) +
      geom_line(linewidth = 1.2) +
      ggtitle("Modelo SEIRSD") +
      labs(x = "Tiempo (días)", y  = "Porcentaje de la Población", color = "Compartimiento") +
      scale_color_manual(values = c("S" = "black", "E" = "dodgerblue2", 
                                    "I" = "firebrick2", "R" = "seagreen3", "D" = "darkorchid2")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafico_seirsd_ecdif <- renderPlotly({
    seirsd_model <- function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
        N_alive <- S + E + I + R
        
        dS <- mu * N - beta * I * S / N_alive + omega * R - mu * S
        dE <- beta * I * S / N_alive - sigma * E - mu * E
        dI <- sigma * E - gamma * I - alpha * I - mu * I
        dR <- gamma * I - omega * R - mu * R
        dD <- alpha * I + mu * (S + E + I + R)
        
        list(c(dS, dE, dI, dR, dD))
      })
    }
    
    parameters <- c(
      beta = input$beta_ecdif_input,
      sigma = input$sigma_ecdif_input,  
      gamma = input$gamma_ecdif_input,
      omega = input$omega_ecdif_input,
      mu = input$mu_ecdif_input,
      alpha = input$alpha_ecdif_input
    )
    
    N <- 100
    
    state <- c(S = N - 1, E = 0, I = 1, R = 0, D = 0)
    
    times <- seq(0, input$t_ecdif_input, by = 1)
    
    output <- ode(y = state, times = times, func = seirsd_model, parms = parameters)
    miOutput <- as.data.frame(output)
    
    output_long <- pivot_longer(miOutput, 
                                cols = c(S, E, I, R, D),
                                names_to = "Compartimiento", 
                                values_to = "Proporción")
    
    output_long$Compartimiento <- factor(output_long$Compartimiento, 
                                         levels = c("S", "E", "I", "R", "D"))
    
    p <- ggplot(output_long, aes(x = time, y = Proporción, color = Compartimiento)) +
      geom_line(linewidth = 1.2) +
      ggtitle("Modelo SEIRSD") +
      labs(x = "Tiempo (días)", y  = "Porcentaje de la Población", color = "Compartimiento") +
      scale_color_manual(values = c("S" = "black", "E" = "dodgerblue2", 
                           "I" = "firebrick2", "R" = "seagreen3", "D" = "darkorchid2")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafico_sintomas <- renderPlotly({
    t <- 1:input$t_sintomas_input         
    N <- 100           
    S <- numeric(length(t))    
    I_low <- numeric(length(t))  
    I_med <- numeric(length(t))  
    I_high <- numeric(length(t)) 
    I <- numeric(length(t))      
    R <- numeric(length(t)) 
    
    pI_low <- input$pI_low_input 
    pI_med <- input$pI_med_input    
    pI_high <- input$pI_high_input
    
    pR_low <- input$pR_low_input
    pR_med <- input$pR_med_input
    pR_high <- input$pR_high_input
    
    I_low[1] <- 1   
    I_med[1] <- 0
    I_high[1] <- 0
    I[1] <- I_low[1] + I_med[1] + I_high[1]
    R[1] <- 0
    S[1] <- N - I[1] - R[1]
    
    TInf <- I[1]        
    TRem <- R[1]    
    
    for (i in 1:(length(t) - 1)) {
      I[i] <- I_low[i] + I_med[i] + I_high[i]
      
      p_combined <- (pI_low * I_low[i] + pI_med * I_med[i] + pI_high * I_high[i]) / max(1, I[i])
      
      new_infected <- rbinom(1, S[i], p_combined)
      
      new_inf_low <- rbinom(1, new_infected, 0.6)  
      new_inf_med <- rbinom(1, new_infected - new_inf_low, 0.7) 
      new_inf_high <- new_infected - new_inf_low - new_inf_med  
      
      removed_low <- rbinom(1, I_low[i], pR_low)
      removed_med <- rbinom(1, I_med[i], pR_med)
      removed_high <- rbinom(1, I_high[i], pR_high)
      total_removed <- removed_low + removed_med + removed_high
      
      TInf <- TInf + new_infected
      TRem <- TRem + total_removed
      
      S[i + 1] <- S[i] - new_infected
      I_low[i + 1] <- I_low[i] + new_inf_low - removed_low
      I_med[i + 1] <- I_med[i] + new_inf_med - removed_med
      I_high[i + 1] <- I_high[i] + new_inf_high - removed_high
      I[i + 1] <- I_low[i + 1] + I_med[i + 1] + I_high[i + 1]
      R[i + 1] <- R[i] + total_removed
    }
    
    miOutput = data.frame(
      t = t,
      S = S,
      I_bajo = I_low,
      I_med = I_med,
      I_alto = I_high,
      I = I,
      R = R
    )
    
    p <- ggplot(miOutput, aes(x = t)) +
      geom_line(aes(y = S, color = "Susceptibles"), linewidth = 1.2) +
      geom_line(aes(y = I, color = "Infectados totales"), linewidth = 1.2) +
      geom_line(aes(y = I_bajo, color = "Síntomas bajos"), linewidth = 1.2) +
      geom_line(aes(y = I_med, color = "Síntomas medios"), linewidth = 1.2) +
      geom_line(aes(y = I_alto, color = "Síntomas altos"), linewidth = 1.2) +
      geom_line(aes(y = R, color = "Recuperados / Removidos"), linewidth = 1.2) +
      ggtitle("Modelo SIR Estándar") +
      labs(x = "Tiempo en días", y  = "Población") +
      scale_color_manual(name = "Compartimiento Poblacional",
                         values = c("Susceptibles" = "dodgerblue2", "Infectados totales" = "black", "Síntomas bajos" = "darkgoldenrod1", "Síntomas medios" = "darkorange1", "Síntomas altos" = "firebrick2", "Recuperados / Removidos" = "seagreen3")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)  
  })
  
  output$grafica_vacunacion <- renderPlotly({
    f_SIR_estandar <- function(t, state, parms) {
      with(as.list(state),
           {
             dxdt = rep(0,length(state))
             
             dxdt[1] = mu * N - beta * S * I/N - mu * S
             dxdt[2] = beta * S * I/N - gamma * I - mu * I
             dxdt[3] = gamma * I - mu * R
             
             return(list(dxdt))
           }
      )
    }
    
    mu <- 0.007
    beta <- 2
    gamma <- 0.147
    N <- 100
    
    I0 <- 1
    S0 <- N * (1 - input$vacunacion_input) - I0
    R0 <- 0
    
    t <- seq(0,365, by = 1)
    init <- c(S=S0,I=I0,R=R0)
    
    miOutput = as.data.frame(ode(y=init, times = t, func = f_SIR_estandar, parms = NULL))
    
    p <- ggplot(miOutput, aes(x = time)) +
      geom_line(aes(y = I, color = "Infectados"), linewidth = 1.2) +
      ggtitle("Infectados en un año") +
      labs(x = "Fracción de tiempo (1 año)", y  = "Fracción de la Población") +
      scale_color_manual(name = "",
                         values = c("Infectados" = "firebrick2")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafico_SIR_estandar_va <- renderPlotly({
    t <- 1:input$t_input  
    N <- 100              
    
    S <- numeric(length(t))
    I <- numeric(length(t))
    R <- numeric(length(t))
    
    pI <- input$pI_estandar_input  
    pR <- input$pR_estandar_input 
    
    I[1] <- 1
    R[1] <- 0
    S[1] <- N - I[1] - R[1]
    
    for (i in 1:(length(t) - 1)) {
      infected <- rbinom(1, S[i], pI) 
      removed <- rbinom(1, I[i], pR) 
      
      S[i + 1] <- S[i] - infected
      I[i + 1] <- I[i] + infected - removed
      R[i + 1] <- R[i] + removed
    }
    
    miOutput = data.frame(
      t = t,
      S = S,
      I = I,
      R = R
    )
    
    p <- ggplot(miOutput, aes(x = t)) +
      geom_line(aes(y = S, color = "Susceptibles"), linewidth = 1.2) +
      geom_line(aes(y = I, color = "Infectados"), linewidth = 1.2) +
      geom_line(aes(y = R, color = "Removidos"), linewidth = 1.2) +
      ggtitle("Modelo SIR Estándar") +
      labs(x = "Tiempo en días", y  = "Población") +
      scale_color_manual(name = "Compartimiento Poblacional",
                         values = c("Susceptibles" = "dodgerblue2", "Infectados" = "firebrick2", "Removidos" = "seagreen3")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        # Maintain the dark theme
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  output$grafico_r0 <- renderPlotly({
    SIRmod = function(t, state, parms)
    {
      S = state[1]
      I = state[2]
      R = state[3]
      
      beta = parms["beta"]
      mu = parms["mu"]
      gamma = parms["gamma"]
      N = parms["N"]
      
      dS = mu * (N - S) - beta * S * I/N
      dI = beta * S * I/N - (mu + gamma) * I
      dR = gamma * I - mu * R
      res = c(dS, dI, dR)
      
      list(res)
    }
    
    paras = c(mu = 0, N = 1, R0 = input$r0_input, gamma = 365/14) 
    paras["beta"] = paras["R0"] * (paras["gamma"] + paras["mu"]) 
    init = c(S = 0.999, I = 0.001, R = 0) * paras["N"]

    
    t <- seq(0, 1, by = 1/365)
    
    miOutput = as.data.frame(ode(y=init, times = t, func = SIRmod, parms = paras))
    
    p <- ggplot(miOutput, aes(x = time)) +
      geom_line(aes(y = I, color = "Infectados"), linewidth = 1.2) +
      ggtitle("Infectados en un año") +
      labs(x = "Fracción de tiempo (1 año)", y  = "Fracción de la Población") +
      scale_color_manual(name = "",
                         values = c("Infectados" = "firebrick2")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafico_SIR_estandar <- renderPlotly({
    
    f_SIR_estandar <- function(t, state, parms) {
      with(as.list(state),
           {
             dxdt = rep(0,length(state))
             
             dxdt[1] = mu * N - beta * S * I/N - mu * S
             dxdt[2] = beta * S * I/N - gamma * I - mu * I
             dxdt[3] = gamma * I - mu * R
             
             return(list(dxdt))
           }
      )
    }
    
    mu <- input$mu_input
    beta <- input$beta_input
    gamma <- input$gamma_input
    N <- 100
    
    I0 <- 1
    S0 <- N - I0
    R0 <- 0
    
    t <- seq(0,365, by = 1)
    init <- c(S=S0,I=I0,R=R0)
    
    miOutput = as.data.frame(ode(y=init, times = t, func = f_SIR_estandar, parms = NULL))
    
    p <- ggplot(miOutput, aes(x = time)) +
      geom_line(aes(y = S, color = "Susceptibles"), linewidth = 1.2) +
      geom_line(aes(y = I, color = "Infectados"), linewidth = 1.2) +
      geom_line(aes(y = R, color = "Removidos"), linewidth = 1.2) +
      ggtitle("Modelo SIR Estándar") +
      labs(x = "Tiempo en días", y  = "Población") +
      scale_color_manual(name = "Compartimiento Poblacional",
                         values = c("Susceptibles" = "dodgerblue2", "Infectados" = "firebrick2", "Removidos" = "seagreen3")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#21295C"),
        plot.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 32),
        axis.title = element_text(color = "#bcd2e2", face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(color = "#bcd2e2"),
        axis.text.y = element_text(color = "#bcd2e2")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Times New Roman", size = 14)
        ),
        # Maintain the dark theme
        paper_bgcolor = "#21295C",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}


shinyApp(ui = ui, server = server)