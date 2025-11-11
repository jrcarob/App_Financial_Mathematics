library(shiny)
library(bslib)
library(ggplot2)
library(DT)
# Removed: library(plotly) 
library(scales)
library(dplyr) # Added for data manipulation in the DT output

ui <- page_fillable( 
  theme = bs_theme(bootswatch = "flatly"),
  
  # 1. Main Banner Title (Now completely separate from tabs)
  tags$div(
    style = "padding: 15px 0; text-align: center; background-color: #f8f9fa; border-bottom: 3px solid #e74c3c;",
    tags$h1(
      style = "font-size: 2.5rem; font-weight: 700; color: #2c3e50; margin: 0;",
      # Logo: Placeholder Image URL (Cannot use local path)
      tags$img(src = "logo.png", class = "navbar-logo", height = "90px", alt = "Financial Maths Logo"),
      #tags$img(src = "https://placehold.co/40x40/2ecc71/ffffff?text=€", style = "margin-right: 15px; border-radius: 5px;"),
      "Calculadora de Crecimiento Financiero"
    ),
    tags$h3(
      "Comparación Interés Simple vs. Interés Compuesto", 
      style = "font-size: 1.4rem; color: #7f8c8d;"
    )
  ),
 
  # 2. Tabs implemented using navset_card_tab (Appears below the banner)
  navset_card_tab(
    id = "main_tabs",
    
    # Calculator Tab
    nav_panel(
      "Calculadora",
      page_sidebar(
      sidebar = sidebar(
        h4("Parámetros de la Inversión"),
        p("Ajustar los parámetros anteriores para ver cómo se comparan el interés simple y el interés compuesto a lo largo del tiempo."),
        numericInput(
          "principal",
          "Cantidad del Principal (€)",
          value = 1000,
          min = 1,
          step = 100
        ),
        numericInput(
          "rate",
          "Tipo Interés Anual (%)",
          value = 5,
          min = 0.01,
          max = 50,
          step = 0.1
        ),
        numericInput(
          "time",
          "Periodo Temporal (años)",
          value = 10,
          min = 1,
          max = 50,
          step = 1
        ),
        numericInput(
          "compound_freq",
          "Frequencia del Compuesto (veces por año)",
          value = 12,
          min = 1,
          max = 365,
          step = 1
        ),
        hr(),
        p("© (2025) José Rafael Caro Barrera & Jesús Pérez Gálvez"),
        p("Área de Economía Aplicada"),
        p("Este trabajo está bajo licencia "), 
        tags$img(src = "cc.png", class = "navbar-logo", width = "80px", height = "28px", alt = "Creative Commons"),
        p("DOI: 10.5281/zenodo.17579699"),
        p("Universidad de Córdoba")
      ),
      
      layout_columns(
        card(
          card_header("Fórmulas Matemáticas"),
          # FIX: Using direct CSS Grid instead of layout_columns to guarantee 2-column rendering
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; padding: 10px;", 
            div(
              h5("Interés Simple"),
              withMathJax("$$C_n = C_0·(1 + i · n)$$"),
              tags$ul(
                tags$li("\\(C_n\\) = Capital Final"),
                tags$li("\\(C_0\\) = Principal"),
                tags$li("\\(i\\) = Tipo interés anual (decimal)"),
                tags$li("\\(n\\) = Tiempo en años")
              )  
            ),
            div(
            #hr(),
              h5("Interés Compuesto"),
              withMathJax("$$C_n = C_0·\\left(1 + i\\right)^{n}$$"),
              tags$ul(
                tags$li("\\(C_n\\) = Capital Final"),
                tags$li("\\(C_0\\) = Principal"),
                tags$li("\\(i\\) = Tipo interés anual  (decimal)"),
                tags$li("\\(n\\) = Tiempo en años")
              )
            )
          )
          # End of revised section
        ),
        
        card(
          card_header("Cálculos Actuales"),
          tableOutput("calculations_table")
        )
      ),
      
      layout_columns(
        card(
          card_header(
            # Plot header with download button
            div("Comparación del Interés en el Tiempo", 
                downloadButton("downloadPlot", "Descargar gráfico (JPEG)", class = "btn-sm float-end")
            )
          ),
          plotOutput("comparison_plot", height = "400px")
        ),
        
        card(
          card_header("Desglose Anual"),
          DT::dataTableOutput("yearly_table")
        )
      )
    )
  ),

  # Theory Tab (nav_panel is replaced by tabPanel)
  nav_panel(
    "Explicación Teórica",
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Interés Simple"),
        div(
          h4("¿Qué es el interés Simple?"),
          p("El interés simple se calcula solo sobre el capital inicial (o inversión inicial). El interés devengado 
            en cada período permanece constante y no se añade al capital para cálculos futuros."),
          
          h5("Fórmula:"),
          withMathJax("$$C_n = C_0·(1 + i·n)$$"),
          p("Esto significa que si se invierten \\(1.000\\) € al \\(5\\%\\) en interés simple \\(10\\) años, 
            se generarán \\(50\\) € cada año, hasta un total de \\(500\\) € en intereses."),
          
          h5("Características:"),
          tags$ul(
            tags$li("Los intereses se calculan solo sobre el principal inicial"),
            tags$li("Los intereses devengados permanecen constantes cada periodo"),
            tags$li("Interés total = \\(C_0 \\times i \\times n\\)"),
            tags$li("El patrón de crecimiento es lineal")
          ),
          
          h5("Ejemplo del mundo real:"),
          tags$ul(
            tags$li("Algunos préstamos y bonos"),
            tags$li("Ciertas cuentas de ahorro"),
            tags$li("Préstamos a corto plazo")
          )
        )
      ),
      
      card(
        card_header("Interés Compuesto"),
        div(
          h4("¿Qué es el interés compuesto?"),
          p("El interés compuesto se calcula tanto sobre el capital como sobre los intereses acumulados de 
            períodos anteriores.Esto crea un efecto compuesto en el que se obtienen «intereses sobre los intereses»."),
          
          h5("Fórmula:"),
          withMathJax("$$C_n = C_0·\\left(1 + i\\right)^{n}$$"),
          p("Cuanto más frecuente sea la capitalización de intereses, más intereses se devengan. 
            La capitalización diaria generará más que la mensual, que a su vez generará más que la anual."),
          
          h5("Características:"),
          tags$ul(
            tags$li("El interés se calcula sobre el capital + los intereses acumulados"),
            tags$li("Los intereses obtenidos aumentan cada período"),
            tags$li("El patrón de crecimiento es exponencia"),
            tags$li("Mayor frecuencia de capitalización = mayores rendimientos")
          ),
          
          h5("Ejemplos del mundo real:"),
          tags$ul(
            tags$li("La mayoría de las cuentas de ahorro"),
            tags$li("Certificados de Depósito (CDs)"),
            tags$li("Cuentas de inversión"),
            tags$li("Planes de pensiones")
          )
        )
      ),
      
      card(
        card_header("Diferencias clave"),
        div(
          h4("Interés Simple vs. Compuesto"),
          
          h5("Patrón de Crecimiento:"),
          tags$ul(
            tags$li(strong("Interés Simple:"), "Crecimiento lineal - misma cantidad añadida cada periodo"),
            tags$li(strong("Interés Compuesto:"), "Crecimiento exponencial - rendimientos acelerados")
          ),
          
          h5("Base de Cálculo:"),
          tags$ul(
            tags$li(strong("Interés Simple:"), "Calculado solo sobre el principal"),
            tags$li(strong("Interés Compuesto:"), "Calculado sobre el principal + intereses previos acumulados")
          ),
          
          h5("Impacto del Tiempo:"),
          tags$ul(
            tags$li(strong("A Corto plazo:"), "Mínima diferencia entre los dos"),
            tags$li(strong("A Largo plazo:"), "El interés compuesto supera significativamente al interés simple")
          ),
          
          h5("El Poder de la Capitalización Compuesta:"),
          p("Albert Einstein supuestamente llamó al interés compuesto «la octava maravilla del mundo». 
            La idea clave es que pequeñas diferencias en las tasas de interés, o la frecuencia de capitalización,
            pueden conducir a resultados dramáticamente diferentes a lo largo de períodos prolongados.")
        )
      ),
      
      card(
        card_header("Aspectos Matemáticos"),
        div(
          h4("Entendiendo las Matemáticas"),
          
          h5("Por Qué el Interés Compuesto Crece más Rápido:"),
          p("En el interés simple, si tenemos 1.000 € al 5 % en 2 años:"),
          tags$ul(
            tags$li("Año 1: \\(1.000\\ € + 50\\ € = 1.050\\ €\\)"),
            tags$li("Año 2: \\(1.050\\ € + 50\\ € = 1.100\\ €\\) (intereses aún sobre el original \\(1.000\\ €\\))")
          ),
          
          p("A Interés Compuesto:"),
          tags$ul(
            tags$li("Año 1: \\(1.000\\ € + 50\\ € = 1.050\\ €\\)"),
            tags$li("Año 2: \\(1.050\\ € + 52,50\\ € = 1.102{,}50\\ €\\) (intereses sobre \\(1.050\\ €\\))")
          ),
          
          h5("Regla del 72:"),
          p("Una forma rápida de calcular cuánto tiempo tarda el dinero en duplicarse con el interés compuesto es 
            dividir \\(72\\) entre el tipo de interés. Al \\(6\\%\\) de interés, el dinero se duplica en aproximadamente \\(12\\) años:"),
          withMathJax("$$\\text{Tiempo de Duplicación} \\approx \\frac{72}{\\text{Tipo de Interés}\\%}$$"),
          
          h5("Efecto de la Frecuencia Compuesta:"),
          p("Una capitalización más frecuente genera mayores rendimientos:"),
          tags$ul(
            tags$li(withMathJax("Anual: \\(C_n = C_0·(1 + i)^n\\)")),
            tags$li(withMathJax("Mensual: \\(C_n = C_0\\left(1 + \\frac{i}{12}\\right)^{12·n}\\)")),
            tags$li(withMathJax("Diaria: \\(C_n = C_0\\left(1 + \\frac{i}{365}\\right)^{365·n}\\)")),
            tags$li(withMathJax("Perpetua: \\(C_n = C_0·e^{i·n}\\)"))
          )
         )
        )
       )
      )
    )
  )

server <- function(input, output, session) {
  
  # Reactive calculations
  calculations <- reactive({
    P <- input$principal
    r <- input$rate / 100 # Convert percentage to decimal
    t <- input$time
    n <- input$compound_freq
    
    # Simple Interest
    simple_amount <- P * (1 + r * t)
    simple_interest <- simple_amount - P
    
    # Compound Interest
    compound_amount <- P * (1 + r/n)^(n * t)
    compound_interest <- compound_amount - P
    
    # Difference
    difference <- compound_amount - simple_amount
    
    list(
      simple_amount = simple_amount,
      simple_interest = simple_interest,
      compound_amount = compound_amount,
      compound_interest = compound_interest,
      difference = difference
    )
  })
  
  # Yearly data for plotting and table
  yearly_data <- reactive({
    P <- input$principal
    r <- input$rate / 100
    t <- input$time
    n <- input$compound_freq
    
    years <- 0:t
    
    simple_amounts <- P * (1 + r * years)
    compound_amounts <- P * (1 + r/n)^(n * years)
    
    data.frame(
      Año = years,
      Simple_Interest = simple_amounts,
      Compound_Interest = compound_amounts,
      Difference = compound_amounts - simple_amounts,
      Simple_Interest_Earned = simple_amounts - P,
      Compound_Interest_Earned = compound_amounts - P
    )
  })
  
  # Summary calculations table
  output$calculations_table <- renderTable({
    calc <- calculations()
    
    data.frame(
      Tipo = c("Capital Final", "Intereses Ganados", "Ventaja Interés Compuesto"),
      Interés_Simple = c(
        # EDITED: Currency symbol moved to suffix and formatting changed to European style (period for thousands, comma for decimal)
        paste0(format(calc$simple_amount, big.mark = ".", decimal.mark = ",", digits = 2, nsmall = 2), " €"), 
        paste0(format(calc$simple_interest, big.mark = ".", decimal.mark = ",", digits = 2, nsmall = 2), " €"), 
        "-"
      ),
      Interés_Compuesto = c(
        # EDITED: Currency symbol moved to suffix and formatting changed to European style
        paste0(format(calc$compound_amount, big.mark = ".", decimal.mark = ",", digits = 2, nsmall = 2), " €"), 
        paste0(format(calc$compound_interest, big.mark = ".", decimal.mark = ",", digits = 2, nsmall = 2), " €"), 
        paste0(format(calc$difference, big.mark = ".", decimal.mark = ",", digits = 2, nsmall = 2), " €") 
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Base plot function (to be used by both renderPlot and downloadHandler)
  create_plot <- function(data, input) {
    ggplot(data, aes(x = Año)) +
      geom_line(aes(y = Simple_Interest, color = "Interés Simple"), linewidth = 1.2) + # Translated legend key
      geom_line(aes(y = Compound_Interest, color = "Interés Compuesto"), linewidth = 1.2) + # Translated legend key
      geom_ribbon(aes(ymin = Simple_Interest, ymax = Compound_Interest, fill = "Compound Advantage"), alpha = 0.3) +
      scale_color_manual(values = c("Interés Simple" = "#3498db", "Interés Compuesto" = "#e74c3c")) +
      scale_fill_manual(values = c("Compound Advantage" = "#2ecc71")) +
      scale_x_continuous(
        breaks = seq(0, input$time, by = 1), 
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "€")) + 
      labs(
        title = paste("Crecimiento de la Inversión: €", format(input$principal, big.mark = ","), " al ", input$rate, "% en ", input$time, " años"), # Translated Title
        x = "Años", # Translated X-Label
        y = "Capital (€)", # Translated Y-Label
        color = "Tipo de Interés" # Translated Legend Title
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "bottom"
      ) +
      guides(fill = "none")
  }
  
  # Comparison plot (Displayed)
  output$comparison_plot <- renderPlot({
    data <- yearly_data()
    create_plot(data, input)
  })
  
  # Download Handler for the Plot (JPEG)
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("crecimiento_financiero_plot-", Sys.Date(), ".jpeg", sep = "")
    },
    content = function(file) {
      data <- yearly_data()
      p <- create_plot(data, input) # Use the base function to generate the plot
      
      # Save the plot as JPEG
      ggsave(file, plot = p, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)
    }
  )
  
  # Yearly breakdown table (ADDED)
  output$yearly_table <- DT::renderDataTable({
    data <- yearly_data()
    
    # Select and rename columns for a clearer presentation in the table
    data_display <- data %>%
      dplyr::select(
        Año,
        Capital_Simple = Simple_Interest,
        Ganancia_con_I._Simple = Simple_Interest_Earned,
        Capital_Compuesto = Compound_Interest,
        Ganancia_con_I._Compuesto = Compound_Interest_Earned,
        Diferencia = Difference
      )
    
    # Use DT formatting functions
    DT::datatable(
      data_display,
      extensions = 'Buttons', # ADDED: Enable buttons extension
      options = list(
        pageLength = 10,
        searching = FALSE,
        dom = 'Bfrtip', # ADDED: Specify DOM elements to include buttons (B)
        buttons = list( # ADDED: Define which buttons to show
          list(extend = 'excel', text = 'Exportar a Excel'),
          list(extend = 'csv', text = 'Exportar a CSV')
        )
      ),
      rownames = FALSE
    ) %>% 
      formatCurrency(
        columns = c("Capital_Simple", "Ganancia_con_I._Simple", "Capital_Compuesto", "Ganancia_con_I._Compuesto", "Diferencia"),
        currency = "€",
        interval = 3,
        mark = ",",
        digits = 2,
        before = FALSE # ADDED: Set to FALSE to place the currency symbol after the number
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)