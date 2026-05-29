# Memanggil Library
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
#========================================================
# MEMBACA DATA
#========================================================
weather <- read.csv("C:/Users/Alda Puspita/Downloads/weather.csv",sep = ";",stringsAsFactors = FALSE)
numerik_vars <- names(weather)[sapply(weather, is.numeric)]
all_vars <- names(weather)
#========================================================
# UI
#========================================================

ui <- fluidPage(
  
  titlePanel("Visualisasi Data Interaktif Dataset Weather"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "plot_type",
        "Pilih Jenis Visualisasi:",
        choices = c(
          "Scatter Plot",
          "Line Plot",
          "Bar Plot",
          "Tabel Data"
        )
      ),
      
      uiOutput("pilihan_variabel")
      
    ),
    
    mainPanel(
      
      conditionalPanel(
        condition = "input.plot_type != 'Tabel Data'",
        plotlyOutput("plot", height = "550px")
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'Tabel Data'",
        DTOutput("table")
      )
      
    )
    
  )
  
)

#========================================================
# SERVER
#========================================================

server <- function(input, output, session){
  
  #------------------------------------------------------
  # INPUT VARIABEL
  #------------------------------------------------------
  
  output$pilihan_variabel <- renderUI({
    
    tagList(
      
      selectInput(
        "xvar",
        "Pilih Variabel X:",
        choices = numerik_vars
      ),
      
      selectInput(
        "yvar",
        "Pilih Variabel Y:",
        choices = numerik_vars,
        selected = if(length(numerik_vars) > 1)
          numerik_vars[2]
        else
          numerik_vars[1]
      )
      
    )
    
  })
  
  #------------------------------------------------------
  # PLOT
  #------------------------------------------------------
  
  output$plot <- renderPlotly({
    
    req(input$xvar, input$yvar)
    
    # Scatter Plot
    if(input$plot_type == "Scatter Plot"){
      
      p <- ggplot(
        weather,
        aes_string(
          x = input$xvar,
          y = input$yvar
        )
      ) +
        geom_point(
          color = "blue",
          size = 3
        ) +
        theme_minimal() +
        labs(
          title = "Scatter Plot",
          x = input$xvar,
          y = input$yvar
        )
      
      ggplotly(p)
      
    }
    
    # Line Plot
    else if(input$plot_type == "Line Plot"){
      
      p <- ggplot(
        weather,
        aes_string(
          x = input$xvar,
          y = input$yvar
        )
      ) +
        geom_line(
          color = "red",
          linewidth = 1
        ) +
        theme_minimal() +
        labs(
          title = "Line Plot",
          x = input$xvar,
          y = input$yvar
        )
      
      ggplotly(p)
      
    }
    
    # Bar Plot
    else if(input$plot_type == "Bar Plot"){
      
      p <- ggplot(
        weather,
        aes_string(
          x = input$xvar,
          y = input$yvar
        )
      ) +
        geom_col(
          fill = "orange"
        ) +
        theme_minimal() +
        labs(
          title = "Bar Plot",
          x = input$xvar,
          y = input$yvar
        )
      
      ggplotly(p)
      
    }
    
  })  
  #------------------------------------------------------
  # TABEL DATA
  #------------------------------------------------------
  
  output$table <- renderDT({
    
    datatable(
      weather,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
}   
#========================================================
# MENJALANKAN APLIKASI
#========================================================

shinyApp(
  ui = ui,
  server = server
)
