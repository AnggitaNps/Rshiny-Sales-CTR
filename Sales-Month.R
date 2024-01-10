library(shiny)
library(ggplot2)

# Data
data <- data.frame(
  Month = month.abb,
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Model regresi
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)

# UI untuk aplikasi web Shiny
ui <- fluidPage(
  titlePanel("Estimasi Volume Penjualan Bulanan"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("visitors", "Jumlah Pengunjung Situs Web:", min = min(data$x1), max = max(data$x1), value = 200000),
      sliderInput("transactions", "Jumlah Transaksi Bulanan:", min = min(data$x2), max = max(data$x2), value = 10000),
      sliderInput("items_per_transaction", "Rata-rata Item per Transaksi:", min = min(data$x3), max = max(data$x3), value = 5),
      sliderInput("rating", "Rating Kepuasan Pelanggan (1-10):", min = 1, max = 10, value = 8, step = 0.1),
      sliderInput("ads", "Jumlah Iklan Online:", min = min(data$x5), max = max(data$x5), value = 30000),
      actionButton("predict_button", "Hitung Estimasi Penjualan")
    ),
    mainPanel(
      plotOutput("sales_plot"),
      verbatimTextOutput("predicted_sales")
    )
  )
)

# Server logic untuk aplikasi web Shiny
server <- function(input, output) {
  output$sales_plot <- renderPlot({
    # Membuat data baru berdasarkan input pengguna
    new_data <- data.frame(
      x1 = input$visitors,
      x2 = input$transactions,
      x3 = input$items_per_transaction,
      x4 = input$rating,
      x5 = input$ads
    )
    
    # Menghitung estimasi penjualan berdasarkan model regresi
    predicted_sales <- predict(model, newdata = new_data)
    
    # Menampilkan grafik estimasi volume penjualan
    ggplot(data, aes_string(x = "x1", y = "y")) +
      geom_point() +
      geom_line(aes(y = predict(model, new_data)), color = "blue") +
      geom_point(data = new_data, aes(y = predicted_sales), color = "red", size = 4, shape = 18) +
      labs(title = "Estimasi Volume Penjualan", x = "Jumlah Pengunjung Situs Web", y = "Volume Penjualan") +
      theme_minimal()
  })
  
  output$predicted_sales <- renderPrint({
    if(input$predict_button > 0) {
      # Membuat data baru berdasarkan input pengguna
      new_data <- data.frame(
        x1 = input$visitors,
        x2 = input$transactions,
        x3 = input$items_per_transaction,
        x4 = input$rating,
        x5 = input$ads
      )
      
      # Menghitung estimasi penjualan berdasarkan model regresi
      predicted_sales <- predict(model, newdata = new_data)
      
      # Menampilkan estimasi penjualan
      paste("Estimasi volume penjualan:", round(predicted_sales, 2))
    }
  })
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
