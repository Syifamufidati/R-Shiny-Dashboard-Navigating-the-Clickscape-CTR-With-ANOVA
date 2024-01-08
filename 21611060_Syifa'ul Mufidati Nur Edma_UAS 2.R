# Install paket-paket jika belum terinstal
# install.packages(c("shiny", "dplyr", "DT", "ggplot2", "tidyr"))

# Load library
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)  # Load tidyr package

# Data contoh
data_left_sidebar <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7)
)

data_center_page <- data.frame(
  Day = 1:10,
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9)
)

data_right_sidebar <- data.frame(
  Day = 1:10,
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Combine data
all_data <- merge(data_left_sidebar, data_center_page, by = "Day", all = TRUE)
all_data <- merge(all_data, data_right_sidebar, by = "Day", all = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Click Through Rates Analyst"),
  tabsetPanel(
    tabPanel("Tambah Data",
             sidebarLayout(
               sidebarPanel(
                 numericInput("new_day", "Tambah Data - Day:", min = 1, max = 20, value = 1),
                 numericInput("new_left_sidebar", "Tambah Data - Left Sidebar:", value = 0),
                 numericInput("new_center_page", "Tambah Data - Center Page:", value = 0),
                 numericInput("new_right_sidebar", "Tambah Data - Right Sidebar:", value = 0),
                 actionButton("addDataButton", "Tambah Data")
               ),
               mainPanel(
                 DTOutput("dataTable")
               )
             )
    ),
    tabPanel("Uji ANOVA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("anova_variable", "Pilih Variabel untuk ANOVA:", names(all_data)[-1]),
                 actionButton("anovaButton", "Hitung ANOVA")
               ),
               mainPanel(
                 verbatimTextOutput("anovaResult")
               )
             )
    ),
    tabPanel("Bar Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput("analysis_type", "Pilih Ad Placement (Day 1):", unique(all_data$Day))
               ),
               mainPanel(
                 plotOutput("plot"),
                 verbatimTextOutput("analysisResult")
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Combine existing data
  reactive_data <- reactiveVal(all_data)
  
  # Render Data Table
  output$dataTable <- renderDT({
    DT::datatable(reactive_data(), options = list(pageLength = 10))
  })
  
  # Add Data
  observeEvent(input$addDataButton, {
    new_data <- data.frame(
      Day = input$new_day,
      Left_Sidebar = input$new_left_sidebar,
      Center_Page = input$new_center_page,
      Right_Sidebar = input$new_right_sidebar
    )
    reactive_data(rbind(reactive_data(), new_data))
  })
  
  # Calculate ANOVA
  output$anovaResult <- renderPrint({
    req(input$anovaButton, !is.null(input$anova_variable))
    
    if (!is.null(input$anova_variable)) {
      formula <- as.formula(paste(input$anova_variable, "~ ."))
      model <- lm(formula, data = reactive_data())
      anova_result <- anova(model)
      return(anova_result)
    }
  })
  
  # Render Bar Chart
  output$plot <- renderPlot({
    req(input$analysis_type)
    
    if (!is.null(input$analysis_type)) {
      selected_data <- filter(reactive_data(), Day == as.numeric(input$analysis_type))
      selected_data_long <- pivot_longer(selected_data, cols = c("Left_Sidebar", "Center_Page", "Right_Sidebar"))
      
      # Palet warna khusus untuk setiap variabel
      my_colors <- c("Left_Sidebar" = "lightblue",  # Warna untuk Left_Sidebar (contoh: oranye)
                     "Center_Page" = "steelblue",   # Warna untuk Center_Page (contoh: biru)
                     "Right_Sidebar" = "#D6604D"  # Warna untuk Right_Sidebar (contoh: hijau)
      )
      
      ggplot(selected_data_long, aes(x = factor(Day), y = value, fill = name)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Bar Chart of Ad Placement (Day 1) by Variable"),
             x = "Day", y = "Count", fill = "Variable") +
        scale_x_discrete(labels = as.character(selected_data_long$Day)) +
        scale_fill_manual(values = my_colors)  # Menambahkan palet warna
    }
  })
  
  
  # Calculate and render analysis results
  output$analysisResult <- renderPrint({
    req(input$analysis_type)
    
    if (!is.null(input$analysis_type)) {
      paste("Analysis details for Ad Placement (Day 1) can be added here.")
    }
  })
}

# Run the application
shinyApp(ui, server)
