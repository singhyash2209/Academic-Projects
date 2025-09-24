# ==============================================
# Final Grants Dashboard - Enhanced Y-Axis Spacing
# ==============================================

# --- Load Libraries ---
library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(readr)

# --- Load Dataset ---
grants_with_naics <- read_csv("grants_with_naics.csv")

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("body { background-color: white; }"))  # Set white background
  ),
  
  titlePanel("🧠 Federal Grants Insight Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("naics_code", "Filter by NAICS Code:"),
      textInput("keyword", "Search by Keyword:"),
      actionButton("search_btn", "Search Grants")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               div(style = "background-color:#e3f2fd;padding:20px;border-radius:10px;margin-bottom:20px;",
                   h4("Total Opportunities", align = "center"),
                   h3(textOutput("total_opportunities"), align = "center"))
        ),
        column(6,
               div(style = "background-color:#bbdefb;padding:20px;border-radius:10px;margin-bottom:20px;",
                   h4("Sum of CFDA", align = "center"),
                   h3(textOutput("total_cfda"), align = "center"))
        )
      ),
      
      br(),
      DTOutput("results_table"),
      
      br(), br(), br(), br(),
      
      plotlyOutput("agency_bar_chart", height = "400px"),
      
      br(), br(), br(), br(),
      
      plotlyOutput("status_pie_chart", height = "400px"),
      
      br(), br(), br(), br(),
      
      plotlyOutput("cfda_by_agency", height = "400px")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  filtered_data <- reactiveVal(grants_with_naics)
  
  observeEvent(input$search_btn, {
    df <- grants_with_naics
    if (input$naics_code != "") df <- df %>% filter(`Mapped NAICS Code` == input$naics_code)
    if (input$keyword != "") df <- df %>% filter(grepl(input$keyword, Keyword, ignore.case = TRUE))
    if (nrow(df) == 0) {
      filtered_data(data.frame(Message = "No matching records."))
    } else {
      filtered_data(df)
    }
  })
  
  output$total_opportunities <- renderText({
    df <- filtered_data()
    if ("Message" %in% colnames(df)) return("0")
    nrow(df)
  })
  
  output$total_cfda <- renderText({
    df <- filtered_data()
    if ("Message" %in% colnames(df)) return("0")
    df$CFDA <- as.numeric(df$CFDA)
    round(sum(df$CFDA, na.rm = TRUE), 2)
  })
  
  output$results_table <- renderDT({
    filtered_data()
  })
  
  # --- Agency Bar Chart with Higher Y-Axis ---
  output$agency_bar_chart <- renderPlotly({
    df <- filtered_data()
    if ("Message" %in% colnames(df)) return(NULL)
    
    plot_data <- df %>%
      group_by(Agency) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    max_y <- max(plot_data$Count, na.rm = TRUE)
    
    plot_ly(plot_data, x = ~Agency, y = ~Count, type = "bar",
            marker = list(color = '#1565C0')) %>%
      layout(
        title = "Opportunities by Agency",
        xaxis = list(title = "Agency", tickangle = -45),
        yaxis = list(title = "Count", range = c(0, max_y * 1.2))
      )
  })
  
  # --- Pie Chart ---
  output$status_pie_chart <- renderPlotly({
    df <- filtered_data()
    if ("Message" %in% colnames(df)) return(NULL)
    
    df %>%
      group_by(Status) %>%
      summarise(Count = n()) %>%
      plot_ly(labels = ~Status, values = ~Count, type = "pie",
              marker = list(colors = c('#1976D2', '#90CAF9'))) %>%
      layout(title = "Grant Status Distribution")
  })
  
  # --- CFDA Bar Chart with Higher Y-Axis ---
  output$cfda_by_agency <- renderPlotly({
    df <- filtered_data()
    if ("Message" %in% colnames(df)) return(NULL)
    
    df$CFDA <- as.numeric(df$CFDA)
    plot_data <- df %>%
      group_by(Agency) %>%
      summarise(Total_CFDA = sum(CFDA, na.rm = TRUE))
    
    max_cfda <- max(plot_data$Total_CFDA, na.rm = TRUE)
    
    plot_ly(plot_data, x = ~Agency, y = ~Total_CFDA, type = "bar",
            marker = list(color = '#1E88E5')) %>%
      layout(
        title = "Total CFDA by Agency",
        xaxis = list(title = "Agency", tickangle = -45),
        yaxis = list(title = "Total CFDA", range = c(0, max_cfda * 1.2))
      )
  })
}

# --- Launch App ---
shinyApp(ui, server)
