library(shiny)
library(tidyverse)
library(randomForest)
library(writexl)
library(DT)
library(plotly)

#reading the final model
final_fit <- readRDS("final_rf_model.rds")

#setting the essential variables
model_vars <- c("Previously_Insured", "Vehicle_Damage", "Age", "Region_Code",
                "Vehicle_Age", "Annual_Premium", "Gender", "Seniority", "Licence_Type")
#selecting the factors
factor_vars <- c("Previously_Insured", "Vehicle_Damage", "Region_Code", 
                 "Licence_Type", "Vehicle_Age", "Gender")
#setting up the design
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f8fb;
        font-family: 'Segoe UI', sans-serif;
        color: #1a1a1a;
      }

      .navbar {
        background-color: #003366;
        border-bottom: 4px solid #005a9c;
      }

      h1, h3 {
        color: #003366;
      }

      .well {
        background-color: #e6f0fa;
        border: 1px solid #b3d1f0;
        border-radius: 10px;
      }

      .tabbable > .nav > li > a {
        color: #003366;
        font-weight: bold;
      }

      .btn {
        background-color: #005a9c;
        color: white;
        border: none;
        font-weight: bold;
        padding: 8px 16px;
      }

      .btn:hover {
        background-color: #004080;
      }

      .dataTables_wrapper {
        margin-top: 15px;
      }
    "))
  ),
  
  #picture selection
  tags$div(
    style = "background-color: #003366; padding: 15px; text-align: center;",
    tags$img(src = "https://images.unsplash.com/photo-1503376780353-7e6692767b70?ixlib=rb-4.0.3&auto=format&fit=crop&w=1350&q=80", 
             height = "200px", style = "border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.3);")
  ),
  
  titlePanel("Insurance Interest Predictor"),
  
  #setting up the part where data is uploaded as csv
  sidebarLayout(
    sidebarPanel(
      h3("Upload your CSV file for prediction"),
      fileInput(inputId = "dataset", label = "Please load your data",
                multiple = FALSE, accept = c(".csv")),
      radioButtons(inputId = "separator", 
                   label = "Separator", 
                   choices = c(comma = ",", semicolon = ";")),
      verbatimTextOutput("fileStatus")  
    ),
    #setting the manin panels of the app
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Uploaded Data", dataTableOutput("loadedData")),
                  tabPanel("Prediction", dataTableOutput("insurance_prediction")),
                  tabPanel("Prediction Distribution", 
                           plotlyOutput("prediction_plot")),
                  tabPanel("Gender and Age Statistics",  
                           h3("Gender Distribution and Average Age for 'Interested' Class"),
                           dataTableOutput("gender_age_stats")),
                  tabPanel("Download Prediction",
                           h3("Save predictions as Excel"),
                           downloadButton(outputId = "download", label = "Download as .xlsx"))
      )
    )
  )
)
#handling missing values, checking if data is not in the correct format or columns are missing
server <- function(input, output, session) {
  validate_file <- function(data) {
    missing_cols <- setdiff(model_vars, colnames(data))
    
    if (length(missing_cols) > 0) {
      return(paste("Error: The following columns are missing from the file: ", paste(missing_cols, collapse = ", ")))
    }
    return(NULL) 
  }
  
  data <- reactive({
    req(input$dataset)
    
    #loading data
    raw_data <- read_delim(file = input$dataset$datapath,
                           delim = input$separator,
                           show_col_types = FALSE)
    
    # checking the errors
    file_status <- validate_file(raw_data)
    
    # displaying feedback
    output$fileStatus <- renderText({
      if (!is.null(file_status)) {
        return(file_status)
      } else {
        return("The file is ready for prediction.")
      }
    })
    
    
    if (is.null(file_status)) {
      #saving id to display at the end
      id_column <- raw_data$id
      raw_data$id <- NULL
      
      # keeoing only variables for the model
      raw_data <- raw_data[, model_vars]
      
      #setting data types
      raw_data[factor_vars] <- lapply(raw_data[factor_vars], as.factor)
      num_vars <- setdiff(model_vars, factor_vars)
      raw_data[num_vars] <- lapply(raw_data[num_vars], as.numeric)
      
      #loading back the id
      raw_data$id <- id_column
    }
    
    raw_data
  })
  
  insurance_prediction <- reactive({
    predict_data <- data()
    

    if (is.null(predict_data)) return(NULL)
    
    #calculating probabilities and outcome
    prob <- predict(final_fit, newdata = predict_data, type = "prob")
    class <- predict(final_fit, newdata = predict_data, type = "response")
    
    #changing for names instead of 1/0 outcome
    class <- ifelse(class == 1, "Interested", "Not Interested")
    

    prob_percent <- round(prob * 100)
    colnames(prob_percent) <- c("Probability_Not Interested (%)", "Probability_Interested (%)")
    
    #Putting together the outcomes 
    prediction <- tibble(
      id = predict_data$id,
      Age = predict_data$Age, 
      Gender = predict_data$Gender,  
      Predicted_Class = class,
      `Probability_Not Interested (%)` = prob_percent[, 1],
      `Probability_Interested (%)` = prob_percent[, 2]
    )
    
    prediction
  })
  
  output$loadedData <- renderDataTable(data())
  output$insurance_prediction <- renderDataTable(insurance_prediction())
  
  #creating the chart for distribution tab
  output$prediction_plot <- renderPlotly({
    prediction_data <- insurance_prediction()
    distribution_data <- prediction_data %>%
      count(Predicted_Class) %>%
      mutate(Percentage = n / sum(n) * 100,
             Text = paste0("Count: ", n, "\nPercentage: ", round(Percentage, 1), "%"))
    
    plot_ly(data = distribution_data, 
            x = ~Predicted_Class, 
            y = ~Percentage, 
            type = 'bar', 
            name = 'Prediction Distribution', 
            marker = list(color = '#005a9c'),
            text = ~Text,  
            hoverinfo = 'text') %>%
      layout(title = 'Prediction Distribution',
             xaxis = list(title = 'Predicted Class'),
             yaxis = list(title = 'Percentage (%)'))
  })
  
  # Gender and age statistics creation
  output$gender_age_stats <- renderDataTable({
    prediction_data <- insurance_prediction()
    
    # only for interested
    interested_data <- prediction_data %>%
      filter(Predicted_Class == "Interested")
    
    #calculating the averages and counts
    gender_stats <- interested_data %>%
      group_by(Gender) %>%
      summarise(
        Count = n(),
        Average_Age = mean(Age, na.rm = TRUE)
      )
    
    gender_stats
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("insurance_predictions_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(insurance_prediction(), path = file)
    }
  )
}

shinyApp(ui = ui, server = server)
