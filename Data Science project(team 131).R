library(readxl)
library(reader)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
##########################################################################################################
# Define UI
ui <- fluidPage(
  theme = shinytheme("readable"),
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel( # input
      fileInput("file", "Choose Your File" , accept = c(".csv",
                                                        ".xls",
                                                        ".xlsx")),
      actionButton("load", "Load Data"),
      tags$br(),tags$br(),
      numericInput("num_clusters", "Number of Clusters (2-4):", value = 2, min = 2, max = 4),
      actionButton("perform_clustering", "Perform Clustering"),
      tags$br(),tags$br(),
      numericInput("minSupport", "Minimum Support for Apriori:",value = 0.000 ,  min = 0.001 , max = 1),
      numericInput("minConfidence", "Minimum Confidence for Apriori:",value = 0.000 ,  min = 0.001 , max = 1),
      actionButton("runApriori", "Run Apriori Analysis")
    ),
    
    mainPanel(
      tabsetPanel( # output
        tabPanel("Original Data", tableOutput("original_data")),
        tabPanel("Cleaned Data", tableOutput("cleaned_data")),
        tabPanel("outlier", plotOutput("outlier")),
        # tabPanel("Boxplot", plotOutput("boxplot")),
        # tabPanel("Age vs Total Spending", plotOutput("age_plot")),
        # tabPanel("City Total Spending", plotOutput("city_plot")),
        # tabPanel("Payment Type Distribution", plotOutput("payment_type_plot")),
        tabPanel("Dashboard", 
                 fluidRow(
                   column(12, plotOutput("boxplot")),
                   column(12, plotOutput("age_plot"))
                 ),
                 fluidRow(
                   column(12, plotOutput("city_plot")),
                   column(12, plotOutput("payment_type_plot"))
                 )),
        tabPanel("Customer Data", tableOutput("customer_data")),
        tabPanel("Cluster Visualization", plotOutput("cluster_plot")),
        tabPanel("Apriori Output", tableOutput("aprioriOutput"))
      )
    )
  )
)
#####################################################################################################################

server <- function(input, output) {
  
  # Reactive expression to load and process data
  
  data <- eventReactive(input$load, {
    req(input$file)  # Require file to be selected
    inFile <- input$file
    file_extension <- tools::file_ext(inFile$datapath)
    
    if (file_extension == "csv") {
      data <- read.csv(inFile$datapath)
    } else if (file_extension %in% c("xlsx", "xls")) {
      data <- read_excel(inFile$datapath)
    }
    
    # Remove duplicates and NA values
    cleaned_data <- unique(data)
    cleaned_data <- na.omit(cleaned_data)
    
    return(cleaned_data)
  })
  
  
  # Render original data table
  output$original_data <- renderTable({
    req(input$load)
    data()
  })
  
  # Render cleaned data table
  output$cleaned_data <- renderTable({
    req(input$load)
    data()
  })
  
  ##############################outlier################################
  
  
  output$outlier <- renderPlot({
    req(input$load)
    cleaned_data <- data()
    boxplot(cleaned_data[,c(2,3,4,6)])
    # Remove outliers
    outlier=boxplot(cleaned_data$count)$out
    outlier
  })
  
  ####################################################################
  # Render boxplot for total spending distribution
  
  output$boxplot <- renderPlot({
    req(input$load)
    cleaned_data <- data()
    
    # Check if 'total' column exists and is numeric
    if ("total" %in% names(cleaned_data) && is.numeric(cleaned_data$total)) 
    {
      boxplot(cleaned_data$total, 
              main = "Distribution of Total Spending", 
              col = "gray", border = "steelblue", 
              xlab = "Total Spending", ylab = "Frequency")
    } 
  })
  
  # Render age vs total spending plot
  output$age_plot <- renderPlot({
    req(input$load)
    age_data <- data() %>% 
      group_by(age) %>%
      summarise(total_spending = sum(total))
    barplot(age_data$total_spending, 
            names.arg = age_data$age, 
            border = "steelblue", 
            col = "gray", 
            main = "Total Spending by Age",
            xlab = "Age",
            ylab = "Total Spending")
  })
  
  # Render city total spending plot
  output$city_plot <- renderPlot({
    req(input$load)
    grouped_data <- data() %>% 
      group_by(city) %>%
      summarise(total_spending = sum(total))
    
    city_total_spending <- grouped_data[order(-grouped_data$total_spending), ]
    
    barplot(city_total_spending$total_spending,
            names.arg = city_total_spending$city,
            main = "Total Spending by City (Descending)",
            col = "gray",
            border = "steelblue",
            xlab = "City",
            ylab = "Total Spending")
  })
  
  # Render payment type distribution pie chart
  output$payment_type_plot <- renderPlot({
    req(input$load)
    x=table(new_data$paymentType)
    # Calculate percentages for each payment type
    percentage=paste(round(x/sum(x)*100),"%")
    # Create the pie chart
    pie(x,labels = percentage,main="compare cash and credit totals",col=c("gray","steelblue"))
    legend("bottomleft",legend=c("credit","cash"),fill=c("gray","steelblue"))
    
  })
  
  ##########################apriori########################################
  
  observeEvent(input$runApriori, {
    req(input$load)  # Make sure input$load is appropriately triggering data loading
    cleaned_data <- data()  # Ensure this function returns data in a correct format
    
    # Convert to transactions
    AS<-strsplit(cleaned_data$items, ",")
    AS <- as(AS, "transactions")
    
    # Running Apriori algorithm
    
    
    rules <- apriori(AS,
                     parameter = list(supp = input$minSupport, conf = input$minConfidence, target = "rules"),
                     control = list(verbose = FALSE))
    
    rules <- sort(rules, by = "confidence", decreasing = TRUE)
    
    output$aprioriOutput <- renderTable({
      if (length(rules) > 0) {
        inspect(rules)  # Show top 5 rules or adjust as needed
      } else {
        "No rules found"
      }
    })
  })
  
  ##########################clustering########################################
  
  # Reactive value for clustered data
  clustered_data <- reactiveVal(NULL)
  
  observeEvent(input$perform_clustering, {
    # Validate number of clusters input
    num_clusters <- as.integer(input$num_clusters)
    if (is.na(num_clusters) || num_clusters < 2 || num_clusters > 4) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Please enter a number between 2 and 4 for the number of clusters."
      ))
    } else {
      # Perform K-means clustering
      clusterdata <- new_data %>% 
        group_by(customer, age) %>% 
        summarise(newTotal = sum(total))
      
      scaled_data <- data.frame(clusterdata$age, clusterdata$newTotal)
      
      kmeans_result <- kmeans(scaled_data, centers = num_clusters)
      
      # Assign cluster numbers to each customer
      finalData <- cbind(clusterdata, Cluster = as.factor(kmeans_result$cluster))
      colnames(finalData) <- c("name", "age", "total", "Cluster")
      
      # Update reactive value with clustered data
      clustered_data(finalData)
    }
  })
  
  # Render customer data table
  output$customer_data <- renderTable({
    req(clustered_data())
    clustered_data() %>% select(name, age, total, Cluster)
  })
  
  # Render cluster visualization plot
  output$cluster_plot <- renderPlot({
    req(clustered_data())
    ggplot(clustered_data(), aes(x = age, y = total, color = Cluster)) +
      geom_point() +
      labs(title = "Customer Clustering", color = "Cluster") +
      theme_minimal()
  })
}
#########################################################################################################################################
# Run the application

shinyApp(ui = ui, server = server)