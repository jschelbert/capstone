## app.R ##
library(data.table)
library(dplyr)
library(shiny)
library(shinydashboard)

## source prediction function
source("../src/predict_stupid_backoff.R")

## load the n-gram data
folder <- "../data/final/en_US/"
ngrams_5 <<- readRDS(paste0(folder, "5-grams_DT_sb_threshold_reduced.rds"))
ngrams_4 <<- readRDS(paste0(folder, "4-grams_DT_sb_threshold_reduced.rds"))
ngrams_3 <<- readRDS(paste0(folder, "3-grams_DT_sb_threshold_reduced.rds"))
ngrams_2 <<- readRDS(paste0(folder, "2-grams_DT_sb_threshold_reduced.rds"))
ngrams_1 <<- readRDS(paste0(folder, "1-grams_DT_sb_threshold_reduced.rds"))
setDT(ngrams_5)
setDT(ngrams_4)
setDT(ngrams_3)
setDT(ngrams_2)
setDT(ngrams_1)
setkey(ngrams_5, ngram)
setkey(ngrams_4, ngram)
setkey(ngrams_3, ngram)
setkey(ngrams_2, ngram)
setkey(ngrams_1, ngram)
setorder(ngrams_5, ngram, -sb_score)
setorder(ngrams_4, ngram, -sb_score)
setorder(ngrams_3, ngram, -sb_score)
setorder(ngrams_2, ngram, -sb_score)
setorder(ngrams_1, -sb_score)
gc()


ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "PredWordR"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "blah!", badgeColor = "green"),
      menuItem("Explanation", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")),
      menuItem("Source code", icon = icon("file-code-o"), 
               href = "https://github.com/jschelbert/capstone/tree/master/app/"),
      menuItem("Contact", icon = icon("address-card-o"), tabName = "contact")
      )
  ),
  ## Body content
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              textInput("usertext", label = h3("Text input"), 
                        placeholder = "Enter your text..."),
              verbatimTextOutput("predictionvalue")
      ),
      tabItem("subitem1", "Sub-item 1 tab content"),
      tabItem("subitem2", "Sub-item 2 tab content") 
      )
    
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$predictionvalue <- renderText({ 
    paste(predict_stupid_backoff_DT(input$usertext), collapse = ", ")
    })
}

shinyApp(ui, server)