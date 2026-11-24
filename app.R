library(shiny)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)

# Load and organize the wine data
wine <- read_csv("AustralianWines.csv", na = c("", "NA", "*"), show_col_types = FALSE)
names(wine) <- str_to_title(trimws(names(wine)))
wine <- wine |>
  mutate(Month = yearmonth(my(Month))) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

# Create the layout of the app
ui <- fluidPage(
  titlePanel("Australian Wine Sales Forecasting"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variety", "Wine varieties", choices = unique(wine$Varietal),
                  selected = c("Red", "Sparkling"), multiple = TRUE),
      dateRangeInput("dates", "Date range", start = min(as.Date(wine$Month)),
                     end = max(as.Date(wine$Month)), min = min(as.Date(wine$Month)),
                     max = max(as.Date(wine$Month))),
      checkboxGroupInput("models", "Forecasting models",
                         choices = c("TSLM", "ETS", "ARIMA"),
                         selected = c("TSLM", "ETS", "ARIMA")),
      sliderInput("split", "Training data (%)", min = 60, max = 90,
                  value = 80, step = 5),
      sliderInput("horizon", "Future forecast (months)", min = 1, max = 24,
                  value = 12),
      helpText("The remaining data is used to test the models.")),
    mainPanel(
      tabsetPanel(
        tabPanel("Sales Overview", plotOutput("salesPlot", height = "450px"),
                 textOutput("splitInfo")),
        tabPanel("Model Specifications", tableOutput("specs")),
        tabPanel("Model Accuracy", h4("Training and validation accuracy"),
                 tableOutput("metrics"), h4("Best model by validation RMSE"),
                 tableOutput("best")),
        tabPanel("Forecasts", h4("Validation forecasts"),
                 plotOutput("validationPlot", height = "450px"),
                 h4("Future forecasts"), plotOutput("futurePlot", height = "450px")),
        tabPanel("About", h3("About this app"),
                 p("This app uses Australian wine sales data to compare
           TSLM, ETS, and ARIMA models."),
                 p("Use the sidebar to select wine varieties, dates,
           models, training split, and forecast period."),
                 p("The best model is selected using the lowest
           validation RMSE."),
                 p("Forecasts include 80% and 95% prediction intervals."),
                 p("Missing sales values are excluded from modeling
           by requiring a complete date range."),
                 p("ChatGPT was used to assist with writing the code."))))))

server <- function(input, output, session) {
  # Filter the data based on the user's selections
  filtered <- reactive({
    validate(need(length(input$variety) > 0, "Select at least one wine variety."))
    req(input$dates)
    data <- wine |>
      filter(Varietal %in% input$variety,
             Month >= yearmonth(input$dates[1]), Month <= yearmonth(input$dates[2]))
    validate(need(n_distinct(data$Month) >= 48, "Select at least 48 months of data."))
    validate(need(!any(is.na(data$Sales)),
                  "Selected data contains missing sales. Choose another variety or date range."))
    data})

  # Split the data in chronological order
  trainEnd <- reactive({
    months <- sort(unique(filtered()$Month))
    total_months <- length(months)
    train_size <- floor(total_months * input$split / 100)
    months[train_size]})
  training <- reactive({ filtered() |> filter(Month <= trainEnd()) })
  validation <- reactive({ filtered() |> filter(Month > trainEnd()) })

  # Fit the three forecasting models
  fitModels <- function(data) {
    validate(need(length(input$models) > 0, "Select at least one model."))
    fits <- data |>
      model(TSLM = TSLM(Sales ~ trend() + season()),
            ETS = ETS(Sales), ARIMA = ARIMA(Sales))
    fits |> select(all_of(input$models))}
  fittedModels <- reactive({ fitModels(training()) })
  validationForecast <- reactive({
    forecast(fittedModels(), new_data = validation())})
  futureForecast <- reactive({
    forecast(fitModels(filtered()), h = input$horizon)})

  # Plot historical sales and the training cutoff
  output$salesPlot <- renderPlot({
    ggplot(filtered(), aes(x = as.Date(Month), y = Sales, color = Varietal)) +
      geom_line() + facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      geom_vline(xintercept = as.numeric(as.Date(trainEnd())), linetype = "dashed") +
      labs(x = "Month", y = "Sales", title = "Historical wine sales") +
      theme_minimal() + theme(legend.position = "none")})
  output$splitInfo <- renderText({
    paste("Training ends:", trainEnd(), "| Validation begins:", min(validation()$Month))})

  # Show the model specifications from the training data
  output$specs <- renderTable({
    fits <- fittedModels()
    # Report each model using its short printed description
    specs <- as_tibble(fits)
    specs |>
      mutate(across(all_of(input$models), format)) |>
      pivot_longer(-Varietal, names_to = "Model",
                   values_to = "Specification")})

  # Compare accuracy on training and validation data
  accuracyResults <- reactive({
    trainAccuracy <- accuracy(fittedModels()) |> mutate(Window = "Training")
    testAccuracy <- accuracy(validationForecast(), filtered()) |> mutate(Window = "Validation")
    bind_rows(trainAccuracy, testAccuracy) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE)})
  output$metrics <- renderTable({ accuracyResults() }, digits = 2)
  output$best <- renderTable({
    accuracyResults() |> filter(Window == "Validation", is.finite(RMSE)) |>
      group_by(Varietal) |> slice_min(RMSE, n = 1, with_ties = FALSE) |>
      ungroup() |> select(Varietal, Model, RMSE)}, digits = 2)

  # Display forecasts with prediction intervals
  output$validationPlot <- renderPlot({
    autoplot(validationForecast(), filtered(), level = c(80, 95)) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales") + theme_minimal()})
  output$futurePlot <- renderPlot({
    autoplot(futureForecast(), filtered(), level = c(80, 95)) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales") + theme_minimal()})}

shinyApp(ui = ui, server = server)
