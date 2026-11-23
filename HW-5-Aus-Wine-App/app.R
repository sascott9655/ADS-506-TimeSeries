library(shiny)
library(bslib)
library(mixtime)
library(fpp3)
library(gt)
library(urca)

# ---- Data -------------------------------------------------------------------

varietals <- c("Dry.white", "Red", "sparkling")
varietal_choices <- c("Dry white" = "Dry.white", "Red" = "Red", "Sparkling" = "sparkling")

aus_wine_ts <- read.csv(
  file.path("data", "AustralianWines.csv"),
  na.strings = c("", "*", "NA", "N/A")
) |>
  pivot_longer(
    cols      = c(Fortified, Red, Rose, sparkling, Sweet.white, Dry.white),
    names_to  = "Varietal",
    values_to = "Sales"
  ) |>
  mutate(Month = yearmonth(my(Month))) |>
  as_tsibble(index = Month, key = Varietal) |>
  filter(Varietal %in% varietals)

train_end  <- 1992
training   <- filter(aus_wine_ts, year(Month) <= train_end)
validation <- filter(aus_wine_ts, year(Month) > train_end)
n_val      <- n_distinct(validation$Month)


# ---- Models (fit once at startup) ---------------------------------------------

fit_models <- function(data) {
  model(
    data,
    lm    = TSLM(Sales ~ trend() + season()),
    ets   = ETS(Sales),
    arima = ARIMA(Sales)
  )
}

fit_train <- fit_models(training)
fit_full  <- fit_models(aus_wine_ts)

fc_val         <- forecast(fit_train, h = n_val)
training_acc   <- accuracy(fit_train)
validation_acc <- accuracy(fc_val, aus_wine_ts)

# Best model per varietal by validation RMSE
best_models <- validation_acc |>
  slice_min(RMSE, by = Varietal) |>
  select(Varietal, .model)

# Model specifications selected on the training data, as printed by the mable
model_specs <- tibble(
  Varietal = fit_train$Varietal,
  TSLM     = format(fit_train$lm),
  ETS      = format(fit_train$ets),
  ARIMA    = format(fit_train$arima)
) |>
  mutate(across(-Varietal, \(x) gsub("[<>]", "", x)))

history_start <- 1990

# ---- Helpers ------------------------------------------------------------------

acc_gt <- function(acc, title, varietals, bold_best = FALSE) {
  tbl <- acc |>
    filter(Varietal %in% varietals) |>
    arrange(Varietal, RMSE) |>
    select(Varietal, .model, RMSE, MAE, MPE, MAPE) |>
    mutate(best = paste(Varietal, .model) %in% paste(best_models$Varietal, best_models$.model))

  out <- tbl |>
    gt(groupname_col = "Varietal") |>
    cols_hide("best") |>
    fmt_number(decimals = 2) |>
    tab_header(title = title) |>
    tab_source_note("Source: Australian Wine data")

  if (bold_best) {
    out <- out |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = best)
      ) |>
      tab_source_note("Bold = lowest validation RMSE for that varietal")
  }
  out
}

# ---- UI -----------------------------------------------------------------------

ui <- page_navbar(
  title = "Australian Wine Sales Forecasts",
  id = "nav",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    id = "sidebar",
    open = "closed",
    checkboxGroupInput(
      "varietal", "Varietal",
      choices = varietal_choices, selected = varietals
    ),
    sliderInput(
      "h", "Forecast horizon (months)",
      min = 6, max = 60, value = 24, step = 6
    ),
    radioButtons(
      "models", "Models shown",
      choices = c("Best per varietal" = "best", "All models" = "all")
    )
  ),
  nav_panel(
    "Description",
    card(card_body(markdown(paste(readLines("description.md"), collapse = "\n"))))
  ),
  nav_panel(
    "Forecast",
    card(
      full_screen = TRUE,
      card_header("Sales history and forecast"),
      plotOutput("forecast_plot"),
      card_footer("Models refit on all data through Dec 1994. Ribbon = 95% prediction interval.")
    )
  ),
  nav_panel(
    "Validation",
    card(
      full_screen = TRUE,
      card_header("Forecasts vs. actual sales, validation window"),
      plotOutput("validation_plot"),
      card_footer("Models trained through Dec 1992. Black line = actual sales; ribbon = 95% prediction interval.")
    )
  ),
  nav_panel(
    "Accuracy",
    layout_columns(
      col_widths = c(12, 6, 6),
      card(full_screen = TRUE, uiOutput("spec_tbl")),
      card(full_screen = TRUE, uiOutput("train_tbl")),
      card(full_screen = TRUE, uiOutput("valid_tbl"))
    )
  ),
  nav_panel(
    "About",
    card(card_body(markdown(paste(readLines("about.md"), collapse = "\n"))))
  )
)

# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  # The inputs only affect the plot tabs, so close the sidebar elsewhere
  observeEvent(input$nav, {
    toggle_sidebar("sidebar", open = input$nav %in% c("Forecast", "Validation"))
  })

  selected <- reactive({
    validate(need(length(input$varietal) > 0, "Select at least one varietal."))
    input$varietal
  })

  history <- reactive({
    filter(aus_wine_ts, Varietal %in% selected(), year(Month) >= history_start)
  })

  # Keep either the best model per varietal or all models
  pick_models <- function(fc) {
    if (input$models == "best") {
      fc <- semi_join(fc, best_models, by = c("Varietal", ".model"))
    }
    fc
  }

  forecast_plot <- reactive({
    fit_full |>
      filter(Varietal %in% selected()) |>
      forecast(h = input$h) |>
      pick_models() |>
      autoplot(history(), level = 95) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      labs(x = NULL, y = "Sales")
  })

  validation_plot <- reactive({
    fc_val |>
      filter(Varietal %in% selected()) |>
      pick_models() |>
      autoplot(history(), level = 95) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      labs(x = NULL, y = "Sales")
  })

  output$forecast_plot   <- renderPlot(forecast_plot())
  output$validation_plot <- renderPlot(validation_plot())

  # Rendered as static HTML: gt 1.3.0's render_gt() registers an input with the
  # output's id, which Shiny flags as a shared input/output id
  output$spec_tbl <- renderUI({
    model_specs |>
      filter(Varietal %in% selected()) |>
      gt() |>
      tab_header(title = "Model Specifications") |>
      tab_source_note(
        "Selected on training data (through 1992). The Forecast tab refits on all data, so orders may differ."
      ) |>
      as_raw_html() |>
      HTML()
  })

  output$train_tbl <- renderUI(
    HTML(as_raw_html(acc_gt(training_acc, "Training Accuracy", selected())))
  )
  output$valid_tbl <- renderUI(
    HTML(as_raw_html(acc_gt(validation_acc, "Validation Accuracy", selected(), bold_best = TRUE)))
  )
}

shinyApp(ui, server)
