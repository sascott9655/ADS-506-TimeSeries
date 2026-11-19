library(shiny)
library(bslib)
library(fpp3)
library(gt)

# Aggregate tourism trips by purpose
data <- tourism |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips), .groups = "drop")

purposes <- sort(unique(data$Purpose))
model_choices <- c("ARIMA" = "arima", "ETS" = "ets")
quarters <- data$Quarter |> unique() |> sort()
cutoff_choices <- as.character(quarters[(length(quarters) - 16):(length(quarters) - 1)])
default_cutoff <- "2017 Q1"

ui <- page_navbar(
  title = "Tourism Forecasting",
  theme = bs_theme(version = 5),
  fillable = TRUE,

  nav_panel(
    "Data",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "purposes", "Purpose",
          choices = purposes, selected = purposes
        ),
        selectInput(
          "cutoff", "Training cutoff (quarter)",
          choices = cutoff_choices, selected = default_cutoff
        ),
        checkboxInput("show_cutoff", "Show training cutoff line", TRUE),
        checkboxInput("free_y", "Free y-axis per panel", TRUE)
      ),
      card(
        full_screen = TRUE,
        card_header("Tourism trips by purpose"),
        plotOutput("data_plot")
      )
    )
  ),

  nav_panel(
    "Accuracy",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "acc_type", "Accuracy type",
          choices = c("Training" = "train", "Forecast" = "forecast")
        ),
        checkboxGroupInput(
          "acc_models", "Models",
          choices = model_choices, selected = names(model_choices) |> tolower()
        ),
        sliderInput("acc_h", "Forecast horizon (quarters)", 1, 8, 4, step = 1),
        helpText(
          "Training cutoff is set on the Data tab. Forecast accuracy is ",
          "computed only over quarters with observed data."
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Model accuracy"),
        gt_output("acc_table")
      )
    )
  ),

  nav_panel(
    "Forecast",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "fc_purposes", "Purpose",
          choices = purposes, selected = purposes
        ),
        checkboxGroupInput(
          "fc_models", "Models",
          choices = model_choices, selected = names(model_choices) |> tolower()
        ),
        sliderInput("fc_h", "Forecast horizon (quarters)", 1, 8, 4, step = 1),
        sliderInput("history_years", "History shown (years)", 1, 10, 2, step = 1),
        checkboxInput("show_pi", "Show prediction intervals", TRUE)
      ),
      navset_card_underline(
        full_screen = TRUE,
        nav_panel("Plot", plotOutput("fc_plot")),
        nav_panel("Point forecasts", gt_output("fc_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  cutoff <- reactive(yearquarter(input$cutoff))

  data_train <- reactive(filter(data, Quarter < cutoff()))

  fit <- reactive({
    data_train() |>
      model(
        arima = ARIMA(Trips),
        ets = ETS(Trips)
      )
  }) |>
    bindCache(input$cutoff)

  # Forecast tab and accuracy tab each use their own horizon
  make_fc <- function(h) {
    fit() |> forecast(h = h)
  }

  # ---- Tab 1: data ----
  output$data_plot <- renderPlot({
    req(input$purposes)
    p <- data |>
      filter(Purpose %in% input$purposes) |>
      autoplot(Trips) +
      labs(y = "Trips", title = "Tourism data") +
      facet_wrap(
        ~Purpose,
        ncol = 1,
        scales = if (input$free_y) "free_y" else "fixed"
      ) +
      theme_minimal(base_size = 14)

    if (input$show_cutoff) {
      p <- p + geom_vline(
        xintercept = as.Date(cutoff()),
        color = "red", linetype = "dashed"
      )
    }
    p
  })

  # ---- Tab 2: accuracy ----
  output$acc_table <- render_gt({
    req(input$acc_models)

    if (input$acc_type == "train") {
      acc <- fit() |> accuracy()
      title <- "Training Accuracy"
    } else {
      acc <- make_fc(input$acc_h) |> accuracy(data)
      title <- "Forecast Accuracy"
    }

    acc |>
      filter(.model %in% input$acc_models) |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2) |>
      tab_header(title = title) |>
      tab_source_note("Source: Tourism data")
  })

  # ---- Tab 3: forecast ----
  fc_selected <- reactive({
    req(input$fc_purposes, input$fc_models)
    make_fc(input$fc_h) |>
      filter(Purpose %in% input$fc_purposes, .model %in% input$fc_models)
  })

  output$fc_plot <- renderPlot({
    history_start <- yearquarter(as.Date(cutoff()) - years(input$history_years))
    fc_selected() |>
      autoplot(
        data |>
          filter(Purpose %in% input$fc_purposes, Quarter >= history_start),
        level = if (input$show_pi) c(80, 95) else NULL
      ) +
      labs(y = "Trips", title = "Tourism data forecasts") +
      facet_wrap(Purpose ~ .model, ncol = 2, scales = "free_y") +
      theme_minimal(base_size = 14)
  })

  output$fc_table <- render_gt({
    fc_selected() |>
      as_tibble() |>
      select(Purpose, .model, Quarter, .mean) |>
      pivot_wider(names_from = Quarter, values_from = .mean) |>
      gt() |>
      fmt_number(decimals = 0) |>
      tab_header(title = "Point forecasts (mean)")
  })
}

shinyApp(ui, server)
