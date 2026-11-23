## About this app

**Data.** Monthly Australian wine sales by varietal (wine type), January 1980 to December 1994 (`AustralianWines.csv`). The app covers Red, Sparkling and Dry white.

**Split.** The data is split into a training set (January 1980 to December 1992) and a validation set (January 1993 to December 1994). Models learn from the training set and are scored on how well they forecast the validation months.

**Models.** For each varietal, three model types are fit with the `fable` package:

- **TSLM**: linear regression with a trend and monthly seasonality.
- **ETS**: exponential smoothing, with the error, trend and seasonal form chosen automatically.
- **ARIMA**: autoregressive integrated moving average, with orders chosen automatically.

**Evaluation.** The Accuracy tab reports RMSE, MAE, MPE and MAPE on the training and validation sets, sorted by varietal and then RMSE. The best model for each varietal is the one with the lowest validation RMSE, shown in bold.

**Forecasts.** The Validation tab shows forecasts from the training-only models against actual sales. The Forecast tab refits each model on all data through December 1994 and projects forward by the chosen horizon, with a 95% prediction interval.

**Reproducing the figures.** Use the sidebar to pick the varietal(s), the forecast horizon (months) and whether to show the best model per varietal or all models. Every plot and table updates to match.

**Caveats.** The validation window is only 24 months per series, so model rankings can change with a different split.
