## Australian Wine Sales Forecasting

This app explores and forecasts monthly Australian wine sales (1980 to 1994) for Dry white, Red and Sparkling wine. For each varietal it fits three forecasting models, scores them on held-out data, and projects the best performer forward, so you can see which model is the more reliable choice for planning.

**How to use it.** On the Forecast and Validation tabs, use the sidebar to pick the varietals, the forecast horizon and whether to show the best model or all models. The other tabs don't use the sidebar.

### App features

- **Varietal selector.** View one varietal or compare several, with one panel per varietal.
- **Adjustable horizon.** Forecast 6 to 60 months ahead, with 95% prediction intervals shown as ribbons.
- **Three models per varietal.** Seasonal linear regression (TSLM), exponential smoothing (ETS) and ARIMA.
- **Best-model view.** Show only the model with the lowest validation RMSE for each varietal, or switch to all models.
- **Validation check.** See forecasts from models trained through 1992 plotted against the actual 1993 to 1994 sales.
- **Accuracy tables.** Training and validation RMSE, MAE, MPE and MAPE, sorted by varietal and then RMSE, with the best validation model in bold.
- **About tab.** The data source, modeling choices and how to reproduce each figure.
