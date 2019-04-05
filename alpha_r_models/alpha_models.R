library(data.table)
library(glue)

accrual_cashflow <- function(lookback){
  my_code <- glue("-delta(assets_curr - (liabilities_curr - debt_st), {lookback})")
  return (my_code)
}

mean_reversion_divided_by_year <- function(lookback_1, lookback_2){
  my_code <- glue("rank(ts_sum((open>close)*(open-close)/(high-low),{lookback_1})/ts_sum((open<close)*(-(open-close)/(high-low)),{lookback_1})/ts_sum((open>close)*(open-close)/(high-low),{lookback_2})/ts_sum((open<close)*(-(open-close)/(high-low)),{lookback_2}))")
}

mean_reversion_volume_multipler <- function(lookback_1, lookback_2, decay){
  my_code <- glue("-rank(ts_delta(close,{lookback_1}))*(1-rank(ts_decay_linear(volume/sum(volume,{lookback_2}),{decay})))")
}

monthly_mean_reversion <- function(lookback_1, lookback_2){
  my_code <- glue("rank(ts_sum(open>close,{lookback_1})/ts_sum(open<close,{lookback_1})/ts_sum(open>close,{lookback_2})/ts_sum(open<close,{lookback_2}))")
}