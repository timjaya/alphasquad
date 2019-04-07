library(data.table)
library(glue)

# Define an alpha by taking a data table dt.variables
# Subset within the function

accrual_cashflow <- function(dt.variables){
  # default val lookback = 60
  lookback <- dt.variables$lookback
  my_code <- glue("-delta(assets_curr - (liabilities_curr - debt_st), {lookback})")
  return (my_code)
}

mean_reversion_divided_by_year <- function(dt.variables){
  # default val lookback_1=20, lookback_2=252
  lookback_1 <- dt.variables$lookback_1
  lookback_2 <- dt.variables$lookback_2
  my_code <- glue("rank(ts_sum((open>close)*(open-close)/(high-low),{lookback_1})/ts_sum((open<close)*(-(open-close)/(high-low)),{lookback_1})/ts_sum((open>close)*(open-close)/(high-low),{lookback_2})/ts_sum((open<close)*(-(open-close)/(high-low)),{lookback_2}))")
  return (my_code)
}

mean_reversion_volume_multipler <- function(dt.variables){
  # default val lookback_1=2, lookback_2=30, decay=10
  lookback_1 <- dt.variables$lookback_1
  lookback_2 <- dt.variables$lookback_2
  decay <- dt.variables$decay
  
  my_code <- glue("-rank(ts_delta(close,{lookback_1}))*(1-rank(ts_decay_linear(volume/sum(volume,{lookback_2}),{decay})))")
  return (my_code)
}

monthly_mean_reversion <- function(dt.variables){
  # default val lookback_1=20, lookback_2=252
  lookback_1 <- dt.variables$lookback_1
  lookback_2 <- dt.variables$lookback_2
  
  my_code <- glue("rank(ts_sum(open>close,{lookback_1})/ts_sum(open<close,{lookback_1})/ts_sum(open>close,{lookback_2})/ts_sum(open<close,{lookback_2}))")
  return (my_code)
}

morning_gap_trend_following <- function(dt.variables){
  # default val lookback_1=20, lookback_2=252, decay = 5, z_score_threshold=3.5, pct_change_threshold=0.1
  lookback_1 <- dt.variables$lookback_1
  lookback_2 <- dt.variables$lookback_2
  decay <- dt.variables$decay
  z_score_threshold <- dt.variables$z_score_threshold
  pct_change_threshold <- dt.variables$pct_change_threshold
  
  my_code <- glue("ts_decay_linear(trade_when(abs(ts_zscore(ts_delay(close,1)-open,{lookback_1}))>{z_score_threshold},ts_delta(close,5)/ts_stddev(close,{lookback_2}),abs(trade_when(abs(ts_zscore(ts_delay(close,1)-open,{lookback_1}))>{z_score_threshold},close,-1)-close)/trade_when(abs(ts_zscore(ts_delay(close,1)-open,{lookback_1}))>{z_score_threshold},close,-1)>{pct_change_threshold}),{decay})")
  return (my_code)
}

price_signal_stop_loss <- function(dt.variables){
  # default val num_down_days = 4, delta_ranking = 2
  num_down_days <- dt.variables$num_down_days
  delta_ranking <- dt.variables$delta_ranking
  
  my_code <- glue("ts_sum(sign(ts_delta(close,1)),{num_down_days})==-{num_down_days}?0:rank(-ts_delta(close,{delta_ranking}))")
  return (my_code)
}

rev_growth <- function(dt.variables){
  # default val lookback=250
  lookback <- dt.variables$lookback
  my_code <- glue("-(assets / delay(assets, {lookback}) - 1)")
  return (my_code)
}

rsi <- function(dt.variables){
  # default val lookback=10
  lookback <- dt.variables$lookback
  my_code <- glue("-(100 - 100 / (1 + sum((delta(close,1) > 0 ? delta(close,1) : 0), {lookback}) / sum((delta(close,1) < 0 ? - delta(close,1) : 0), {lookback})))")
  return (my_code)
}

sma_20 <- function(dt.variables){
  # default val weighting=1.6, lookback=20
  weighting <- dt.variables$weighting
  lookback <- dt.variables$lookback
  my_code <- glue("close>ts_sum(close,{lookback})/{lookback}?{weighting}*rank(-ts_delta(close,2)):rank(-ts_delta(close,2))")
  return (my_code)
}








