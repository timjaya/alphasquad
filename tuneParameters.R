library(data.table)
library(glue)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

source(paste0(parent_dir, "/alpha_models.R"))

funcGridSearch <- function(alpha_name, my_alpha_model, param_grid){
  # my_alpha_model is an alpha model ins alpha_models.R 
  # param_grid is a data table of parameters we want to expand grid: parameter, value
  
  parameter_list <- unique(param_grid$parameter)
  value_list <- lapply(parameter_list, function(x) unique(param_grid[parameter == x]$value))
  
  dt.expanded_grid <- data.table(expand.grid(data.frame(assignVec(parameter_list, value_list))))
  dt.expanded_grid$id <- 1:nrow(dt.expanded_grid)
  
  dt.alpha_list <- data.table()
  
  for (id_val in dt.expanded_grid$id){
    dt.temp <- dt.expanded_grid[id == id_val]
    dt.temp[,id := NULL]
    my_code <- my_alpha_model(dt.temp)
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id = id_val,
                                      name = alpha_name,
                                      code = my_code))
    
  }
  return (dt.alpha_list)
}

funcOutputCode <- function(dt.alpha){
  # Output strategy to folder
  for (i in 1:nrow(dt.alpha)){
    dt.temp <- dt.alpha[id == i]
    alpha_name <- dt.temp$name
    writeChar(dt.temp$code, glue("{parent_dir}/test_combinations/{alpha_name}_", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

############################################### mean_reversion_divided_by_year ###############################################
dt.param_grid_mean_reversion_divided_by_year <- data.table(parameter = rep(c("lookback_1","lookback_2"), each = 11),
                                                           value = c(seq(from = 10, to = 30, by = 2),
                                                                     seq(from = 52, to = 252, by = 20)))

dt.alpha_mean_reversion_divided_by_year <- funcGridSearch(my_alpha_model = mean_reversion_divided_by_year,
                                                          alpha_name = "mean_reversion_divided_by_year",
                                                          param_grid = dt.param_grid_mean_reversion_divided_by_year)
funcOutputCode(dt.alpha_mean_reversion_divided_by_year)


############################################### accrual_cashflow ###############################################
dt.param_grid_accrual_cashflow <- data.table(parameter = rep(c("lookback"), each = 11),
                                             value = seq(from = 30, to = 90, by = 6))
dt.alpha_param_grid_accrual <- funcGridSearch(my_alpha_model = accrual_cashflow,
                                              alpha_name = "accrual_cashflow",
                                              param_grid = dt.param_grid_accrual_cashflow)
funcOutputCode(dt.alpha_param_grid_accrual)

############################################### mean_reversion_volume_multipler ###############################################
dt.param_grid_mean_reversion_volume_multipler <- data.table(parameter = rep(c("lookback_1","lookback_2", "decay"), each = 5),
                                                            value = c(seq(from = 2, to = 6, by = 1),
                                                                      seq(from = 20, to = 40, by = 5),
                                                                      seq(from = 8, to = 20, by = 3)))
dt.alpha_mean_reversion_volume_multipler <- funcGridSearch(my_alpha_model = mean_reversion_volume_multipler,
                                                           alpha_name = "mean_reversion_volume_multipler",
                                                           param_grid = dt.param_grid_mean_reversion_volume_multipler)
funcOutputCode(dt.alpha_mean_reversion_volume_multipler)

############################################### monthly_mean_reversion ###############################################
dt.param_grid_monthly_mean_reversion <- data.table(parameter = rep(c("lookback_1","lookback_2"), each = 11),
                                                   value = c(seq(from = 10, to = 30, by = 2),
                                                             seq(from = 52, to = 252, by = 20)))

dt.alpha_monthly_mean_reversion <- funcGridSearch(my_alpha_model = monthly_mean_reversion,
                                                          alpha_name = "monthly_mean_reversion",
                                                          param_grid = dt.param_grid_monthly_mean_reversion)
funcOutputCode(dt.alpha_monthly_mean_reversion)

############################################### morning_gap_trend_following ###############################################
# default val lookback_1=20, lookback_2=252, decay = 5, z_score_threshold=3.5, pct_change_threshold=0.1
dt.param_grid_morning_gap_trend_following <- data.table(parameter = rep(c("lookback_1","lookback_2", "decay", 
                                                                          "z_score_threshold", "pct_change_threshold"), each = 3),
                                                   value = c(seq(from = 10, to = 30, by = 10),
                                                             seq(from = 52, to = 252, by = 100),
                                                             seq(from = 4, to = 6, by = 1),
                                                             seq(from = 2.5, to = 4.5, by = 1),
                                                             seq(from = 0.05, to = 0.15, by = 0.05)))

dt.alpha_morning_gap_trend_following <- funcGridSearch(my_alpha_model = morning_gap_trend_following,
                                                  alpha_name = "morning_gap_trend_following",
                                                  param_grid = dt.param_grid_morning_gap_trend_following)
funcOutputCode(dt.alpha_morning_gap_trend_following)

############################################### price_signal_stop_loss ###############################################
dt.param_grid_price_signal_stop_loss <- data.table(parameter = rep(c("num_down_days","delta_ranking"), each = 5),
                                                   value = c(seq(from = 2, to = 6, by = 1),
                                                             seq(from = 2, to = 6, by = 1)))

dt.alpha_price_signal_stop_loss <- funcGridSearch(my_alpha_model = price_signal_stop_loss,
                                                  alpha_name = "price_signal_stop_loss",
                                                  param_grid = dt.param_grid_price_signal_stop_loss)
funcOutputCode(dt.alpha_price_signal_stop_loss)

############################################### rev_growth ###############################################
dt.param_grid_rev_growth <- data.table(parameter = rep(c("lookback"), each = 11),
                                             value = seq(from = 52, to = 252, by = 20))
dt.alpha_param_rev_growth <- funcGridSearch(my_alpha_model = rev_growth,
                                              alpha_name = "rev_growth",
                                              param_grid = dt.param_grid_rev_growth)
funcOutputCode(dt.alpha_param_rev_growth)

############################################### rsi ###############################################
dt.param_grid_rsi <- data.table(parameter = rep(c("lookback"), each = 11),
                                       value = seq(from = 5, to = 15, by = 1))
dt.alpha_param_rsi <- funcGridSearch(my_alpha_model = rsi,
                                            alpha_name = "rsi",
                                            param_grid = dt.param_grid_rsi)
funcOutputCode(dt.alpha_param_rsi)

############################################### sma_20 ###############################################
dt.param_grid_sma_20 <- data.table(parameter = rep(c("weighting","lookback"), each = 11),
                                                   value = c(seq(from = 1.1, to = 2.1, by = 0.1),
                                                             seq(from = 10, to = 60, by = 5)))

dt.alpha_sma_20 <- funcGridSearch(my_alpha_model = sma_20,
                                  alpha_name = "sma_20",
                                  param_grid = dt.param_grid_sma_20)
funcOutputCode(dt.alpha_sma_20)










