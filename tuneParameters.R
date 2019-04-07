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

# Example
dt.param_grid_mean_reversion_divided_by_year <- data.table(parameter = rep(c("lookback_1","lookback_2"), each = 11),
                                                           value = rep(seq(from = 30, to = 90, by = 6), 2))

dt.alpha_mean_reversion_divided_by_year <- funcGridSearch(my_alpha_model = mean_reversion_divided_by_year,
                                                          alpha_name = "mean_reversion_divided_by_year",
                                                          param_grid = dt.param_grid)
funcOutputCode(dt.alpha_mean_reversion_divided_by_year)




