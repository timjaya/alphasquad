library(data.table)
library(glue)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

funcInitialRatioGeneration <- function(){
  dt.fundamental <- fread(paste0(parent_dir, "/alpha_list/fundamental_factor.csv"))
  
  dt.write.this <- rbindlist(lapply(unique(dt.fundamental$type), function(x){
    dt.return.this <- data.table()
    dt.temp <- dt.fundamental[type == x]
    dt.temp[, code := paste0("rank(", metric, ")")]
    # dt.return.this <- rbind(dt.return.this, dt.temp[,list(code)])
    for (i in 1:(nrow(dt.temp) - 1)){
      for (j in (i+1):nrow(dt.temp)){
        dt.append.this <- data.table(code = paste0("rank(", dt.temp[i]$metric, "/", dt.temp[j]$metric, ")"))
        dt.return.this <- rbind(dt.return.this, dt.append.this)
        dt.append.this <- data.table(code = paste0("rank(", dt.temp[j]$metric, "/", dt.temp[i]$metric, ")"))
        dt.return.this <- rbind(dt.return.this, dt.append.this)
      }
    }
    return (dt.return.this)
  }))
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioSumGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/results_combinations/fundamental_ratio")
  result_list <- list.files(dir_path)
  
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  dt.candidates <- dt.results[sharpe_ratio > 1 & score_delta > 0]
  dt.write.this <- data.table()
  for (i in 1:(nrow(dt.candidates) - 1)){
    for (j in (i+1):nrow(dt.candidates)){
      my_file_a <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[i]$alpha_id)
      my_code_a <- trimws(readChar(my_file_a, 
                                   file.info(my_file_a)$size), "both")
      my_file_b <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[j]$alpha_id)
      my_code_b <- trimws(readChar(my_file_b, 
                                   file.info(my_file_b)$size), "both")
      
      dt.write.this <- rbind(dt.write.this,
                              data.table(code = paste0(my_code_a, "+", my_code_b)))
    }
  }
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/combination_fundamental_alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioMultGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/results_combinations/fundamental_ratio")
  result_list <- list.files(dir_path)
  
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  dt.candidates <- dt.results[sharpe_ratio > 1 & score_delta > 0]
  dt.write.this <- data.table()
  for (i in 1:(nrow(dt.candidates) - 1)){
    for (j in (i+1):nrow(dt.candidates)){
      my_file_a <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[i]$alpha_id)
      my_code_a <- trimws(readChar(my_file_a, 
                                   file.info(my_file_a)$size), "both")
      my_file_b <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[j]$alpha_id)
      my_code_b <- trimws(readChar(my_file_b, 
                                   file.info(my_file_b)$size), "both")
      
      dt.write.this <- rbind(dt.write.this,
                             data.table(code = paste0(my_code_a, "*", my_code_b)))
    }
  }
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/combination_fundamental_alpha_mult", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioThreeVarSumGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/results_combinations/fundamental_ratio")
  result_list <- list.files(dir_path)
  
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  dt.candidates <- dt.results[sharpe_ratio > 1.25 & score_delta > 0]
  dt.write.this <- data.table()
  
  for (i in 1:(nrow(dt.candidates) - 2)){
    for (j in (i+1):(nrow(dt.candidates)-1)){
      for (k in (j+1):(nrow(dt.candidates))){
        my_file_a <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[i]$alpha_id)
        my_code_a <- trimws(readChar(my_file_a, 
                                     file.info(my_file_a)$size), "both")
        my_file_b <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[j]$alpha_id)
        my_code_b <- trimws(readChar(my_file_b, 
                                     file.info(my_file_b)$size), "both")
        my_file_c <- paste0(parent_dir, "/test_combinations/fundamental_ratio_combinations/", dt.candidates[k]$alpha_id)
        my_code_c <- trimws(readChar(my_file_c, 
                                     file.info(my_file_c)$size), "both")
        
        dt.write.this <- rbind(dt.write.this,
                               data.table(code = paste0(my_code_a, "+", my_code_b, "+", my_code_c)))
      }
    }
  }
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    # writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/combination_fundamental_alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
    writeChar(dt.temp$code, paste0("D:/Desktop/kyle_testing", "/alpha", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioFourVarSumGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/results_combinations/fundamental_ratio_sum")
  result_list <- list.files(dir_path)
  
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  dt.candidates <- dt.results[sharpe_ratio > 1 & sharpe_ratio < 1.25 & score_delta > 100 & fitness > 1]
  dt.write.this <- data.table()
  for (i in 1:(nrow(dt.candidates) - 1)){
    for (j in (i+1):nrow(dt.candidates)){
      my_file_a <- paste0(parent_dir, "/test_combinations/fundamental_ratio_sum_combinations/", dt.candidates[i]$alpha_id)
      my_code_a <- trimws(readChar(my_file_a, 
                                   file.info(my_file_a)$size), "both")
      my_file_b <- paste0(parent_dir, "/test_combinations/fundamental_ratio_sum_combinations/", dt.candidates[j]$alpha_id)
      my_code_b <- trimws(readChar(my_file_b, 
                                   file.info(my_file_b)$size), "both")
      
      dt.write.this <- rbind(dt.write.this,
                             data.table(code = paste0(my_code_a, "+", my_code_b)))
    }
  }
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0("D:/Desktop/kyle_testing", "/alpha", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioMultSumGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path_alt <- paste0(parent_dir, "/results_combinations/fundamental_ratio_mult")
  result_list <- list.files(dir_path_alt)
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path_alt, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/fundamental_ratio_mult")
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  alpha_list <- alpha_list[alpha_list %in% dt.results[sharpe_ratio > 0 & score_delta > 0]$alpha_id]
  
  dt.write.this <- data.table()
  
  for (i in 1:(length(alpha_list) - 1)){
    for (j in (i+1):(length(alpha_list))){
      my_code_i <- trimws(readChar(paste0(dir_path, "/", alpha_list[i]), file.info(paste0(dir_path, "/", alpha_list[i]))$size))
      my_code_j <- trimws(readChar(paste0(dir_path, "/", alpha_list[j]), file.info(paste0(dir_path, "/", alpha_list[j]))$size))
      dt.append.this <- data.table(code = paste0(my_code_i, "+", my_code_j))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
    }
  }
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/kyle_testing_3/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}

############################################ Includes estimate data ############################################ 

funcEstimatedRatio(){
  dt.fundamental <- fread(paste0(parent_dir, "/alpha_list/estimate_fundamental_factor.csv"))
  
  dt.write.this <- rbindlist(lapply(unique(dt.fundamental$type), function(x){
    dt.return.this <- data.table()
    dt.temp <- dt.fundamental[type == x]
    dt.temp[, code := paste0("rank(", metric, ")")]
    dt.return.this <- rbind(dt.return.this, dt.temp[,list(code)])
    for (i in 1:(nrow(dt.temp) - 1)){
      for (j in (i+1):nrow(dt.temp)){
        dt.append.this <- data.table(code = paste0("rank(", dt.temp[i]$metric, "/", dt.temp[j]$metric, ")"))
        dt.return.this <- rbind(dt.return.this, dt.append.this)
        dt.append.this <- data.table(code = paste0("rank(", dt.temp[j]$metric, "/", dt.temp[i]$metric, ")"))
        dt.return.this <- rbind(dt.return.this, dt.append.this)
      }
    }
    return (dt.return.this)
  }))
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcActualEstimateRatio(){
  dt.fundamental <- fread(paste0(parent_dir, "/alpha_list/combined_fundamental_factor.csv"))
  
  
  dt.write.this <- rbindlist(lapply(1:nrow(dt.fundamental), function(x){
    dt.return.this <- data.table()
    dt.temp <- dt.fundamental[x]
    dt.temp[, code := paste0("rank(", actual_value,  "/", estimated_value, ")")]
    dt.return.this <- rbind(dt.return.this, dt.temp[,list(code)])
    dt.temp[, code := paste0("rank(", estimated_value,  "/", actual_value, ")")]
    dt.return.this <- rbind(dt.return.this, dt.temp[,list(code)])
  }))
    
  for (i in 1:(nrow(dt.fundamental) - 1)){
    for (j in (i+1):nrow(dt.fundamental)){
      dt.append.this <- data.table(code = paste0("rank(", dt.fundamental[i]$actual_value, "/", dt.fundamental[j]$estimated_value, ")"))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
      dt.append.this <- data.table(code = paste0("rank(", dt.fundamental[j]$actual_value, "/", dt.fundamental[i]$estimated_value, ")"))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
      }
  }

  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcActualEstimateRatioSum <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path_alt <- paste0(parent_dir, "/results_combinations/fundamental_estimate_actual_ratio_sum")
  result_list <- list.files(dir_path_alt)
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path_alt, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio_sum")
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  alpha_list <- alpha_list[alpha_list %in% dt.results[sharpe_ratio > 0& score_delta > 0]$alpha_id]
  
  dt.write.this <- data.table()
  
  for (i in 1:(length(alpha_list) - 1)){
    for (j in (i+1):(length(alpha_list))){
      my_code_i <- trimws(readChar(paste0(dir_path, "/", alpha_list[i]), file.info(paste0(dir_path, "/", alpha_list[i]))$size))
      my_code_j <- trimws(readChar(paste0(dir_path, "/", alpha_list[j]), file.info(paste0(dir_path, "/", alpha_list[j]))$size))
      dt.append.this <- data.table(code = paste0(my_code_i, "+", my_code_j))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
    }
  }
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcRatioThreeVarSumGeneration <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/results_combinations/fundamental_estimate_actual_ratio")
  result_list <- list.files(dir_path)
  
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  dt.candidates <- dt.results[sharpe_ratio > 0.7]
  dt.write.this <- data.table()
  
  for (i in 1:(nrow(dt.candidates) - 2)){
    for (j in (i+1):(nrow(dt.candidates)-1)){
      for (k in (j+1):(nrow(dt.candidates))){
        my_file_a <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio/", dt.candidates[i]$alpha_id)
        my_code_a <- trimws(readChar(my_file_a, 
                                     file.info(my_file_a)$size), "both")
        my_file_b <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio/", dt.candidates[j]$alpha_id)
        my_code_b <- trimws(readChar(my_file_b, 
                                     file.info(my_file_b)$size), "both")
        my_file_c <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio/", dt.candidates[k]$alpha_id)
        my_code_c <- trimws(readChar(my_file_c, 
                                     file.info(my_file_c)$size), "both")
        
        dt.write.this <- rbind(dt.write.this,
                               data.table(code = paste0(my_code_a, "+", my_code_b, "+", my_code_c)))
      }
    }
  }
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcActualEstimateRatioSum <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path_alt <- paste0(parent_dir, "/results_combinations/fundamental_estimate_actual_ratio")
  result_list <- list.files(dir_path_alt)
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path_alt, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio")
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  alpha_list <- alpha_list[alpha_list %in% dt.results[sharpe_ratio > 0& score_delta > 0]$alpha_id]
  
  dt.write.this <- data.table()
  
  for (i in 1:(length(alpha_list) - 1)){
    for (j in (i+1):(length(alpha_list))){
      my_code_i <- trimws(readChar(paste0(dir_path, "/", alpha_list[i]), file.info(paste0(dir_path, "/", alpha_list[i]))$size))
      my_code_j <- trimws(readChar(paste0(dir_path, "/", alpha_list[j]), file.info(paste0(dir_path, "/", alpha_list[j]))$size))
      dt.append.this <- data.table(code = paste0(my_code_i, "*", my_code_j))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
    }
  }
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcActualEstimateRatioMultSum <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path_alt <- paste0(parent_dir, "/results_combinations/fundamental_estimate_actual_ratio_mult")
  result_list <- list.files(dir_path_alt)
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path_alt, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio_mult")
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  alpha_list <- alpha_list[alpha_list %in% dt.results[sharpe_ratio > 0.5 & score_delta > 200]$alpha_id]
  
  dt.write.this <- data.table()
  
  for (i in 1:(length(alpha_list) - 1)){
    for (j in (i+1):(length(alpha_list))){
      my_code_i <- trimws(readChar(paste0(dir_path, "/", alpha_list[i]), file.info(paste0(dir_path, "/", alpha_list[i]))$size))
      my_code_j <- trimws(readChar(paste0(dir_path, "/", alpha_list[j]), file.info(paste0(dir_path, "/", alpha_list[j]))$size))
      dt.append.this <- data.table(code = paste0(my_code_i, "+", my_code_j))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
    }
  }
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}

funcActualEstimateRatioMultDoubleSum <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path_alt <- paste0(parent_dir, "/results_combinations/fundamental_estimate_actual_ratio_mult_sum")
  result_list <- list.files(dir_path_alt)
  dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path_alt, "/", x))))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  dt.results <- dt.results[status != "ERROR"]
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/fundamental_estimate_actual_ratio_mult_sum")
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  alpha_list <- alpha_list[alpha_list %in% dt.results[sharpe_ratio > 1 & score_delta > 700]$alpha_id]
  
  dt.write.this <- data.table()
  
  for (i in 1:(length(alpha_list) - 1)){
    for (j in (i+1):(length(alpha_list))){
      my_code_i <- trimws(readChar(paste0(dir_path, "/", alpha_list[i]), file.info(paste0(dir_path, "/", alpha_list[i]))$size))
      my_code_j <- trimws(readChar(paste0(dir_path, "/", alpha_list[j]), file.info(paste0(dir_path, "/", alpha_list[j]))$size))
      dt.append.this <- data.table(code = paste0(my_code_i, "+", my_code_j))
      dt.write.this <- rbind(dt.write.this, dt.append.this)
    }
  }
  
  dt.write.this[,id := 1:nrow(dt.write.this)]
  dt.write.this[,file_name := paste0("alpha_", id)]
  
  for (i in 1:nrow(dt.write.this)){
    dt.temp <- dt.write.this[id == i]
    writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/", dt.temp$file, ".txt"), nchars = nchar(dt.temp$code))
  }
}
