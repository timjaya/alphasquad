library(data.table)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

alpha_list <- list.files(paste0(parent_dir, "/alpha_list"))

volatility_indicator_file_name <- paste0(parent_dir, "/indicator_list/volatility_signal.txt")
volume_indicator_file_name <- paste0(parent_dir, "/indicator_list/volume_signal.txt")
sentiment_indicator_file_name <- paste0(parent_dir, "/indicator_list/sentiment_signal.txt")
ema_indicator_file_name <- paste0(parent_dir, "/indicator_list/ema_signal.txt")

volatility_signal_code <- readChar(volatility_indicator_file_name, 
                                   file.info(volatility_indicator_file_name)$size)

volume_signal_code <- readChar(volume_indicator_file_name, 
                                   file.info(volume_indicator_file_name)$size)

sentiment_signal_code <- readChar(sentiment_indicator_file_name,
                                  file.info(sentiment_indicator_file_name)$size)

ema_signal_code <- readChar(ema_indicator_file_name,
                            file.info(ema_indicator_file_name)$size)

dt.alpha_list <- data.table()

# Get volatility combinations
if (length(list.files(paste0(parent_dir, "/test_combinations"))) == 0){
  id <- 1
} else{
  id <- length(list.files(paste0(parent_dir, "/test_combinations")))
}

for (i in 1:(length(alpha_list)-1)){
  for (j in 2:length(alpha_list)){
    alpha_1 <- paste0(parent_dir, "/alpha_list/", alpha_list[i])
    alpha_2 <- paste0(parent_dir, "/alpha_list/", alpha_list[j])
    
    alpha_1_code <- trimws(readChar(alpha_1, 
                             file.info(alpha_1)$size), "both")
    alpha_1_code_final <- paste0("a=", alpha_1_code, ";")
    
    alpha_2_code <- trimws(readChar(alpha_2, 
                             file.info(alpha_2)$size), "both")
    alpha_2_code_final <- paste0("b=", alpha_2_code, ";")
    
    my_alpha_final <- paste0(alpha_1_code_final, alpha_2_code_final, volatility_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id,
                                      code=my_alpha_final))
    
    alpha_3_code <- trimws(readChar(alpha_1, 
                                    file.info(alpha_1)$size), "both")
    alpha_3_code_final <- paste0("b=", alpha_3_code, ";")
    
    alpha_4_code <- trimws(readChar(alpha_2, 
                                    file.info(alpha_2)$size), "both")
    alpha_4_code_final <- paste0("a=", alpha_2_code, ";")
    
    my_alpha_final_2 <- paste0(alpha_3_code_final, alpha_4_code_final, volatility_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id+1,
                                      code=my_alpha_final_2))
    
    id <- id + 2
  }
}

# Get volume combinations
for (j in 1:length(alpha_list)){
  alpha_1 <- paste0(parent_dir, "/alpha_list/", alpha_list[i])
  alpha_1_code <- trimws(readChar(alpha_1, 
                                  file.info(alpha_1)$size), "both")
  alpha_1_code_final <- paste0("a=", alpha_1_code, ";")
  my_alpha_final <- paste0(alpha_1_code_final, volume_signal_code)
  dt.alpha_list <- rbind(dt.alpha_list,
                         data.table(id=id,
                                    code=my_alpha_final))
  id <- id + 1
}

# Get sentiment signal
for (i in 1:(length(alpha_list)-1)){
  for (j in 2:length(alpha_list)){
    alpha_1 <- paste0(parent_dir, "/alpha_list/", alpha_list[i])
    alpha_2 <- paste0(parent_dir, "/alpha_list/", alpha_list[j])
    
    alpha_1_code <- trimws(readChar(alpha_1, 
                                    file.info(alpha_1)$size), "both")
    alpha_1_code_final <- paste0("a=", alpha_1_code, ";")
    
    alpha_2_code <- trimws(readChar(alpha_2, 
                                    file.info(alpha_2)$size), "both")
    alpha_2_code_final <- paste0("b=", alpha_2_code, ";")
    
    my_alpha_final <- paste0(alpha_1_code_final, alpha_2_code_final, sentiment_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id,
                                      code=my_alpha_final))
    
    alpha_3_code <- trimws(readChar(alpha_1, 
                                    file.info(alpha_1)$size), "both")
    alpha_3_code_final <- paste0("b=", alpha_3_code, ";")
    
    alpha_4_code <- trimws(readChar(alpha_2, 
                                    file.info(alpha_2)$size), "both")
    alpha_4_code_final <- paste0("a=", alpha_2_code, ";")
    
    my_alpha_final_2 <- paste0(alpha_3_code_final, alpha_4_code_final, sentiment_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id+1,
                                      code=my_alpha_final_2))
    
    id <- id + 2
  }
}

# Get EMA signal
for (i in 1:(length(alpha_list)-1)){
  for (j in 2:length(alpha_list)){
    alpha_1 <- paste0(parent_dir, "/alpha_list/", alpha_list[i])
    alpha_2 <- paste0(parent_dir, "/alpha_list/", alpha_list[j])
    
    alpha_1_code <- trimws(readChar(alpha_1, 
                                    file.info(alpha_1)$size), "both")
    alpha_1_code_final <- paste0("a=", alpha_1_code, ";")
    
    alpha_2_code <- trimws(readChar(alpha_2, 
                                    file.info(alpha_2)$size), "both")
    alpha_2_code_final <- paste0("b=", alpha_2_code, ";")
    
    my_alpha_final <- paste0(alpha_1_code_final, alpha_2_code_final, ema_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id,
                                      code=my_alpha_final))
    
    alpha_3_code <- trimws(readChar(alpha_1, 
                                    file.info(alpha_1)$size), "both")
    alpha_3_code_final <- paste0("b=", alpha_3_code, ";")
    
    alpha_4_code <- trimws(readChar(alpha_2, 
                                    file.info(alpha_2)$size), "both")
    alpha_4_code_final <- paste0("a=", alpha_2_code, ";")
    
    my_alpha_final_2 <- paste0(alpha_3_code_final, alpha_4_code_final, ema_signal_code)
    
    dt.alpha_list <- rbind(dt.alpha_list,
                           data.table(id=id+1,
                                      code=my_alpha_final_2))
    
    id <- id + 2
  }
}

# Output strategy to folder
for (i in 1:nrow(dt.alpha_list)){
  dt.temp <- dt.alpha_list[id == i]
  writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
}

