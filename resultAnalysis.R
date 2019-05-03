library(data.table)
library(ggplot2)
library(plotly)

mode <- "auto_refresh"
sleep_timer <- 120

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
folder <- ""
dir_path <- paste0(parent_dir, "/results_combinations/", folder)


if (mode == "auto_refresh"){
  while(TRUE){
    result_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
    
    
    dt.results <- rbindlist(lapply(result_list, function(x) {
      print(x)
      fread(paste0(dir_path, "/", x))}))
    dt.results[,V1 := NULL]
    dt.results[,score_delta := as.numeric(score_delta)]
    dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
    # dt.results <- dt.results[status != "ERROR" & score_delta > -1000]
    dt.results[,strategy := paste(head(unlist(strsplit(alpha_id, "\\_")), -1), collapse = "_"), by = 1:nrow(dt.results)]
    dt.results[,turn_over := as.numeric(sub("%", "", turn_over))]
    dt.results[sharpe_ratio < 1.25, status := "FAIL"]
    dt.results[turn_over > 70, status := "FAIL"]
    dt.results <- dt.results[turn_over < 70]
    
    my_plot <- ggplot(dt.results, aes(x = sharpe_ratio, score_delta, text = paste0("alpha_id: ", alpha_id,
                                                                                   "\nturn_over: ", turn_over,
                                                                                   "\nfitness: ", fitness,
                                                                                   "\nreturns: ", returns),
                                      color = status, alpha = score_delta)) + geom_point(size = 1.5) + 
      geom_hline(yintercept = 0, size=1) + geom_vline(xintercept = 0, size=1) + geom_vline(xintercept = 1.25, size = 1, color = "red")
    
    # Get the best sharpe ratio for each strategy
    dt.results[dt.results[, .I[sharpe_ratio == max(sharpe_ratio)], by=strategy]$V1]
    dt.results[order(score_delta, decreasing = TRUE)]
    
    dt.results_good <- dt.results[sharpe_ratio > 1.25 & status == "PASS" & score_delta > 0]
    dt.results_good[order(score_delta, decreasing = TRUE)]
    
    print(my_plot)
    
    Sys.sleep(sleep_timer)
    
    dev.off(dev.list()["RStudioGD"])
  }
} else{
  result_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  
  
  dt.results <- rbindlist(lapply(result_list, function(x) {
    print(x)
    fread(paste0(dir_path, "/", x))}))
  dt.results[,V1 := NULL]
  dt.results[,score_delta := as.numeric(score_delta)]
  dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
  # dt.results <- dt.results[status != "ERROR" & score_delta > -1000]
  dt.results[,strategy := paste(head(unlist(strsplit(alpha_id, "\\_")), -1), collapse = "_"), by = 1:nrow(dt.results)]
  dt.results[,turn_over := as.numeric(sub("%", "", turn_over))]
  dt.results[sharpe_ratio < 1.25, status := "FAIL"]
  dt.results[turn_over > 70, status := "FAIL"]
  dt.results <- dt.results[turn_over < 70]
  
  my_plot <- ggplot(dt.results, aes(x = sharpe_ratio, score_delta, text = paste0("alpha_id: ", alpha_id,
                                                                                 "\nturn_over: ", turn_over,
                                                                                 "\nfitness: ", fitness,
                                                                                 "\nreturns: ", returns),
                                    color = status, alpha = score_delta)) + geom_point(size = 1.5) + 
    geom_hline(yintercept = 0, size=1) + geom_vline(xintercept = 0, size=1) + geom_vline(xintercept = 1.25, size = 1, color = "red")
  
  # Get the best sharpe ratio for each strategy
  dt.results[dt.results[, .I[sharpe_ratio == max(sharpe_ratio)], by=strategy]$V1]
  dt.results[order(score_delta, decreasing = TRUE)]
  
  dt.results_good <- dt.results[sharpe_ratio > 1.25 & status == "PASS" & score_delta > 0]
  print(dt.results_good[order(score_delta, decreasing = TRUE)])
  
  ggplotly(my_plot)
}


# # Move file back to parent folder
# for (i in 1:nrow(dt.results_good)){
#   dt.temp <- dt.results_good[i]
#   chr.path <- paste0(parent_dir, "/test_combinations/form4_combined/", dt.temp$alpha_id)
#   file.copy(chr.path, paste0(parent_dir, "/test_combinations/", dt.temp$alpha_id))
# }
