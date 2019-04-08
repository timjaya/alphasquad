library(data.table)
library(ggplot2)
library(plotly)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dir_path <- paste0(parent_dir, "/results_combinations")
result_list <- list.files(dir_path)

dt.results <- rbindlist(lapply(result_list, function(x) fread(paste0(dir_path, "/", x))))
dt.results[,V1 := NULL]
dt.results[,score_delta := as.numeric(score_delta)]
dt.results[,sharpe_ratio := as.numeric(sharpe_ratio)]
dt.results <- dt.results[status != "ERROR"]
dt.results[,strategy := paste(head(unlist(strsplit(alpha_id, "\\_")), -1), collapse = "_"), by = 1:nrow(dt.results)]
my_plot <- ggplot(dt.results, aes(x = sharpe_ratio, score_delta, text = paste0("alpha_id: ", alpha_id,
                                                                    "\nturn_over: ", turn_over,
                                                                    "\nfitness: ", fitness,
                                                                    "\nreturns: ", returns),
                       color = status)) + geom_point(size = 1.5) + 
  geom_hline(yintercept = 0, size=1) + geom_vline(xintercept = 0, size=1)

ggplotly(my_plot)

# Get the best sharpe ratio for each strategy
dt.results[dt.results[, .I[sharpe_ratio == max(sharpe_ratio)], by=strategy]$V1]


