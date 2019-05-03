library(data.table)
library(glue)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dt.oos <- fread(paste0(parent_dir, "/", "oos_strats/oos_strats.csv"))
dt.oos[,code := trimws(code)]
dt.oos <- dt.oos[type == "simple"]

dt.write.this <- data.table()

for (i in 1:(nrow(dt.oos)-1)){
  for (j in (i+1):nrow(dt.oos)){
    chr.temp <- paste0(dt.oos[i]$code, "+", dt.oos[j]$code)
    dt.write.this <- rbind(dt.write.this,
                           data.table(code = chr.temp))
    
  }
}

dt.write.this[,id := 1:nrow(dt.write.this)]
dt.write.this[,file_name := paste0("alpha_", id)]

for (i in 1:nrow(dt.write.this)){
  dt.temp <- dt.write.this[id == i]
  writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
}