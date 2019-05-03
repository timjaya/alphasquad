library(data.table)
library(glue)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

factor_list <- c("open", "close", "high", "low", "vwap")
num_factors <- length(factor_list)
function_list <- c("normalize", "scale", "rank", "zscore")

dt.write.this <- data.table()

# Form 1: A
for (func in function_list){
  for (factor in factor_list){
    charTemp <- paste0(func, "(", factor, ")")
    dt.write.this <- rbind(dt.write.this, data.table(code = charTemp))
  }
}

# Form 2: A + B
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      factorA <- factor_list[i]
      factorB <- factor_list[j]
      charTemp <- paste0(func, "(", factorA, "+", factorB, ")")
      dt.write.this <- rbind(dt.write.this, data.table(code = charTemp))
    }
  }
}

# Form 3: A - B
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      factorA <- factor_list[i]
      factorB <- factor_list[j]
      charTemp1 <- paste0(func, "(", factorA, "-", factorB, ")")
      charTemp2 <- paste0(func, "(", factorB, "-", factorA, ")")
      dt.write.this <- rbind(dt.write.this, data.table(code = charTemp1))
      dt.write.this <- rbind(dt.write.this, data.table(code = charTemp2))
    }
  }
}

# Form 4: A / B
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      factorA <- factor_list[i]
      factorB <- factor_list[j]
      charTemp1 <- paste0(func, "(", factorA, "/", factorB, ")")
      charTemp2 <- paste0(func, "(", factorB, "/", factorA, ")")
      dt.write.this <- rbind(dt.write.this, data.table(code = charTemp1))
      dt.write.this <- rbind(dt.write.this, data.table(code = charTemp2))
    }
  }
}

# Form 5: (A+B)/C
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      for (k in 1:num_factors){
        factorA <- factor_list[i]
        factorB <- factor_list[j]
        factorC <- factor_list[k]
        charTemp1 <- paste0(func, "((", factorA, "+", factorB, ")/(", factorC, "))")
        dt.write.this <- rbind(dt.write.this, data.table(code = charTemp1))
      }
    }
  }
}

# Form 6: (A-B)/C
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      for (k in 1:num_factors){
        factorA <- factor_list[i]
        factorB <- factor_list[j]
        factorC <- factor_list[k]
        charTemp1 <- paste0(func, "((", factorA, "-", factorB, ")/(", factorC, "))")
        charTemp2 <- paste0(func, "((", factorB, "-", factorA, ")/(", factorC, "))")
        dt.write.this <- rbind(dt.write.this, data.table(code = charTemp1))
        dt.write.this <- rbind(dt.write.this, data.table(code = charTemp2))
      }
    }
  }
}

# Form : (A+B)/(C+D)
for (func in function_list){
  for (i in 1:(num_factors-1)){
    for (j in (i+1):(num_factors)){
      for (k in 1:(num_factors - 1)){
        for(l in (k+1):(num_factors)){
          factorA <- factor_list[i]
          factorB <- factor_list[j]
          factorC <- factor_list[k]
          factorD <- factor_list[l]
          charTemp1 <- paste0(func, "((", factorA, "+", factorB, ")/(", factorC, "+", factorD, "))")
          dt.write.this <- rbind(dt.write.this, data.table(code = charTemp1))
        }
      }
    }
  }
}

dt.write.this[,id := 1:nrow(dt.write.this)]
dt.write.this[,file_name := paste0("alpha_", id)]

for (i in 1:nrow(dt.write.this)){
  dt.temp <- dt.write.this[id == i]
  writeChar(dt.temp$code, paste0(parent_dir, "/test_combinations/alpha_", i, ".txt"), nchars = nchar(dt.temp$code))
}









