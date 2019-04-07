library(RSelenium)
library(rvest)
library(data.table)
library(glue)
library(jsonlite)

url = "https://websim.worldquantvrc.com/simulate"

driver <- rsDriver(browser=c("chrome"), port = 4444L, chromever="73.0.3683.68")
remDr <- driver[["client"]]
remDr$navigate(url)

# Jquery to grab text editor
# document.getElementsByClassName("monaco-scrollable-element editor-scrollable vs-dark")[0].textContent

dt.results <- data.table()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dir_path <- paste0(parent_dir, "/test_combinations")
alpha_list <- list.files(dir_path)

for (i in alpha_list){
  print(i)
  file_name <- paste0(dir_path, "/", i)
  my_code <- trimws(readChar(file_name, 
                             file.info(file_name)$size), "both")
  # check if there is an en dash in our code, if so replace it with a normal dash
  
  # Check to make sure text edit is blank
  remDr$findElement(using = "xpath", 
                    value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "control", "a"))
  remDr$findElement(using = "xpath", 
                    value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "backspace"))
  remDr$findElement(using = "xpath", 
                    value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "control", "a"))
  remDr$findElement(using = "xpath", 
                    value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "backspace"))
  
  # Enter our alpha code
  remDr$findElement(using = "xpath", 
                    value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(my_code))
  
  # JQuery to run simulation
  # document.querySelector(".editor-simulate-button").firstElementChild.click()
  remDr$executeScript(script = 'document.querySelector(".editor-simulate-button").firstElementChild.click()')
  
  Sys.sleep(10)
  
  # Get pass fail status
  run_status <- NULL
  while (is.null(run_status)){
    run_status <- tryCatch({
      pass_fail <- remDr$findElement(using = "xpath",
                                     value = '//*[@id="alphas-testingStatus"]/div/div[2]/div/div/div[1]')$getElementText()[[1]]
      pass_fail_status <- unlist(strsplit(pass_fail, " "))[2]
    },
    error = function(e){
      
    }
    )
    Sys.sleep(2)
  }
  
  # Get Sharpe
  sharpe_ratio <- as.numeric(remDr$findElement(using = "xpath",
                                               value = '//*[@id="alphas-summary"]/div/div[2]/div/div[1]/div/div[2]/div[2]')$getElementText()[[1]])
  # Get Turnover
  turn_over <- remDr$findElement(using = "xpath",
                                 value = '//*[@id="alphas-summary"]/div/div[2]/div/div[1]/div/div[3]/div[2]')$getElementText()[[1]]
  
  # Get Fitness
  fitness <- as.numeric(remDr$findElement(using = "xpath",
                                          value = '//*[@id="alphas-summary"]/div/div[2]/div/div[1]/div/div[4]/div[2]')$getElementText()[[1]])
  # Get Returns
  returns <- remDr$findElement(using = "xpath",
                               value = '//*[@id="alphas-summary"]/div/div[2]/div/div[1]/div/div[5]/div[2]')$getElementText()[[1]]
  
  # JQuery to run correlation
  # document.getElementsByClassName("correlation__content-status-time-refresh")[0].click()
  # remDr$executeScript(script = 'document.getElementsByClassName("correlation__content-status-time-refresh")[0].click()')
  # Sys.sleep(5)
  # correlation_value <- as.numeric(remDr$findElement(using = "xpath",
  #                                                   value = '//*[@id="alphas-correlation"]/div[2]/div/div[1]/div[2]/div[2]')$getElementText()[[1]])
  # 
  # while (is.null(correlation_value) || is.na(correlation_value)){
  #   correlation_value <- tryCatch({
  #     # JQuery to grab correlation value
  #     # document.getElementsByClassName("correlation__content-status-higher-value")[0].textContent
  #     value <- as.numeric(remDr$findElement(using = "xpath",
  #                                                       value = '//*[@id="alphas-correlation"]/div[2]/div/div[1]/div[2]/div[2]')$getElementText()[[1]])
  #   },
  #   error = function(e){
  #     
  #   }
  #   )
  #   Sys.sleep(900)
  # }
  correlation_value <- NA
  
  # JQuery to run IQC Performance Comparison
  # document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()
  remDr$executeScript(script = 'document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()')
  
  score_delta <- NULL
  while (is.null(score_delta) || is.na(score_delta)){
    score_delta <- tryCatch({
      # JQuery to grab positive text content
      # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--positive")[0].textContent
      # JQuery to grab negative text content
      # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--negative")[0].textContent
      score <- as.numeric(gsub(",", "", remDr$findElement(using = "xpath",
                                                          value = '//*[@id="alphas-performanceComparison"]/div[2]/div[1]/div[4]/div[2]')$getElementText()[[1]]))
    },
    error = function(e){
      
    }
    )
    Sys.sleep(2)
  }
  dt.insert.this <- data.table(alpha_id = i,
                               status = run_status,
                               correlation = correlation_value,
                               score_delta = score_delta,
                               sharpe_ratio = sharpe_ratio,
                               turn_over = turn_over,
                               fitness = fitness,
                               returns = returns)
  dt.results <- rbind(dt.results, dt.insert.this)
  
  print(dt.insert.this)
  
  Sys.sleep(runif(1, 1.0, 5))
}

write.csv(dt.results, "results.csv")

remDr$closeall()
rm(driver)
gc()

