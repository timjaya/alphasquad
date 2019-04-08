library(RSelenium)
library(rvest)
library(data.table)

# Note: User login is not automated yet so I cannot parallelized this process neatly
# Have to login 3 times separately which creates massive redundancy

url = "https://websim.worldquantvrc.com/simulate"

# for authentication
# make sure to set environment variables through console e.g. execute
# Sys.setenv("USERNAME"= insert username here, "PASSWORD"= insert password)
username = Sys.getenv("USERNAME")
password = Sys.getenv("PASSWORD")

authenticate <- function(driver, username, password) {
  email_path = '//*[@id="root"]/div/section/div/article/div/div/form/div[2]/div[1]'
  password_path = '//*[@id="password"]'
  # enter username and password then sign in 
  driver$findElement(using = "xpath", value = email_path)$sendKeysToActiveElement(sendKeys = list(username))
  driver$sendKeysToActiveElement(sendKeys = list(key = "tab"))
  driver$findElement(using = "xpath", value = password_path)$sendKeysToActiveElement(sendKeys = list(password))
  driver$executeScript(script = 'document.querySelector(".button--lg").firstElementChild.click()')
  
  # to deal with skip button
  Sys.sleep(8)
  driver$executeScript(script = 'document.querySelector(".introjs-skipbutton").click()')
  
  # refresh to redirect cursor
  driver$refresh()
  Sys.sleep(8) 
  return(driver)
}

# headless
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

driver1 <- rsDriver(browser=c("chrome"), port = 4444L, chromever="73.0.3683.68", extraCapabilities = eCaps)
driver2 <- rsDriver(browser=c("chrome"), port = 4445L, chromever="73.0.3683.68", extraCapabilities = eCaps)
driver3 <- rsDriver(browser=c("chrome"), port = 4446L, chromever="73.0.3683.68", extraCapabilities = eCaps)

remDr1 <- driver1[["client"]]
remDr1$navigate(url)
remDr2 <- driver2[["client"]]
remDr2$navigate(url)
remDr3 <- driver3[["client"]]
remDr3$navigate(url)

remDr1 = authenticate(remDr1, username, password)
remDr2 = authenticate(remDr2, username, password)
remDr3 = authenticate(remDr3, username, password)

# Jquery to grab text editor
# document.getElementsByClassName("monaco-scrollable-element editor-scrollable vs-dark")[0].textContent

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dir_path <- paste0(parent_dir, "/test_combinations")
alpha_list <- list.files(dir_path)

# When you fix clusterapply use this chunk of code
# alpha_split_list <- split(alpha_list, 1:3)
# sub_list <- alpha_list[x]
# remDrList <- c(remDr1, remDr2, remDr3)
# remDr <- remDrList[[x]]
remDrList <- c(remDr1, remDr2, remDr3)

for (ind in 1:ceiling(length(alpha_list)/3)){
  vec_alpha <- alpha_list[(3*(ind-1) + 1):(3*ind)] 
  vec_alpha <- vec_alpha[!is.na(vec_alpha)]
  for (ind2 in 1:length(vec_alpha)){
    i <- vec_alpha[ind2]
    remDr <- remDrList[[ind2]]
    file_name <- paste0(dir_path, "/", i)
    my_code <- trimws(readChar(file_name, 
                               file.info(file_name)$size), "both")
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
  }
  
  Sys.sleep(10)
  
  for (ind2 in 1:length(vec_alpha)){
    i <- vec_alpha[ind2]
    remDr <- remDrList[[ind2]]
    file_name <- paste0(dir_path, "/", i)
    my_code <- trimws(readChar(file_name, 
                               file.info(file_name)$size), "both")
    
    # Get pass fail status
    run_status <- NULL
    print("Grabbing run status")
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
    
    print("Grabbing run metrics")
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
    
    print("Getting score delta")
    # JQuery to run IQC Performance Comparison
    # document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()
    
    # Only run on the first iteration to save time
    if (ind2 == 1){
      for (ind3 in 1:length(vec_alpha)){
        remDrTemp <- remDrList[[ind3]]
        iqc_run_status <- NULL
        while (is.null(iqc_run_status)){
          iqc_run_status <- tryCatch({
            iqc_run_status <- remDrTemp$executeScript(script = 'document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()')
          },
          error = function(e){
          }
          )
          Sys.sleep(2)
        }
      }
    }
    
    score_delta <- NULL
    while (is.null(score_delta) || is.na(score_delta)){
      score_delta <- tryCatch({
        # JQuery to grab positive text content
        # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--positive")[0].textContent
        # JQuery to grab negative text content
        # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--negative")[0].textContent
        score <- as.numeric(gsub(",", "", remDr$findElement(using = "xpath",
                                                            value = '//*[@id="alphas-performanceComparison"]/div[2]/div[1]/div[4]/div[2]')$getElementText()[[1]]))
        print(score)
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
    
    print(dt.insert.this)
    
    # Write insert this to result folder
    write.csv(dt.insert.this, paste0(parent_dir, "/results_combinations/", unlist(strsplit(i, "\\."))[1], ".csv"))
    
    Sys.sleep(runif(1, 1.0, 5))
  }
}


remDr1$closeall()
remDr2$closeall()
remDr3$closeall()
rm(driver1)
rm(driver2)
rm(driver3)
gc()
