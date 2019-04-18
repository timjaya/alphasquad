library(RSelenium)
library(rvest)
library(data.table)

remDr1$closeall()
remDr2$closeall()
remDr3$closeall()
rm(driver1)
rm(driver2)
rm(driver3)
gc()

url = "https://websim.worldquantvrc.com/simulate"

# for authentication
# make sure to set environment variables through console e.g. execute
# Sys.setenv("USERNAME"= "insert", "PASSWORD"= "insert")

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
  
  Sys.sleep(5)
  tryCatch({
    # Click on new tab to avoid no tab error
    driver$executeScript(script = 'document.querySelector(".editor-tabs__new-tab-dropdown-element").click()')
  }, error=function(e){})
  
  
  # to deal with skip button
  skip_status <- NULL
  while (is.null(skip_status)){
    skip_status <- tryCatch({
      driver$executeScript(script = 'document.querySelector(".introjs-skipbutton").click()')
      }, error = function(e){})
    Sys.sleep(1)
  }
  
  # Click on new tab to avoid no tab error
  driver$executeScript(script = 'document.querySelector(".editor-tabs__new-tab-dropdown-element").click()')
  
  
  # refresh to redirect cursor
  driver$refresh()
  return(driver)
}

authenticateInactive <- function(driver, username, password){
  email_path = '//*[@id="root"]/div/section/div/article/div/div/form/div[2]/div[1]'
  password_path = '//*[@id="password"]'
  # enter username and password then sign in 
  driver$findElement(using = "xpath", value = email_path)$sendKeysToActiveElement(sendKeys = list(username))
  driver$sendKeysToActiveElement(sendKeys = list(key = "tab"))
  driver$findElement(using = "xpath", value = password_path)$sendKeysToActiveElement(sendKeys = list(password))
  driver$executeScript(script = 'document.querySelector(".button--lg").firstElementChild.click()')
}

funcRun <- function(offset=0, subtest_folder = "", subresult_folder = "", bln.corr = FALSE){
  
  driver1 <- rsDriver(browser=c("chrome"), port = 4444L, chromever="73.0.3683.68")
  driver2 <- rsDriver(browser=c("chrome"), port = 4445L, chromever="73.0.3683.68")
  driver3 <- rsDriver(browser=c("chrome"), port = 4446L, chromever="73.0.3683.68")
  
  remDr1 <- driver1[["client"]]
  remDr1$navigate(url)
  remDr2 <- driver2[["client"]]
  remDr2$navigate(url)
  remDr3 <- driver3[["client"]]
  remDr3$navigate(url)
  
  # Authenticate my drivers
  authenticate(remDr1, username, password)
  authenticate(remDr2, username, password)
  authenticate(remDr3, username, password)
  
  
  # Grab combinations to test
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dir_path <- paste0(parent_dir, "/test_combinations/", subtest_folder)
  alpha_list <- setdiff(list.files(dir_path),list.dirs(dir_path,recursive=F, full.names = F))
  if (offset > 0){
    alpha_list <- tail(alpha_list, -offset)
  }
  
  remDrList <- c(remDr1, remDr2, remDr3)
  
  ind_list <- 1:ceiling(length(alpha_list)/3)
  
  for (ind in ind_list){
    vec_alpha <- alpha_list[(3*(ind-1) + 1):(3*ind)] 
    vec_alpha <- vec_alpha[!is.na(vec_alpha)]
    
    # Check simulation run status, if it's running for too long, then kill it and restart
    simulation_status <- NULL
    while (is.null(simulation_status)){
      for (ind2 in 1:length(vec_alpha)){
        i <- vec_alpha[ind2]
        remDr <- remDrList[[ind2]]
        file_name <- paste0(dir_path, "/", i)
        my_code <- trimws(readChar(file_name,
                                   file.info(file_name)$size), "both")
        
        # Check to make sure text edit is blank
        # Jquery to grab text editor
        # document.getElementsByClassName("monaco-scrollable-element editor-scrollable vs-dark")[0].textContent
        # Press tab three times in case cursor is not on editor
        # Try to find this element, if it doesn't find it, it means that we were logged out due to inactivity
        tryCatch({
          remDr$findElement(using = "xpath",
                            value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "tab"))
        }, 
        error = function(e){
          authenticateInactive(remDr, username, password)
        })
        remDr$findElement(using = "xpath",
                          value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "tab"))
        remDr$findElement(using = "xpath",
                          value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "tab"))
        remDr$findElement(using = "xpath",
                          value = '//*[@id="root"]/div/div[2]/div/div[3]/div[2]/div/div[1]/div/div/div/div[1]/div/div[1]/div[2]')$sendKeysToActiveElement(sendKeys = list(key = "tab"))
        
        # Clear text editor
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
        
        testing_status <- NULL
        testing_count <- 0
        while (is.null(testing_status)){
          testing_status <- tryCatch({
            remDr$findElement(using = "xpath",
                              value = '//*[@id="alphas-testingStatus"]/div/div[1]/div')$getElementText()[[1]]
          }, error = function(e){
            testing_count <- testing_count + 1
            print(paste0("Testing count: ", testing_count))
            if (testing_count > 180){
              # If it failed too many times then let's try to restart the simulation
              remDr$executeScript(script = 'document.querySelector(".editor-simulate-button").firstElementChild.click()')
              testing_count <- 0
            }
            Sys.sleep(1)
          })
        }
        
        print(testing_status)
        # We already tested this alpha
        if (testing_status == "OS Testing Status"){
          next
        }
        
        # Get pass fail status
        run_status <- NULL
        # Use run count to keep track of how many runs it's been so far after 180 runs, we kill it and restart the loop 
        run_count <- 0
        print("Grabbing run status")
        while (is.null(run_status)){
          run_status <- tryCatch({
            print(paste0("Run count: ", run_count))
            if (run_count > 180){
              # Click on cancel button
              # If cancel button doesn't exist, then restart simulation
              tryCatch({
                remDr$executeScript(script = 'document.getElementsByClassName("simulation__cancel--here")[0].click()')
                # Restart counter and click on simulation again
                run_count <- 0
                # Restart simulation
                remDr$executeScript(script = 'document.querySelector(".editor-simulate-button").firstElementChild.click()')
              }, error = function(e){
                # Restart simulation
                remDr$executeScript(script = 'document.querySelector(".editor-simulate-button").firstElementChild.click()')
              })
              Sys.sleep(5)
            }
            run_count <- run_count + 1
            pass_fail <- remDr$findElement(using = "xpath",
                                           value = '//*[@id="alphas-testingStatus"]/div/div[2]/div/div/div[1]')$getElementText()[[1]]
            pass_fail_status <- unlist(strsplit(pass_fail, " "))[2]
            pass_fail2 <- remDr$findElement(using = "xpath",
                                           value = '//*[@id="alphas-testingStatus"]/div/div[2]/div/div/div[2]')$getElementText()[[1]]
            pass_fail_status2 <- unlist(strsplit(pass_fail2, " "))[2]
            if (pass_fail == "FAIL" | pass_fail == "FAIL"){
              "FAIL"
            } else{
              "PASS"
            }
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
        
        # Get correlation
        if (bln.corr){
          # JQuery to run correlation
          # document.getElementsByClassName("correlation__content-status-time-refresh")[0].click()
          remDr$executeScript(script = 'document.getElementsByClassName("correlation__content-status-time-refresh")[0].click()')
          Sys.sleep(5)
          correlation_value <- as.numeric(remDr$findElement(using = "xpath",
                                                            value = '//*[@id="alphas-correlation"]/div[2]/div/div[1]/div[2]/div[2]')$getElementText()[[1]])

          while (is.null(correlation_value) || is.na(correlation_value)){
            correlation_value <- tryCatch({
              # JQuery to grab correlation value
              # document.getElementsByClassName("correlation__content-status-higher-value")[0].textContent
              value <- as.numeric(remDr$findElement(using = "xpath",
                                                                value = '//*[@id="alphas-correlation"]/div[2]/div/div[1]/div[2]/div[2]')$getElementText()[[1]])
            },
            error = function(e){

            }
            )
            Sys.sleep(900)
          }
        } else{
          correlation_value <- NA
        }
        print("Getting score delta")
        # JQuery to run IQC Performance Comparison
        # document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()
        
        # Only run on the first iteration to save time
        if (ind2 == 1){
          for (ind3 in 1:length(vec_alpha)){
            remDrTemp <- remDrList[[ind3]]
            
            testing_status <- NULL
            while (is.null(testing_status)){
              testing_status <- tryCatch({
                remDrTemp$findElement(using = "xpath",
                                  value = '//*[@id="alphas-testingStatus"]/div/div[1]/div')$getElementText()[[1]]
              }, error = function(e){
                Sys.sleep(1)
              })
            }
            
            # We already tested this alpha
            print(testing_status)
            if (testing_status == "OS Testing Status"){
              next
            }
            
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
        na_count <- 0
        while (is.null(score_delta) || is.na(score_delta)){
          score_delta <- tryCatch({
            # JQuery to grab positive text content
            # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--positive")[0].textContent
            # JQuery to grab negative text content
            # document.getElementsByClassName("summary-metrics-info summary-metrics-info--data summary-metrics-info--negative")[0].textContent
            score <- as.numeric(gsub(",", "", remDr$findElement(using = "xpath",
                                                                value = '//*[@id="alphas-performanceComparison"]/div[2]/div[1]/div[4]/div[2]')$getElementText()[[1]]))
            # Count number of times we scraped NA, if it's over 180, then it's likely bugged out and we need to restart it
            if (is.na(score)){
              na_count <- na_count + 1
            }
            
            # Restart IQC calculation
            if (na_count > 180){
              remDr$executeScript(script = 'document.getElementsByClassName("correlation__content-status-time-refresh")[1].click()')
              na_count <- 0
            }
            score
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
        write.csv(dt.insert.this, paste0(parent_dir, "/results_combinations/", subresult_folder, unlist(strsplit(i, "\\."))[1], ".csv"))
        # Move on to next iteration
        simulation_status <- TRUE
        Sys.sleep(runif(1, 1.0, 5))
      }
    }
  }
}

# Offset defines how many files you want to skip
funcRun(offset = 0)
