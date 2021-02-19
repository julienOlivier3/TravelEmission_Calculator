


scrapeDistance <- function(destination, 
                           departure = "Mannheim")
  {
  tryCatch(
    
    ########################################################################
    # Try part: define the expression(s) you want to "try"                 #
    ########################################################################
    
    {
      # Just to highlight: 
      # If you want to use more than one R expression in the "try part" 
      # then you'll have to use curly brackets. 
      # Otherwise, just write the single expression you want to try and 
      
      # Define landing page
      landing <- "https://www.luftlinie.org/"
      
      # Navigate to landing page
      remDr$client$navigate(landing)
      
      # Wait until page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], landing)},
                               error = function(e){NULL})
        #loop until site has been loaded
      }
      
      
      # Insert start 
      startField <- remDr$client$findElement(using = "xpath", "//*[@id='start']")
      startField$sendKeysToElement(list(departure, key="enter"))
      
      Sys.sleep(1)
      
      # Insert destination
      try(destField <- remDr$client$findElement(using = "xpath", "//*[@id='end']"))
      try(destField <- remDr$client$findElement(using = "xpath", "/html/body/div[1]/div[1]/div[2]/div[2]/input"))
      destField$sendKeysToElement(list(destination, key="enter"))
      
      Sys.sleep(3)
      
      # Extract air distance
      temp <- remDr$client$findElement(using = "xpath", "/html/body/div[1]/div[1]/div[1]/span[2]/span[1]") 
      dist_air <- temp$getElementText()[[1]] %>% 
        str_remove("Entfernung: ") %>% 
        str_remove_all("\\.") %>% 
        str_replace_all(",", ".")
      
      # Extract driving distance
      temp <- remDr$client$findElement(using = "xpath", "/html/body/div[1]/div[3]/div[1]/div[2]/span[2]/span[1]") 
      dist_car <- temp$getElementText()[[1]] %>% 
        str_remove_all("\\.") %>% 
        str_replace_all(",", ".")
      
      
      #return(c(departure, destination, dist_air, dist_car))
      return(tibble(
        VON = departure, 
        NACH = destination, 
        LUFTLINIE = dist_air,
        FAHRTSTRECKE = dist_car
      ))
    },
    
    ########################################################################
    # Condition handler part: define how you want conditions to be handled #
    ########################################################################
    
    # Handler when a warning occurs:
    warning = function(cond) {
      message(paste("Reading the URL caused a warning:", landing))
      message("Here's the original warning message:")
      message(cond)
      
      # Choose a return value when such a type of condition occurs
      #return(NULL)
      return(tibble(
        VON = departure, 
        NACH = destination, 
        LUFTLINIE = as.character(NA),
        FAHRTSTRECKE = as.character(NA)
      ))
    },
    
    # Handler when an error occurs:
    error = function(cond) {
      message(paste("This seems to be an invalid departure-destination combination:", departure, "-", destination))
      message("Here's the original error message:")
      message(cond)
      
      # Choose a return value when such a type of condition occurs
      #return(NULL)
      return(tibble(
        VON = departure, 
        NACH = destination, 
        LUFTLINIE = as.character(NA),
        FAHRTSTRECKE = as.character(NA)
      ))
    },
    
    ########################################################################
    # Final part: define what should happen AFTER                          #
    # everything has been tried and/or handled                             #
    ########################################################################
    
    finally = {
      #message(paste("Processed Destination:", destination))
      #message("Some message at the end\n")
    }
  )    
  
}


