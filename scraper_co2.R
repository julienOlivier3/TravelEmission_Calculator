# Scraper car information
scrapeCar <- function(destination, 
                      departure = "Mannheim",
                      fuel_type = "Petrol",               # Choose one of: Petrol, Diesel, L.P.G.
                      car_type = "Medium",               # Small, Medium, Large, Very large
                      round_trip = TRUE                  # Choose one of: TRUE (round trip), FALSE (one way)
                      ){
  tryCatch(
    
    ########################################################################
    # Try part: define the expression(s) you want to "try"                 #
    ########################################################################
    
    {
      # Just to highlight: 
      # If you want to use more than one R expression in the "try part" 
      # then you'll have to use curly brackets. 
      # Otherwise, just write the single expression you want to try and 
      
      # Get km
      source(file.path(getwd(), "scraper_distance.R"), encoding = "utf-8")
      distance <- scrapeDistance(destination = destination)                                           # scrape distance
      distance <- as.double(str_extract(distance$FAHRTSTRECKE, pattern = regex("\\d{1,}")))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      # Define landing page first
      landing <- "https://www.greentripper.org/default.aspx?cl=en&olt=car"
      
      # Navigate to landing page
      remDr$client$navigate(landing)
      
      # Wait until page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], landing)},
                               error = function(e){NULL})
        #loop until site has been loaded
      }
      
      
      
      # Navigate to CO2 calculation pane
      remDr$client$findElement(using = "xpath", value = "//*[@id='train-section']")
      
      # Select fuel type 
      if(fuel_type == "Petrol"){
        fuelField <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[2]/div[1]/span/label[1]")
        fuelField$clickElement()
      }
      else if(fuel_type == "Diesel"){
        fuelField <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[2]/div[1]/span/label[2]")
        fuelField$clickElement()
      }
      else if(fuel_type == "L.P.G."){
        fuelField <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[2]/div[1]/span/label[3]")
        fuelField$clickElement()
      }
      else{
        fuelField <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[2]/div[1]/span/label[1]")
        fuelField$clickElement()
      }
      
      # Insert consumption
      classField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_DropdownListEngine']")
      classField$sendKeysToElement(list(car_type, key="enter"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      
      # Insert km
      kmField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_TextBoxKm']")
      distance <- as.character(ifelse(round_trip, 2*as.double(distance), distance))
      kmField$sendKeysToElement(list(distance, key="enter"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      
      # Click calculate and compensate button
      calculateButton <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_ButtonCarCalculate']")
      calculateButton$clickElement()
      
      
      # Wait until new page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], "https://www.greentripper.org/calculator.")},
                               error = function(e){NULL})
        #loop until CO2-calculator site has been loaded
      }
      
      
      # Extract CO2 emissions
      co2Emission <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[3]/div[1]/div/div/div[1]/div")
      co2Emission <- co2Emission$getElementText()[[1]] %>% 
        str_replace_all(",", ".")
      
      # Extract (average) compensation costs
      co2Table <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table")
      co2Text <- co2Table$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table/tbody/tr/td[1]/div[4]")
      
      co2Costs <- co2Text$getElementText()[[1]] %>% 
        str_extract_all(pattern = regex("€.{1,} VAT Included")) %>%
        unlist()
      
      co2Cost <- str_extract_all(co2Costs, regex("\\d{1,},\\d{0,}")) %>% 
        unlist() %>% 
        str_replace_all(",", ".") %>% 
        as.double() %>% 
        mean()
      
      co2Cost <- paste(round(co2Cost/21*121, 2), "€")
      
      #return(c(co2Emission, co2Cost))
      return(tibble(
        VON = departure, 
        NACH = destination, 
        CO2_EMISSIONEN = co2Emission,
        CO2_KOSTEN = co2Cost
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
      ))
    },
    
    ########################################################################
    # Final part: define what should happen AFTER                          #
    # everything has been tried and/or handled                             #
    ########################################################################
    
    finally = {
      message(paste("Processed destination (by car):", destination))
      #message("Some message at the end\n")
    }
  )    
  
}



# Scraper train information
scrapeTrain <- function(destination, 
                        departure = "Mannheim",
                        train_type = "Train",               # Choose one of: Train, Night train, TGV, Eurostar, Thalys
                        n_people = "1",                     # Choose number of passengers
                        round_trip = TRUE                   # Choose one of: TRUE (round trip), FALSE (one way)
                        ){
  tryCatch(
    
    ########################################################################
    # Try part: define the expression(s) you want to "try"                 #
    ########################################################################
    
    {
      # Just to highlight: 
      # If you want to use more than one R expression in the "try part" 
      # then you'll have to use curly brackets. 
      # Otherwise, just write the single expression you want to try and 
      
      # Define landing page first
      landing <- "https://www.greentripper.org/default.aspx?cl=en&olt=train"
      
      # Navigate to landing page
      remDr$client$navigate(landing)
      
      # Wait until page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], landing)},
                               error = function(e){NULL})
        #loop until site has been loaded
      }
      
      # Navigate to CO2 calculation pane
      remDr$client$findElement(using = "xpath", value = "//*[@id='train-section']")
      
      # Insert train type 
      trainField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_DropdownListTrainType']")
      trainField$sendKeysToElement(list(train_type, key="enter"))
      
      # Insert departure 
      departField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_TextBoxTrainStart']")
      departField$sendKeysToElement(list(departure, key="enter"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      # Insert destination
      destField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_TextBoxTrainEnd']")
      destField$sendKeysToElement(list(destination, key="enter"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      # Insert number of passengers
      peopleField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_TextBoxTrainPassengers']")
      peopleField$sendKeysToElement(list(n_people, key="enter"))
      
      
      if(round_trip){
        
        # Change from single to return 
        returnButton <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[5]/div[1]/span/label[2]")
        returnButton$clickElement()
        Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
        
      }
      
      # Click calculate and compensate button
      calculateButton <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_ButtonTrainCalculate']")
      calculateButton$clickElement()
      
      
      # Wait until new page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], "https://www.greentripper.org/calculator.")},
                            error = function(e){NULL})
        #loop until CO2-calculator site has been loaded
      }
      

      # Extract CO2 emissions
      co2Emission <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[3]/div[1]/div/div/div[1]/div")
      co2Emission <- co2Emission$getElementText()[[1]] %>% 
        str_replace_all(",", ".")
      
      # Extract (average) compensation costs
      co2Table <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table")
      co2Text <- co2Table$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table/tbody/tr/td[1]/div[4]")
      
      co2Costs <- co2Text$getElementText()[[1]] %>% 
        str_extract_all(pattern = regex("€.{1,} VAT Included")) %>%
        unlist()
      
      co2Cost <- str_extract_all(co2Costs, regex("\\d{1,},\\d{0,}")) %>% 
        unlist() %>% 
        str_replace_all(",", ".") %>% 
        as.double() %>% 
        mean()
      
      co2Cost <- paste(round(co2Cost/21*121, 2), "€")
      
      #return(c(co2Emission, co2Cost))
      return(tibble(
        VON = departure, 
        NACH = destination, 
        CO2_EMISSIONEN = co2Emission,
        CO2_KOSTEN = co2Cost
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
      ))
    },
    
    ########################################################################
    # Final part: define what should happen AFTER                          #
    # everything has been tried and/or handled                             #
    ########################################################################
    
    finally = {
      message(paste("Processed destination (by train):", destination))
      #message("Some message at the end\n")
    }
  )    

}

# Scraper flight information
scrapeFlight <- function(destination, 
                        departure = "Mannheim",
                        flight_class = "Economy",           # Choose one of: Economy, Premium Economy, Buisness, First
                        n_people = "1",                     # Choose number of passengers
                        round_trip = TRUE                   # Choose one of: TRUE (round trip), FALSE (one way)
                        ){
  tryCatch(
    
    ########################################################################
    # Try part: define the expression(s) you want to "try"                 #
    ########################################################################
    
    {
      # Just to highlight: 
      # If you want to use more than one R expression in the "try part" 
      # then you'll have to use curly brackets. 
      # Otherwise, just write the single expression you want to try and 
      
      # Define landing page first
      landing <- "https://www.greentripper.org/default.aspx?cl=en&olt=plane"
      
      # Navigate to landing page
      remDr$client$navigate(landing)
      
      # Wait until page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], landing)},
                               error = function(e){NULL})
        #loop until site has been loaded
      }
      
      # Navigate to CO2 calculation pane
      remDr$client$findElement(using = "xpath", value = "//*[@id='plane-section']")
      
      # Insert departure 
      Fields <- remDr$client$findElements(using = "xpath", "//input[contains(@id,'eac-')]")
      departField <- Fields[[1]]
      departField$clearElement()
      departField$sendKeysToElement(list(departure))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      departField$sendKeysToElement(list(","))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      departField_ac <- remDr$client$findElement(using = "xpath", "//div[contains(@class,'eac-item')]")
      departPort <- departField_ac$getElementText()[[1]]
      departField$clearElement()
      departField$sendKeysToElement(list(departPort, key="escape"))
      #departField$sendKeysToElement(list(key="escape"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      # Insert destination
      destField <- Fields[[2]]
      destField$clearElement()
      destField$sendKeysToElement(list(destination))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      destField$sendKeysToElement(list(","))
      Sys.sleep(time = runif(1, min = 1.9, max = 2.1))
      destField_ac <- destField$findChildElement(using = "xpath", "//div[contains(@class,'eac-item')]")
      destField_ac <- destField_ac$findChildElements(using = "xpath", "//div[contains(@class,'eac-item')]")
      #attrNumber <- destField$getElementAttribute(attrName = "id")[[1]] %>% str_extract("\\d{1,}")
      #destField_ac <- remDr$client$findElement(using = "xpath", paste0("//div[contains(@id,'eac-container-eac-", attrNumber,"')]"))
      
      
      # Take first text element which contains destination name as auto completion for destination field
      for (i in 1:length(destField_ac)){
        ac <- destField_ac[[i]]$getElementText()[[1]]
        if(str_detect(ac, destination)){
          break  
        }
      }
      
      # If first auto complete element does not contain destination name, abort (assuming no flight connection exists between departure and destination)
      # Else, proceed with scraping process
      # if(!str_detect(ac, destination)){
      #   return(tibble(
      #     VON = departure,
      #     NACH = destination,
      #     CO2_EMISSIONEN = as.character(NA),
      #     CO2_KOSTEN = as.character(NA)
      #   ))
      # } else {
      destPort <- ac
      destField$clearElement()
      destField$sendKeysToElement(list(destPort, key="escape"))
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      
      
      # Insert flight class
      classField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_DropDownClass']")
      classField$sendKeysToElement(list(flight_class, key="enter"))
      
      # Insert number of passengers
      peopleField <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_TextBoxPassengers']")
      peopleField$clearElement()
      peopleField$sendKeysToElement(list(n_people))
      
      
      if(round_trip){
        
        # Change from single to return 
        returnButton <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div[2]/div/div[2]/div[1]/div/div[6]/span/label[2]")
        returnButton$clickElement()
        
      }
      
      # Click calculate and compensate button
      calculateButton <- remDr$client$findElement(using = "xpath", "//*[@id='ContentPlaceHolder_ButtonPlaneCalculate']")
      calculateButton$clickElement()
      
      
      # Wait until new page has been loaded
      currentURL <- NULL
      while(is.null(currentURL)){
        currentURL <- tryCatch({str_detect(remDr$client$getCurrentUrl()[[1]], "https://www.greentripper.org/calculator.")},
                               error = function(e){NULL})
        #loop until CO2-calculator site has been loaded
      }
      
      
      # Extract CO2 emissions
      co2Emission <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[3]/div[1]/div/div/div[1]/div")
      co2Emission <- co2Emission$getElementText()[[1]] %>% 
        str_replace_all(",", ".")
      
      # Extract (average) compensation costs
      co2Table <- remDr$client$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table")
      Sys.sleep(time = runif(1, min = 0.1, max = 0.9))
      co2Text <- co2Table$findElement(using = "xpath", "/html/body/div[6]/div[1]/form/div[3]/div/div/div[2]/div[2]/table/tbody/tr/td[1]/div[4]")
      
      co2Costs <- co2Text$getElementText()[[1]] %>% 
        str_extract_all(pattern = regex("€.{1,} VAT Included")) %>%
        unlist()
      
      co2Cost <- str_extract_all(co2Costs, regex("\\d{1,},\\d{0,}")) %>% 
        unlist() %>% 
        str_replace_all(",", ".") %>% 
        as.double() %>% 
        mean()
      
      co2Cost <- paste(round(co2Cost/21*121, 2), "€")
      
      return(tibble(
        VON = departPort,
        NACH = destPort,
        CO2_EMISSIONEN = co2Emission,
        CO2_KOSTEN = co2Cost
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
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
        CO2_EMISSIONEN = as.character(NA),
        CO2_KOSTEN = as.character(NA)
      ))
    },
    
    ########################################################################
    # Final part: define what should happen AFTER                          #
    # everything has been tried and/or handled                             #
    ########################################################################
    
    finally = {
      message(paste("Processed destination (by plane):", destination))
      #message("Some message at the end\n")
    }
  )    
  
}






