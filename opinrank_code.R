if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#Returns a raw version of the inputs for the given city.
get_raw <- function(city) {
  
  url_base <- "https://raw.githubusercontent.com/edx-ds/opinrank/main/datasets/"; 
  
  cols_all <- c("doc_id", "hotel_name", "hotel_url", "street", "city", "state",
                "country", "zip", "class", "price", "num_reviews", "cleanliness",
                "room", "service", "location", "value", "comfort", "overall_rating", "source")
  
  #Retrieving the given CSV file from the corresponding GitHub repository.
  temp <- tempfile()
  url <- paste(paste(url_base, city, sep=""), ".csv", sep="")
  download.file(url, temp)
  
  #Extracting the file contents.
  raw <- fread(text = gsub("::", "\t", readLines(temp)), 
               col.names = cols_all, fill=TRUE)
  rm(temp)
  
  raw
}

#Returns a data frame containing the inputs for the given city.
get_inputs <- function(city) {
  
  raw <- as.data.frame(get_raw(city))
  
  raw %>% mutate(
    doc_id = as.character(doc_id), 
    hotel_name = as.character(hotel_name),
    hotel_url = as.character(hotel_url), 
    street = as.character(street),
    city = as.character(city), 
    state = as.character(state),
    country = as.character(country), 
    zip = as.character(zip),
    class = as.character(class), 
    price = as.numeric(price),
    num_reviews = as.numeric(num_reviews), 
    cleanliness = as.numeric(cleanliness),
    room = as.numeric(room), 
    service = as.numeric(service),
    location = as.numeric(location), 
    value = as.numeric(value),
    comfort = as.numeric(comfort), 
    overall_rating = as.numeric(overall_rating),
    source = "")
}

#Returns the train/test sets associated with the given input.
partition_inputs <- function(inputs) {
  
  test_index <- createDataPartition(
    y = inputs$source, times = 1, p = 0.2, list = FALSE)
  
  list("train" = inputs[-test_index,], "test" = inputs[test_index,])
}

#Returns the main train/test sets associated with the given city.
get_sets <- function(city) {
  
  partition_inputs(get_inputs(city))
}

#Returns the main train/test sets associated with all the cities.
get_sets0 <- function(cities) {
  
  sets0 <- list()
  for (city in cities) {
    sets0[[city]] <- get_sets(city)
  }
  
  sets0
}

#Performs the required modifications (e.g., focusing just on the training set and/or 
#updating the values of certain column/predictor) in the input train/test sets such 
#that they can be used in the given intermediate calculations.
adapt_sets <- function(sets0, col, round_digits, only_train = TRUE) {
  
  sets <- NULL
  if (only_train) {
    #The train/test sets are being redefined by only using the data from the main train dataset.
    sets <- partition_inputs(sets0$train)
  }
  else {
    sets <- sets0
  }
  
  #The values for the given column/predictor are rounded to the input number of digits.
  sets$train[[col]] <- round(sets$train[[col]], digits = round_digits)
  sets$test[[col]] <- round(sets$test[[col]], digits = round_digits)
  
  sets
}

#Returns the predictions delivered by the model under the given predictions (RMSE values or whole arrays).
#It also adapts the train/test data sets if required.
get_adapted_prediction <- function(sets0, col, digits, only_rmse = TRUE, only_train = TRUE) {
  
  #Adapting the train/test datasets to meet the expected conditions.
  sets <- adapt_sets(sets0, col, digits, only_train)
  train_set <- sets$train
  test_set <- sets$test
  
  #Applying the model to the given datasets and predictor/column (used for bias calculation).
  pred <- get_prediction(train_set, test_set, col)
  
  if (only_rmse) {
    #Only the resulting RMSE value is returned.
    get_rmse(pred, test_set$overall_rating)
  } 
  else {
    #All the original and predicted values are returned.
    list("pred" = pred, "orig" = test_set$overall_rating)
  }
}

#Returns the best number of significant decimal digits for the input predictor/column.
get_best_digit <- function(sets0, cities, col) {
  
  digits_all <- c(0, 1, 2, 3)
  bests <- c()
  
  for (city in cities) {
    
    sets <- sets0[[city]]
    best <- 0
    best_rmse <- 100
    
    for (digits in digits_all) {
      #Calculates the RMSE associated with the predictions for the
      #current city, predictor and number of digits.
      rmse <- get_adapted_prediction(sets, col, digits)
      if (rmse < best_rmse) {
        #The number of digits outputing the lowest RMSE value for 
        #the given city is chosen as the best one.
        best_rmse <- rmse
        best <- digits
      }
    }
    
    #The best value for the current city is stored in the array bests.
    bests <- append(bests, best)
  }
  
  #The (floored) average value for all the cities is assumed to be the
  #ideal number of significant decimal digits for the given predictor.
  floor(mean(bests))
}

#Returns the best number of significant decimal digits for all the predictors.
get_best_digits <- function(sets0, cities, cols) {
  
  digits <- list()
  for (col in cols) {
    digits[[col]] <- get_best_digit(sets0, cities, col)
  }
  
  digits
}

#Returns the RMSE values associated with all the predictions for all the scenarios (cities and predictors).
get_rmses <- function(sets0, cities, cols, digits) {
  
  rmses_cols <- c("name")
  rmses_cols <- append(rmses_cols, cols)
  rmses <- data.frame(matrix(ncol = length(rmses_cols), nrow = 0))
  colnames(rmses) <- rmses_cols
  
  #Iterating through all the predictors/columns.
  for (col_i in 1:length(cols)) {
    
    col <- cols[col_i]
    col_i2 <- col_i + 1
    
    #Iterating through all the cities.
    for (city_i in 1:length(cities)) {
      
      city <- cities[city_i]
      
      if (col_i == 1) {
        rmses[city_i, 1] <- city
      }
      
      #Storing the given RMSE value in the corresponding row/col.
      rmses[city_i, col_i2] <- get_adapted_prediction(
        sets0[[city]], col, unlist(digits[col])) 
    }
  }
  
  rmses
}

#Returns the ideal predictors for all the cities.
get_best_columns <- function(cities, rmses) {
  
  cols_best <- list()
  for (i in 1:length(cities)) {
    #Determining the ideal predictor for the given city by looking for the lowest RMSE value.
    cols_best[[cities[i]]] <- which.min(rmses[i, 2:ncol(rmses)])
  }
  
  cols_best
}

#Returns the average ratings for the given training set.
get_aver_ratings <- function(train_set) {
  mean(train_set$overall_rating)
}

#Returns the model bias associated with the given inputs.
get_bias <- function(train_set, aver_r, col, lambda = 0) {
  train_set %>% group_by_at(col) %>% 
    summarize(bias = sum(overall_rating - aver_r) / (n() + lambda))
}

#Returns the predictions on the test set from the input training set and further conditions. 
get_prediction <- function(train_set, test_set, col, lambda = 0, cleaning = TRUE, only_pred = TRUE) {
  
  #Getting the essential parts of the predictive model by using the training set.
  aver_r <- get_aver_ratings(train_set)
  bias <- get_bias(train_set, aver_r, col, lambda)
  
  #Applying the values generated by using the training set to make the predictions on the test set. 
  output <- test_set %>% 
    left_join(bias, by = col) %>% 
    mutate(bias = ifelse(is.na(bias), 0, bias)) %>%
    mutate(pred = aver_r + bias)
  
  if (cleaning) {
    #Replacing invalid values caused by wrong input data and calculations. 
    min_r <- min(train_set$overall_rating)
    max_r <- max(train_set$overall_rating)
    output <- output %>% mutate(
      pred = ifelse(is.na(pred), aver_r, #NA to average.
                    ifelse(pred < min_r, min_r, #Below minimum to minimum.
                           ifelse(pred > max_r, max_r, #Above maximum to maximum. 
                                  pred))))
  }
  
  if (only_pred) {
    #Returning only the predictions.
    output$pred 
  }
  else { 
    #Returning all the variables (inputs, predictions and intermediate calculations).
    output 
  }
}

#Returns the RMSE associated with the input values.
get_rmse <- function(pred, real) { 
  sqrt(mean((pred - real)^2)) 
}

set.seed(1)

cities <- c("beijing", "chicago", "dubai", "las-vegas", "london",
            "montreal", "new-delhi", "new-york-city", 
            "san-francisco", "shanghai")

cols <- c("cleanliness", "room", "service", "location", "value", "comfort")

sets0 <- get_sets0(cities)
digits <- get_best_digits(sets0, cities, cols)
rmses <- get_rmses(sets0, cities, cols, digits)
cols_best <- get_best_columns(cities, rmses)

outputs_cols <- c("city", "event", "orig", "pred")
outputs_1 <- data.frame(matrix(ncol = length(outputs_cols), nrow = 0))
outputs_2 <- data.frame(matrix(ncol = length(outputs_cols), nrow = 0))
max_12 <- length(cities) / 2;

#Iterating through all the cities.
for (i in 1:length(cities)) {
  
  city <- cities[i]
  sets <- sets0[[city]]
  
  #Calculating the required parameters and predictions for the given city.
  best_city <- unlist(cols_best[city])
  col <- cols[best_city]
  digit <- unlist(digits[col])
  values <- get_adapted_prediction(
    sets, col, digit, FALSE, FALSE)
  length <- length(values$pred)
  
  #Storing all the new information in a data frame in order to facilitate
  #its addition to the main output variables.
  new <- data.frame(rep(city, length), seq(length),
                    values$orig, values$pred)
  
  if (i > max_12) {
    outputs_2 <- rbind(outputs_2, new)
  }
  else {
    outputs_1 <- rbind(outputs_1, new)
  }
}
colnames(outputs_1) <- outputs_cols
colnames(outputs_2) <- outputs_cols

#Calculating some variables to be used in the report below.
rmse_1 <- get_rmse(outputs_1$pred, outputs_1$orig)
cor_1 <- round(cor(outputs_1$pred, outputs_1$orig), 3)
rmse_2 <- get_rmse(outputs_2$pred, outputs_2$orig)
cor_2 <- round(cor(outputs_2$pred, outputs_2$orig), 3)