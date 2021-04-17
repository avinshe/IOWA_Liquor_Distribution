#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/


library(plumber)

#* @apiTitle Decision Tree predict if product can be a top seller in Polk County.

#* @apiDescription This API is for the distributor to test if the product that has been launched and 
#* whose samples were being sold are capable of being a potential product of generating more revenue in Polk County

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}


list_of_packages <- c("tidyverse", "plumber", "caret")
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print("Installing packages\n")
  install.packages(new.packages())
}

#Load the saved model from the local machine
path <- "C:/Users/kinja/OneDrive/Desktop/All/2. SEM2/DPA/Descision_Tree/DecisionTree/DecisionTree_Plumber/decisiontree.rdata"
model1 <- load(path) 


#' Model adding Volume Sold, Number of bottles, Sales in Dollars per pack. 
#' predict the probability of an product being potential seller(y/n).
 
#' @param Bottle_vol:numeric Volume in litres.
#' @param State.Bottle.Cost:numeric Costo of bottles
#' @param Bottles.Sold:numeric Number of bottles per pack 
#' @post /model_prediction
#' @html
#' @response Return prediction
 
predict_potential_brand <- function(Bottle_vol, State.Bottle.Cost, Bottles.Sold){
  
  
  log_Bottle_vol <- log(as.numeric(Bottle_vol))
  log_State.Bottle.Cost <- log(as.numeric(State.Bottle.Cost))
  log_Bottles.Sold <- log(as.numeric(Bottles.Sold))
  
  input_data <- data.frame(log_Bottle_vol,
                           log_State.Bottle.Cost,
                           log_Bottles.Sold)
  
  op <- predict(get(model1), input_data, type = "class")
  op <- as.character(op[[1]])
  
} 
