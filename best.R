best <- function (state,outcome){
  #read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  filted_data <- as.data.frame(cbind(data[, 2],   # hospital
                            data[, 7],   # state
                            data[, 11],  # heart attack
                            data[, 17],  # heart failure
                            data[, 23]), # pneumonia
                            stringsAsFactors = FALSE)
  #set column's header
  colnames(filted_data) <- c("Hospital","State","heart attack","heart failure","pneumonia")
  
  
  #check that state and outcome are valid
  if(!state %in% filted_data[, "State"]){
    stop('invalid state')
  }
  
  else if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop('invalid outcome')
  }
  
  else{
    #rank based on heart attack
    wanted_state <- which(filted_data[, "State"] == state)
    wanted_data <- filted_data[wanted_state, ]
    wanted_data[, outcome] <- as.numeric(wanted_data[, outcome])
    
    #rate
    ranked_hosp <- order(wanted_data[, outcome], na.last=NA, decreasing = FALSE)
    #deal with tie
    if(wanted_data[ranked_hosp[1], outcome] == wanted_data[ranked_hosp[2], outcome]){
      tie <- wanted_data[ranked_hosp[1:2],]     
      tie_index <- order(tie[, "Hospital"], decreasing = FALSE)
      result <- as.character(tie[tie_index[1],1])
    }
    #rank as normal if no tie
    else{
      result <- as.character(wanted_data[ranked_hosp[1],1])
    }
    
  }
  
  #return hospital name in that state with lowest 30-day death
  return(result)
}
