testme <- 1

test <- function(testme){
  
  bg_test <- callr::r_bg(test_bg, args = list(testme), stderr = stringr::str_c("eraseme.txt"), supervise = TRUE)
  bg_test$wait()
  error_list = readLines(stringr::str_c('eraseme.txt'))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
  
  return(bg_test$get_result())
}




#----------------------------------------------------------------------------------------
test_bg <- function(testme){
  
  test2 <- function(){
    testme <- testme + 2
    return(testme)
  }
  
  return(test2())
}

result = test(testme)

