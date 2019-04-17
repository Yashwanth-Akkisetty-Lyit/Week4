add_percent <- function(x)
{
  percent <- round(x*100, digits=1)
  result <- paste(percent, "%", sep="")
  #Return the calculation.Note this is optional
  #since R returns the value of the last line of code
  return(result)
}
sample_vector <- c(0.458, 1.6653, 0.83112)
updated_vector <- add_percent(sample_vector)
updated_vector

mystats <- function(values, parametric = TRUE, allow_print = FALSE){
  if(parametric) {
    central_tendency <- mean(values)
    spread <- sd(values)
  }
  else
  {
    central_tendency <- median(values)
    spread <- mad(values)
  }
  if(allow_print & parametric) {
    cat("Mean =", central_tendency, "\n", "SD=")
    
  }
}