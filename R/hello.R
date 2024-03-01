# Define your function
printSomething <- function() {
  # Your function logic here
  message <- paste("Hello, World!", Sys.time())
  return(message)  # For example
}

show_dataframe <- function(df) {
  head(df)
}