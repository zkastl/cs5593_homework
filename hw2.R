data_file <- 
  "C:/Users/zkast/Downloads/Wholesale customers data.csv"

data <- read.table(data_file, sep = ",", header = TRUE, na.strings = "?")
Wholesale.customers.data <- 
  read.csv("C:/Users/zkast/Downloads/Wholesale customers data.csv")
data2 <- data[, c("Milk", "Fresh", "Delicassen")]

create_boxplot <- function(d) {
  boxplot(d)
}

create_boxplot(data2)