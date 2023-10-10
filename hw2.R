min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_file <- 
  "C:/Users/zkast/source/repos/cs5593_homework/Wholesale customers data.csv"

data <- read.table(data_file, sep = ",", header = TRUE, na.strings = "?")
Wholesale.customers.data <- 
  read.csv("C:/Users/zkast/source/repos/cs5593_homework/Wholesale customers data.csv")
data2 <- data[, c("Milk", "Fresh", "Delicassen")]

milk_norm <- rnorm(220, mean=mean(data$Milk, na.rm=TRUE), sd=sd(data$Milk, na.rm = TRUE))
fresh_norm <- rnorm(220, mean=mean(data$Fresh), sd=sd(data$Fresh))
deli_norm <- rnorm(220, mean=mean(data$Delicassen), sd=sd(data$Delicassen))

b <- boxplot(data$Milk, data$Fresh, data$Delicassen,
        main="Normalized Values for Attributes",
        at=c(1,2,3),
        names=c("Milk", "Fresh", "Delicassen"),
        las=3, col=c("red", "blue", "green"),
        notch=TRUE, horizontal=TRUE)

#m <- boxplot(min_max_norm(data$Milk), names=c("Milk"), las=1, col=c("red"), notch=TRUE)