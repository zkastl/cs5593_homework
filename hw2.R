data_file <- 
  "C:/Users/zkast/source/repos/cs5593_homework/Wholesale customers data.csv"

data <- read.table(data_file, sep = ",", header = TRUE, na.strings = "?")
Wholesale.customers.data <- 
  read.csv("C:/Users/zkast/source/repos/cs5593_homework/Wholesale customers data.csv")
data2 <- data[, c("Milk", "Fresh", "Delicassen")]

#milk_norm <- rnorm(220, mean=mean(data$Milk, na.rm=TRUE), sd=sd(data$Milk, na.rm = TRUE))
#fresh_norm <- rnorm(220, mean=mean(data$Fresh), sd=sd(data$Fresh))
#deli_norm <- rnorm(220, mean=mean(data$Delicassen), sd=sd(data$Delicassen))

#b <- boxplot(milk_norm, fresh_norm, deli_norm,main="Normalized Values for Attributes",at=c(1,2,3),names=c("Milk", "Fresh", "Delicassen"),las=2, col=c("red", "blue", "green"),notch=TRUE, horizontal=TRUE)

b <- boxplot(data$Milk, data$Fresh, data$Delicassen,
              main="Normalized Values for Attributes",
              at=c(1,2,3),
              names=c("Milk", "Fresh", "Delicassen"),
              las=3, col=c("red", "blue", "green"),
              notch=TRUE, horizontal=TRUE)

quartiles_milk <- quantile(data$Milk, probs=c(.25,.75), na.rm=FALSE)
milk_iqr <- IQR(data$Milk)
milk_lower <- quartiles_milk[1] - 1.5*milk_iqr
milk_upper <- quartiles_milk[2] + 1.5*milk_iqr
milk_no_outlier <- subset(data$Milk,
                          data$Milk > milk_lower & data$Milk < milk_upper)

quartiles_fresh <- quantile(data$Fresh, probs=c(.25,.75), na.rm=FALSE)
fresh_iqr <- IQR(data$Fresh)
fresh_lower <- quartiles_fresh[1] - 1.5*fresh_iqr
fresh_upper <- quartiles_fresh[2] + 1.5*fresh_iqr
fresh_no_outlier <- subset(data$Fresh,
                          data$Fresh > fresh_lower & data$Fresh < fresh_upper)

quartiles_deli <- quantile(data$Delicassen, probs=c(.25,.75), na.rm=FALSE)
deli_iqr <- IQR(data$Delicassen)
deli_lower <- quartiles_deli[1] - 1.5*deli_iqr
deli_upper <- quartiles_deli[2] + 1.5*deli_iqr
deli_no_outlier <- 
  subset(data$Delicassen, data$Delicassen > deli_lower & data$Delicassen < deli_upper)


b2 <- boxplot(milk_no_outlier, fresh_no_outlier, deli_no_outlier,
             main="Normalized Values for Attributes",
             at=c(1,2,3),
             names=c("Milk", "Fresh", "Delicassen"),
             las=3, col=c("red", "blue", "green"),
             notch=TRUE, horizontal=TRUE)