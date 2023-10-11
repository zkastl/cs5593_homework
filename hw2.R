library(C50)
library(rpart)

data_file <- 
  "C:/Users/zkast/source/repos/cs5593_homework/Wholesale customers data.csv"

data <- read.table(data_file, sep = ",", header = TRUE, na.strings = "?")
data2 <- data[, c("Milk", "Fresh", "Delicassen")]

# a
b <- boxplot(data2,
              main="Raw Boxplots for Attributes: Milk, Fresh, Delicassen",
              at=c(1,2,3),
              names=c("Milk", "Fresh", "Delicassen"),
              las=3, col=c("red", "blue", "green"),
              notch=TRUE, horizontal=TRUE)

# b
show(b$out)

quartiles_milk <- quantile(data$Milk, probs=c(.25,.75))
milk_iqr <- IQR(data$Milk)
milk_lower <- quartiles_milk[1] - 1.5*milk_iqr
milk_upper <- quartiles_milk[2] + 1.5*milk_iqr
milk_no_outlier <- subset(data$Milk,
                          data$Milk > milk_lower & data$Milk < milk_upper)

quartiles_fresh <- quantile(data$Fresh, probs=c(.25,.75))
fresh_iqr <- IQR(data$Fresh)
fresh_lower <- quartiles_fresh[1] - 1.5*fresh_iqr
fresh_upper <- quartiles_fresh[2] + 1.5*fresh_iqr
fresh_no_outlier <- subset(data$Fresh,
                          data$Fresh > fresh_lower & data$Fresh < fresh_upper)

quartiles_deli <- quantile(data$Delicassen, probs=c(.25,.75))
deli_iqr <- IQR(data$Delicassen)
deli_lower <- quartiles_deli[1] - 1.5*deli_iqr
deli_upper <- quartiles_deli[2] + 1.5*deli_iqr
deli_no_outlier <- 
  subset(data$Delicassen, data$Delicassen > deli_lower & data$Delicassen < deli_upper)

data3 <- data[which((data$Milk > milk_lower & data$Milk < milk_upper) &
            (data$Delicassen > deli_lower & data$Delicassen < deli_upper) &
              (data$Fresh > fresh_lower & data$Fresh < fresh_upper)),]


# c
b2 <- boxplot(data3[,c("Milk", "Fresh", "Delicassen")],
             main="Values for Milk, Fresh, Delicassen (Outliers Removed)",
             at=c(1,2,3),
             names=c("Milk", "Fresh", "Delicassen"),
             las=3, col=c("red", "blue", "green"),
             notch=TRUE, horizontal=TRUE)

# d
vars <- c("Milk", "Fresh", "Delicassen")
str(data3[, c(vars, "Region")])
set.seed(1111)
in_train <- sample(1:nrow(data3), size=266)
train_data <- data3[in_train,]
test_data <- data3[-in_train,]
train_data$Region <- as.factor(train_data$Region)
tree_mod <- C5.0(x=train_data[,vars], y=train_data$Region)
summary(tree_mod)