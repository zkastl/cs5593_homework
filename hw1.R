library(base)

# Find and fix missing data values within a grid
# For now, assume that data_grid[,X] and column_types [X] are the same length
modified_grid <- function(data_grid, column_types) {
    modified <- data_grid
    n_cols <- ncol(modified)
    for (i in (1:(n_cols - 1))) {
        if (column_types[i] == "numeric") {
            mean_value <- mean(modified[, i], na.rm = TRUE)
            for(j in 1:nrow(modified)) {
              if (is.na(modified[j,i])) {
                modified[j,i] <- mean_value
              }
            }
        } else
        if (column_types[i] == "character") {
            mode_value <- getmode(data_grid[, i][which(!is.na(modified[, i]))])
            for(j in 1:nrow(modified)) {
              if (is.na(modified[j,i])) {
                modified[j,i] <- mode_value
              }
            }
        }
    }
    
    return(modified)
}

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_file <- 
  "C:/Users/zkast/source/repos/fall2023homework/CS5593/HW1/data/crx.data"
col_types <- c("character", "numeric", "numeric", "character", "character",
               "character", "character", "numeric", "character", "character",
               "numeric", "character", "character", "numeric", "numeric",
               "character")

data <- read.table(data_file, sep = ",", header = FALSE,
                   colClasses = col_types, na.strings = "?")

# Call the modified grid method to replace missing data with mean/mode values
data2 <- modified_grid(data, col_types)

# Pick with replacement sample rows from the data set
sample_rows <- sample(1:nrow(data2), size=100, replace=TRUE)

# set the columns to filter
columns_to_keep <- c(2, 3, 8, 11, 14, 15)

# filter the matrix by column
filtered_matrix <- data2[sample_rows, columns_to_keep]

# Create a matrix of scatter plots using pairs()
pairs(filtered_matrix)

# Customize the axis labels
par(xaxt = "n")  # Remove the x-axis labels
par(yaxt = "n")  # Remove the y-axis labels
for (i in 1:nrow(filtered_matrix)) {
  mtext(text = colnames(filtered_matrix)[i], side = 1, line = 2, at = 0.5)
  mtext(text = colnames(filtered_matrix)[i], side = 2, line = 2, at = 0.5)
}

print(cor(filtered_matrix))