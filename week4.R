# This script builds the managers dataset
# and populates it with data

# Load data from previous session
column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")

# Enter data into vectors before constructing the data frame
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99) # 99 is one of the values in the age attribute - will require recoding
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

# Construct a data frame using the data from all vectors above
managers <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)

# Add column names to data frame using column_names vector
colnames(managers) <- column_names

# Recode the incorrect 'age' data to NA
managers$Age[managers$Age == 99] <- NA

# Create a new attribute called AgeCat and set valuess
# in AgeCat to the following if true:
# <= 25 = Young
# >= 26 & <= 44 = Middle Aged
# >= 45 = Elderly
# We will also recode age 'NA' to Elder

managers$AgeCat[managers$Age >= 45] <- "Elder"
managers$AgeCat[managers$Age >= 26 & managers$Age <= 44] <- "Middle Aged"
managers$AgeCat[managers$Age <= 25] <- "Young"
managers$AgeCat[is.na(managers$Age)] <- "Elder"

# Recode AgeCat so that is ordinal and factored with the
# order Young, Middle aged, Elder
# We'll srore the ordinal factored data in variable 'AgeCat'
AgeCat <- factor(managers$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))

# Replace managers's AgeCat attribute with newly ordinal foctored data
managers$AgeCat <- AgeCat

# Create a new column called 'summary_col' that
# contains a summary of each row
summary_col <- managers$Q1 + managers$Q2 + managers$Q3 + managers$Q4 + managers$Q5
summary_col

# Add summary_col to the end of the data frame
managers <- data.frame(managers, summary_col)


# Calculate mean value for each row
mean_value <- rowMeans(managers[5:9])

# Add mean_value to end of managers data frame
managers <- data.frame(managers, mean_value)

# Show data frame contents
managers

# Change the name of this column to "mean value"
names(managers)[12] <- "mean value"

# Change name of summary_col to "Answer total"
names(managers)[11] <- "Answer total"

# Show 
str(managers)

new_managers_data <- read.csv("MoreData(1).csv")
str(new_managers_data)

names(new_managers_data)
include_list <- new_managers_data[c(6, 3, 7, 4, 8:12)]
include_list

rbind(managers, include_list)

blank_vectors <- c("Agecat", "Answer total", "mean value")
include_list[, blank_vectors] <- NA
include_list

attach(include_list)
include_list$AgeCat[Age >= 45] <- "Elder"
include_list$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
include_list$AgeCat[Age <= 25] <- "Young"
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)
include_list

attach(include_list)
include_list$`Answer total` <- Q1 + Q2 + Q3 + Q4 + Q5
#find mean values
include_list$`mean value` <- rowMeans(include_list[5:9])
#Default dateformat in R is yyyy-mm-dd
#Define position of the date contents first
#in the imported data frame eg using %d,%m etc
#R then puts the date field into yyyy-mm-dd
#format when using the date type

restructed.date <- as.Date(include_list$Date, "%m%d%Y")
restructured_date
include_list$Date <- restructed.date

#Restructure the managers date field
managers_restructured_date <- as.Date(managers$Date, "%Y-%d-%m")
managers_restructured_date

#Assign restructured date to the managers data frame
managers$Date <- managers_restructured_date

str(managers)
str(include_list)
#combine both datasets with the rbind() function
new_managers_dataframe <- rbind(managers, include_list)
str(new_managers_dataframe)
head(new_managers_dataframe, 10)

#Set AgeCat with ordered factors
new_managers_dataframe$AgeCat <- factor(new_managers_dataframe$Agecat, levels =c("Young", "Middle Aged", "Elder"),ordered = TRUE)

