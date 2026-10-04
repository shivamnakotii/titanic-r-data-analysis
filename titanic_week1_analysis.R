# ============================================================
# Week 1 - Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic passenger data (891 rows, 12 columns)
# Author: Shivam Nakoti
# ============================================================

# 1. Packages
packages <- c("dplyr", "ggplot2", "tidyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# 2. Load publicly available data
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic <- read.csv(url, stringsAsFactors = FALSE)

# 3. Initial inspection
dim(titanic)
names(titanic)
str(titanic)
summary(titanic)
head(titanic)

# 4. Missing-value audit
missing_summary <- data.frame(
  Column = names(titanic),
  Missing = sapply(titanic, function(x) sum(is.na(x))),
  Missing_Percent = round(sapply(titanic, function(x) mean(is.na(x)) * 100), 2)
)
print(missing_summary)

# 5. Duplicate check
sum(duplicated(titanic))

# 6. Data cleaning
cleaned <- titanic

# Age: median imputation (robust to extreme values)
cleaned$Age[is.na(cleaned$Age)] <- median(cleaned$Age, na.rm = TRUE)

# Embarked: mode imputation
mode_embarked <- names(sort(table(cleaned$Embarked), decreasing = TRUE))[1]
cleaned$Embarked[is.na(cleaned$Embarked)] <- mode_embarked

# Cabin is too sparse for direct imputation. Preserve useful information
# through a binary indicator and remove the sparse raw text column.
cleaned$CabinKnown <- ifelse(is.na(cleaned$Cabin), "No", "Yes")
cleaned$Cabin <- NULL

# Convert categorical fields to factors
cleaned$Sex <- factor(cleaned$Sex)
cleaned$Embarked <- factor(cleaned$Embarked)
cleaned$Pclass <- factor(cleaned$Pclass,
                         levels = c(1,2,3),
                         labels = c("1st", "2nd", "3rd"))
cleaned$CabinKnown <- factor(cleaned$CabinKnown)

# 7. Outlier detection using IQR
fare_q1 <- quantile(cleaned$Fare, 0.25)
fare_q3 <- quantile(cleaned$Fare, 0.75)
fare_iqr <- IQR(cleaned$Fare)
fare_upper <- fare_q3 + 1.5 * fare_iqr
cleaned$Fare_Outlier <- cleaned$Fare > fare_upper

age_q1 <- quantile(cleaned$Age, 0.25)
age_q3 <- quantile(cleaned$Age, 0.75)
age_iqr <- IQR(cleaned$Age)
age_lower <- age_q1 - 1.5 * age_iqr
age_upper <- age_q3 + 1.5 * age_iqr
cleaned$Age_Outlier <- cleaned$Age < age_lower | cleaned$Age > age_upper

cat("Fare upper IQR limit:", fare_upper, "\n")
cat("Potential Fare outliers:", sum(cleaned$Fare_Outlier), "\n")
cat("Potential Age outliers:", sum(cleaned$Age_Outlier), "\n")

# 8. Normalization (min-max scaling)
minmax <- function(x) (x - min(x, na.rm=TRUE)) /
                       (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
cleaned$Age_Normalized <- minmax(cleaned$Age)
cleaned$Fare_Normalized <- minmax(cleaned$Fare)

# 9. Encoding categorical variables (one-hot encoding)
encoded <- model.matrix(~ Sex + Embarked + Pclass + CabinKnown - 1,
                        data = cleaned)
encoded <- as.data.frame(encoded)
head(encoded)

# 10. Descriptive statistics
numeric_summary <- cleaned %>%
  summarise(
    Passengers = n(),
    Survival_Rate = mean(Survived) * 100,
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    Mean_Fare = mean(Fare),
    Median_Fare = median(Fare)
  )
print(numeric_summary)

# 11. Survival analysis
survival_by_sex <- cleaned %>%
  group_by(Sex) %>%
  summarise(
    Passengers = n(),
    Survivors = sum(Survived),
    Survival_Rate = mean(Survived) * 100
  )
print(survival_by_sex)

survival_by_class <- cleaned %>%
  group_by(Pclass) %>%
  summarise(
    Passengers = n(),
    Survivors = sum(Survived),
    Survival_Rate = mean(Survived) * 100
  )
print(survival_by_class)

survival_by_port <- cleaned %>%
  group_by(Embarked) %>%
  summarise(
    Passengers = n(),
    Survivors = sum(Survived),
    Survival_Rate = mean(Survived) * 100
  )
print(survival_by_port)

# 12. Correlation analysis
correlation_data <- cleaned %>%
  select(Survived, Age, SibSp, Parch, Fare)
print(cor(correlation_data, use = "complete.obs"))

# 13. Visualizations
ggplot(cleaned, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Sex",
       x = "Sex", y = "Proportion", fill = "Survived")

ggplot(cleaned, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Passenger Class",
       x = "Passenger Class", y = "Proportion", fill = "Survived")

ggplot(cleaned, aes(x = Fare)) +
  geom_histogram(bins = 30) +
  labs(title = "Fare Distribution", x = "Fare", y = "Count")

# 14. Export cleaned data
write.csv(cleaned, "titanic_cleaned.csv", row.names = FALSE)
write.csv(encoded, "titanic_encoded.csv", row.names = FALSE)
