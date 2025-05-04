# Install Requird Libraries and packages


install.packages("DescTools")
install.packages("reshape2")
install.packages("moments")
install.packages("dplyr")
install.packages("tidyr")
library(DescTools)
library(dplyr) # enables pipes %>% and more
library(tidyr) # for spliting on the period see below
library(moments)
library(reshape2)
library(psych)
install.packages("psych")


# Data Loading( Read the CSV File)

BD11 <- read.csv("C:/Users/sanja/OneDrive/Desktop/Nishu_R_Stats_Assignment/proportional_species_richness_NAs_removed.csv")


# Filter the data_set to include only the specified columns 

BD6_mygroup <- c("Bryophytes", "Isopods", "Bird", "Hoverflies", "Bees", "Grasshoppers_._Crickets")

# Calculate the mean value for each row across the specified BD6 taxonomic groups.
BD6_meanScore <- rowMeans(BD11[, BD6_mygroup])

install.packages("dplyr")
library(dplyr) # enables pipes %>% and more
# Extract the 6 specified taxonomic groups and relevant columns from BD11, then add the row means as a new column.
BD6 <- BD11 %>%  
  select("Location", all_of(BD6_mygroup), "Easting", "Northing", "dominantLandClass","ecologicalStatus", "period") %>% 
  mutate(BD6_eco_values = BD6_meanScore)

###Univariate analysis and basic R programming
install.packages("stringr")
library(stringr)

# Convert columns to factors and filter rows where 'dominantLandClass' contains 's'.
BD6 <- BD6 %>%
  mutate(
    dominantLandClass = as.factor(dominantLandClass),
    period = as.factor(period)
  ) %>%
  filter(str_detect(dominantLandClass, "s"))

# View the filtered data.
View(BD6)

# Function to calculate the 25% Winsorized mean.
winsorized_mean <- function(var) {
  x <- sort(var)
  n <- length(x)
  k <- floor(0.25 * n)
  x[1:k] <- x[k + 1]
  x[(n - k + 1):n] <- x[n - k]
  return(mean(x))
}

# Generate summary statistics and 25% Winsorized mean for BD6 taxonomic groups.
summary_table <- data.frame()

for (i in 2:7) {
  data <- BD6[, i]
  stats <- summary(data)
  summary_table <- rbind(summary_table, c(
    Taxonomic_Species_Group = names(BD6)[i],
    Min = round(stats[1], 4),
    `1st_Q` = round(stats[2], 4),
    Median = round(stats[3], 4),
    Mean = round(stats[4], 4),
    `3rd_Q` = round(stats[5], 4),
    Max = round(stats[6], 4),
    Win_Mean_25 = round(winsorized_mean(data), 4)
  ))
}

colnames(summary_table) <- c("Taxonomic_group", "Min", "1st_Q", "Median", "Mean", "3rd_Q", "Max", "Win_Mean_25")

View(summary_table)
knitr::kable(summary_table, caption = "Summary Statistics for Taxonomic Groups")

  ##2
# Calculate and display the correlation matrix for selected BD6 taxonomic groups.
tax_group_cor_values <- BD6 %>% select(2:7)
correlation_matrix <- cor(tax_group_cor_values, use = 'pairwise.complete.obs') %>%
  round(2)
View(correlation_matrix)
knitr::kable(correlation_matrix, caption = "Correlation Matrix of  in BD6 group")

  ##3
# Extract the "Birds" variable for analysis.
BD6_birds <- BD6$Bird

# Visualize bird distribution across periods.
par(mfrow = c(1, 2))  # Set up a two-panel plot layout.
boxplot(BD6_birds~ BD6$period, 
        main = "Birds Over Periods",
        ylab = "Bird Count",
        col = "skyblue")  # Boxplot for bird counts by period.

# Calculate key statistics: quartiles and interquartile range (IQR).
Q1 <- quantile(BD6_birds, 0.25)  # First quartile (25th percentile).
Q3 <- quantile(BD6_birds, 0.75)  # Third quartile (75th percentile).
IQR <- Q3 - Q1  # Interquartile range.

# Determine outlier thresholds.
lower_bound <- Q1 - 2 * IQR  # Lower threshold
upper_bound <- Q3 + 2 * IQR  # Upper threshold

# Identify outliers based on thresholds.
outliers <- BD6_birds[BD6_birds < lower_bound | BD6_birds > upper_bound]
outliers_new <- round(outliers, 5)
formatted_outliers <- paste(outliers_new, collapse = ", ")
wrapped_outliers <- strwrap(formatted_outliers, width = 80)
cat("Outliers:\n", paste(wrapped_outliers, collapse = "\n"), "\n")

# Visualize overall bird distribution and highlight outliers.
boxplot(BD6_birds, 
        main = "Bird Count Distribution",
        ylab = "Bird Count",
        col = "lightgreen",
        outline = TRUE)

# Add lines for the outlier thresholds on the boxplot.
abline(h = c(lower_bound, upper_bound), col = "darkorange", lty = 2)


###HYPOTHESIS TESTS

##the Kolmogorov-Smirnov test
# Compare distributions of BD11_eco_status and BD6_eco_status.

# Extract relevant columns for comparison.
BD11_eco_status <- BD6 %>% pull(ecologicalStatus)
BD6_eco_status <- BD6 %>% pull(BD6_eco_values)

# Visualize distributions using boxplots and QQ plot.
par(mfrow = c(2, 2))
boxplot(BD11_eco_status, main = "BD11: Box Plot of Ecological Status")
boxplot(BD6_eco_status, main = "BD6: Box Plot of BD6_eco_status")
qqplot(BD11_eco_status, BD6_eco_status, main = "QQ Plot: BD11 vs BD6")
abline(0, 1, col = 'green')

# Plot CDFs of the two samples.
plot(ecdf(BD11_eco_status), col = 'blue', main = "CDF: BD11 vs BD6",
     xlab = "Value", ylab = "Fn(x)")
lines(ecdf(BD6_eco_status), col = 'red')

# Perform Kolmogorov-Smirnov test.
Test_1_Result <- ks.test(BD11_eco_status, BD6_eco_status)
print(Test_1_Result)

#t-test
# Compare the mean of BD6_eco_change and BD11_eco_change using a t-test.
install.packages("tidyr")
library(tidyr)

# Prepare data for hypothesis testing by calculating differences.
BD6_wide <- BD6 %>% 
  select(Location, period, BD6_eco_values) %>% 
  pivot_wider(names_from = period, values_from = BD6_eco_values) %>% 
  mutate(BD6_eco_change = Y00 - Y70)

BD11_wide <- BD6 %>% 
  select(Location, period, ecologicalStatus) %>% 
  pivot_wider(names_from = period, values_from = ecologicalStatus) %>% 
  mutate(BD11_eco_change = Y00 - Y70)

# Extract differences for hypothesis testing.
BD6_eco_change <- BD6_wide %>% pull(BD6_eco_change)
BD11_eco_change <- BD11_wide %>% pull(BD11_eco_change)
BD11_mean <- mean(BD11_eco_change)

# Visualize differences using boxplots.
par(mfrow = c(1, 2))
hist(BD11_eco_change, main = "BD11_eco_change")
hist(BD6_eco_change, main = "BD6_eco_change")

# Perform a one-sample t-test.
Test_2_Result <- t.test(BD6_eco_change, mu = BD11_mean, conf.level = 0.92)
print(Test_2_Result)


###Contingency table/comparing categorical variables

#Create a contingency table to compare ecological changes between BD11 and BD6.

# Extract and merge relevant data by location.
eco_change_BD6 <- BD6_wide %>% select(Location, BD6_eco_change)
eco_change_BD11 <- BD11_wide %>% select(Location, BD11_eco_change)
both_eco_change <- inner_join(eco_change_BD6, eco_change_BD11, by = "Location")

# Categorize ecological changes as 'UP' or 'DOWN'.
both_eco_change <- both_eco_change %>%
  mutate(
    BD6_up = ifelse(BD6_eco_change > 0, "UP", "DOWN"),
    BD11_up = ifelse(BD11_eco_change > 0, "UP", "DOWN")
  )

# Create a contingency table and add margins.
contingency_table <- table(both_eco_change$BD6_up, both_eco_change$BD11_up)
print(contingency_table)
grand_total <- addmargins(contingency_table)
print(grand_total)

# Calculate the independent model table.
independent_model_table <- round(outer(rowSums(contingency_table), colSums(contingency_table)) / sum(contingency_table))
independent_model_table <- as.table(independent_model_table)
print(independent_model_table)

# Add margins to the independent table.
independent_g_total_table <- addmargins(independent_model_table)
print(independent_g_total_table)

# Display summary of the original contingency table.
summary(contingency_table)


#Likelihood-ratio test at 93% Confidence Interval.
install.packages("DescTools")
library(DescTools)
# Perform the G-test to analyze the contingency table

likelihood_test <- GTest(contingency_table)
print(likelihood_test)

# Calculate the count of "UP" and total events for BD6 and BD11.
up_count_BD6 <- sum(contingency_table["UP", ])
total_count_BD6 <- sum(contingency_table)
up_count_BD11 <- sum(contingency_table[, "UP"])
total_count_BD11 <- sum(contingency_table)

# Compute the proportion of "UP" events for BD6 and BD11.
BD6_up_fraction <- up_count_BD6 / total_count_BD6
BD11_up_fraction <- up_count_BD11 / total_count_BD11

# Perform a proportion test to compare "UP" proportions between BD6 and BD11.
prop_comparison_test <- prop.test(
  x = c(up_count_BD6, up_count_BD11),
  n = c(total_count_BD6, total_count_BD11),
  conf.level = 0.93
)

# Display the calculated proportions and test results.
cat("Proportion of 'UP' events in BD6:", BD6_up_fraction, "\n")
cat("Proportion of 'UP' events in BD11:", BD11_up_fraction, "\n")
print(prop_comparison_test)


#Estimate odds-ratio, sensitivity, specificity, and Youden’s index.
print(contingency_table)
str(contingency_table)
GTest(contingency_table)

#Calculate odds ratio manually.
odds_ratio_manual <- (contingency_table[1, 1] / contingency_table[1, 2]) / 
  (contingency_table[2, 1] / contingency_table[2, 2])
print(odds_ratio_manual)

#Calculate odds ratio using a built-in function
odds_ratio_function <- OddsRatio(contingency_table)
print(odds_ratio_function)

# Calculate sensitivity, specificity, and Youden's index.
sensitivity <- contingency_table[2, 2] / colSums(contingency_table)[2]
specificity <- contingency_table[1, 1] / colSums(contingency_table)[1]
youden_index <- sensitivity + specificity - 1

# Display results.
print(sensitivity)
print(specificity)
print(youden_index)


###SIMPLE LINEAR REGRESSION

install.packages("dplyr")
library(dplyr) # enables pipes %>% and more
# Filter BD11 dataset for rows where 'dominantLandClass' contains 's' (e.g., data for Scotland).
BD1 <- BD11 %>% filter(grepl('s', dominantLandClass))

# Perform linear regression with 'Ladybird' as the dependent variable and 'eco_status_6' as the independent variable.
lin_reg <- lm(BD1$Ladybirds ~ BD6$BD6_eco_values)
summary(lin_reg)

# Calculate correlation between fitted and observed values.
correlation <- cor(lin_reg$fitted.values, lin_reg$model[[1]])
cat("Correlation between fitted and observed values:", correlation, "\n")

# Combine 'BD6_eco_status' and 'Ladybird' into a data frame for plotting.
ladybird_data <- data.frame(BD6_eco_values = BD6$BD6_eco_values, Ladybird = BD1$Ladybird)

# Scatter plot with regression line using ggplot2.
library(ggplot2)
ggplot(ladybird_data, aes(x = BD6_eco_values, y = Ladybird)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "pink", linetype = "solid") +
  labs(title = "BD6 Eco Values vs Ladybird", x = "BD6 Eco Status", y = "Ladybird")

# Diagnostic plots for regression analysis.
par(mfrow = c(1, 3))

# Scatter plot with regression and 1:1 lines.
plot(BD6$BD6_eco_values, BD1$Ladybird, xlab = "BD6 Eco Values", ylab = "Ladybird",
     main = "BD6 Eco Values vs Ladybird", col = "blue", pch = 19)
abline(0, 1, col = "red", lty = 2)  # 1:1 line
abline(lin_reg, col = "darkgreen", lwd = 2)  # Regression line

# Residuals vs Fitted plot.
plot(lin_reg$fitted.values, lin_reg$residuals, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted", col = "purple", pch = 19)
abline(h = 0, col = "red", lwd = 2)

# Q-Q plot for residuals.
qqnorm(lin_reg$residuals, main = "Q-Q Plot of Residuals", col = "blue", pch = 19)
qqline(lin_reg$residuals, col = "red", lwd = 2)


###MULTIPLE LINEAR REGRESSION

##1)Estimating the AIC for the initial MLR model
my_mlr_model <- lm(BD1$Ladybirds ~ ., data = BD6[c("Bryophytes", "Isopods", "Bird", "Hoverflies", "Bees", "Grasshoppers_._Crickets")], y=T)
cat("My MLR Model Summary:\n")
print(summary(my_mlr_model))
AIC(my_mlr_model)
cat("AIC (My MLR Model Summary):", AIC(my_mlr_model), "\n")
cor(my_mlr_model$fitted.values, BD1$Ladybirds)
cat("Correlation (My MLR Model Summary):", cor(my_mlr_model$fitted.values, BD1$Ladybirds), "\n")

##2)feature selection based on p-values and AIC from the regression.
my_reduced_mlr_model <- lm(BD1$Ladybirds ~ ., data = BD6[c("Bryophytes", "Isopods", "Bird", "Hoverflies", "Bees", "Grasshoppers_._Crickets")])

cat("\nMy Reduced MLR Model Summary:\n")
print(summary(my_reduced_mlr_model))

cat("\nMy Reduced MLR Model AIC Summary:\n")
AIC(my_reduced_mlr_model)

cat("\nMy Reduced MLR Model Correlation Summary:\n")
cor(my_reduced_mlr_model$fitted.values, BD1$Ladybirds)

##3)interaction between predictor variables in the BD6 group. Select a linear regression model that has a low AIC as possible

#Interaction model with predictor variables in the BD6 group
my_mlr_inter_model <- lm(BD1$Ladybirds ~., data = BD6[c("Bryophytes", "Isopods", "Bird", "Hoverflies", "Bees", "Grasshoppers_._Crickets")])

cat("\nMy MLR Interaction Model Summary:\n")
print(summary(my_mlr_inter_model))

cat("\nMy MLR Interaction Model AIC Summary:\n")
AIC(my_mlr_inter_model)

cat("\nMy MLR Interaction Model Correlation Summary:\n")
cor(my_mlr_inter_model$fitted.values, BD1$Ladybirds)

#Comparing my MLR AIC models
AIC_values <- c(
  My_MLR_Model = AIC(my_mlr_model),
  My_Reduced_MLR_Model = AIC(my_reduced_mlr_model),
  MY_MLR_Inter_Model = AIC(my_mlr_inter_model)
)
cat("\nMy MLR AIC Model Comparison:\n")
print(AIC_values)

#Plotting Fitted vs Ladybirds my MLR model
par(mfrow=c(1,3))
plot(BD1$Ladybirds ~ my_mlr_inter_model$fitted.values, xlab = "Fitted Values", ylab = "Ladybirds",
     main = "Fitted Interaction Model VS Ladybirds Interaction Model")
abline(0,1,col="green")

#Plotting Fitted vs Residuals My MLR model
plot(my_mlr_model$fitted.values, my_mlr_model$residuals,xlab = "Fitted Values", ylab = "Residuals",
     main = "My MLR Model Fitted vs My MLR Model Fitted")
abline(abline(h = 0, col = "yellow"))

#Plotting QQ Plot for my MLR model
qqnorm(my_mlr_model$residuals, main = "Q-Q Plot of residuals(My MLR Model)", col = "red", pch = 19)
qqline(my_mlr_model$residuals, col="blue", lwd = 2)

#Identifying the best MLR model
cat("\nBest MLR Model based on AIC:\n")
my_best_mlr_model <- ifelse(which.min(AIC_values)==1, "My MLR Model",
                     ifelse(which.min(AIC_values)==2, "My Reduced MLR Model", "My MLR Interaction Model"))
cat("\ The Best MLR Model is:", my_best_mlr_model, "\n")
