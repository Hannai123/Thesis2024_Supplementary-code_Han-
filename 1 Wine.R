rm(list = ls())
setwd("/Users/littlelai/Library/CloudStorage/OneDrive-WageningenUniversity&Research/6_Master Thesis/5 DataSet/1 Firm Data_")
getwd()

'
install.packages("tidyverse")
install.packages("tidygeocoder")
install.packages("janitor")
install.packages("readxl")
install.packages("readr")
install.packages("ggmap")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("data.table")
install.packages("reshape2")
install.packages("Hmisc")
install.packages("lubridate")
install.packages("plyr")
install.packages("esmisc")
install.packages("sandwich")
install.packages("MASS")
install.packages("e1071")
install.packages("terra")
install.packages("raster")
install.packages("sf")
install.packages("eurostat")
install.packages("remotes")
install.packages("ggspatial")
install.packages("prettymapr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("weathermetrics")
install.packages("ncdf4")
install.packages("chron")
install.packages("RColorBrewer")
install.packages("lattice")
install.packages("viridis")
install.packages("R.utils")
install.packages("usethis")
install.packages("fixest")
install.packages("patchwork")
install.packages("modelsummary")
install.packages("stargazer")
install.packages("purrr") 
install.packages("DescTools")
'

library(tidygeocoder)
library(reshape2)
library(readxl)
library(readr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(grid)
library(gridExtra)
library(raster)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(terra)
library(raster)
library(janitor)
library(sf)
library(eurostat)
library(remotes)
library(ggspatial)
library(prettymapr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridisLite)
library(weathermetrics)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(viridis)
library(readxl)
library(fixest)
library(marginaleffects)
library(patchwork)
library (modelsummary)
library(stargazer)
library(broom)
library(purrr)
library(tibble)
library(ggrepel)
library(patchwork)
library(DescTools)
library(ggpubr)
#==========Read the data file======================
df_Panel <- read_csv("df_Panel.csv")
str(df_Panel)

count.firm <-count(df_Panel$Company)

summary(count.firm$freq)
sd(count.firm$freq)
#=======================================
# Bar chart for Assets Classification
#=======================================
# Handle possible conversion issues (e.g., non-numeric characters in the column)
df_Panel$Total.assets <- as.numeric(gsub(",", "", df_Panel$Total.assets)) # Remove commas if present

# Apply the cut function with the correct breaks
df_Panel$AssetClass <- cut(
  df_Panel$Total.assets,
  breaks = c(-Inf, 2000, 10000, 43000, Inf),
  labels = c("Micro (< EUR 2 million)", "Small (2-10 million)", "Medium (10-43 million)", "Large (Above 43 million)"),
  right = FALSE  # This makes the intervals closed on the left and open on the right
)

# Count the number of unique firms in each AssetClass
firm_counts <- df_Panel %>%
  group_by(AssetClass) %>%
  summarise(Count = n_distinct(Company))

# Create the bar chart
ggplot(firm_counts, aes(x = AssetClass, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Asset Class") +
  ylab("Number of Firms") +
  ggtitle("Number of Companies by Asset Class")

#=========Robutness Check================
Tmax <- read_csv("Tmax_firm_output(3-11).csv")

#Set up the new threshold for the Tmax

#Delete the duplicate row 
Tmax <- unique(Tmax)

#Set 2103 to the data frame indicates the highest temperature in this year
Tmax$`2013` <- apply(Tmax[, 4:249], 1, max, na.rm = TRUE)
Tmax$`2014` <- apply(Tmax[, 250:495], 1, max, na.rm = TRUE)
Tmax$`2015` <- apply(Tmax[, 496:741], 1, max, na.rm = TRUE)
Tmax$`2016` <- apply(Tmax[, 742:987], 1, max, na.rm = TRUE)
Tmax$`2017` <- apply(Tmax[, 988:1233], 1, max, na.rm = TRUE)
Tmax$`2018` <- apply(Tmax[, 1234:1479], 1, max, na.rm = TRUE)
Tmax$`2019` <- apply(Tmax[, 1480:1725], 1, max, na.rm = TRUE)
Tmax$`2020` <- apply(Tmax[, 1726:1971], 1, max, na.rm = TRUE)
Tmax$`2021` <- apply(Tmax[, 1972:2217], 1, max, na.rm = TRUE)
Tmax$`2022` <- apply(Tmax[, 2218:2463], 1, max, na.rm = TRUE)

#Only keep the Tmax value per year 
Tmax <- Tmax[, -c(4:2463)]

Tmax_long <- Tmax %>%
  pivot_longer(cols = starts_with("2013"):starts_with("2022"), 
               names_to = "Year", 
               values_to = "Max_temperature") 
str(Tmax_long)

#=======================Draw Density Plot=========================
ggplot(Tmax_long, aes(x = Max_temperature, fill = factor(Year))) +
  geom_density(alpha = 0.5, bw = 0.5) +  
  labs(title = "Density Plot of Annual Maximum Temperatures", x = "Maximum Temperature (◦C)", y = "Density ") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(Tmax_long$Max_temperature), max(Tmax_long$Max_temperature), by = 1))  

install.packages("pracma") 
library(pracma)  

# Calculate the kernel density estimate
density_est <- density(Tmax_long$Max_temperature, bw = 0.5)

# Define the temperature range (From 33 to 40)
range_start <- 33
range_end <- 34

# Find the density values within the range
density_in_range <- density_est$y[density_est$x >= range_start & density_est$x <= range_end]
x_in_range <- density_est$x[density_est$x >= range_start & density_est$x <= range_end]

# Calculate the density integral over this range
density_sum <- trapz(x_in_range, density_in_range)

# Print the result 
cat("The density within the range", range_start, "to", range_end, "is:", density_sum, "\n")

#=========Robutness Check: 90; 95; 96 th pctl================
summary(Tmax_long$Max_temperature)

quantile(Tmax_long$Max_temperature, probs = 0.50) #37 degree
quantile(Tmax_long$Max_temperature, probs = 0.75) #39 degree

#==========Arrange the Dataset for 90; 95; 96 th pctl ============
Tmax_Above.33 <- read_csv("Tmax_Above.33.Simplify_output.csv")
Tmax_Above.37 <- read_csv("Tmax_Above.37.Simplify_output.csv")
Tmax_Above.39 <- read_csv("Tmax_Above.39.Simplify_output.csv")

#============33 dgree================
#Convert to the Long data set 
Tmax_Above.33 <- reshape2::melt(Tmax_Above.33, id.vars = c("Company","Latitude","Longitude"),
                                   variable.name = "Year", value.name = "Days.Above33") %>% arrange(Company, Year) 
#delete the duplicate row
Tmax_Above.33 <- unique(Tmax_Above.33)

Tmax_Above.33$Year <- str_sub(Tmax_Above.33$Year, -4, -1)

df_Panel$Year <- as.character(df_Panel$Year)
Tmax_Above.33$Year <- as.character(Tmax_Above.33$Year)

merged_df.33 <- inner_join(df_Panel, Tmax_Above.33, by = c("Company", "Year"))

#============37 degree================
Tmax_Above.37 <- reshape2::melt(Tmax_Above.37, id.vars = c("Company","Latitude","Longitude"),
                                variable.name = "Year", value.name = "Days.Above37") %>% arrange(Company, Year) 
#delete the duplicate row
Tmax_Above.37 <- unique(Tmax_Above.37)

Tmax_Above.37$Year <- str_sub(Tmax_Above.37$Year, -4, -1)

Tmax_Above.37$Year <- as.character(Tmax_Above.37$Year)

merged_df.37 <- inner_join(merged_df.33, Tmax_Above.37, by = c("Company", "Year"))

#============39 degree================
Tmax_Above.39 <- reshape2::melt(Tmax_Above.39, id.vars = c("Company","Latitude","Longitude"),
                                variable.name = "Year", value.name = "Days.Above39") %>% arrange(Company, Year) 
#delete the duplicate row
Tmax_Above.39 <- unique(Tmax_Above.39)

Tmax_Above.39$Year <- str_sub(Tmax_Above.39$Year, -4, -1)

Tmax_Above.39$Year <- as.character(Tmax_Above.39$Year)

merged_df.39 <- inner_join(merged_df.37, Tmax_Above.39, by = c("Company", "Year"))

#The complete data set
df_Panel.33_39 <- merged_df.39[, -c(20, 21, 23, 24, 26, 27)]

#Create a new column for cluster standard error 
df_Panel.33_39$clus_ISO_year<-as.factor(paste(df_Panel.33_39$Country.ISO.code,df_Panel.33_39$Year))

write.csv(df_Panel.33_39, "df_Panel.33_39_fe", row.names = FALSE)

#==========================Fixed Effect Analysis (Main) ===================================
#  Fixed effect model 
#==========================================================================================
df_Panel.33_39 <- read_csv("df_Panel.33_39_fe")

# Rename multiple columns
str(df_Panel.33_39)

# Identify missing values in Precipitation column 
missing_values <- is.na(df_Panel.33_39 $Sum_Precipitation)

# Get the row and column indices of missing values
missing_indices <- which(missing_values, arr.ind = TRUE)

#check the na value in precipitation 
sum(is.na(df_Panel.33_39 $Sum_Precipitation))

#delete the na value in precipitation
df_Panel.33_39  <- df_Panel.33_39 [!is.na(df_Panel.33_39$Sum_Precipitation),]

#Plot the histogram of the each financia performance 
QQ_OP <- ggqqplot(df_Panel.33_39$Operating.profit, color = "blue", main="Normal Q-Q Plot of Operating Profit")
QQ_OR <- ggqqplot(df_Panel.33_39$Operating.Revenue, color = "blue", main="Normal Q-Q Plot of Operating Revenue")
QQ_MC <- ggqqplot(df_Panel.33_39$Material.costs, color = "blue", main="Normal Q-Q Plot of Material Costs")

QQ_OP
QQ_OR
QQ_MC

grid.arrange(
  QQ_OP, QQ_OR, QQ_MC,
  ncol = 3)

#==============Winzorize the dataset================
# Calculate the quantiles
lower_bound.op <- quantile(df_Panel.33_39$Operating.profit, 0.01, na.rm = TRUE)
upper_bound.op <- quantile(df_Panel.33_39$Operating.profit, 0.99, na.rm = TRUE)

lower_bound.or <- quantile(df_Panel.33_39$Operating.Revenue, 0.01, na.rm = TRUE)
upper_bound.or <- quantile(df_Panel.33_39$Operating.Revenue, 0.99, na.rm = TRUE)

lower_bound.mc <- quantile(df_Panel.33_39$Material.costs, 0.01, na.rm = TRUE)
upper_bound.mc <- quantile(df_Panel.33_39$Material.costs, 0.99, na.rm = TRUE)

# Apply Winsorization
Operating_profit_win <- pmin(pmax(df_Panel.33_39$Operating.profit, lower_bound.op), upper_bound.op)
Operating_Revenue_win <- pmin(pmax(df_Panel.33_39$Operating.Revenue, lower_bound.or), upper_bound.or)
Material_cost_win <- pmin(pmax(df_Panel.33_39$Material.costs, lower_bound.mc), upper_bound.mc)

df_Panel.33_39$Operating.Revenue<- Operating_Revenue_win
df_Panel.33_39$Operating.profit <- Operating_profit_win
df_Panel.33_39$Material.costs <- Material_cost_win

#QQ plot the cut-off dataset 
QQ_OP.win <- ggqqplot(df_Panel.33_39$Operating.profit, color = "red", main="Normal Q-Q Plot of Operating Profit")
QQ_OR.win <- ggqqplot(df_Panel.33_39$Operating.Revenue, color = "red", main="Normal Q-Q Plot of Operating Revenue")
QQ_MC.win <- ggqqplot(df_Panel.33_39$Material.costs, color = "red", main="Normal Q-Q Plot of Material Costs")

QQ_OP.win
QQ_OR.win
QQ_MC.win

grid.arrange(
  QQ_OP.win, QQ_OR.win, QQ_MC.win,
  ncol = 3)

#====================================Material Costs====================================
# List of controlled variables to test
controlled_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List to store the models
models_MC.main <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Material.costs ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  models_MC.main[[var]] <- summary(model, vcov = ~clus_ISO_year)
}

# Print summaries of the models
for (var in controlled_vars) {
  print(paste("Summary for model with controlled variable:", var))
  print(models_MC.main[[var]])
}

#====================================Operating Revenue====================================
# List of controlled variables to test
controlled_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List to store the models
models_OR.main <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Operating.Revenue ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  models_OR.main[[var]] <- summary(model, vcov = ~clus_ISO_year)
}

# Print summaries of the models
for (var in controlled_vars) {
  print(paste("Summary for model with controlled variable:", var))
  print(models_OR.main[[var]])
}

#====================================Operating Profit====================================
# List of controlled variables to test
controlled_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List to store the models
models_OP.main <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Operating.profit ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  models_OP.main[[var]] <- summary(model, vcov = ~clus_ISO_year)
}

# Print summaries of the models
for (var in controlled_vars) {
  print(paste("Summary for model with controlled variable:", var))
  print(models_OP.main[[var]])
}

#================================Plot the Main Results======================================
# Plot the marginal effects --> Four of them --> 33 & 35 & 37 & 39
#===========================================================================================
# List of controlled variables to test
controlled_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# Custom labels for x-axis
custom_labels <- c("Days.Above33" = "Days above \n 33˚C", 
                   "Days.Above35" = "Days above \n 35˚C", 
                   "Days.Above37" = "Days above \n 37˚C", 
                   "Days.Above39" = "Days above \n 39˚C")


# Initialize an empty list to store the results for plotting --> For Material Costs
results_list.MC <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Material.costs ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  model_summary <- summary(model, vcov = ~clus_ISO_year)
  
  # Extract coefficient and confidence intervals for the variable of interest
  coef_value <- coef(model_summary)[var]
  conf_int <- confint(model_summary, level = 0.95)[var, ]
  
  # Create a data frame with the results
  temp_df <- data.frame(
    Term = var,
    Estimate = coef_value,
    Conf.Low = conf_int[1],
    Conf.High = conf_int[2]
  )
  
  # Append the results to the list
  results_list.MC[[var]] <- temp_df
}

# Combine the list into a single data frame
results.MC <- do.call(rbind, results_list.MC)


# Define the custom colors for each Term
colors <- c("Days.Above33" = "black", "Days.Above35" = "black", "Days.Above37" = "black", "Days.Above39" = "black")

#=======Plot the Materidal Costs=======
# Plot with custom colors
plot.MC <- ggplot(results.MC, aes(x = Term, y = Estimate, fill = Term)) +
  geom_point(aes(color = Term), stat = "identity", position = position_dodge(width = 0.7), size = 3) +  # Adjust size if needed
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5.., color = Term), 
                width = 0.3, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey" ,size = 1) +
  labs(title = ("c"),
       x = "Extreme Heat Thresholds",
       y = "Impact of one additional day of extreme heat on \nMaterial Costs (1,000€)",
       fill = "Term") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_discrete(labels = custom_labels) + 
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = colors)  
# Print the plot
plot.MC

#=======Plot the Operating Revenue=======
results_list.OR <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Operating.Revenue ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  model_summary <- summary(model, vcov = ~clus_ISO_year)
  
  # Extract coefficient and confidence intervals for the variable of interest
  coef_value <- coef(model_summary)[var]
  conf_int <- confint(model_summary, level = 0.95)[var, ]
  
  # Create a data frame with the results
  temp_df <- data.frame(
    Term = var,
    Estimate = coef_value,
    Conf.Low = conf_int[1],
    Conf.High = conf_int[2]
  )
  
  # Append the results to the list
  results_list.OR[[var]] <- temp_df
}

# Combine the list into a single data frame
results.OR <- do.call(rbind, results_list.OR)

# Plot the bar chart with 95% confidence intervals
plot.OR <-ggplot(results.OR, aes(x = Term, y = Estimate, fill = Term)) +
  geom_point(aes(color = Term), stat = "identity", position = position_dodge(width = 0.7), size = 3) +  # Adjust size if needed
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5.., color = Term), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey" ,size = 1) +
  labs(title = "b",
       x = "Extreme Heat Thresholds",
       y = "Impact of one additional day of extreme heat on \nOperating Revenue (1,000€)",
       fill = "Term") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_discrete(labels = custom_labels) + 
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = colors)  

plot.OR

#=======Plot the Operating Profit=======
results_list.OP <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("Operating.profit ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  model_summary <- summary(model, vcov = ~clus_ISO_year)
  
  # Extract coefficient and confidence intervals for the variable of interest
  coef_value <- coef(model_summary)[var]
  conf_int <- confint(model_summary, level = 0.95)[var, ]
  
  # Create a data frame with the results
  temp_df <- data.frame(
    Term = var,
    Estimate = coef_value,
    Conf.Low = conf_int[1],
    Conf.High = conf_int[2]
  )
  
  # Append the results to the list
  results_list.OP[[var]] <- temp_df
}

# Combine the list into a single data frame
results.OP <- do.call(rbind, results_list.OP)

# Plot the bar chart with 95% confidence intervals
plot.OP <- ggplot(results.OP, aes(x = Term, y = Estimate, fill = Term)) +
  geom_point(aes(color = Term), stat = "identity", position = position_dodge(width = 0.7), size = 3) +  # Adjust size if needed
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5.., color = Term), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 1) +
  labs(title = "a", 
       x = "Extreme Heat Thresholds",
       y = "Impact of one additional day of extreme heat on \nOperating Profit (1,000€)",
       fill = "Term") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_discrete(labels = custom_labels) + 
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = colors)  

plot.OP

#=====Combine the three Main plots together======
grid.arrange(
  plot.OP, plot.OR, plot.MC,
  ncol = 3)



#=======Plot the Material Costs in Log=======
# Plot with custom colors
# Initialize an empty list to store the results for plotting --> For Material Costs
results_list.MC.log <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("log(Material.costs) ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  model_summary <- summary(model, vcov = ~clus_ISO_year)
  
  # Extract coefficient and confidence intervals for the variable of interest
  coef_value <- coef(model_summary)[var]
  conf_int <- confint(model_summary, level = 0.95)[var, ]
  
  # Create a data frame with the results
  temp_df <- data.frame(
    Term = var,
    Estimate = coef_value,
    Conf.Low = conf_int[1],
    Conf.High = conf_int[2]
  )
  
  # Append the results to the list
  results_list.MC.log[[var]] <- temp_df
}


# Combine the list into a single data frame
results.MC.log <- do.call(rbind, results_list.MC.log)

# Plot with custom colors
plot.MC.log <- ggplot(results.MC.log, aes(x = Term, y = Estimate, fill = Term)) +
  geom_point(aes(color = Term), stat = "identity", position = position_dodge(width = 0.7), size = 3) +  # Adjust size if needed
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5.., color = Term), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 1) +
  labs(title = "b", 
       x = "Extreme Heat Thresholds",
       y = "Impact of a unit change on \nlog Material Costs",
       fill = "Term") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 12),
        legend.position = "none") +
  scale_x_discrete(labels = custom_labels) + 
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = colors)  

# Print the plot
plot.MC.log


#=======Plot the Operating Revenue in Log=======
results_list.OR.log <- list()

# Loop over the controlled variables
for (var in controlled_vars) {
  formula <- as.formula(paste("log(Operating.Revenue) ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
  model <- feols(formula, data = df_Panel.33_39)
  model_summary <- summary(model, vcov = ~clus_ISO_year)
  
  # Extract coefficient and confidence intervals for the variable of interest
  coef_value <- coef(model_summary)[var]
  conf_int <- confint(model_summary, level = 0.95)[var, ]
  
  # Create a data frame with the results
  temp_df <- data.frame(
    Term = var,
    Estimate = coef_value,
    Conf.Low = conf_int[1],
    Conf.High = conf_int[2]
  )
  
  # Append the results to the list
  results_list.OR.log[[var]] <- temp_df
}

# Combine the list into a single data frame
results.OR.log <- do.call(rbind, results_list.OR.log)

# Plot the bar chart with 95% confidence intervals
plot.OR.log <-ggplot(results.OR.log, aes(x = Term, y = Estimate, fill = Term)) +
  geom_point(aes(color = Term), stat = "identity", position = position_dodge(width = 0.7), size = 3) +  # Adjust size if needed
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5.., color = Term), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 1) +
  labs(title = "a", 
       x = "Extreme Heat Thresholds",
       y = "Impact of a unit change on \nlog Operating Revenue",
       fill = "Term") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 12),
        legend.position = "none") +
  scale_x_discrete(labels = custom_labels) + 
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = colors)  

plot.OR.log 

log_OR <- feols(log(Operating.Revenue) ~ Days.Above39 + poly(Sum_Precipitation, 2) | Company + Year, data = df_Panel.33_39)
summary(log_OR, vcov = ~clus_ISO_year)



####

# Single Row Layout
# Adding a title
grid.arrange(
  plot.OR.log, plot.MC.log,
  ncol = 2
  )


#=========================Robustness --> Firm Size==========================================
# Robustness Check: Different Firm Size
#===========================================================================================
# Select the columns of interest
selected_columns <- df_Panel.33_39 %>%
  dplyr::select(AssetClass, Company)  #this dataframe is created for checking the missing value in the AssetClass corresponding the company name


#To check if there is any Missing Value in the AssetClasses
sum(is.na(df_Panel.33_39$AssetClass))  

#delete the na value in precipitation
df_Panel.33_39 <- df_Panel.33_39 [!is.na(df_Panel.33_39$AssetClass),]

# Use distinct() to keep unique rows based on 'Company' column --> there are 5,860 firms
unique_df_Panel_firm <- df_Panel.33_39 %>% distinct(Company, .keep_all = TRUE)

#check the na value 
sum(is.na(unique_df_Panel_firm$AssetClass))

#=====Create the histogram chart=====
# Specify the order you want for the x-axis levels
desired_order <- c("Micro (< EUR 2 million)", "Small (2-10 million)", "Medium (10-43 million)", "Large (Above 43 million)")  # Replace with your specific firm size categories

ggplot(unique_df_Panel_firm, aes(x = factor(AssetClass, levels = desired_order))) +
  geom_bar(stat = "count", fill = "skyblue", width = 0.6) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Firm Size in the Sample",
       x = "Firm Size",
       y = "Number of Firms") +
  theme_minimal()

#======================================================================
#=====================Loop over each asset class=======================
#Change the column name to AssetClass

#=============Subset Fixed Effect --> Firm Size===============================
# Access individual subgroups
# List of datasets
subset_asset <- list(subset_large_df, subset_medium_df, subset_small_df, subset_micro_df)
names(subset_asset) <- c("Large", "Medium", "Small", "Micro")

# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(subset_asset)) {
  df_subset <- subset_asset[[dataset_name]]
  
  for (var in independent_vars) {
    formula <- as.formula(paste("Material.costs ~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
    model <- feols(formula, data = df_subset)
    
    model_name <- paste(dataset_name, var, sep = "_")
    models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

#================Plot Subset Fixed Effect --> Firm Size===================================
# Subset Analysis --> Plot the marginal effects for each Asset Class
#============================================================
# Subset assets
subset_micro_df <- subset(df_Panel.33_39, AssetClass == "Micro (< EUR 2 million)")
subset_small_df <- subset(df_Panel.33_39, AssetClass == "Small (2-10 million)")
subset_medium_df <- subset(df_Panel.33_39, AssetClass == "Medium (10-43 million)")
subset_large_df <- subset(df_Panel.33_39, AssetClass == "Large (Above 43 million)")

#dev.off()
# Subsets with custom titles
subsets <- list(
  "Micro-sized firm" = subset(df_Panel.33_39, AssetClass == "Micro (< EUR 2 million)"),
  "Small-sized firm" = subset(df_Panel.33_39, AssetClass == "Small (2-10 million)"),
  "Medium-sized firm" = subset(df_Panel.33_39, AssetClass == "Medium (10-43 million)"),
  "Large-sized firm" = subset(df_Panel.33_39, AssetClass == "Large (Above 43 million)")
)

# Variables to plot
variables <- c("Operating.profit", "Operating.Revenue", "Material.costs")

# Colors for each subset
subset_colors <- c("Micro-sized firm" = "blue", 
                   "Small-sized firm" = "green", 
                   "Medium-sized firm" = "orange", 
                   "Large-sized firm" = "red")

# Create an empty list to store plots
plot_list <- list()

# Loop through subsets and variables to create histograms
for (subset_name in names(subsets)) {
  # Create a list to store plots for a single subset
  subset_plots <- list()
  
  for (variable in variables) {
    plot <- ggplot(subsets[[subset_name]], aes_string(x = variable)) +
      geom_histogram(binwidth = 200, fill = subset_colors[[subset_name]]) +
      labs(y = "Frequency") +
      theme_minimal()
    
    # Add the plot to the subset list
    subset_plots[[variable]] <- plot
  }
  
  # Create the title grob and rotate it to be vertical
  subset_title <- textGrob(subset_name, gp = gpar(fontsize = 12, fontface = "bold"), rot = 90)
  
  # Combine the plots for the subset with a shared left-side title
  combined_plots <- arrangeGrob(grobs = subset_plots, nrow = 1)
  plot_list[[subset_name]] <- arrangeGrob(arrangeGrob(subset_title), combined_plots, nrow = 1, widths = c(1, 10))
}

# Arrange all subset plots in a single grid
grid.arrange(grobs = plot_list, ncol = 1)


#--------------------------
subset_asset <- list(subset_large_df, subset_medium_df, subset_small_df, subset_micro_df)
names(subset_asset) <- c("Large", "Medium", "Small", "Micro")

# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("Material.costs", "Operating.Revenue", "Operating.profit")

# List to store the models
models <- list()


# Loop over each dataset and each independent variable
for (dataset_name in names(subset_asset)) {
  df_subset <- subset_asset[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}


# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results <- data.frame(
    AssetClass = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        asset_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results <- rbind(results, data.frame(
          AssetClass = asset_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  

  # Set the order of AssetClass from Small to Large
  results$AssetClass <- factor(results$AssetClass, levels = c("Micro", "Small", "Medium", "Large"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("Micro" = "#1f77b4", "Small" = "#ff7f0e", "Medium" = "#2ca02c", "Large" = "#d62728")
  
  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results, aes(x = TemperatureThreshold, y = Coefficient, color = AssetClass)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      title = paste("Impact of Extreme Heat on", dep_var, "of Wine Processors"),
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "Asset Class"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.2, 5.5), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}

# Generate plots for each dependent variable
plot_MC <- create_plot("Material.costs")
plot_OR <- create_plot("Operating.Revenue")
plot_OP <- create_plot("Operating.profit")

# Manually change the titles for the three plots
plot_MC <- plot_MC + labs(title = "c", y = "Impact of one additional day of extreme heat on \nMaterial Costs (1,000€)")
plot_OR <- plot_OR + labs(title = "b", y = "Impact of one additional day of extreme heat on \nOperating Revenue (1,000€)")
plot_OP <- plot_OP + labs(title = "a", y = "Impact of one additional day of extreme heat on \nOperating Profit (1,000€)")

# Display the plots
print(plot_MC)
print(plot_OR)
print(plot_OP)

# Single Row Layout
grid.arrange(
   plot_OP, plot_OR, plot_MC,
  ncol = 3
)


#======Plot Subset Fixed Effect --> Firm Size in Log==============

subset_asset <- list(subset_large_df, subset_medium_df, subset_small_df, subset_micro_df)
names(subset_asset) <- c("Large", "Medium", "Small", "Micro")

# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("log(Material.costs)", "log(Operating.Revenue)")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(subset_asset)) {
  df_subset <- subset_asset[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results <- data.frame(
    AssetClass = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        asset_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results <- rbind(results, data.frame(
          AssetClass = asset_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  
  # Set the order of AssetClass from Small to Large
  results$AssetClass <- factor(results$AssetClass, levels = c("Micro", "Small", "Medium", "Large"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("Micro" = "#1f77b4", "Small" = "#ff7f0e", "Medium" = "#2ca02c", "Large" = "#d62728")
  
  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results, aes(x = TemperatureThreshold, y = Coefficient, color = AssetClass)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "Asset Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.2, 5.6), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}

# Generate plots for each dependent variable
plot_MC_log <- create_plot("Material.costs")
plot_OR_log <- create_plot("Operating.Revenue")


# Manually change the titles for the three plots
plot_MC_log <- plot_MC_log + labs(y = "Impact of a unit change on \nlog Material Costs")
plot_OR_log <- plot_OR_log + labs(y = "Impact of a unit change on \nlog Operating Revenue")

# Display the plots
plot_MC_log
plot_OR_log


# Single Row Layout
grid.arrange(
  plot_OR_log, plot_MC_log,
  ncol = 2
)

####################
print_conf_int <- function(dep_var) {
  # Extract confidence intervals and print them
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      conf_int <- confint(model, level = 0.95)
      
      # Ensure confidence intervals are available
      if (!is.null(conf_int) && nrow(conf_int) > 0) {
        # Print the confidence intervals
        cat("Model:", model_name, "\n")
        print(conf_int)
      }
    }
  }
}


print_conf_int("Operating.Revenue")


#==============================================================
# Subset Analysis --> Basing on the North, West, South and East
#==============================================================
#=====Plot the map by four regions=====
df_Panel.33_39_map <- df_Panel.33_39 %>% distinct(Company, .keep_all = TRUE)
str(df_Panel.33_39_map)

# Convert the Country_ISO code into three digital 
europe_iso_codes <- data.frame(
  iso_code = c(
    "ALB", "AND", "ARM", "AUT", "AZE", "BLR", "BEL",
    "BIH", "BGR", "HRV", "CYP", "CZE",
    "DNK", "EST", "FIN", "FRA", "GEO", "DEU", "GRC",
    "HUN", "ISL", "IRL", "ITA", "KAZ", "XKX", "LVA",
    "LIE", "LTU", "LUX", "MLT", "MDA", "MCO",
    "MNE", "NLD", "MKD", "NOR", "POL", "PRT",
    "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "ESP",
    "SWE", "CHE", "TUR", "UKR", "GBR", "VAT"
  )
)

# 提取 df1 和 df2 中列的前两位数字
df_Panel.33_39_map <- df_Panel.33_39_map %>%
  mutate(country_code_prefix = substr(Country.ISO.code, 1, 2))
str(europe_iso_codes)

europe_iso_codes <- europe_iso_codes %>%
  mutate(country_code_prefix = substr(iso_code, 1, 2))

#convert it in Excel 
write.csv(df_Panel.33_39_map, "map_ISO_country code.csv", row.names = FALSE)
write.csv(europe_iso_codes, "europe_iso_codes.csv", row.names = FALSE)

map_regions <- read_delim("map_ISO_country code.csv", delim = ";")

# Download the Global map data
world <- ne_countries(scale = "medium", returnclass = "sf")

europe <- world %>% 
  filter(continent == "Europe" & name != "Russia")
str(europe)

# merge the map data with the region data
europe <- europe %>%
  left_join(map_regions, by = c("gu_a3" = "country_coDEU_prefix"))

# 绘制地图
ggplot(data = europe) +
  geom_sf(aes(fill = region)) +
  scale_fill_manual(
    values = c("North" = "blue", "West" = "green", "South" = "red", "East" = "purple"),
    na.value = "gray",
    labels = c("North" = "Northern Europe", "West" = "Western Europe", "South" = "Southern Europe", "East" = "Eastern Europe")
  ) +
  coord_sf(xlim = c(-30, 45), ylim = c(30, 72), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Map of sub-sample splits by four regions",
    fill = "Region"  # Set the legend title
  ) +
  theme(
    legend.position = "right",        # Position legend on the right
    legend.direction = "vertical",    # Arrange legend items vertically
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 10)                   # Customize legend text
  )

ggplot


#======Subset fixed effect --> Spatial Analysis===============
#==============Initialize the list 
# Subset the data based on the country 
subset_Spain <- df_Panel.33_39 %>% filter(Country.ISO.code == "ES")
subset_Italy <- df_Panel.33_39 %>% filter(Country.ISO.code == "IT")
subset_France <- df_Panel.33_39 %>% filter(Country.ISO.code == "FR")
subset_Portugal <- df_Panel.33_39 %>% filter(Country.ISO.code == "PT")

datasets_iso <- list(
  ES = subset_Spain,
  IT = subset_Italy,
  FR = subset_France,
  PT = subset_Portugal
)

# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("Material.costs", "Operating.Revenue", "Operating.profit")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(datasets_iso)) {
  df_subset <- datasets_iso[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results <- data.frame(
    region = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        region_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results <- rbind(results, data.frame(
          region = region_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  
  
  # Set the order and Regions 
  results$region<- factor(results$region, levels = c("ES", "IT", "FR","PT"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("ES" = "#1f77b4", "IT" = "#ff7f0e", "FR" = "#2ca02c", "PT" = "#d62728")

  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results, aes(x = TemperatureThreshold, y = Coefficient, color = region)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      title = paste("Impact of Extreme Heat on", dep_var, "of Wine Processors"),
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "Regions"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.3, 4.8), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}

# Generate plots for each dependent variable
plot_MC.region <- create_plot("Material.costs")
plot_OR.region <- create_plot("Operating.Revenue")
plot_OP.region <- create_plot("Operating.profit")

# Manually change the titles for the three plots
plot_MC.region <- plot_MC.region + labs(title = "c", y = "Impact of one additional day of extreme heat on \nMaterial Costs (1,000€)")
plot_OR.region <- plot_OR.region + labs(title = "b", y = "Impact of one additional day of extreme heat on \nOperating Revenue (1,000€)")
plot_OP.region <- plot_OP.region + labs(title = "a", y = "Impact of one additional day of extreme heat on \nOperating Profit (1,000€)")

# Display the plots
print(plot_MC.region)
print(plot_OR.region)
print(plot_OP.region)

# Single Row Layout
grid.arrange(
  plot_OP.region, plot_OR.region, plot_MC.region,
  ncol = 3
)


#======Plot the subset fixed effect --> Spatial Analysis in Log===============
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("log(Operating.Revenue)", "log(Material.costs)")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(datasets_iso)) {
  df_subset <- datasets_iso[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results <- data.frame(
    region = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        region_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results <- rbind(results, data.frame(
          region = region_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  
  
  # Set the order and Regions 
  results$region<- factor(results$region, levels = c("ES", "IT", "FR","PT"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("ES" = "#1f77b4", "IT" = "#ff7f0e", "FR" = "#2ca02c", "PT" = "#d62728")
  
  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results, aes(x = TemperatureThreshold, y = Coefficient, color = region)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "Regions"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.3, 4.8), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}

# Generate plots for each dependent variable
plot_MC.region <- create_plot("Material.costs")
plot_OR.region <- create_plot("Operating.Revenue")

# Manually change the titles for the three plots
plot_MC.region <- plot_MC.region + labs(y = "Impact of one additional day of extreme heat on \nMaterial Costs")
plot_OR.region <- plot_OR.region + labs(y = "Impact of one additional day of extreme heat on \nOperating Revenue")

# Display the plots
print(plot_MC.region)
print(plot_OR.region)


# Single Row Layout
grid.arrange(
  plot_OR.region, plot_MC.region,
  ncol = 2
)


#=============Subset Fixed Effect --> NACE - A1.2.1 nace-code Subset =============================================
# Subset the data based on the regions
subset_121 <- subset(df_Panel.33_39, NACE.Core.secondary == 121)
subset_non.121 <- subset(df_Panel.33_39, NACE.Core.secondary != 121)

subset_NACE <- list(subset_121, subset_non.121)
names(subset_NACE) <- c("121", "non.121")

# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("Material.costs", "Operating.Revenue", "Operating.profit")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(subset_NACE)) {
  df_subset <- subset_NACE[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results.NACE <- data.frame(
    NACE = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        NACE_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results.NACE <- rbind(results.NACE, data.frame(
          NACE = NACE_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  
  
  # Set the order and Regions 
  results.NACE$NACE<- factor(results.NACE$NACE, levels = c("121", "non.121"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("121" = "#2ca02c", "non.121" = "#1f77b4")
  
  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results.NACE, aes(x = TemperatureThreshold, y = Coefficient, color = NACE)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "NACE Category"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.3, 7), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}


# Generate plots for each dependent variable
plot_MC.NACE <- create_plot("Material.costs")
plot_OR.NACE <- create_plot("Operating.Revenue")
plot_OP.NACE <- create_plot("Operating.profit")

# Manually change the titles for the three plots
plot_MC.NACE <- plot_MC.NACE + labs(title = "c", y = "Impact of one additional day of extreme heat on \nMaterial Costs (1,000€)")
plot_OR.NACE <- plot_OR.NACE + labs(title = "b", y = "Impact of one additional day of extreme heat on \nOperating Revenue (1,000€)")
plot_OP.NACE <- plot_OP.NACE + labs(title = "a", y = "Impact of one additional day of extreme heat on \nOperating Profit (1,000€)")

# Display the plots
print(plot_MC.NACE)
print(plot_OR.NACE)
print(plot_OP.NACE)

# Single Row Layout
grid.arrange(
  plot_OP.NACE, plot_OR.NACE, plot_MC.NACE,
  ncol = 3
)

#=============Subset Fixed Effect --> NACE - A1.2.1 nace-code Subset Log=============================================
# List of independent variables
independent_vars <- c("Days.Above33", "Days.Above35", "Days.Above37", "Days.Above39")

# List of dependent variables
dependent_vars <- c("log(Operating.Revenue)", "log(Material.costs)")

# List to store the models
models <- list()

# Loop over each dataset and each independent variable
for (dataset_name in names(subset_NACE)) {
  df_subset <- subset_NACE[[dataset_name]]
  
  for (dep_var in dependent_vars) {
    for (var in independent_vars) {
      formula <- as.formula(paste(dep_var, "~", var, "+ poly(Sum_Precipitation, 2) | Company + Year"))
      model <- feols(formula, data = df_subset)
      
      model_name <- paste(dataset_name, dep_var, var, sep = "_")
      models[[model_name]] <- summary(model, vcov = ~clus_ISO_year)
    }
  }
}

# Print summaries of the models
for (model_name in names(models)) {
  print(paste("Summary for model:", model_name))
  print(models[[model_name]])
}

# Function to extract results and create ggplot for a given dependent variable
create_plot <- function(dep_var) {
  # Create a list to store the results
  results.NACE <- data.frame(
    NACE = character(),
    TemperatureThreshold = character(),
    Coefficient = numeric(),
    Conf.Low = numeric(),
    Conf.High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract coefficients and confidence intervals
  for (model_name in names(models)) {
    if (grepl(dep_var, model_name)) {
      model <- models[[model_name]]
      coef <- coef(model)
      conf_int <- confint(model, level = 0.95)
      
      # Ensure coefficients and confidence intervals are available
      if (!is.null(coef) && length(coef) > 0 && !is.null(conf_int) && nrow(conf_int) > 0) {
        # Extract asset class and variable from the model name
        dataset_var <- strsplit(model_name, "_")[[1]]
        NACE_class <- dataset_var[1]
        temperature_threshold <- dataset_var[3]
        
        results.NACE <- rbind(results.NACE, data.frame(
          NACE = NACE_class,
          TemperatureThreshold = temperature_threshold,
          Coefficient = coef[1],
          Conf.Low = conf_int[1, 1],
          Conf.High = conf_int[1, 2]
        ))
      }
    }
  }
  
  
  # Set the order and Regions 
  results.NACE$NACE<- factor(results.NACE$NACE, levels = c("121", "non.121"))
  
  # Define the colors for each AssetClass using a different color palette
  colors <- c("121" = "#2ca02c", "non.121" = "#1f77b4")
  
  # Define custom labels for the x-axis
  x_labels <- c(
    "Days.Above33" = "Days above \n 33˚C",
    "Days.Above35" = "Days above \n 35˚C",
    "Days.Above37" = "Days above \n 37˚C",
    "Days.Above39" = "Days above \n 39˚C"
  )
  
  # Create the point range plot
  plot <- ggplot(results.NACE, aes(x = TemperatureThreshold, y = Coefficient, color = NACE)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), position = position_dodge(width = 0.5), width = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "darkgrey",size = 0.6) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = x_labels) +
    labs(
      x = "Extreme Heat Thresholds",
      y = paste("Impact on the", dep_var),
      color = "NACE Category"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 12),
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      legend.position = c(0, 1.05),  # Move legend box to the top-left corner
      legend.justification = c(-0.3, 7), # Aligns the legend box with the top-left corner
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", size = 0.5) # Define the color of the legend keys
    )
  
  return(plot)
}


# Generate plots for each dependent variable
plot_MC.NACE.log <- create_plot("Material.costs")
plot_OR.NACE.log <- create_plot("Operating.Revenue")


# Manually change the titles for the three plots
plot_MC.NACE.log <- plot_MC.NACE.log + labs(title = "c", y = "Impact of one additional day of extreme heat on \nLog Material Costs")
plot_OR.NACE.log <- plot_OR.NACE.log + labs(title = "b", y = "Impact of one additional day of extreme heat on \nLog Operating Revenue")

# Display the plots
print(plot_MC.NACE.log)
print(plot_OR.NACE.log)

# Single Row Layout
grid.arrange(
    plot_OR.NACE.log, plot_MC.NACE.log,
  ncol = 2
)

install.packages("gplots")
library(gplots)
plotmeans(y ~ Year, main="Heterogeineityacross years", data=df_Panel.33_39)



