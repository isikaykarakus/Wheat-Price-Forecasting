#import libraries
#########
library(readr)
library(ggplot2)
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(dplyr)
library(scales)
library(bit64)
library(gbm)
library(tidyr)
library(lubridate)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(caret)


############


#######
#import-export-countyr-year dataset
FAOSTAT_data_en_1_4_2024 <- read_csv("FAOSTAT_data_en_1-4-2024.csv")
View(FAOSTAT_data_en_1_4_2024)

#EDA for import export
# Pivot the data to wide format
reshaped_import_export <- FAOSTAT_data_en_1_4_2024 %>%
  pivot_wider(names_from = Element, values_from = Value, 
              names_prefix = "Element_") %>%
  select(Area, `Year Code`, Item, `Element_Import Quantity`, `Element_Import Value`, 
         `Element_Export Quantity`, `Element_Export Value`)

reshaped_import_export <- reshaped_import_export %>%
  rename(Import_Quantity = `Element_Import Quantity`,
         Import_Value = `Element_Import Value`,
         Export_Quantity = `Element_Export Quantity`,
         Export_Value = `Element_Export Value`,
         Year = `Year Code`)

#remove na from import-export/country
reshaped_import_export <- reshaped_import_export %>%
  group_by(Area, Year, Item) %>%
  summarize(Import_Quantity = sum(Import_Quantity, na.rm = TRUE),
            Import_Value = sum(Import_Value, na.rm = TRUE),
            Export_Quantity = sum(Export_Quantity, na.rm = TRUE),
            Export_Value = sum(Export_Value, na.rm = TRUE),
            .groups = 'drop')

#grouped_data contains items and year with import-export
grouped_data <- reshaped_import_export %>%
  group_by(Year, Item) %>%
  summarize(Total_Import_Quantity = sum(Import_Quantity, na.rm = TRUE),
            Total_Import_Value = sum(Import_Value, na.rm = TRUE),
            Total_Export_Quantity = sum(Export_Quantity, na.rm = TRUE),
            Total_Export_Value = sum(Export_Value, na.rm = TRUE),
            .groups = 'drop')
head(grouped_data)

#Import-Export Value and Quantity Over Years

ggplot(grouped_data, aes(x = Year)) +
  geom_line(aes(y = Total_Import_Quantity, color = "Import Quantity")) +
  geom_line(aes(y = Total_Export_Quantity, color = "Export Quantity")) +
  geom_line(aes(y = Total_Import_Value, color = "Import Value")) +
  geom_line(aes(y = Total_Export_Value, color = "Export Value")) +
  facet_wrap(~ Item, scales = "free_y") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels with commas
  scale_color_manual(values = c("Import Quantity" = "blue", 
                                "Export Quantity" = "red",
                                "Import Value" = "green",
                                "Export Value" = "purple")) +
  labs(title = "Import-Export Value and Quantity Over Years",
       x = "Year",
       y = "Value / Quantity",
       color = "Legend") +
  theme_minimal()

#seperated graphs in case of need 

unique_items <- unique(grouped_data$Item)

for (item in unique_items) {
  item_data <- grouped_data[grouped_data$Item == item, ]
  
  p <- ggplot(item_data, aes(x = Year)) +
    geom_line(aes(y = Total_Import_Quantity, color = "Import Quantity")) +
    geom_line(aes(y = Total_Export_Quantity, color = "Export Quantity")) +
    geom_line(aes(y = Total_Import_Value, color = "Import Value")) +
    geom_line(aes(y = Total_Export_Value, color = "Export Value")) +
    scale_y_continuous(labels = label_comma()) +
    scale_color_manual(values = c("Import Quantity" = "blue", 
                                  "Export Quantity" = "red",
                                  "Import Value" = "green",
                                  "Export Value" = "purple")) +
    labs(title = paste("Import-Export Value and Quantity Over Years for", item),
         x = "Year", y = "Value / Quantity", color = "Legend") +
    theme_minimal()
  
  # Optional: Save the plot
  # ggsave(filename = paste0("plot_", item, ".png"), plot = p, width = 10, height = 8)
  
  print(p)  # Print the plot
}
# Summarize data by country for see the top importers-exporters
country_summary <- reshaped_import_export %>%
  group_by(Area) %>%
  summarize(Total_Import_Quantity = sum(Import_Quantity, na.rm = TRUE),
            Total_Import_Value = sum(Import_Value, na.rm = TRUE),
            Total_Export_Quantity = sum(Export_Quantity, na.rm = TRUE),
            Total_Export_Value = sum(Export_Value, na.rm = TRUE))
#only for the wheat importes-exportes 

wheat_import_exports <- grouped_data %>%
  filter(Item == "Wheat")

names(wheat_import_exports)

wheat_import_exports <- wheat_import_exports%>%
  select(Year, Total_Import_Quantity, Total_Import_Value, Total_Export_Quantity ,Total_Export_Value)


# Filter for only 'Wheat' item
wheat_summary <- reshaped_import_export %>%
  filter(Item == "Wheat") %>%
  group_by(Area) %>%
  summarize(Total_Import_Quantity = sum(Import_Quantity, na.rm = TRUE),
            Total_Import_Value = sum(Import_Value, na.rm = TRUE),
            Total_Export_Quantity = sum(Export_Quantity, na.rm = TRUE),
            Total_Export_Value = sum(Export_Value, na.rm = TRUE))
# Top 10 importers by quantity-value
top_importers_quantity <- wheat_summary %>%
  arrange(desc(Total_Import_Quantity)) %>%
  head(10)

top_importers_value <- wheat_summary %>%
  arrange(desc(Total_Import_Value)) %>%
  head(10)
# Top 10 exporters by quantity-value
top_exporters_quantity <- wheat_summary %>%
  arrange(desc(Total_Export_Quantity)) %>%
  head(10)

top_exporters_value <- wheat_summary %>%
  arrange(desc(Total_Export_Value)) %>%
  head(10)

#seperated plots for top exporters-importes, quantity-value

ggplot(top_importers_quantity, aes(x = reorder(Area, Total_Import_Quantity), y = Total_Import_Quantity)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # Formatting y-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Wheat Importers by Quantity", x = "Country", y = "Total Import Quantity")

ggplot(top_importers_value, aes(x = reorder(Area, Total_Import_Value), y = Total_Import_Value)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # Formatting y-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Wheat Importers by Value", x = "Country", y = "Total Import Value")

ggplot(top_exporters_value, aes(x = reorder(Area, Total_Export_Value), y = Total_Export_Value)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # Formatting y-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Wheat Exporters by Value", x = "Country", y = "Total Export Value")

ggplot(top_exporters_quantity, aes(x = reorder(Area, Total_Export_Quantity), y = Total_Export_Quantity)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # Formatting y-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Wheat Exporters by Quantity", x = "Country", y = "Total Export Quantity")
###############

########wheat price

# US_Wheat_Futures_Historical_Data_1 <- read_csv("US Wheat Futures Historical Data (1).csv")
# US_Wheat_Futures_Historical_Data_2_ <- read_csv("US Wheat Futures Historical Data (2).csv")
# wheat_dataset<-rbind(US_Wheat_Futures_Historical_Data_1, US_Wheat_Futures_Historical_Data_2_)
# 
# #EDA for wheat price
# 
# # Convert 'Date' to Date format
# wheat_dataset$Date <- as.Date(wheat_dataset$Date, format = "%m/%d/%Y")
# 
# # Convert 'Change %' to numeric by removing the percent sign
# wheat_dataset$`Change %` <- as.numeric(sub("%", "", wheat_dataset$`Change %`))
# 
# # Assuming you have a 'Date' variable in your dataset
# wheat_dataset$Date <- as.Date(wheat_dataset$Date, format = "%m/%d/%Y")
# 
# # Create a 'Year' variable from the 'Date'
# 
# wheat_dataset$Year <- format(wheat_dataset$Date, "%Y")  # Extract the year

# Load the dataset
wheat_dataset <- read_csv("PWHEAMTUSDM (1).csv")

# Convert DATE column to Date type and extract Year
wheat_dataset$DATE <- as.Date(wheat_dataset$DATE, format = "%Y-%m-%d")
wheat_dataset$Year <- format(wheat_dataset$DATE, "%Y")

# Convert Year to numeric
wheat_dataset$Year <- as.numeric(wheat_dataset$Year)

# Ensure Price is numeric
wheat_dataset$Price <- as.numeric(wheat_dataset$PWHEAMTUSDM)

# Removing the first column as it's no longer needed
wheat_dataset <- wheat_dataset[, -1]

# Ensure the data is sorted by Year
wheat_dataset <- wheat_dataset %>% arrange(Year)

# Check for missing values in the dataset
sum(is.na(wheat_dataset$Year))
sum(is.na(wheat_dataset$Price))

# Remove rows with missing values
wheat_dataset <- wheat_dataset %>% 
  filter(!is.na(Year) & !is.na(Price))


# Create the ggplot
wheat_plot <- ggplot(wheat_dataset, aes(x = Year, y = Price)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Average Wheat Prices Over Years",
       x = "Year", y = "Average Price in US Dollars per Metric Tonnes") +
  scale_x_continuous(breaks = seq(min(wheat_dataset$Year), max(wheat_dataset$Year), by = 2)) +
  theme_minimal()

# Display the plot
print(wheat_plot)

# # Convert 'Date' to Date format
# wheat_dataset$Year <- as.Date(wheat_dataset$Year, format = "%m/%d/%Y")
# 
# # Extract the year from the 'Date' column
# wheat_dataset$Year <- lubridate::year(wheat_dataset$Date)
# 
# average_prices <- wheat_dataset %>%
#   group_by(Year) %>%
#   summarise(Average_Price = mean(Price, na.rm = TRUE),
#             Average_Change = mean(`Change %`, na.rm = TRUE))
# 
# # Remove the last row if necessary
# average_prices <- average_prices[1:(nrow(average_prices)-1), ]
# 
# 
# ggplot(wheat_dataset, aes(x = Year, y = Price)) +
#   geom_line(color = "orange") +       # Add a line
#   geom_point(color = "brown", size = 3) +  # Add points
#   labs(title = "Average Wheat Prices Over Years",     # Add a title
#        x = "Year", y = "Average Price in US Dollars per Metric Tonnes") +  # Label axes
#   scale_x_continuous(breaks = seq(min(wheat_dataset$Year), max(wheat_dataset$Year), by = 2)) +  # Set x-axis breaks every 2 years
#   theme_minimal()  # Use a minimal theme for better readability
#######

#####
#wheat production 

wheat_production <- read_csv("crop_production.csv")
head(wheat_production)
#EDA for wheat production

# Filter the original dataset to include only data for wheat
wheat_production <- wheat_production %>%
  filter(SUBJECT == "WHEAT" & !(LOCATION %in% c("WLD", "BRICS", "EU28", "OECD")))
wheat_production <- wheat_production %>%
  rename(Year = TIME)
# Group by country and calculate total production
total_production_by_country <- wheat_production %>%
  group_by(LOCATION) %>%
  summarise(Total_Production = sum(Value))

# Select the top 10 wheat-producing countries
top_countries <- total_production_by_country %>%
  arrange(desc(Total_Production)) %>%
  slice(1:10)

# Filter the original dataset to include only data for the top 10 countries
wheat_dataset_top_countries <- wheat_production %>%
  filter(LOCATION %in% top_countries$LOCATION)

# Create a bar plot for each country and year
ggplot(wheat_dataset_top_countries, aes(x = LOCATION, y = Value, fill = LOCATION)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Wheat Producing Countries Over Time",
       x = "Country",
       y = "Wheat Production",
       fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  scale_y_continuous(labels = scales::comma_format(scale = 1e6))  # Format y-axis labels without scientific notation

#line plot for total production 
average_production_per_year <- wheat_production %>%
  group_by(Year) %>%
  summarize(Average_Production = mean(Value, na.rm = TRUE))

# Plot the average production per year
ggplot(average_production_per_year, aes(x = Year, y = Average_Production)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Average Global Wheat Production Per Year",
       x = "Year",
       y = "Average Production (tonnes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  scale_y_continuous(labels = comma_format(scale = 1)) + # Format y-axis labels without scientific notation
  scale_x_continuous(breaks = seq(min(average_production_per_year$Year), 
                                  max(average_production_per_year$Year), by = 2))  # Set x-axis breaks every 2 years

###########

####other factors that can affect wheat price but this dataset from 2003
#creating the correlation matrix please pay attention to this!!

wheat_disappearence_andmanymore <- read_excel("wheat_disappearence_andmanymore.xlsx")
names(wheat_disappearence_andmanymore)

wheat_disappearence_andmanymore <- wheat_disappearence_andmanymore %>%
  select(Year, `Domestic disappearance million metric tons`)

# Plot with faceting
ggplot(wheat_disappearence_andmanymore, aes(x = Year, y = `Domestic disappearance million metric tons`)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Domestic disappearance million metric tons Over Years", x = "Year", y = "Domestic disappearance million metric tons") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Improve x-axis label readability
  scale_x_continuous(breaks = seq(min(wheat_disappearence_andmanymore$Year), 
                                  max(wheat_disappearence_andmanymore$Year), by = 2))  # Set x-axis breaks every 2 years
####

#global temperatures

global_tempreture_anomally <- read_excel("global_tempreture_anomally.xlsx")

global_tempreture_anomally$Year <- as.numeric(as.character(global_tempreture_anomally$Year))
global_tempreture_anomally$Anomaly <- as.numeric(as.character(global_tempreture_anomally$Anomaly))

ggplot(global_tempreture_anomally, aes(x = Year, y = Anomaly)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Global Temperature Anomaly Over Time",
       x = "Year",
       y = "Temperature Anomaly (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Improve x-axis label readability
  scale_x_continuous(breaks = seq(min(global_tempreture_anomally$Year), 
                                  max(global_tempreture_anomally$Year), by = 2))  # Set x-axis breaks every 2 years
######

###Flour price
flour_price <- read_csv("flour_price.csv")
flour_price <- flour_price %>%
  rename( Year = DATE,
          `Flour Price`= APU0000701111 )

# Convert 'Date' to Date format
flour_price$Year <- as.Date(flour_price$Year, format = "%m/%d/%Y")
flour_price$Year <- format(flour_price$Year, "%Y")  # Extract the year

flour_price <- flour_price %>%
  filter(!is.na(`Flour Price`) & !is.na(Year)) %>%
  mutate(Year = as.numeric(Year),
         `Flour Price` = as.numeric(`Flour Price`))

flour_price <- flour_price %>%
  group_by(Year) %>%
  summarise(`Flour Price` = mean(`Flour Price`, na.rm = TRUE),)

ggplot(flour_price, aes(x = Year, y = `Flour Price`)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Flour Price Over Time",
       x = "Year",
       y = "Price in US Dollars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Improve x-axis label readability
  scale_x_continuous(breaks = seq(min(flour_price$Year), 
                                  max(flour_price$Year), by = 2))  # Set x-axis breaks every 2 years
########

###Pasta Price

pasta_price <- read_csv("pasta_price.csv")
pasta_price <- pasta_price %>%
  rename( Year = DATE,
          `Pasta Price`= APU0000701322 )

# Convert 'Date' to Date format
pasta_price$Year <- as.Date(pasta_price$Year, format = "%m/%d/%Y")
pasta_price$Year <- format(pasta_price$Year, "%Y")  # Extract the year

pasta_price <- pasta_price %>%
  filter(!is.na(`Pasta Price`) & !is.na(Year)) %>%
  mutate(Year = as.numeric(Year),
         `FLour Price` = as.numeric(`Pasta Price`))

pasta_price <- pasta_price %>%
  group_by(Year) %>%
  summarise(`Pasta Price` = mean(`Pasta Price`, na.rm = TRUE),)

ggplot(pasta_price, aes(x = Year, y = `Pasta Price`)) +
  geom_line(color = "orange") +       # Add a line
  geom_point(color = "brown", size = 3) +  # Add points
  labs(title = "Pasta Price Over Time",
       x = "Year",
       y = "Price in US Dollars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Improve x-axis label readability
  scale_x_continuous(breaks = seq(min(pasta_price$Year), 
                                  max(pasta_price$Year), by = 2))  # Set x-axis breaks every 2 years

################# correlation matrix

combined_data<-merge(wheat_dataset,average_production_per_year, by = "Year")
combined_data<-merge(combined_data,global_tempreture_anomally, by = "Year")
combined_data<-merge(combined_data,wheat_import_exports, by = "Year")
combined_data<-merge(combined_data,pasta_price, by = "Year")
combined_data<-merge(combined_data,flour_price, by = "Year")
combined_data<-merge(combined_data,wheat_disappearence_andmanymore, by = "Year")#now we have dataset from 2003 to 2022

names(combined_data)
combined_data <- combined_data %>% 
  rename(`Tempreture Change` = Anomaly, 
         `Production` = `Average_Production`,
         `Total Import Value` = `Total_Import_Value`, 
         `Total Export Value` = `Total_Export_Value`, 
         `Total Import Quantity` = `Total_Import_Quantity`, 
         `Total Export Quantity` = `Total_Export_Quantity`, 
         `Domestic Disappearance Million in Metric Tonnes` = `Domestic disappearance million metric tons`)


#CORRELATION MATRIX

numeric_data <- combined_data[sapply(combined_data, is.numeric)]

correlation_matrix <- cor(numeric_data, use = "complete.obs")
correlation_matrix

melted_correlation_matrix <- melt(correlation_matrix)

ggplot(melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low ="forestgreen", high = "orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "") +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 3)

# Assuming 'correlation_matrix' is your correlation matrix
# Filter to include only correlations with 'Price'
price_correlation <- correlation_matrix[, "Price", drop = FALSE]
rownames(price_correlation) <- rownames(correlation_matrix)

# Melt the filtered correlation data
melted_price_correlation <- melt(price_correlation)

# Plot using ggplot2
ggplot(melted_price_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low ="forestgreen", high = "orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "") +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 3)

#TIME DEPENDENCY REMOVED
# Remove the 'Year' column or any other time-related columns from the data
numeric_data <- combined_data %>%
  select(-Year) %>%
  select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix for plotting
melted_correlation_matrix <- melt(correlation_matrix)

# Create the correlation plot
ggplot(melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "forestgreen", high = "orange", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "") +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 3)


########different correlation graph 

ggcorrplot(correlation_matrix, type = "lower", 
           lab = TRUE, method = "square", 
           colors = c("forestgreen", "white", "orange"), 
           tl.col = "black", tl.srt = 35, 
           outline.col = "black")

##########
numeric_data <- combined_data[sapply(combined_data, is.numeric)]

correlation_matrix <- cor(numeric_data[,-1], use = "complete.obs", method = "pearson")

# Melting the correlation matrix into a long format
melted_correlation_matrix <- melt(correlation_matrix, na.rm = TRUE)

# Plotting
ggplot(data = melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "forestgreen", high = "orange", midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 3)


####

########EDA FINISH

#####MODELS
View(combined_data)
combined_data_ts <- ts(combined_data[, -1], start = 2003, frequency = 1)


# Fit the Linear Regression Model
lm_wheat_price_model <- lm(Price ~ Year, data = combined_data)
summary(lm_wheat_price_model)

# Calculate MSE and RMSE
mse_linear_model <- mean(residuals(lm_wheat_price_model)^2)
rmse_linear_model <- sqrt(mse_linear_model)

#RSE linear model
rse_linear_model <- summary(lm_wheat_price_model)$sigma

# AIC 
aic_linear_model <- AIC(lm_wheat_price_model)

# Print the statistics
cat("MSE:", mse_linear_model, "\n")
cat("RMSE:", rmse_linear_model, "\n")
cat("RSE:", rse_linear_model, "\n")
cat("AIC:", aic_linear_model, "\n")

# Plot of Wheat Price Model
plot(combined_data$Year, combined_data$Price, xlab = "Year", ylab = "Wheat Price", type="l")
abline(lm_wheat_price_model, col = "orange")

# Durbin-Watson Test for Autocorrelation in Wheat Price Model

dwtest_result <- dwtest(lm_wheat_price_model)
print(dwtest_result)

# Residuals Plot for Wheat Price Model
residuals_wheat_price <- residuals(lm_wheat_price_model)
plot(combined_data$Year, residuals_wheat_price, xlab = "Year", ylab = "Residuals for Wheat Price", type="l")
abline(h=0, col="brown")  # Adds a horizontal line at 0 for reference

acf(residuals(lm_wheat_price_model)) #significance spike at lag 0

#############
#non-linear model exponential smoothing

# Fit an Exponential Smoothing model - we'll use the 'ets' function which automatically selects the best model
ets_model <- ets(combined_data$Price)

# Check the model summary
summary(ets_model)

# Calculate AIC
aic_ets <- ets_model$aic

# Perform predictions using the fitted model
ets_predictions <- forecast(ets_model, h=length(combined_data$Price))

# Calculate MSE and RMSE
mse_ets <- mean((ets_predictions$mean - combined_data$Price)^2, na.rm=TRUE)
rmse_ets <- sqrt(mse_ets)

# Print the statistics
print("non-linear model exponential smoothing")
cat("MSE:", mse_ets, "\n")
cat("RMSE:", rmse_ets, "\n")
cat("AIC:", aic_ets, "\n")

# Plot the fitted model against the actual data
plot(ets_predictions, main="Exponential Smoothing Predictions vs Actual Data")
lines(combined_data$Price, col="brown")

# Calculate and plot residuals
residuals_ets <- residuals(ets_model)
plot(residuals_ets, type="b", main="Residuals of the Exponential Smoothing Model")

# Durbin-Watson Test for Autocorrelation in Residuals
dwtest_result <- dwtest(lm(residuals_ets ~ 1))
print(dwtest_result)

#Bass model

# Bass Model for  Wheat Price

bass_model <- BM(combined_data$Price, display = TRUE)

summary(bass_model)  # Summary of the Bass Model for Price

########

#ARIMA for wheat price

# Time Series for Wheat Price
ts_wheat_price <- ts(combined_data$Price, start = min(combined_data$Year), end = max(combined_data$Year), frequency = 1)
plot(ts_wheat_price)

##ARIMA

# ARIMA Model for Wheat Price
arima_wheat_price <- Arima(combined_data$Price, order = c(1,0,1))
summary(arima_wheat_price)

# Residuals for Wheat Price ARIMA Model
residuals_wheat_price_111 <- residuals(arima_wheat_price)
tsdisplay(residuals_wheat_price_111)

#ACF for arima

acf(residuals_wheat_price_111)
pacf(residuals_wheat_price_111)

# Fitted Values for Wheat Price ARIMA Model
fitted_wheat_price_111 <- fitted(arima_wheat_price)

# Plotting Fitted Values vs. Actual Values for Wheat Price
plot(combined_data$Year, combined_data$Price, main="Actual vs Fitted Wheat Price (ARIMA 1,0,1)", type="o")
lines(combined_data$Year, fitted_wheat_price_111, col = 2)

# Forecasting Wheat Price with ARIMA(1,1,1) for next 5 years
forecast_wheat_price_111 <- forecast(arima_wheat_price, h=5)
plot(forecast_wheat_price_111, main="Forecast Wheat Price (ARIMA 1,1,1)")

# Calculate MSE and RMSE for the ARIMA(1,1,1) model
mse_arima_wheat_price_111 <- mean(residuals_wheat_price_111^2)
rmse_arima_wheat_price_111 <- sqrt(mse_arima_wheat_price_111)
aic_arima_wheat_price_111 <- arima_wheat_price$aic

cat("ARIMA(1,1,1) Model Metrics:\n")
cat("MSE:", mse_arima_wheat_price_111, "\n")
cat("RMSE:", rmse_arima_wheat_price_111, "\n")
cat("AIC:", aic_arima_wheat_price_111, "\n\n")

# Use auto.arima to automatically select the best fitting ARIMA model for comparison
auto_arima_wheat_price <- auto.arima(ts_wheat_price)
summary(auto_arima_wheat_price)

# Diagnostic checks for auto ARIMA model
checkresiduals(auto_arima_wheat_price)

# Forecasting Wheat Price with the best ARIMA model found by auto.arima for next 5 years
forecast_wheat_price_auto <- forecast(auto_arima_wheat_price, h=5)
plot(forecast_wheat_price_auto, main="Forecast Wheat Price (auto.arima)")

# Calculate residuals for the ARIMA model selected by auto.arima
residuals_wheat_price_auto <- residuals(auto_arima_wheat_price)

# MSE and RMSE for the ARIMA model selected by auto.arima
mse_arima_wheat_price_auto <- mean(residuals_wheat_price_auto^2)
rmse_arima_wheat_price_auto <- sqrt(mse_arima_wheat_price_auto)
aic_arima_wheat_price_auto <- auto_arima_wheat_price$aic

cat("Auto ARIMA Model Metrics:\n")
cat("MSE:", mse_arima_wheat_price_auto, "\n")
cat("RMSE:", rmse_arima_wheat_price_auto, "\n")
cat("AIC:", aic_arima_wheat_price_auto, "\n\n")

# Comparison of ARIMA(1,1,1) and Auto ARIMA models
cat("Comparison of ARIMA Models:\n")
cat("Metric\t\tARIMA(1,1,1)\tAuto ARIMA\n")
cat("MSE:\t\t", mse_arima_wheat_price_111, "\t", mse_arima_wheat_price_auto, "\n")
cat("RMSE:\t\t", rmse_arima_wheat_price_111, "\t", rmse_arima_wheat_price_auto, "\n")
cat("AIC:\t\t", aic_arima_wheat_price_111, "\t", aic_arima_wheat_price_auto, "\n")

############

#ARMAX Model for Wheat Price

# ARMAX Model for Wheat Price using Average- 1,0,1
armax_wheat_price_average <- Arima(combined_data$Price, xreg= combined_data$Year, order = c(1,0,1))
summary(armax_wheat_price_average)

# Generate the future years' data
future_years <- max(combined_data$Year) + (1:5)
future_data <- data.frame(Year = future_years)

# Forecast with the ARMAX model including the future values of the external regressor
forecasted_values_armax <- forecast(armax_wheat_price_average, xreg=future_data$Year, h=5)

# Plotting Fitted vs Actual Prices for ARMAX model with actual values for comparison
plot(combined_data$Year, combined_data$Price, main="Actual vs Fitted Prices for ARMAX Model", 
     type="o", xlab="Year", ylab="Price", col="brown", pch=16, xaxt="n", las=1)
lines(combined_data$Year, fitted(armax_wheat_price_average), col="orange", type="o", pch=16)# Add fitted values
axis(1, at=seq(min(combined_data$Year), max(combined_data$Year), by=2), las=2)# Add custom x-axis at 2-year intervals
legend("topleft", legend=c("Actual", "Fitted"), col=c("brown", "orange"), lty=1, pch=16)# Add a legend for clarity

# Convert 'Year' to a numeric value if it's not already
combined_data$Year <- as.numeric(as.character(combined_data$Year))

# Create a time series object for the actual data
historical_ts <- ts(combined_data$Price, start=min(combined_data$Year), end=max(combined_data$Year), frequency=1)

# Combine historical and forecasted data into one time series object
# Ensure the forecast starts after the last historical year
forecast_years <- seq(max(combined_data$Year) + 1, by=1, length.out=5)
forecast_ts <- ts(forecasted_values_armax$mean, start=max(forecast_years) - 4, frequency=1)

# Get the years for the historical data
historical_years <- combined_data$Year

# Generate the future years based on the forecast horizon
future_years <- seq(max(historical_years) + 1, by = 1, length.out = length(forecasted_values_armax$mean))

# Combine historical and forecasted data
all_years <- c(combined_data$Year, future_years)
all_prices <- c(combined_data$Price, forecasted_values_armax$mean)

# Combine your historical and forecasted data into a single data frame
combined_df <- data.frame(Year = c(all_years, future_years),
                          Price = c(all_prices, forecasted_values_armax$mean),
                          Type = c(rep("Historical", length(all_years)), rep("Forecasted", length(future_years))))

# Create the plot as a line graph with points using ggplot2
ggplot(data = combined_df, aes(x = Year, y = Price, color = Type, group = Type)) +
  geom_point() +
  geom_line() +  # Add this line to connect the data points
  labs(x = "Year", y = "Price", title = "ARMAX Model Forecast for Wheat Price") +
  scale_color_manual(values = c("brown", "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  guides(color = guide_legend(title = "Data")) +
  scale_x_continuous(breaks = seq(from = min(all_years), to = max(future_years), by = 2))

# Residuals of ARMAX Model using Average Price
residuals_wheat_price_average <- residuals(armax_wheat_price_average)

# Calculate MSE and RMSE for ARMAX model
mse_armax <- mean(residuals_wheat_price_average^2)
rmse_armax <- sqrt(mse_armax)

# AIC for ARMAX Model using Average Price
aic_armax_wheat_price_average <- AIC(armax_wheat_price_average)

# ACF and PACF for ARMAX model residuals
Acf(residuals_wheat_price_average, main="ACF for ARMAX Model Residuals")
Pacf(residuals_wheat_price_average, main="PACF for ARMAX Model Residuals")

# Print out the metrics for comparison
cat("ARMAX Model Metrics:\n")
cat("MSE:", mse_armax, "\n")
cat("RMSE:", rmse_armax, "\n")
cat("AIC:", aic_armax_wheat_price_average, "\n\n")

#######

# Auto ARIMA for Price 
# Convert Price to a ts object for ARIMA modeling
historical_data_ts <- ts(combined_data$Price, start = min(combined_data$Year), frequency = 1)

# Fit the ARIMA model
auto_arima_wheat_price <- auto.arima(historical_data_ts, xreg = combined_data$Year)

# Prepare future data
#future_years <- max(combined_data$Year) + (1:5)
#future_data <- data.frame(Year = future_years)

# Forecast the next 5 years
forecasted_values <- forecast(auto_arima_wheat_price, xreg = future_data$Year, h = 5)

# Data for plotting
historical_df <- data.frame(Year = as.numeric(time(historical_data_ts)), Price = as.numeric(historical_data_ts))
forecast_df <- data.frame(Year = future_years, Price = forecasted_values$mean)

# Plot
ggplot() +
  geom_line(data = historical_df, aes(x = Year, y = Price, color = "Historical"), linewidth = 0.5) +
  geom_point(data = historical_df, aes(x = Year, y = Price, color = "Historical")) +
  geom_line(data = forecast_df, aes(x = Year, y = Price, color = "Forecasted"), linewidth = 0.5) +
  geom_point(data = forecast_df, aes(x = Year, y = Price, color = "Forecasted")) +
  labs(title = "Wheat Price Forecast", x = "Year", y = "Price") +
  scale_color_manual("", 
                     breaks = c("Historical", "Forecasted"),
                     values = c("Historical" = "brown", "Forecasted" = "orange")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(from = min(combined_data$Year), to = max(future_years), by = 2))


#######
#CHECK SEASONALITY-data is not seasonal!!!!!!!!!!!!!

# Convert your 'Year' column to a Date or POSIXct object if it's not already
#combined_data$Date <- as.Date(as.character(combined_data$Year), format="%Y")
#ts_wheat_price <- ts(combined_data$Price, frequency=1)

# Perform a seasonal decomposition using STL (Seasonal and Trend decomposition using Loess)
#decomp <- stl(ts_wheat_price, s.window="periodic")

# Plot the seasonal decomposition
#plot(decomp)

# Examine ACF and PACF plots to look for seasonality
#####
Acf(ts_wheat_price, main="ACF of Wheat Price")
Pacf(ts_wheat_price, main="PACF of Wheat Price")

############

# Generalized Bass Model for Wheat Price
ggm_model <- GGM(combined_data$Price, prelimestimates = c(5.161303e+03, 0.001, 0.01, 2.672597e-02, 1.222895e-01))
summary(ggm_model)

# Predictions from GGM Model
predicted_ggm <- predict(ggm_model, newx = c(1:100))
instant_ggm_predictions <- make.instantaneous(predicted_ggm)

# Fitted Values from GGM Model
fitted_ggm <- fitted(ggm_model)
instant_fitted_ggm <- make.instantaneous(fitted_ggm)

#ARIMA with External Covariate from GGM for Wheat Price

# Fit an ARIMA model with GGM fitted values as external covariate.
arima_with_ggm_covariate <- Arima(ts_wheat_price, order = c(1, 0, 1),
                                  seasonal = list(order = c(1, 0, 1), period = 3),
                                  xreg = instant_fitted_ggm)

# Summarize the ARIMA model
summary(arima_with_ggm_covariate)

# Extract the fitted values from the ARIMA model
fitted_arima_with_ggm <- fitted(arima_with_ggm_covariate)

# Get the last year of the actual data to know where to start forecasting
last_year <- max(combined_data$Year)

# Generate a new sequence for the forecasted years
forecast_years <- seq(last_year + 1, by = 1, length.out = 5)

# Assuming the mapping between newx and Year is direct and starts from the first year in your dataset
newx_years <- seq(from = min(combined_data$Year), by = 1, length.out = 100)

# Subset the predicted GGM values for the forecast years
future_instant_ggm <- instant_ggm_predictions[(forecast_years - min(newx_years) + 1)]

# Forecast using the ARIMA model with the GGM covariate
forecast_arima_with_ggm <- forecast(arima_with_ggm_covariate, h = 5, xreg = future_instant_ggm)

# Calculate AIC, RMSE, and MSE for the ARIMA model with the GGM covariate
aic_arima_with_ggm <- AIC(arima_with_ggm_covariate)
rmse_arima_with_ggm <- sqrt(mean(residuals(arima_with_ggm_covariate)^2))
mse_arima_with_ggm <- mean(residuals(arima_with_ggm_covariate)^2)

# Print the metrics
cat("ARIMA with GGM Covariate Model Metrics:\n")
cat("AIC:", aic_arima_with_ggm, "\n")
cat("RMSE:", rmse_arima_with_ggm, "\n")
cat("MSE:", mse_arima_with_ggm, "\n")

# Plot ACF and PACF for residuals to check for any remaining patterns
Acf(residuals(arima_with_ggm_covariate), main="ACF for ARIMA with GGM Covariate Model")
Pacf(residuals(arima_with_ggm_covariate), main="PACF for ARIMA with GGM Covariate Model")

# Now we plot everything
# Plotting GGM Model Predictions
plot(combined_data$Price, type = "b", xlab = "Year", ylab = "Wheat Price", pch = 16, lty = 3, cex = 0.6, xlim = c(1, 60))
lines(instant_ggm_predictions, lwd = 2, col = 2)

# Plot the actual data with ARIMA and GGM fitted values and forecasts
plot(combined_data$Year, combined_data$Price, type = "b", xlab = "Year", ylab = "Wheat Price", pch = 16, col = "black", xlim = c(min(combined_data$Year), max(forecast_years)), ylim = range(c(combined_data$Price, forecast_arima_with_ggm$mean)), xaxt = "n")
axis(1, at = seq(from = floor(min(combined_data$Year)), to = ceiling(max(forecast_years)), by = 2), las = 2)
lines(combined_data$Year, instant_fitted_ggm, col = "orange", lty = 2)
lines(combined_data$Year, fitted_arima_with_ggm, col = "brown", lty = 1)
lines(forecast_years, forecast_arima_with_ggm$mean, col = "green", lty = 1)
legend("topleft", cex = 0.75, legend = c("Actual", "Fitted (ARIMA)", "Fitted (GGM)", "Forecast"), col = c("black", "brown", "orange", "green"), lty = c(1, 1, 2, 1), pch = 5)


# Calculate residuals from the GGM model
residuals_ggm <- combined_data$Price - instant_fitted_ggm

# Calculate RMSE for GGM Model
rmse_ggm <- sqrt(mean(residuals_ggm^2))

# Calculate MSE for GGM Model
mse_ggm <- mean(residuals_ggm^2)

# Print the GGM Model metrics
cat("GGM Model Metrics:\n")
cat("RMSE:", rmse_ggm, "\n")
cat("MSE:", mse_ggm, "\n")

############
# Plot ACF and PACF for residuals to check for any remaining patterns
Acf(residuals(arima_with_ggm_covariate), main="ACF for ARIMA with GGM Covariate Model")
Pacf(residuals(arima_with_ggm_covariate), main="PACF for ARIMA with GGM Covariate Model")

#####################

# GBM with Rectangular Shock for Wheat Price
gbm_rectangular_shock <- GBM(combined_data$Price, shock = "rett", nshock = 1, prelimestimates = c(5.161303e+03, 2.672597e-02, 1.222895e-01, 4, 16, 0.4))
summary(gbm_rectangular_shock)

# Predictions from GBM Rectangular Shock Model
predicted_gbm_rect_shock <- predict(gbm_rectangular_shock, newx = c(1:60))
instant_gbm_rect_shock <- make.instantaneous(predicted_gbm_rect_shock)

# Plotting Rectangular Shock Model Predictions
plot(combined_data$Price, type = "b", xlab = "Quarter", ylab = "Wheat Price",pch = 16, lty = 3, cex = 0.6, xlim = c(1, 60))
lines(instant_gbm_rect_shock, lwd = 2, col = 2)

# GBM with Exponential Shock for Wheat Price
gbm_exponential_shock <- GBM(combined_data$Price, shock = "exp", nshock = 1, prelimestimates = c(5.161303e+03, 2.672597e-02, 1.222895e-01, 2, -0.03, 0.07))
summary(gbm_exponential_shock)

# Predictions from GBM Exponential Shock Model
predicted_gbm_exp_shock <- predict(gbm_exponential_shock, newx = c(1:60))
instant_gbm_exp_shock <- make.instantaneous(predicted_gbm_exp_shock)

# Plotting Exponential Shock Model Predictions
plot(combined_data$Price, type = "b", xlab = "Quarter", ylab = "Wheat Price",pch = 16, lty = 3, cex = 0.6, xlim = c(1, 60))
lines(instant_gbm_exp_shock, lwd = 2, col = 2)

# residuals for both models
residuals_rect <- combined_data$Price - instant_gbm_rect_shock
residuals_exp <- combined_data$Price - instant_gbm_exp_shock

# RMSE calculation
rmse_rect <- sqrt(mean(residuals_rect^2, na.rm = TRUE))
rmse_exp <- sqrt(mean(residuals_exp^2, na.rm = TRUE))

# MSE calculation
mse_rect <- mean(residuals_rect^2, na.rm = TRUE)
mse_exp <- mean(residuals_exp^2, na.rm = TRUE)


# Print the results
cat("Rectangular Shock Model Metrics:\n")
cat("RMSE:", rmse_rect, "\n")
cat("MSE:", mse_rect, "\n")


cat("Exponential Shock Model Metrics:\n")
cat("RMSE:", rmse_exp, "\n")
cat("MSE:", mse_exp, "\n")

####Generalized Additive Models (GAM)

# Loading necessary libraries
if (!require("mgcv")) install.packages("mgcv")
library(mgcv)

# Fit the GAM model for Wheat Price
gam_model_wheatprice <- gam(Price ~ s(Year), data = combined_data)
summary(gam_model_wheatprice)

# Calculate predictions for the GAM model for Wheat Price
predictions_gam_wheatprice <- predict(gam_model_wheatprice, newdata = combined_data)

# Plotting GAM Model for Wheat Price
ggplot(combined_data, aes(x = Year, y = Price)) +
  geom_point() +
  geom_line(aes(y = predictions_gam_wheatprice), color = "orange") +
  labs(title = "GAM Model Fit for Wheat Price",
       x = "Year", y = "Price") +
  theme_minimal()

# Calculate MSE and RMSE for the GAM model for Wheat Price
mse_gam_wheatprice <- mean((combined_data$Price - predictions_gam_wheatprice)^2)
rmse_gam_wheatprice <- sqrt(mse_gam_wheatprice)

# Calculate AIC for the GAM model
aic_gam_wheatprice <- AIC(gam_model_wheatprice)

# Print RMSE, MSE and AIC for the GAM model
cat("MSE for GAM model:", mse_gam_wheatprice, "\n")
cat("RMSE for GAM model:", rmse_gam_wheatprice, "\n")
cat("AIC for GAM model:", aic_gam_wheatprice, "\n")

# Forecasting future values
# Assume you want to predict for the next 5 years
# Forecasting future values
years_to_forecast <- (max(combined_data$Year) + 1):(max(combined_data$Year) + 5)
new_data <- data.frame(Year = years_to_forecast)
predictions_future <- predict(gam_model_wheatprice, newdata = new_data)

new_data$Price <- predictions_future

# Plotting the observed and forecasted values
ggplot() +
  geom_point(data = combined_data, aes(x = Year, y = Price)) +
  geom_line(data = combined_data, aes(x = Year, y = predictions_gam_wheatprice), color = "orange") +
  geom_point(data = new_data, aes(x = Year, y = Price), color = "brown", shape = 1) +
  geom_line(data = new_data, aes(x = Year, y = Price), color = "brown", linetype = "dashed") +
  labs(title = "GAM Model Fit and Forecast for Wheat Price",
       x = "Year", y = "Price") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$Year), max(new_data), by = 1))
#######

# ###Local Regression Model 
# 
# plot(x,y)
# sm.regression(x, y,   h = 10, add = T)
# 
# #increase the number of points where the function is calculated
# sm.regression(x, y,   h = 10, add = T, ngrid=200, col=2)
# 
# 
# 
# #We try with different values for h
# #increasing the value of h we have a smoother function, a lower value of h implies a jumpy function
# 
# sm.regression(x, y,   h = 30, ngrid=200, col=1)
# sm.regression(x, y,   h = 50, add = T, ngrid=200, col=2)
# sm.regression(x, y,   h = 5,  add = T, ngrid=200, col=3)
# sm.regression(x, y,   h = 3,  add = T, col=3, ngrid=200)
# 
# 
# #We add variability bands
# sm.regression(x, y,   h = 30, ngrid=200, display="se")
# 
# #########
# #Loess (no library required, default tool of R)
# plot(x, y, xlab="engine size", ylab="distance")
# lo1 <- loess.smooth(x,y) 
# #default span= 0.75
# lines(lo1)
# # we try with other smoothing parameters 'span' 
# lo2 <- loess.smooth(x,y,span=0.9)
# lines(lo2,col=2)
# # "jumpier"
# lo3 <- loess.smooth(x,y,span=0.4)
# lines(lo3,col=3)
# 
# 
# #another way to perform loess, performs directly the plot
# #default span= 2/3
# 
# scatter.smooth(x,y) 
# scatter.smooth(x,y, span=0.3)
# scatter.smooth(x,y, span=0.3, evaluation=200)
# # (two possibilities for regression)



###########

#ML MODELS
########## GBM for Wheat - Using gbm.boosting

# Ensure combined_data is a data frame
combined_data <- as.data.frame(combined_data)

# Check the structure of combined_data
str(combined_data)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(combined_data), 0.8 * nrow(combined_data))
data_train <- combined_data[train_indices, ]
data_test <- combined_data[-train_indices, ]

# Check dimensions of data_train and data_test
cat("Dimensions of data_train:", dim(data_train), "\n")
cat("Dimensions of data_test:", dim(data_test), "\n")
# Check the number of rows in the training data
cat("Number of rows in training data:", nrow(data_train), "\n")

# Fit the GBM model with adjusted parameters
gbm_model <- gbm(
  Price ~ . -Year,
  data = data_train,
  distribution = "gaussian",
  n.trees = 1500,
  interaction.depth = 1,
  shrinkage=0.01,
  bag.fraction = 0.5, # Lowered the bag fraction
  n.minobsinnode = 2  # Decreased the minimum observations in nodes
)


# Check the summary of the model
gbm_summary<-summary(gbm_model)

# Convert the GBM summary to a data frame for plotting with ggplot2
gbm_importance_df <- data.frame(
  Variable = gbm_summary$var,
  Importance = gbm_summary$rel.inf
)

# Create the ggplot
ggplot(gbm_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  labs(title = "Variable Importance", x = "Relative Importance", y = "Variables") +
  theme_minimal()

# Predictions on the test set using the best number of trees
yhat_boost <- predict(gbm_model, newdata = data_test, n.trees = 100)

# Calculate MSE for predictions
mse_boost = mean((combined_data$Price - yhat_boost)^2)

# Calculate RMSE for predictions
rmse_boost = sqrt(mse_boost)

# Calculate AIC for the GBM model - AIC is not directly applicable to GBM models but can be computed for comparison
n = nrow(data_test)  # Number of observations
p = gbm_model$finalModel$ntree  # Number of parameters (number of trees as proxy)

# Print the metrics
print("GBM boosting errors")
cat("MSE:", mse_boost, "\n")
cat("RMSE:", rmse_boost, "\n")

#######

# library(randomForest)
# library(caret) # for train/test split and RMSE
# 
# # Create lagged features
# number_of_lags <- 10
# for (i in 1:number_of_lags) {
#   lag_name <- paste("lag", i, sep="_")
#   combined_data[[lag_name]] <- stats::lag(combined_data$Price, -i)
# }
# 
# # Remove rows with NA values created by lagging
# combined_data <- na.omit(combined_data)
# 
# # Select features and target
# target_column <- "Price"
# features <- setdiff(names(combined_data), target_column)
# X <- combined_data[, features]
# y <- combined_data[, target_column, drop = FALSE]
# 
# # Train/test split
# split_index <- round(nrow(combined_data) * 0.8)
# X_train <- X[1:split_index, ]
# y_train <- y[1:split_index, ]
# X_test <- X[(split_index + 1):nrow(combined_data), ]
# y_test <- y[(split_index + 1):nrow(combined_data), ]
# 
# # If y_train is a data frame, convert it to a numeric vector
# y_train <- as.numeric(unlist(y_train))
# y_test <- as.numeric(unlist(y_test))
# 
# # Train the model
# set.seed(42) # for reproducibility
# rf_model <- randomForest(x = X_train, y = y_train, ntree = 40)
# 
# # Make predictions
# predictions <- predict(rf_model, X_test)
# 
# # Calculate MSE
# mse <- mean((predictions - y_test) ^ 2)
# cat("Test MSE:", mse, "\n")
# 
# # Evaluate the model
# rmse <- caret::RMSE(predictions, y_test)
# cat("Test RMSE:", rmse, "\n")
# 
# # Note: For actual forecasting beyond the available data,
# 
# # Plot the actual vs predicted values
# plot(y_test, type = 'l', col = 'orange', xlab = "Index", ylab = " Price", main = "Actual vs Predicted")
# lines(predictions, col = 'brown')
# legend("topright", legend = c("Actual", "Predicted"), col = c("orange", "brown"), lty = 1)
# 
# # plot them separately to see them more clearly
# # Plot the actual values
# plot(y_test, type = 'l', col = 'orange', xlab = "Index", ylab = "Price", main = "Actual Values")
# 
# # Plot the predicted values
# plot(predictions, type = 'l', col = 'brown', xlab = "Index", ylab = "Price", main = "Predicted Values")

#KNN  
install.packages("FNN")

# Load necessary libraries
library(FNN)

library(class)
library(dplyr)

# Assuming combined_data is already loaded

# Convert 'Year' and 'Price' to numeric if they are not already
combined_data$Year <- as.numeric(as.character(combined_data$Year))
combined_data$Price <- as.numeric(as.character(combined_data$Price))

# Normalizing the data
scale_data <- function(data) {
  (data - min(data)) / (max(data) - min(data))
}

#combined_data$Year <- scale_data(combined_data$Year)
combined_data$Price <- scale_data(combined_data$Price)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(combined_data), 0.8 * nrow(combined_data))
data_train <- combined_data[train_indices, ]
data_test <- combined_data[-train_indices, ]

# k Value for kNN
k_value <- 5  # Adjust this based on your requirement

# Train the kNN model for regression
model_knn <- knn.reg(train = data_train[, "Year", drop = FALSE],
                     test = data_test[, "Year", drop = FALSE],
                     y = data_train$Price,
                     k = k_value)

# Predictions
predictions <- model_knn$pred

# Calculate MSE and RMSE
mse <- mean((data_test$Price - predictions)^2)
rmse <- sqrt(mse)

# Output MSE and RMSE
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))

# Create a new data frame for plotting
plot_data <- data.frame(
  Year = data_test$Year,
  ActualPrice = data_test$Price,
  PredictedPrice = predictions
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Year)) +
  geom_point(aes(y = ActualPrice), color = "blue", alpha = 0.6) +  # Actual values
  geom_line(aes(y = PredictedPrice), color = "red") +  # Predicted values
  labs(title = "kNN Regression: Actual vs Predicted Prices",
       x = "Year (normalized)", y = "Price (normalized)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), length.out = 5)) +
  scale_y_continuous(breaks = seq(min(plot_data$ActualPrice, plot_data$PredictedPrice),
                                  max(plot_data$ActualPrice, plot_data$PredictedPrice),
                                  length.out = 5))

#####BAR PLOTS
library(ggplot2)
library(reshape2)

# Prepare the data
model_metrics <- data.frame(
  Model = c("Linear", "ETS", "ARIMA(1,1,1)", "Auto ARIMA", "ARMAX", "ARIMA w/ GGM", "GGM", "Rectangular Shock", "Exponential Shock", "GAM", "GBM Boosting", "Random Forest"),
  MSE = c(20436.07, 139187.4, 10383.46, 8590.499, 7820.115, 6069.376, 18667.81, 109708.7, 89687.9, 2611.592, 27057.62, 6792.969),
  RMSE = c(142.9548, 373.0783, 101.8992, 92.68494, 88.43141, 77.9062, 136.6302, 331.2231, 299.4794, 51.10374, 164.492, 82.41947),
  AIC = c(261.2587, 255.9393, 237.3321, 248.0749, 247.7778, 247.9948, NA, NA, NA, 234.4349, NA, NA)  # NA for models where AIC is not applicable
)

# Melt the data for easy plotting
model_metrics_melted <- melt(model_metrics, id.vars = "Model")

# Plot
ggplot(model_metrics_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(title = "Comparison of Forecasting Models", y = "Value", x = "Model") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")

####DOTS

library(ggplot2)
library(reshape2)

# Prepare the data
model_metrics <- data.frame(
  Model = c("Linear", "ETS", "ARIMA(1,1,1)", "Auto ARIMA", "ARMAX", "ARIMA w/ GGM", "GGM", "Rectangular Shock", "Exponential Shock", "GAM", "GBM Boosting", "Random Forest"),
  MSE = c(20436.07, 139187.4, 10383.46, 8590.499, 7820.115, 6069.376, 18667.81, 109708.7, 89687.9, 2611.592, 27057.62, 6792.969),
  RMSE = c(142.9548, 373.0783, 101.8992, 92.68494, 88.43141, 77.9062, 136.6302, 331.2231, 299.4794, 51.10374, 164.492, 82.41947),
  AIC = c(261.2587, 255.9393, 237.3321, 248.0749, 247.7778, 247.9948, NA, NA, NA, 234.4349, NA, NA)  # NA for models where AIC is not applicable
)

# Melt the data for easy plotting
model_metrics_melted <- melt(model_metrics, id.vars = "Model")

# Create the dot plot
ggplot(model_metrics_melted, aes(x = Model, y = value, color = variable)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Comparison of Forecasting Models", y = "Value", x = "Model") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")

###

library(ggplot2)
library(reshape2)

# Prepare the data
model_metrics <- data.frame(
  Model = c("Linear", "ETS", "ARIMA(1,1,1)", "Auto ARIMA", "ARMAX", "ARIMA w/ GGM", "GGM", "Rectangular Shock", "Exponential Shock", "GAM", "GBM Boosting", "Random Forest"),
  MSE = c(20436.07, 139187.4, 10383.46, 8590.499, 7820.115, 6069.376, 18667.81, 109708.7, 89687.9, 2611.592, 27057.62, 6792.969),
  RMSE = c(142.9548, 373.0783, 101.8992, 92.68494, 88.43141, 77.9062, 136.6302, 331.2231, 299.4794, 51.10374, 164.492, 82.41947),
  AIC = c(261.2587, 255.9393, 237.3321, 248.0749, 247.7778, 247.9948, NA, NA, NA, 234.4349, NA, NA)
)

# Melt the data for easy plotting
model_metrics_melted <- melt(model_metrics, id.vars = "Model")

# Create the dot plot with labels
ggplot(model_metrics_melted, aes(x = Model, y = value, label = Model, color = variable)) +
  geom_point(size = 3) +
  geom_text(aes(label = Model), vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "Comparison of Forecasting Models", y = "Value", x = "") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  )

####

library(ggplot2)
library(reshape2)

# Prepare the data
model_metrics <- data.frame(
  Model = c("Linear", "ETS", "ARIMA(1,1,1)", "Auto ARIMA", "ARMAX", "ARIMA w/ GGM", "GGM", "Rectangular Shock", "Exponential Shock", "GAM", "GBM Boosting", "Random Forest"),
  MSE = c(20436.07, 139187.4, 10383.46, 8590.499, 7820.115, 6069.376, 18667.81, 109708.7, 89687.9, 2611.592, 27057.62, 6792.969),
  RMSE = c(142.9548, 373.0783, 101.8992, 92.68494, 88.43141, 77.9062, 136.6302, 331.2231, 299.4794, 51.10374, 164.492, 82.41947),
  AIC = c(261.2587, 255.9393, 237.3321, 248.0749, 247.7778, 247.9948, NA, NA, NA, 234.4349, NA, NA)
)

# Separate plots for each metric

# Plot for MSE
ggplot(subset(model_metrics, !is.na(MSE)), aes(x = Model, y = MSE, label = Model)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "MSE Comparison of Forecasting Models", y = "MSE", x = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Plot for RMSE
ggplot(subset(model_metrics, !is.na(RMSE)), aes(x = Model, y = RMSE, label = Model)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "RMSE Comparison of Forecasting Models", y = "RMSE", x = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Plot for AIC
ggplot(subset(model_metrics, !is.na(AIC)), aes(x = Model, y = AIC, label = Model)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "AIC Comparison of Forecasting Models", y = "AIC", x = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


