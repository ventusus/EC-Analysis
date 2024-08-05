

# install libraries

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

amazon_sale_rep_data <- read.csv("Amazon Sale Report.csv")
int_sale_rep_data <- read.csv("International sale Report.csv")
sale_rep_data  <- read.csv("Sale Report.csv")


# -------------------------------- Data cleaning ---------------------------------------
# Amazon Sale Report
head(amazon_sale_rep_data)

# Find if there are missing values
colSums(is.na(amazon_sale_rep_data))

# There are obviously missing values in some columns such as currency, Courier.status etc.
# They're probabbly empty strings

# Replace empty string values with NA
amazon_sale_rep_data[amazon_sale_rep_data == ''] <- NA


# Now we can obtain the real missing values
colSums(is.na(amazon_sale_rep_data))

# Remove redundant columns that are idea just for references or already implied
# (ship country and currency we already know it's a dataset for india)
amazon_sale_rep_data <- select(amazon_sale_rep_data, -index, -promotion.ids, -Unnamed..22, -fulfilled.by, -ship.country, -currency)
head(amazon_sale_rep_data)

# Convert Date to Date datatype
amazon_sale_rep_data <- amazon_sale_rep_data %>%
  mutate(Date = as.Date(Date, format="%m-%d-%y"))

# Now has 128975 observations 18 columns
# Datatype seems appropriate as well
str(amazon_sale_rep_data)

# Remove duplicate values (6 of them)
amazon_sale_rep_data <- amazon_sale_rep_data %>% distinct()
str(amazon_sale_rep_data)

# omit NA values
amazon_sale_rep_data <- na.omit(amazon_sale_rep_data)
nrow(amazon_sale_rep_data)

# 116013 observations left
amazon_cleaned <- amazon_sale_rep_data


# Summary statistics of cleaned data
summary(amazon_cleaned)

# exporting amazon cleaned data
write.csv(amazon_cleaned, "amazon_cleaned.csv", row.names = TRUE)

# Sale Report

summary(sale_rep_data)

# Remove index (redundant)
sale_rep_data <- select(sale_rep_data, -index)

# Lots of empty rows at the bottom, datatype looks fine
# 9271 observations
str(sale_rep_data)

# Check for NA values
colSums(is.na(sale_rep_data))

# Not much NA detected, means they're empty spaces so convert them to NA
sale_rep_data[sale_rep_data == ""] <- NA
sale_rep_data <- na.omit(sale_rep_data)

# 9188 observations now
str(sale_rep_data)

# check for duplicates and remove them (3 found)
sale_rep_data <- sale_rep_data %>% distinct()
str(sale_rep_data)

# Remove any sizes that are not in the range of XS - XXXL to remove anomalies and for better and more specific analysis
allowed_sizes <- c("XS", "S", "M", "L", "XL", "XXL", "XXXL")
sale_rep_data <- sale_rep_data %>% filter(Size %in% allowed_sizes)

sale_cleaned <- sale_rep_data

# Summary statistics of cleaned data
summary(sale_cleaned)

# exporting sales cleaned data
write.csv(sale_cleaned, "sale_cleaned.csv", row.names = TRUE)

# International data

# The data is empty empty from 18637 on wards to 19676 and the columns are in wrong orders 19678 onwards
# We can split the data into two parts and rejoin them later

correct_part <- int_sale_rep_data %>% slice(1:18635)
head(correct_part)

# Exclude the last row because its style and sku is filled with nonsense
wrong_part <- int_sale_rep_data %>% slice(19678:nrow(int_sale_rep_data)-1)
head(wrong_part)

# Rearrange the columns correctly by creating a new dataframe
wrong_part_corrected <- data.frame(index = wrong_part$index, DATE = wrong_part$Months, Months = wrong_part$CUSTOMER, CUSTOMER = wrong_part$DATE, Style = wrong_part$Style, SKU = wrong_part$SKU, Size = NA, PCS = wrong_part$Size, RATE = wrong_part$PCS, GROSS.AMT = wrong_part$RATE)
head(wrong_part_corrected)

#Fill up the Size column by extracting the last few digits from SKU using RegEx
wrong_part_corrected <- wrong_part_corrected %>% mutate(Size = ifelse(grepl("-", SKU), sub(".*-([A-Za-z]+)$", "\\1", SKU), ""))

# Rejoin the dataframe
int_sale_rep_data <- bind_rows(correct_part, wrong_part_corrected)


# View the updated dataframe with the columns in correct positions
head(int_sale_rep_data)

# There are still a lot of rows with incomplete data, this is due to anomalies or the data entry for SKU is different from ordinary resulting the sub function to extract incorrect parts
# Because there are too many combinations of errors, we will treat them as anomalies and remove anything that aren't size ranging from (XS to XXXL)

allowed_sizes <- c("XS", "S", "M", "L", "XL", "XXL", "XXXL")
int_sale_rep_data <- int_sale_rep_data %>% filter(Size %in% allowed_sizes)


str(int_sale_rep_data)

# Check for NA values and remove them
int_sale_rep_data[int_sale_rep_data == ""] <- NA
colSums(is.na(int_sale_rep_data))

int_sale_rep_data <- na.omit(int_sale_rep_data)

str(int_sale_rep_data)

# Find out which size has the most count
sizes_chart <- int_sale_rep_data %>%
  group_by(Size)%>%
  summarise(Count = n(), .groups = 'drop')
print(sizes_chart)

int_sale_rep_data <- int_sale_rep_data %>% distinct() # Check for duplicates and remove if any is found (None)

# Remove index (redundant)
int_sale_rep_data <- select(int_sale_rep_data, -index)

# Convert Date to Date format
int_sale_rep_data <- int_sale_rep_data %>%
  mutate(DATE = as.Date(DATE, format="%m-%d-%y"))

# Convert gross.amt to numeric
int_sale_rep_data <- int_sale_rep_data %>%
  mutate(`GROSS.AMT` = as.numeric(`GROSS.AMT`))

# Convert PCS to numeric
int_sale_rep_data <- int_sale_rep_data %>%
  mutate(`PCS` = as.numeric(`PCS`))

international_cleaned <- int_sale_rep_data

# Summary statistics of cleaned data
summary(international_cleaned)

# exporting international cleaned data
write.csv(international_cleaned, "international_cleaned.csv", row.names = TRUE)

# ------------------------------------------------------- Problem statements --------------------------------------------------------------------------


# Problem Statement 1: Comparing the profitability of one-time vs repeat customers

# International sale report consist of sales from 5 June 2021 to 31 March 2022
print(max(international_cleaned$DATE))
print(min(international_cleaned$DATE))

# Identify unique customers and their purchase histories
customer_purchases <- international_cleaned %>%
  group_by(CUSTOMER) %>%
  summarise(
    First_Purchase = min(DATE),
    Last_Purchase = max(DATE),
    Days_Since_Last_Purchase = max(DATE) - min(DATE),
    Purchase_Count = n_distinct(DATE), # How many purchases made on different dates
    Total_purchase_frequency = n(), # Total number of items purchased
    Total_amount_spent = sum(GROSS.AMT) # Total amount spent (INR) per customer
  )
head(customer_purchases)

total_customers <- nrow(customer_purchases) # 123 unqiue customers
print(paste("Total unique customers:", total_customers))

repeat_customers <- customer_purchases %>% filter(Purchase_Count > 1)
head(repeat_customers)
# 41 repeating customers
total_repeat_customers <- nrow(repeat_customers)
print(paste("Total repeat customers:", total_repeat_customers))

one_time_customers <- customer_purchases %>% filter(Purchase_Count == 1)
print(one_time_customers)
# 82 one time customers
total_one_time_customers <- nrow(one_time_customers)
print(paste("Total one-time customers:", total_one_time_customers))

# 82/123 customers don't come back (66.7%)

# Do repeated customer buy things in bulk?
head(international_cleaned)

# Print the average quantity per purchase for one time customers
one_time_customers_avg_qty <- international_cleaned %>%
  filter(CUSTOMER %in% one_time_customers$CUSTOMER) %>%
  summarise(
    Avg_Qty_Per_Purchase = mean(PCS)
  )
print("One-time Customers Average Quantity Per Purchase")
print(one_time_customers_avg_qty) # 1.24

# Calculate average quantity per purchase for repeat customers
repeat_customers_avg_qty <- international_cleaned %>%
  filter(CUSTOMER %in% repeat_customers$CUSTOMER) %>%
  group_by(CUSTOMER) %>%
  summarise(
    Avg_Qty_Per_Purchase = mean(PCS)
  )
print("Repeat Customers Average Quantity Per Purchase")
print(repeat_customers_avg_qty %>% summarise(Avg_Qty_Per_Purchase = mean(Avg_Qty_Per_Purchase))) # 1.26


repeat_customers$metric <- repeat_customers$Total_amount_spent / repeat_customers$Total_purchase_frequency
one_time_customers$metric <- one_time_customers$Total_amount_spent / one_time_customers$Total_purchase_frequency
#  Calculate the average of the metrics
average_repeat <- mean(repeat_customers$metric, na.rm = TRUE)
average_one_time <- mean(one_time_customers$metric, na.rm = TRUE)

# Create a data frame for plotting
average_data <- data.frame(
  Customer_Type = c("Repeat Customers", "One-time Customers"),
  Average_Metric = c(average_repeat, average_one_time)
)

# Plot the bar plot using ggplot2
ggplot(average_data, aes(x = Customer_Type, y = Average_Metric, fill = Customer_Type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average Spending Per Purchase Based on Customer Types",
       x = " ",
       y = "Average Spending Per Purchase") +
  theme_minimal() +
  scale_fill_manual(values = c("Repeat Customers" = "lightblue", "One-time Customers" = "pink")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Do repeat customer really spend more than one-time customers?

# Summary statistics of the total amount spent by one-time customers and repeat customers
one_time_summary <- one_time_customers %>% select(Total_amount_spent) %>%
  summary(Total_amount_spent)
print("One-time Customers Spending Summary")
print(one_time_summary)

repeat_summary <- repeat_customers %>% select(Total_amount_spent) %>%
  summary(Total_amount_spent)
print("Repeat Customers Spending Summary")
print(repeat_summary)
head(one_time_customers)


total_spent <- data.frame(Group = c("One Time Customer", "Repeat Customer"),
                          count= c(sum(one_time_customers$Total_amount_spent),
                                   sum(repeat_customers$Total_amount_spent)))
total_spent <- total_spent %>%
  mutate(percent = paste(100*round(count/sum(count), 3), "%"))
head(total_spent)

ggplot(total_spent, aes(x="", y=count, fill=Group)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5),
            size = 8,
            color = c("white","black")) +
  coord_polar("y", start=0) +
  labs(title = "Total Spent by both Groups") + 
  scale_fill_manual(values = c("pink","lightblue")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.border = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#ebf2ff"),
        plot.background = element_rect(fill = "#ebf2ff"),
        legend.background = element_rect(fill = "#ebf2ff"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


# This shows that repeated customers spend more than one time customers

# What causes them to come back and spend more?

# Analyze the distribution of repeat customers over time
repeat_customer_trends <- repeat_customers %>%
  mutate(YearMonth = floor_date(Last_Purchase, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Repeat_Customer_Count = n()) %>%
  arrange(desc(Repeat_Customer_Count))

# Most of the customers repeated come back during January, a huge amount of customer also comes back around October 
# January for new year? October for Deepavali? These spike in increase repeat customer count may be caused by special occasions
print(repeat_customer_trends)

# Plot the trends

ggplot(repeat_customer_trends, aes(x = YearMonth, y = Repeat_Customer_Count)) +
  geom_line() +
  labs(title = "Trends of Repeat Customers Over Time", x = "Months", y = "Number of Repeat Customers") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.background = element_rect(fill = "#ebf2ff")) +
  geom_smooth(method=lm, se=FALSE, col='lightblue', size=1)

# Problem Statement 2: Analysis of revenue made on each product category

#creating revenue column
amazon_cleaned <- amazon_cleaned %>%
  mutate(Revenue = Qty * Amount)
head(amazon_cleaned)

#total revenue for each category 
total_revenue <- amazon_cleaned %>%
  group_by(Category) %>%
  summarise(Total_Revenue = sum(Revenue))
head(total_revenue)

#Counts in courier status
status_count <- amazon_cleaned %>%
  count(Courier.Status)
print(status_count)

#counts in Category
counts <- amazon_cleaned%>%
  count(Category)
print(counts)

#combine total revenue and counts 
merged_df <- merge(total_revenue, counts, by = "Category")
head(merged_df)
colnames(merged_df)[3] <- "Number of products sold"

#finding the average revenue 
merged_df <- merged_df %>%
  mutate(Average_Revenue_Per_Product = Total_Revenue / `Number of products sold`)
head(merged_df)

# Summary statistics of data
summary(amazon_cleaned)

#side by side bar chart for total rev and amount sold


melt_bar <- melt(merged_df[,c('Category','Total_Revenue',
                              'Number of products sold')],id.vars = 1)
head(melt_bar)
merged_df2 <- merged_df %>%
  select(Category, Average_Revenue_Per_Product)

melt_bar2 <- left_join(melt_bar, merged_df2, by = "Category")
head(melt_bar2)

ggplot(data=melt_bar2, aes(x= reorder(Category, value), y=value, fill=variable, group=variable)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) +
  scale_fill_manual(values = c("pink","lightblue")) +
  scale_color_manual(values=c("blue")) +
  scale_y_log10() +
  geom_line(aes(x = reorder(Category, value), y = Average_Revenue_Per_Product, color = "Average_Revenue_Per_Product"), group = 1) +
  theme_bw() +
  labs(title = "Total Revenue vs Quantity Sold for each Product",
       x = "Products",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid = element_line(color = "white"),
        panel.background = element_rect(fill = "#ebf2ff"),
        legend.position = c(0.16,0.88),
        legend.background = element_rect(fill = "#ebf2ff"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.spacing.y = unit(-0.2, "cm"))

# Problem Statement 3: Analysis on popularity of category by colour
#rename SKU
sale_cleaned <- sale_cleaned %>%
  rename(SKU = SKU.Code)

#join on amazon dataset and sales report
combined_dataset <- left_join(amazon_cleaned, sale_cleaned, by = "SKU")
head(combined_dataset)

#remove unwanted columns 
cleared_dataset <- combined_dataset[,c("Color", "Category.x")]
cleaned_dataset <- cleared_dataset %>%
  filter(!is.na(Color))
head(cleaned_dataset)

color_count <- cleaned_dataset %>%
  count(Category.x, Color)
head(color_count)

#most popular colors for kurta in descending order
kurta_counts <- cleaned_dataset %>%
  filter(Category.x == "kurta") %>%  
  count(Color) %>%                 
  arrange(desc(n))  
head(kurta_counts)

#most popular colors for blouse in descending order
blouse_counts <- cleaned_dataset %>%
  filter(Category.x == "Blouse") %>% 
  count(Color) %>%                   
  arrange(desc(n))  
head(blouse_counts)

#most popular colors for bottom in descending order
bottom_counts <- cleaned_dataset %>%
  filter(Category.x == "Bottom") %>%  
  count(Color)               
head(bottom_counts)

#most popular colors for ethic dress in descending order
ethnic_dress_counts <- cleaned_dataset %>%
  filter(Category.x == "Ethnic Dress") %>%  
  count(Color) %>%                    
  arrange(desc(n))  
head(ethnic_dress_counts)

#most popular colors for saree in descending order
saree_counts <- cleaned_dataset %>%
  filter(Category.x == "Saree") %>%  
  count(Color) %>%                   
  arrange(desc(n))  
head(saree_counts)

#most popular colors for set in descending order
set_counts <- cleaned_dataset %>%
  filter(Category.x == "Set") %>% 
  count(Color) %>%                    
  arrange(desc(n))  
head(set_counts)

#most popular colors for top in descending order
top_counts <- cleaned_dataset %>%
  filter(Category.x == "Top") %>% 
  count(Color) %>%                    
  arrange(desc(n))  
head(top_counts)

#most popular colors for western dress in descending order
western_dress_counts <- cleaned_dataset %>%
  filter(Category.x == "Western Dress") %>%  
  count(Color) %>%              
  arrange(desc(n))  
head(western_dress_counts)


## DATA VISUALISATION ##

# Create a list of all category counts with their respective category names
category_counts_list <- list(
  Kurta = kurta_counts,
  Blouse = blouse_counts,
  Bottom = bottom_counts,
  `Ethnic Dress` = ethnic_dress_counts,
  Saree = saree_counts,
  Set = set_counts,
  Top = top_counts,
  `Western Dress` = western_dress_counts
)

# Filter out NULL entries and combine the remaining data frames
all_counts <- lapply(names(category_counts_list), function(category) {
  df <- category_counts_list[[category]]
  if (!is.null(df)) {
    df <- mutate(df, Category = category)
  }
  df
}) %>% bind_rows()

# Define a mapping from specific colors to broader color categories
color_mapping <- c(
  "Blue" = "Blue", "Teal" = "Blue", "Turquoise Blue" = "Blue", "Navy Blue" = "Blue", 
  "Powder Blue" = "Blue", "Sky Blue" = "Blue", "Dark Blue" = "Blue", "Light Blue" = "Blue", 
  "Navy" = "Blue", "Turquoise" = "Blue", "TEAL BLUE" = "Blue", "NAVY" = "Blue",
  "TEAL BLUE " = "Blue",
  "Pink" = "Pink", "Light Pink" = "Pink", "Magenta" = "Pink", "Hot Pink" = "Pink", 
  "Coral Pink" = "Pink", "CORAL PINK" = "Pink",
  "Black" = "Black",
  "Green" = "Green", "Light Green" = "Green", "Dark Green" = "Green", 
  "Olive Green" = "Green", "Sea Green" = "Green", "Olive" = "Green", 
  "Turquoise Green" = "Green", "Teal Green" = "Green", "TEAL GREEN " = "Green",
  "Yellow" = "Yellow", "Gold" = "Yellow", "Light Yellow" = "Yellow", 
  "LIGHT YELLOW" = "Yellow", "Mustard" = "Yellow", "Lemon" = "Yellow", "LEMON" = "Yellow",
  "LEMON " = "Yellow",
  "Red" = "Red", "Dark Red" = "Red", "Maroon" = "Red", "Wine" = "Red",
  "White" = "White", "OFF WHITE" = "White",
  "Grey" = "Grey", "Charcoal" = "Grey",
  "Peach" = "Peach", "Coral" = "Peach", "CORAL" = "Peach", "Coral Orange" = "Peach", 
  "CORAL " = "Peach", 
  "Orange" = "Orange", "ORANGE" = "Orange", "Rust" = "Orange", "CORAL ORANGE" = "Orange",
  "Purple" = "Purple", "Violet" = "Purple", "Mauve" = "Purple", "Indigo" = "Purple",
  "Brown" = "Brown", "Beige" = "Brown", "Tan" = "Brown", "Light Brown" = "Brown",
  "Multicolor" = "Multicolor",
  "Cream" = "Cream", "Taupe" = "Brown", "Lemon Yellow" = "Yellow", "Khaki" = "Brown",
  "LIME GREEN" = "Green", "BURGUNDY" = "Red", "MINT" = "Green", "MINT GREEN" = "Green"
)

# Map specific colors to broader color categories
all_counts$BroadColor <- color_mapping[all_counts$Color]


# Aggregate the data by BroadColor and Category
broad_color_counts <- all_counts %>%
  group_by(Category, BroadColor) %>%
  summarise(Count = sum(n, na.rm = TRUE)) %>%
  ungroup()

# Create the heatmap OF COLOUR CATEGORY
ggplot(broad_color_counts, aes(x = BroadColor, y = Category, fill = Count, label = Count)) +
  geom_tile() +
  geom_text(color = "black", size = 3) +
  scale_fill_gradient(low = "pink", high = "blue") +
  labs(title = "Heatmap of Color Popularity by Category",
       x = "Color",
       y = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# Total counts for each broad color
total_color_counts <- all_counts %>%
  group_by(BroadColor) %>%
  summarise(TotalCount = sum(n, na.rm = TRUE)) %>%
  ungroup()

# Define the sizes corresponding to each broad color category
size_mapping <- c(1, 13, 6, 1, 9, 2, 1, 4, 5, 6, 4, 5, 2, 8)

# Add a new column with the corresponding sizes
total_color_counts$BubbleSize <- size_mapping

# Create the bubble plot with customized aesthetics
ggplot(total_color_counts, aes(x = BroadColor, y = TotalCount, size = BubbleSize)) +
  geom_point(aes(color = BroadColor)) +  
  geom_text(aes(label = BubbleSize, size = 0.8)) +
  labs(title = "Total Counts for Each Broad Colour",
       x = "Broad Colour",
       y = "Total Count",
       size = "Colour Categorisation Count") +  # Add size legend label
  scale_size_continuous(range = c(3, 15), guide = guide_legend(title = "Colour Categorisation Count")) +  
  scale_color_manual(values = c("darkgrey","lightblue", "brown","beige", "green", "lightgrey", "lavender" ,"orange","MistyRose","pink", "purple", "red", "Ivory", "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")
