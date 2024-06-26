# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
library(readxl)
# 2.0 Importing Files ----

read_excel("~/GitHub/ss24-bdsb-Lostzorro/01_bike_sales/01_raw_data/bikes.xlsx")

read_excel("~/GitHub/ss24-bdsb-Lostzorro/01_bike_sales/01_raw_data/orderlines.xlsx")

read_excel("~/GitHub/ss24-bdsb-Lostzorro/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----
orderlines

glimpse(orderlines)

# 4.0 Joining Data ----

left_join(orderlines, bikes, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines %>%
  left_join(bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse


# 5.0 Wrangling Data ----

####bike_orderlines_joined_tbl %>% 
 ## select(category) %>%
 ## filter(str_detect(category, "^Mountain")) %>% 
 ## unique()
##bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
 # separate(col    = category,
   #        into   = c("category.1", "category.2", "category.3"),
   #        sep    = " - ") %>%
##  mutate(total.price = price * quantity) %>%
 # select(-...1, -gender) %>%
# select(-ends_with(".id")) %>%
#  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
#  select(order.id, contains("order"), contains("model"), contains("category"),
   #      price, quantity, total.price,
  #       everything()) %>%
#  rename(bikeshop = name) %>%
 # set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_joined_tbl%>% 
  select(location) 
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%   separate(col  = location,
         into   = c("city", "state"),
        sep    = " - ") %>% 
  mutate(total.price = price * quantity) %>% 
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
   bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
   select(order.id, contains("order"), contains("model"), contains("category"),
     price, quantity, total.price,
      everything()) %>%
   rename(bikeshop = name) %>%
   set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
library(lubridate)
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales)) +
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, city) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, city) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €"))
# Step 2 - Visualize


sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = sales, y = state, fill = city)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ city) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category"
  )
# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----


