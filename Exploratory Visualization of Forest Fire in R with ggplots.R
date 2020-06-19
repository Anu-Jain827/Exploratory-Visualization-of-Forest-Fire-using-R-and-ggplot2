Project Title - Exploratory Visualization of Forest Fire Data
Aim - To Perform 

Step 1 - Loading the packages : 

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

Step 2 - Importing the data file
forest_fires <- read.csv("C:\\Users\\anuja\\Downloads\\forestfires.csv")

Step3 - Bar Charts
  To show number of forest fires occuring during each month
  
  fires_by_month <- forest_fires %>%
    group_by(month) %>%
    summarize(total_fires = n())
  ggplot(data = fires_by_month) +
    aes(x = month, y = total_fires) +
    geom_bar(stat = "identity")  +
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(size = 0.25, 
                                   colour = "black"))

  Step4 - Bar Charts
  To show number of forest fires occurring on each day of the week
  
  fires_by_DOW <- forest_fires %>%
    group_by(day) %>%
    summarize(total_fires = n())
  ggplot(data = fires_by_DOW) +
    aes(x = day, y = total_fires) +
    geom_bar(stat = "identity") +
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(size = 0.25, 
                                   colour = "black")) 
  
  Step 5 - Changing the data type of month to factor and specifying the order of months
  
  forest_fires <- forest_fires %>%
    mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")), 
           day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))
  
  Step 6 - Bar chart
  To show number of forest fires occuring during each month after mutation
  
  fires_by_month <- forest_fires %>%
    group_by(month) %>%
    summarize(total_fires = n())
  ggplot(data = fires_by_month) +
    aes(x = month, y = total_fires) +
    geom_bar(stat = "identity")  +
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(size = 0.25, 
                                   colour = "black"))
  
  Step 7 - Bar chart
  To show number of forest fires occurring on each day of the week after mutation
  
  fires_by_DOW <- forest_fires %>%
    group_by(day) %>%
    summarize(total_fires = n())
  ggplot(data = fires_by_DOW) +
    aes(x = day, y = total_fires) +
    geom_bar(stat = "identity") +
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(size = 0.25, 
                                   colour = "black")) 
  Step 8 - Functions
  To write a function to create a boxplot for visualizing variable distributions by month and day of the week
  
  create_boxplots <- function(x, y) {
    ggplot(data = forest_fires) + 
      aes_string(x = x, y = y) +
      geom_boxplot(alpha = 0.3) +
      theme(panel.background = element_rect(fill = "white"))
    }
  
  ## Assign x and y variable names 
  x_var_month <- names(forest_fires)[3]   ## month
  x_var_day <- names(forest_fires)[4]     ## day
  y_var <- names(forest_fires)[5:12]
  
  Step 9 - Visualising Boxplots
  
  # use the map() function to apply the function to the variables of interest
  month_box <- map2(x_var_month, y_var, create_boxplots) ## visualize variables by month
  day_box <- map2(x_var_day, y_var, create_boxplots) ## visualize variables by day
  month_box
  day_box
 
  Step 10 Functions
  To Create scatter plots to see which variables may affect forest fire size: 
    
  
  ## function 
  create_scatterplots = function(x, y) {
    ggplot(data = forest_fires) + 
      aes_string(x = x, y = y) +
      geom_point() +
      theme(panel.background = element_rect(fill = "white"))
  }
  
  ## Assign x and y variable names 
  x_var_scatter <- names(forest_fires)[5:12]
  y_var_scatter <- names(forest_fires)[13]
  
  Step 11 - Visualising Scatterplots using the map() function
  
  scatters <- map2(x_var_scatter, y_var_scatter, create_scatterplots)
  scatters
  
  Conclusion - Exploratory  Visualization of Forest Fire Data is completed.
  
  
  
  