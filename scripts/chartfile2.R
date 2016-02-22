#Assignment 7: Chart file 2

#load libraries
library(dplyr)
library(plotly)
library(jsonlite)

# creates a bar chart comparing the distribution of
# class standings
make_bar_chart <- function (the_data) {
  # Creates new data.frame grouped by class standing and
  # number of students in each respective class standing.
  summarized_data <- the_data %>% 
    group_by(class_standing) %>% 
    summarise(
      num_students = n()
    )
  
  # Makes bar chart
  plot_ly(summarized_data,
          x = class_standing, 
          y = num_students,
          name = "Class Standing Distribution of INFO498F",
          type = "bar"
  ) %>% layout(
    barmode = "stack",
    title = "Distribution of Class Standing",
    xaxis = list(title = "Class Standing"),
    yaxis = list(title = "Number of Students")
  )
}