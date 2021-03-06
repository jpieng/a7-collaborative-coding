#Assignment 7: Chart file 2

# creates a bar chart comparing the distribution of
# class standings
make_bar_chart <- function (the_data) {
  # Creates new data.frame grouped by class standing and
  # number of students in each respective class standing.
  summarized_data <- the_data %>% 
    group_by(What.is.your.current.class.standing.) %>% 
    summarise(
      num_students = n()
    )
  
  # Makes bar chart
  plot_ly(summarized_data,
          x = What.is.your.current.class.standing., 
          y = num_students,
          name = "Class Standing Distribution of INFO498F",
          type = "bar"
  ) %>% layout(
    barmode = "stack",
    title = "Distribution of Class Standing for INFO498f",
    xaxis = list(title = "Class Standing"),
    yaxis = list(title = "Number of Students")
  )
}