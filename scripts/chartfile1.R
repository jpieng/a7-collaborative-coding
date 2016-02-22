# Assignment 7 - Chart File 1
# data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")

# Write a function that takes in a dataset as a parameter, and returns a visualization 
# of that data, allowing you to call your charting function from your index.Rmd file.
make_INFOinterest_chart <- function(dataset) {
  p <- dataset %>% count(Are.you.interested.in.applying.to.the.Informatics.major., What.operating.system.do.you.typically.use.) %>%
    plot_ly(x = Are.you.interested.in.applying.to.the.Informatics.major., 
            y = n, 
            type = "bar", 
            color = What.operating.system.do.you.typically.use.) %>%
    
    layout(title = "INFO498F Winter 2016 Class Interest in the Informatics Major & Operating Systems Used",
            xaxis = list(title = "Interest in Applying to the Informatics Major"),
            yaxis = list(title = "Number of Students")
    )
}
