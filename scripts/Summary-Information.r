library(dplyr)

info_function <- function(dataset) {
    ret <- list()
    ret$length <- length(dataset)
    
    dataset_formatted <- gsub(" ", "_", colnames(dataset))
    
    colnames(dataset_formatted) <- c("class_standing", "applying_to_major", "operating_system", "command-line_use", "git_familiarity", "markdown_familiarity", "r_familiarity", "programming_experience", "countries_visited", "pet_person", "seahawks_fan")
    
    #Number of people per class standing
    ret$num_of_freshman <- dataset_formatted %>% filter(class_standing == "Freshman") %>% nrow()
    ret$num_of_sophomores <- dataset_formatted %>% filter(class_standing == "Sophomore") %>% nrow()
    ret$num_of_junior <- dataset_formatted %>% filter(class_standing == "Junior") %>% nrow()
    ret$num_of_senior <- dataset_formatted %>% filter(class_standing == "Senior") %>% nrow()
    
    #Number of people applying to the major
    
    
    #Number of people per operating system
    
    
    #Number of people with command-line familiarity
    
    
    #Number of people with git familiarity
    
    
    #Number of people with markdown familiarity
    
    
    #Number of people with r familiarity
    
    
    #Number of people with programming familiarity
    
    
    #Average number of countries visited
    
    
    #Number of dog / cat people
    
    
    #Number of seahawks fans
    
    
    
    
    return (ret)        
}