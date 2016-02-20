library(dplyr)

info_function <- function(dataset) {
    ret <- list()
    ret$length <- length(dataset)
    
    dataset_formatted <- gsub(" ", "_", colnames(dataset))
    
    colnames(dataset_formatted) <- c("class_standing", "applying_to_major", "operating_system", "command-line_familiarity", "git_familiarity", "markdown_familiarity", "r_familiarity", "programming_experience", "countries_visited", "pet_person", "seahawks_fan")
    
    #Number of people per class standing
    ret$num_of_freshman <- dataset_formatted %>% filter(class_standing == "Freshman") %>% nrow()
    ret$num_of_sophomores <- dataset_formatted %>% filter(class_standing == "Sophomore") %>% nrow()
    ret$num_of_junior <- dataset_formatted %>% filter(class_standing == "Junior") %>% nrow()
    ret$num_of_senior <- dataset_formatted %>% filter(class_standing == "Senior") %>% nrow()
    
    #Number of people applying to the major
    ret$num_applying_to_major <- dataset_formatted %>% filter(applying_to_major == "Yes") %>% nrow()
    ret$num_not_applying_to_major <- dataset_formatted %>% filter(applying_to_major == "No") %>% nrow()
    ret$num_maybe_applying_to_major <- dataset_formatted %>% filter(applying_to_major == "Maybe") %>% nrow()
    
    #Number of people per operating system
    ret$num_mac_users <- dataset_formatted %>% filter(operating_system == "Mac") %>% nrow()
    ret$num_windows_users <- dataset_formatted %>% filter(operating_system == "Windows") %>% nrow()
    
    #Number of people with command-line familiarity
    ret$num_not_command-line_familiar <- dataset_formatted %>% filter(command-line_familiarity == "Never used it") %>% nrow()
    ret$num_intermediate_command-line_user <- dataset_formatted %>% filter(command-line_familiarity == "Intermediate user") %>% nrow()
    ret$num_slight_command-line_git <- dataset_formatted %>% filter(command-line_familiarity == "Have used it a few times") %>% nrow()
    
    #Number of people with git familiarity
    ret$num_not_git_familiar <- dataset_formatted %>% filter(git_familiarity == "Never used it") %>% nrow()
    ret$num_intermediate_git_user <- dataset_formatted %>% filter(git_familiarity == "Intermediate user") %>% nrow()
    ret$num_slight_git_familiarity <- dataset_formatted %>% filter(git_familiarity == "Have used it a few times") %>% nrow()
    
    #Number of people with markdown familiarity
    ret$num_not_markdown_familiar <- dataset_formatted %>% filter(markdown_familiarity == "Never used it") %>% nrow()
    ret$num_intermediate_markdown_user <- dataset_formatted %>% filter(markdown_familiarity == "Intermediate user") %>% nrow()
    ret$num_slight_markdown_familiarity <- dataset_formatted %>% filter(markdown_familiarity == "Have used it a few times") %>% nrow()
    
    #Number of people with r familiarity
    ret$num_not_r_familiar <- dataset_formatted %>% filter(r_familiarity == "Never used it") %>% nrow()
    ret$num_intermediate_r_user <- dataset_formatted %>% filter(r_familiarity == "Intermediate user") %>% nrow()
    ret$num_slight_r_familiarity <- dataset_formatted %>% filter(r_familiarity == "Have used it a few times") %>% nrow()
    
    #Number of people with programming familiarity
    ret$num_no_programming_exp <- dataset_formatted %>% filter(programming_familiarity == "Never written code") %>% nrow()
    ret$num_some_programming_exp <- dataset_formatted %>% filter(programming_familiarity == "Experimented a bit with simple programming") %>% nrow()
    ret$num_moderate_programming_exp <- dataset_formatted %>% filter(programming_familiarity == "Moderate experience with a scripting language (Python, R, JavaScript, Java, etc.)") %>% nrow()
    ret$num_high_programmig_exp <- dataset_formatted %>% filter(programming_familiarity == "Lots of experience with a scripting language (Python, R, JavaScript, Java, etc.)") %>% nrow()
    
    #Average number of countries visited
    ret$mean_countries_visited <- dataset_formatted %>% select(countries_visited) %>% mean()
    
    #Number of dog / cat people
    ret$num_dog_person <- dataset_formatted %>% filter(pet_person == "A dog person.") %>% nrow()
    ret$num_cat_person <- dataset_formatted %>% filter(pet_person == "A cat person.") %>% nrow()
    ret$num_both_pets_person <- dataset_formatted %>% filter(pet_person == "Both!") %>% nrow()
    ret$num_not_pet_person <- dataset_formatted %>% filter(pet_person == "Neither") %>% nrow()
    
    #Number of seahawks fans
    ret$num_seahawks_fan <- dataset_formatted %>% filter(seahawks_fan == "Yes") %>% nrow()
    ret$num_major_seahawks_fan <- dataset_formatted %>% filter(seahawks_fan == "YES!") %>% nrow()
    ret$num_not_seahawks_fan <- dataset_formatted %>% filter(seahawks_fan == "No") %>% nrow()
    
    return (ret)        
}