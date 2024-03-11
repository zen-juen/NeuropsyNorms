#' Extract Demographic Variables
#'
#' Tidy demographic variables related to age, education and gender according to normative samples used.
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Tombaugh1999_Canada` etc.
#' @return A list of tidied demographic variables to be passed into norming functions.
#' @examples
#' demo <- extract_demographic(education=10, age=50, male=TRUE, source="Tombaugh1999_Canada");
#' @export

extract_demographic <- function(education, age, male, source) {
  
  # ----- Collinson et al. (2014) norms ----- #
  if (source == "Collinson2014_Singapore") {
    # education
    if(c(education == 0)){
      education <- "0"
    } else if (c(education >= 1 && education <= 3)) {education <- "1-3"
    } else if (c(education >= 4 && education <= 6)) {education <- "4-6"
    } else if (c(education >= 7 && education <= 10)) {education <- "7-10"
    } else if (education > 10) {education <- ">10"
    } else {
      print(paste("Error: Please key in education in number of years."))
    }
    
    # age
    if(c(age >= 54 && age <= 59)){
      age <- "54-59"
    } else if (c(age >= 60 && age <= 64)) {age <- "60-64"
    } else if (c(age >= 65 && age <= 69)) {age <- "65-69"
    } else if (c(age >= 70 && age <= 74)) {age <- "70-74"
    } else if (age >= 75) {age <- "≥75"
    } else {
      print(paste("Error: Please key in age of 54 years and above."))
    }
    gender = NA
    
  # ----- Whittle et al. (2007) norms ----- #
  } else if (source == "Whittle2007_US") { 
    # age
    if(c(age >= 90 && age <= 91)){
      age <- "90-91"
    } else if (c(age >= 92 && age <= 94)) {age <- "92-94"; gender = NA
    } else if (age >= 95) {age <- "95+"
    } else {
      print(paste("Error: Please key in age above 90 years old."))
    }
    
    education = NA
    gender = NA
  
  # ----- Tombaugh et al. (1999) norms ----- #
  } else if (source == "Tombaugh1999_Canada") {
    # education
    if(c(education >= 0 && education <= 8)){
      education <- "0-8"
    } else if (c(education >= 9 && education <= 12)) {education <- "9-12"
    } else if (c(education >= 13 && education <= 21)) {education <- "13-21"
    } else {
      print(paste("Error: Please key in education in number of years."))
    }
    
    # age
    if(c(age >= 16 && age <= 59)){
      age <- "16-59"
    } else if (c(age >= 60 && age <= 79)) {age <- "60-79"
    } else if (c(age >= 80 && age <= 95)) {age <- "80-95"
    } else {
      print(paste("Error: Please key in age between 16 and 95."))
    }
    # gender
    gender = NA
  
  # ----- Lee et al. (2012) norms ----- #
  } else if (source == "Lee2012_Singapore") {
    # education
    if(c(education >= 0 && education <= 3)){
      education <- "0-3"
    } else if (c(education >= 4 && education <= 6)) {education <- "4-6"
    } else if (education > 6) {education <- ">6"
    } else {
      print(paste("Error: Please key in education in number of years."))
    }
    
    # age
    if(c(age >= 54 && age <= 59)){
      age <- "54-59"
    } else if (c(age >= 60 && age <= 64)) {age <- "60-64"
    } else if (c(age >= 65 && age <= 69)) {age <- "65-69"
    } else if (c(age >= 70 && age <= 74)) {age <- "70-74"
    } else if (age >= 75) {age <- "≥75"
    } else {
      print(paste("Error: Please key in age of 54 years and above."))
    }
    # Gender
    gender = NA
    
  # ----- Wallace et al. (2017) norms ----- # 
  } else if (source == "Wallace2017_ChineseAus"){
    # education
    if(c(education >= 12)){
      education <- "≥12"
    } else if (education < 12) {education <- "<12"
    } else {
      print(paste("Error: Please key in education in number of years."))
    }
    
    # age
    if(c(age >= 55 && age <= 72)){
      age <- "55-72"
    } else if (c(age >= 73 && age <= 87)) {age <- "73-87"
    } else {
      print(paste("Error: Please key in age between 55 and 87 years old."))
    }
    # Gender
    if (male){
      gender = "M"
    } else {
      gender = "F"
    }
  
  # ----- Benedict et al. (1997) norms ----- #
  } else if (source == "Benedict1997_US"){
    # education and gender
    education = NA
    gender = NA
    
    # retain age (no manipulation needed)
  
  # ----- Tombaugh et al. (2004) norms ----- #
  } else if (source == "Tombaugh2004_Canada") {
    # education
    if (c(education >= 0 && education <= 12)){
      education <- "0-12"
    } else if (education > 12) {education <- ">12"
    } else {
      print(paste("Error: Please key in education in number of years."))
    }
    
    # age
    if(c(age >= 18 && age <= 24)){
      age <- "18-24"; education = NA
    } else if (c(age >= 25 && age <= 34)) {age <- "25-34"; education = NA
    } else if (c(age >= 35 && age <= 44)) {age <- "35-44"; education = NA
    } else if (c(age >= 45 && age <= 54)) {age <- "45-54"; education = NA
    } else if (c(age >= 55 && age <= 59)) {age <- "55-59"
    } else if (c(age >= 60 && age <= 64)) {age <- "60-64"
    } else if (c(age >= 65 && age <= 69)) {age <- "65-69"
    } else if (c(age >= 70 && age <= 74)) {age <- "70-74"
    } else if (c(age >= 75 && age <= 79)) {age <- "75-79"
    } else if (c(age >= 80 && age <= 84)) {age <- "80-84"
    } else if (c(age >= 85 && age <= 89)) {age <- "85-89"
    } else {
      print(paste("Error: Please key in age between 25 and 89."))
    }
    
    gender = NA
  
  # ----- Hsieh & Tori (2007) norms ----- #
  } else if (source == "Hsieh2007_China"){
    # gender
    if (male){
      gender = "M"
    } else {
      gender = "F"
    }
    
    # age 
    if(c(age >= 30 && age <= 45)){
      age <- "30-45"; gender = NA
    } else if (c(age >= 46 && age <= 60)) {age <- "46-60"; gender = NA
    } else if (c(age >= 61 && age <= 81)) {age <- "61-81"
    } else {
      print(paste("Error: Please key in age between 30 and 81 years old."))
    }
    education = NA

  # ----- Kiely et al. (2014) norms (SDMT written) ----- #
  } else if (source == "Kiely2014_Aus_Written") {
    # education
    if (is.character(education)) {
      education = tolower(education)
    }
    
    if (education == "tertiary") {education <- "Tertiary"
    } else if(c(education == "postsecondary" | education == "post-secondary")) {education <- "Postsecondary"
    } else if (c(education == "highschool" | education == "high school")) {education <- "Completed high school"
    } else if (education == "year 11") {education <- "≤Year 11"
    } else {
      print(paste("For Kiely et al. (2014) norms, please key in highest level of education as `tertiary`, `postsecondary` (nontertiary certs and diplomas reflecting trade/vocational/technial qualifications), `high school` (year 12), or `year 11`."))
    }
    
    # age
    if(c(age >= 15 && age <= 19)){
      age <- "15-19"
    } else if (c(age >= 20 && age <= 24)) {age <- "20-24"
    } else if (c(age >= 25 && age <= 29)) {age <- "25-29"
    } else if (c(age >= 30 && age <= 34)) {age <- "30-34"
    } else if (c(age >= 35 && age <= 39)) {age <- "35-39"
    } else if (c(age >= 40 && age <= 44)) {age <- "40-44"
    } else if (c(age >= 45 && age <= 49)) {age <- "45-49"
    } else if (c(age >= 50 && age <= 54)) {age <- "50-54"
    } else if (c(age >= 55 && age <= 59)) {age <- "55-59"
    } else if (c(age >= 60 && age <= 64)) {age <- "60-64"
    } else if (c(age >= 65 && age <= 69)) {age <- "65-69"
    } else if (c(age >= 70 && age <= 74)) {age <- "70-74"
    } else if (c(age >= 75 && age <= 79)) {age <- "75-79"
    } else if (c(age >= 80 && age <= 84)) {age <- "80-84"
    } else if (age >= 85) {age <- "85+"
    } else {
      print(paste("Error: Please key in age of 15 years and above."))
    }
    # gender
    if (male){
      gender = "M"
    } else {
      gender = "F"
    }
  }
  
  # output
  return(list(education, age, gender))
}
