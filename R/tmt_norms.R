#' Computes Trail Making Test (TMT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Tombaugh2004_Canada`, `Hsieh2007_China`, or ` Whittle2007_US`.
#' @param trailsA Trails A performance in seconds.
#' @param trailsB Trails B performance in seconds.
#' @return A table of standardised scores and their descriptors. Note that z-scores were computed using `(mean - x) / sd` formula, hence larger z-scores correspond to better performance.
#' @examples
#' out1 <- tmt_norms(education=10, age=55, trailsA=30, trailsB=40);
#' out2 <- tmt_norms(education=10, age=91, trailsA=30, trailsB=40, source="Whittle2007_US")
#' @importFrom readxl read_excel
#' @importFrom readr parse_number 
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_extract
#' @export

tmt_norms <- function(education, age, male=TRUE,
                      source="Tombaugh2004_Canada",
                      trailsA=NA, 
                      trailsB=NA){

  #  Load necessary packages and functions
  # if (!exists("check_packages")) {
  #   source("check_packages.R")
  #   check_packages(c("tidyverse", "readxl", "readr", "tidyr", "stringr"))
  # }
  # if (!exists("extract_demographic")) {
  #   source("extract_demographic.R")
  # }
  # if (!exists("extract_descriptors")) {
  #   source("extract_descriptors.R")

  
  # Get normative sample
  data <- read_excel("../Database/TMT_Norms.xlsx", sheet=source, col_names=FALSE)
  
  if (source == "Tombaugh2004_Canada") {
    print(paste("Tombaugh (2004) study ``N`` = 911 in English-speaking individuals residing in Canada aged between 18 - 89, of education (in years) of 0-12 and >12. Tests were administered in English. Scores are age-adjusted for persons between ages 18 - 54, and both age- and education-adjusted for ages 55-89."))
  } else if (source == "Hsieh2007_China") {
    print(paste("Hsieh & Tori (2007) study ``N`` = 324 in Mandarin-speaking individuals residing in China aged between 30 - 81. Tests were translated and administered in Mandarin. Scores are age-adjusted for persons between 30 - 81, and both age- and gender-adjusted for ages 61-81."))
  } else if (source == "Whittle2007_US") {
    print(paste("Whittle et al. (2007) ``N`` = 339  (The 90+ Study) in English-speaking individuals residing in US aged between 90 - 103. Tests were administered in English. Scores are age-adjusted."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }
  
  
  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  gender <- demographic[[3]]
  
  # tidy df
  if (source == "Whittle2007_US") {
    colnames(data) <- data[1, ]
    df <- as.data.frame(data[-1, ])
  } else {
    df <- t(data)
    row.names(df) <- NULL
    colnames(df) <- df[1, ]
    df <- as.data.frame(df[-1, ])
  }

  # Get reference
  if (source == "Tombaugh2004_Canada") {
    if (!is.na(education)) {
      ref_group <- df[df$`Education (Years)` == education & df$Age == age, ]
    } else {
      ref_group <- df[df$Age == age, ]
    }
  } else if (source == "Hsieh2007_China") {
    if (!is.na(gender)) {
      ref_group <- df[df$Age == age & df$Gender == gender, ]
    } else {
      ref_group <- df[df$Age == age, ]
    } 
  } else if (source == "Whittle2007_US") {
    ref_group <- df[df$Age == age, ]
    ref_group <- pivot_wider(ref_group, names_from = Test,
                             names_glue = "{Test}_{.value}",
                             values_from = Mean)
  }
  
  colnames(ref_group) <- tolower(colnames(ref_group))

  # Compute standardised scores
  out <- .compute_tmt_scores(ref_group, source,
                             trailsA, trailsB)
  
  extract_descriptors(out)
  
  }



# Utility functions -------------------------------------------------------

.compute_tmt_scores <- function(reference, source,
                                trailsA=NA, trailsB=NA){
  
  # Precaution: Hsieh & Tori (2007) does not compute norms for trails B; force NA if these entries are entered by user
  if (source == "Hsieh2007_China") {
    trailsB = NA
  } 

  # Trails A
  if (!is.na(trailsA)) {
    if (source == "Whittle2007_US") {
      trailsA_mean <- parse_number(reference$`trail a (s)_mean`)
      trailsA_mean <- trailsA_mean[!is.na(trailsA_mean)]
      trailsA_sd <- str_extract(reference$`trail a (s)_mean`, "(?<=\\().*(?=\\))")
      trailsA_sd <- trailsA_sd[!is.na(trailsA_sd)]
      trailsA_zscore = (as.numeric(trailsA_mean) - trailsA) / as.numeric(trailsA_sd)
    } else {
      trailsA_mean = parse_number(reference$`trail a (s)`)
      trailsA_sd = str_extract(reference$`trail a (s)`, "(?<=\\().*(?=\\))")
      trailsA_zscore = (as.numeric(trailsA_mean) - trailsA) / as.numeric(trailsA_sd)
    }
  } else {
    trailsA_zscore = NA
  }
  
  # Trails B
  if (!is.na(trailsB)) {
    if (source == "Whittle2007_US") {
      trailsB_mean <- parse_number(reference$`trail b (s)_mean`)
      trailsB_mean <- trailsB_mean[!is.na(trailsB_mean)]
      trailsB_sd <- str_extract(reference$`trail b (s)_mean`, "(?<=\\().*(?=\\))")
      trailsB_sd <- trailsB_sd[!is.na(trailsB_sd)]
      trailsB_zscore = (as.numeric(trailsB_mean) - trailsB) / as.numeric(trailsB_sd)
    } else {
      trailsB_mean = parse_number(reference$`trail b (s)`)
      trailsB_sd = str_extract(reference$`trail b (s)`, "(?<=\\().*(?=\\))")
      trailsB_zscore = (as.numeric(trailsB_mean) - trailsB) / as.numeric(trailsB_sd)
    }
  } else {
    trailsB_zscore = NA
  }
  
  zscores <- list(
    TrailsA = c(trailsA_zscore, trailsA),
    TrailsB = c(trailsB_zscore, trailsB)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  
  return(out)
}


