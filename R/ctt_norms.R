#' Computes Colour Trails Test (CTT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Lee2012_Singapore` or ` Hsieh2007_China`.
#' @param trails1 Trails 1 performance in seconds.
#' @param trails2 Trails 2 performance in seconds.
#' @return A table of standardised scores and their descriptors. Note that z-scores were computed using `(mean - x) / sd` formula, hence larger z-scores correspond to better performance.
#' @examples
#' out <- ctt_norms(education=10, age=55, trails1=30, trails2=40, source="Hsieh2007_China");
#' @export
 
ctt_norms <- function(education, age, male=TRUE,
                      source="Lee2012_Singapore",
                      trails1=NA, 
                      trails2=NA){

  #  Load necessary packages and functions
  if (!exists("check_packages")) {
    source("check_packages.R")
    check_packages(c("tidyverse", "readxl", "stringr"))
  }
  if (!exists("extract_demographic")) {
    source("extract_demographic.R")
  }
  if (!exists("extract_descriptors")) {
    source("extract_descriptors.R")
  }
  
  # Get normative sample
  data <- read_excel("../Database/CTT_Norms.xlsx", sheet=source, col_names=FALSE)
  
  if (source == "Lee2012_Singapore") {
    print(paste("Lee et al. (2012) study ``N`` = 525 in Elderly Chinese individuals residing in Singapore aged between 54 - ≥75, education (in years) of 0 to >6. Tests were administered in different local languages (not taken into account in the norms reported). Scores are both age- and education-adjusted. Warning: groups may be even smaller after education and age group stratification."))
  } else if (source == "Hsieh2007_China") {
    print(paste("Hsieh & Tori (2007) study ``N`` = 324 in Mandarin-speaking individuals residing in China aged between 30 - 81. Tests were translated and administered in Mandarin. Scores are age-adjusted for persons between 30 - 81, and both age- and gender-adjusted for ages 61-81."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }
  
  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  gender <- demographic[[3]]
  
  # tidy df
  df <- t(data)
  row.names(df) <- NULL
  colnames(df) <- df[1, ]
  df <- as.data.frame(df[-1, ])
  
  # Get reference
  if (source == "Lee2012_Singapore") {
    ref_group <- df[df$`Education (Years)` == education & df$Age == age, ]
  } else if (source == "Hsieh2007_China") {
    if (!is.na(gender)) {
      ref_group <- df[df$Age == age & df$Gender == gender, ]
    } else {
      ref_group <- df[df$Age == age, ]
    }
  }
  colnames(ref_group) <- tolower(colnames(ref_group))
  
  # Compute standardised scores
  out <- .compute_ctt_scores(ref_group, source,
                             trails1, trails2)
  
  extract_descriptors(out)
}



# Utility functions -------------------------------------------------------


.compute_ctt_scores <- function(reference, source,
                                trails1=NA, trails2=NA){
  
  # Colour Trails 1
  if (!is.na(trails1)) {
    trails1_mean = reference[str_detect(colnames(reference), "(trail1).+(mean)")]
    trails1_sd = reference[str_detect(colnames(reference), "(trail1).+(sd)")]
    trails1_zscore = (as.numeric(trails1_mean) - trails1) / as.numeric(trails1_sd)
  } else {
    trails1_zscore = NA
  }
  
  # Colour Trails 2
  if (!is.na(trails2)) {
    trails2_mean = reference[str_detect(colnames(reference), "(trail2).+(mean)")]
    trails2_sd = reference[str_detect(colnames(reference), "(trail2).+(sd)")]
    trails2_zscore = (as.numeric(trails2_mean) - trails2) / as.numeric(trails2_sd)
  } else {
    trails2_zscore = NA
  }
  
  zscores <- list(
    ColourTrails1 = c(trails1_zscore, trails1),
    ColourTrails2 = c(trails2_zscore, trails2)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  
  return(out)
}


