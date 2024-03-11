#' Computes Controlled Oral Word Association Test (COWAT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Tombaugh1999_Canada` or ` Whittle2007_US`.
#' @param fas_score FAS score.
#' @param animals_score Animals score.
#' @return A table of standardised scores and their descriptors.
#' @examples
#' out1 <- cowat_norms(education=10, age=55, fas_score=NA, animals_score=20, source="Tombaugh1999_Canada";
#' out2 <- cowat_norms(education=10, age=91, fas_score=50, animals_score=NA, source="Whittle2007_US");
#' @export

cowat_norms <- function(education, age, male=TRUE,
                        source="Tombaugh1999_Canada",
                        fas_score=NA, animals_score=NA){
  
  #  Load necessary packages and functions
  if (!exists("check_packages")) {
    source("check_packages.R")
    check_packages(c("tidyverse", "readxl"))
  }
  if (!exists("extract_demographic")) {
    source("extract_demographic.R")
  }
  if (!exists("extract_descriptors")) {
    source("extract_descriptors.R")
  }
  
  if (source == "Tombaugh1999_Canada") {
    print(paste("Tombaugh et al. (1999) study ``N`` = 1300 in English-speaking individuals residing in Canada aged between 16 - 95, education (in years) of 0 to 21. Scores are age- and education-adjusted. Tests were administered in English. Warning: Small ``N`` of 12 (FAS) and 4 (Animals) for group aged 16-59 with 0-8 years of education."))
  } else if (source == "Whittle2007_US") {
    print(paste("Whittle et al. (2007) ``N`` = 339  (The 90+ Study) in English-speaking individuals residing in US aged between 90 - 103. Only the letter `F` was administered as part of letter fluency to avoid fatigue. Scores are age-adjusted. The assessment was administered in English. Scores are age-adjusted."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }

  # Extract normative sample for FAS
  if (!is.na(fas_score)) {
    data <- read_excel("../Database/COWAT_Norms.xlsx", sheet=paste0(source, " - FAS"), col_names=FALSE)
    # tidy df
    if (source == "Tombaugh1999_Canada") {
      df <- t(data)
      row.names(df) <- NULL
      colnames(df) <- df[1, ]
      df_fas <- as.data.frame(df[-1, ])
    } else if (source == "Whittle2007_US") {
      colnames(data) <- data[1, ]
      df_fas <- as.data.frame(data[-1, ])
    }
  }
    
  # Extract normative sample for Animals
  if (!is.na(animals_score)) {
    data <- read_excel("Database/COWAT_Norms.xlsx", sheet=paste0(source, " - Animals"), col_names=FALSE)
    # tidy df
    if (source == "Tombaugh1999_Canada") {
      df <- t(data)
      row.names(df) <- NULL
      colnames(df) <- df[1, ]
      df_animals <- as.data.frame(df[-1, ])
    } else if (source == "Whittle2007_US") {
      colnames(data) <- data[1, ]
      df_animals <- as.data.frame(data[-1, ])
    }
  }

  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  gender <- demographic[[3]]
  
  # get reference group and compute standardised scores
  if (source == "Tombaugh1999_Canada") {
    if (!is.na(fas_score)) {
      ref_group_fas <- df_fas[df_fas$`Education (Years)` == education & df_fas$Age == age, ]
      colnames(ref_group_fas) <- tolower(colnames(ref_group_fas))
      fas_zscore = .compute_cowat_scores(ref_group_fas, source, fas_score)
      fas_zscore = extract_descriptors(fas_zscore)
    } else {
      fas_zscore = NA
    }
    if (!is.na(animals_score)) {
      ref_group_animals <- df_animals[df_animals$`Education (Years)` == education & df_animals$Age == age, ]
      colnames(ref_group_animals) <- tolower(colnames(ref_group_animals))
      animals_zscore = .compute_cowat_scores(ref_group_animals, source, animals_score)
      animals_zscore = extract_descriptors(animals_zscore)
    } else {
      animals_zscore = NA
    }
  } else if (source == "Whittle2007_US") {
    if (!is.na(fas_score)) {
      ref_group_fas <- df_fas[df_fas$Age == age, ]
      colnames(ref_group_fas) <- tolower(colnames(ref_group_fas))
      fas_zscore = .compute_cowat_scores(ref_group_fas, source, fas_score)
      fas_zscore = extract_descriptors(fas_zscore)
    } else {
      fas_zscore = NA
    } 
    if (!is.na(animals_score)) {
      ref_group_animals <- df_animals[df_animals$Age == age, ]
      colnames(ref_group_animals) <- tolower(colnames(ref_group_animals))
      animals_zscore = .compute_cowat_scores(ref_group_animals, source, animals_score)
      animals_zscore = extract_descriptors(animals_zscore)
    } else {
      animals_zscore = NA
    }
  }
  
  out <- list(
    FAS = fas_zscore,
    Animals = animals_zscore
  )
  return(out)
}






# Utility functions -------------------------------------------------------



.compute_cowat_scores <- function(reference, source,
                                  score){
  
  zscore = (score - as.numeric(reference$mean)) / as.numeric(reference$sd)

  # Prepare output
  zscores <- list(
    Scores = c(zscore, score)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  out
}






