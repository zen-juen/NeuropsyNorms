#' Computes Brief Visuospatial Memory Test - Revised (BVMT-R) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Lee2012_Singapore` or ` Benedict1997_US`.
#' @param t1 Trial 1 score
#' @param t2 Trial 2 score
#' @param t3 Trial 3 score
#' @param learning Learning score (higher of trial 2 or 3 - trial 1 score)
#' @param total Totals (Trials 1-3) score.
#' @param delayed_recall Delayed Free Recall score
#' @param percent_retained Amount of info retained after delay. (delayed recall score * 100) / higher of trial 2 or 3
#' @param recog_correct Recognition total number of correct items/hits.
#' @param recog_FA Recognition False Alarms.
#' @param recog_discrimination Recognition discrimination index (Recognition Hits - Recognition False Alarms).
#' @return A table of standardised scores and their descriptors.
#' @examples
#' out1 <- bvmt_norms(education=10, age=55, t1=5, t2=5, t3=6, total=13, delayed_recall=10, source="Benedict1997_US");
#' @export

bvmtr_norms <- function(education, age, male=TRUE,
                        source="Lee2012_Singapore",
                        t1=NA, t2=NA, t3=NA,
                        learning=NA, total=NA, delayed_recall=NA,
                        percent_retained=NA,
                        recog_correct=NA, recog_FA=NA, recog_discrimination=NA){
  
  #  Load necessary packages and functions
  if (!exists("check_packages")) {
    source("check_packages.R")
    check_packages(c("tidyverse", "readxl", "tidyr", "stringr"))
  }
  if (!exists("extract_demographic")) {
    source("extract_demographic.R")
  }
  if (!exists("extract_descriptors")) {
    source("extract_descriptors.R")
  }
  
  # Get normative samples
  data <- read_excel("../Database/BVMT-R_Norms.xlsx", sheet=source, col_names=FALSE)
  
  if (source == "Lee2012_Singapore") {
    print(paste("Lee et al. (2012) study ``N`` = 525 in Elderly Chinese individuals residing in Singapore aged between 54 - ≥75, education (in years) of 0 to >6. Tests were administered in different local languages (not taken into account in the norms reported). Scores are both age- and education-adjusted. Warning: groups may be even smaller after education and age group stratification."))
  } else if (source == "Benedict1997_US"){
    print(paste("Benedict et al. (1997) study ``N`` = 588 in English-speaking individuals residing in US aged between 18 - 79. Warning: mean education (in years) is 13.4, norms may overestimate impairment among those with low levels of education. Scores are age-adjusted."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }
  
  # Extract midpoint age for Benedict 1997 norms (overlapping age groups)
  if (source == "Benedict1997_US") {
    colnames(data) <- data[1, ]
    data <- as.data.frame(data[-1, ])
    data$`Midpoint Age` <- as.numeric(data$`Midpoint Age`)
    age <- data$`Age group`[which.min(abs(data$`Midpoint Age` - age))]
  } 
  
  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  
  if (source == "Lee2012_Singapore") {
    df <- t(data)
    row.names(df) <- NULL
    colnames(df) <- df[1, ]
    df <- as.data.frame(df[-1, ])
    # Get reference
    ref_group <- df[df$`Education (Years)` == education & df$Age == age, ]

  } else if (source == "Benedict1997_US"){
    # Get reference
    ref_group <- data[data$`Age group` == age, ]
    ref_group <- ref_group %>% 
      select(-Education, -Age) %>% 
      pivot_wider(names_from = Variable,
                  names_glue = "{.value}_{Variable}",
                  values_from = c(T1, T2, T3, Total,
                                  Learning, DelayedRecall, PercentRetained,
                                  RecogHits, RecogFA, Recog_DiscriminationIndex, Recog_ResponseBias))
  }
  
  colnames(ref_group) <- tolower(colnames(ref_group))
  
  # Compute standardised scores
  out <- .compute_bvmtr_scores(ref_group, source,
                               t1, t2, t3,
                               learning, total,
                               delayed_recall, percent_retained,
                               recog_correct, recog_FA, recog_discrimination)
  
  extract_descriptors(out)
}






# Utility functions -------------------------------------------------------

.compute_bvmtr_scores <- function(reference, source,
                                  t1=NA, t2=NA, t3=NA,
                                  learning=NA, total=NA, delayed_recall=NA,
                                  percent_retained=NA,
                                  recog_correct=NA, recog_FA=NA, recog_discrimination=NA){
  
  # Precaution: Lee et al. (2012) does not compute norms for following variables; force NA if these entries are entered by user
  if (source == "Lee2012_Singapore") {
    t1 = NA
    t2 = NA
    learning = NA
    percent_retained = NA
    recog_discrimination = NA
  } 

  
  # Trial 1
  if (!is.na(t1)) {
    exclude_colnames <- colnames(reference[str_detect(colnames(reference), "t1-t3")])
    ref_t1 <- reference[!(colnames(reference) %in% exclude_colnames)]
    t1_mean = ref_t1[str_detect(colnames(ref_t1), "(^t1).+(mean)")]
    t1_sd = ref_t1[str_detect(colnames(ref_t1), "(^t1).+(sd)")]
    t1_zscore = (t1 - as.numeric(t1_mean[[1]])) / as.numeric(t1_sd)
  } else {
    t1_zscore = NA
  } 
  
  # Trial 2
  if (!is.na(t2)) {
    t2_mean = reference[str_detect(colnames(reference), "(^t2).+(mean)")]
    t2_sd = reference[str_detect(colnames(reference), "(^t2).+(sd)")]
    t2_zscore = (t2 - as.numeric(t2_mean[[1]])) / as.numeric(t2_sd[[1]])
  } else {
    t2_zscore = NA
  }
  
  # Trial 3
  if (!is.na(t3)) {
    t3_mean = reference[str_detect(colnames(reference), "(^t3).+(mean)")]
    t3_sd = reference[str_detect(colnames(reference), "(^t3).+(sd)")]
    t3_zscore = (t3 - as.numeric(t3_mean[[1]])) / as.numeric(t3_sd[[1]])
  } else {
    t3_zscore = NA
  }
  
  # Total
  if (!is.na(total)) {
    total_mean = reference[str_detect(colnames(reference), "(^t1-t3).+(mean)|(^total).+(mean)")]
    total_sd = reference[str_detect(colnames(reference), "(^t1-t3).+(sd)|(^total).+(sd)")]
    total_zscore = (total - as.numeric(total_mean[[1]])) / as.numeric(total_sd[[1]])
  } else {
    total_zscore = NA
  }
  
  # Learning
  if (!is.na(learning)) {
    learning_mean = reference[str_detect(colnames(reference), "(^learning).+(mean)")]
    learning_sd = reference[str_detect(colnames(reference), "(^learning).+(sd)")]
    learning_zscore = (learning - as.numeric(learning_mean[[1]])) / as.numeric(learning_sd[[1]])
  } else {
    learning_zscore = NA
  }
  
  # Delayed recall
  if (!is.na(delayed_recall)) {
    delayed_recall_mean = reference[str_detect(colnames(reference), "(^delay).+(mean)")]
    delayed_recall_sd = reference[str_detect(colnames(reference), "(^delay).+(sd)")]
    delayed_recall_zscore = (delayed_recall - as.numeric(delayed_recall_mean[[1]])) / as.numeric(delayed_recall_sd[[1]])
  } else {
    delayed_recall_zscore = NA
  }
  
  # Percent Retained
  if (!is.na(percent_retained)) {
    percent_retained_mean = reference[str_detect(colnames(reference), "(^percentretained).+(mean)")]
    percent_retained_sd = reference[str_detect(colnames(reference), "(^percentretained).+(sd)")]
    percent_retained_zscore = (percent_retained - as.numeric(percent_retained_mean[[1]])) / as.numeric(percent_retained_sd[[1]])
  } else {
    percent_retained_zscore = NA
  }
  
  # Recognition Correct
  if (!is.na(recog_correct)) {
    recog_correct_mean = reference[str_detect(colnames(reference), "(^recogcorrect).+(mean)|(^recoghits).+(mean)")]
    recog_correct_sd = reference[str_detect(colnames(reference), "(^recogcorrect).+(sd)|(^recoghits).+(sd)")]
    recog_correct_zscore = (recog_correct - as.numeric(recog_correct_mean[[1]])) / as.numeric(recog_correct_sd)
  } else {
    recog_correct_zscore = NA
  }
  
  # Recognition FA
  if (!is.na(recog_FA)) {
    recog_FA_mean = reference[str_detect(colnames(reference), "(^recogfa).+(mean)|(^falsealarms).+(mean)")]
    recog_FA_sd = reference[str_detect(colnames(reference), "(^recogfa).+(sd)|(^falsealarms).+(sd)")]
    recog_FA_zscore = (recog_FA - as.numeric(recog_FA_mean[[1]])) / as.numeric(recog_FA_sd)
  } else {
    recog_FA_zscore = NA
  }
  
  # Discrimination Index
  if (!is.na(recog_discrimination)) {
    recog_discrimination_mean = reference[str_detect(colnames(reference), "(discrimination).+(mean)")]
    recog_discrimination_sd = reference[str_detect(colnames(reference), "(discrimination).+(sd)")]
    recog_discrimination_zscore = (recog_discrimination - as.numeric(recog_discrimination_mean[[1]])) / as.numeric(recog_discrimination_sd[[1]])
  } else {
    recog_discrimination_zscore = NA
  }
  
  # Prepare output
  zscores <- list(
    Trial1 = c(t1_zscore, t1),
    Trial2 = c(t2_zscore, t2),
    Trial3 = c(t3_zscore, t3),
    Learning = c(learning_zscore, learning),
    Total = c(total_zscore, total),
    DelayedRecall = c(delayed_recall_zscore, delayed_recall),
    PercentRetained = c(percent_retained_zscore, percent_retained),
    Recognition_Hits = c(recog_correct_zscore, recog_correct),
    Recognition_FA = c(recog_FA_zscore, recog_FA),
    Recognition_Discrimination = c(recog_discrimination_zscore, recog_discrimination)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  out
}






