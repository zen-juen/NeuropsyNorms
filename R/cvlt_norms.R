#' Computes California Verbal Learning Test (CVLT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Only computes `Whittle2007_US` norms (CVLT-II short form) for now.
#' @param version Character string. Only computes `II` for now (i.e., CVLT-II)
#' @param form Character string. Only computes `short` for now (i.e., CVLT short form)
#' @param t1 Trial 1 score
#' @param t2 Trial 2 score
#' @param t3 Trial 3 score
#' @param t4 Trial 4 score
#' @param t5 Trial 5 score
#' @param listB List B score
#' @param t6 Trial 6 score
#' @param total Totals (Trials 1-5 for standard form; Trails 1-4 for short form)
#' @param delayed_free_recall Delayed Free Recall score
#' @param delayed_cued_recall Delayed Cued Recall score
#' @param recog_correct Recognition total number of correct items/hits.
#' @param recog_FA Recognition False Alarms.
#' @return A table of standardised scores and their descriptors.
#' @examples
#' out1 <- cvlt_norms(education=10, age=80, t1=5, t2=5, t3=6, t4=6, total=22, delayed_free_recall=10);
#' @export
 
cvlt_norms <- function(education, age, male=TRUE,
                       source="Whittle2007_US", version="II", form ="short",
                       t1=NA, t2=NA, t3=NA, t4=NA, t5=NA,
                       listB=NA, t6=NA, total=NA, delayed_free_recall=NA,
                       delayed_cued_recall=NA,
                       recog_correct=NA, recog_FA=NA){
  
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
  
  # Extract correct version and form
  if (tolower(form) == "standard") {
    get = paste0("CVLT", version) 
  } else if (tolower(form) == "short") {
    get = paste0("CVLT", version, "SF")
  }
   
  data <- read_excel("../Database/CVLT_Norms.xlsx", sheet=paste0(source, " - ", get), col_names=FALSE)
  
  # Get normative samples
  if (source == "Whittle2007_US") {
    print(paste("Whittle et al. (2007) ``N`` = 339  (The 90+ Study) in English-speaking individuals residing in US aged between 90 - 103. Only the letter `F` was administered as part of letter fluency to avoid fatigue. Scores are age-adjusted. The assessment was administered in English. Scores are age-adjusted."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }
  
  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  gender <- demographic[[3]]
  
  # get reference group
  if (source == "Whittle2007_US") {
    colnames(data) <- data[1, ]
    df <- as.data.frame(data[-1, ])
    # Get reference
    ref_group <- df[df$Age == age, ]
    ref_group <- ref_group %>% 
      select(-`5%`, -`10%`, -`25%`, -`50%`, -`75%`, -`90%`, -`95%`, -`Sample Size`) %>% 
      pivot_wider(names_from = Measure,
                  names_glue = "{Measure}_{.value}",
                  values_from = c(Mean, SD))
  }
  
  colnames(ref_group) <- tolower(colnames(ref_group))
  
  # Compute standardised scores
  out <- .compute_cvlt_scores(ref_group, source,
                              t1, t2, t3, t4, t5,
                              listB, t6, total,
                              delayed_free_recall,
                              delayed_cued_recall, 
                              recog_correct, recog_FA)
  extract_descriptors(out)
}






# Utility functions -------------------------------------------------------

.compute_cvlt_scores <- function(reference, source,
                                 t1=NA, t2=NA, t3=NA, t4=NA, t5=NA,
                                 listB=NA, t6=NA, total=NA, delayed_free_recall=NA,
                                 delayed_cued_recall=NA,
                                 recog_correct=NA, recog_FA=NA){
  
  # Precaution: Whittle et al. (2007) does not compute norms for following variables; force NA
  if (source == "Whittle2007_US") {
    t2 = NA; t3 = NA; t5 = NA; listB = NA; t6 = NA; recog_correct = NA; recog_FA = NA
  } 

  # Trial 1
  if (!is.na(t1)) {
    exclude_colnames <- colnames(reference[str_detect(colnames(reference), "t4|t5")])
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
  
  # Trial 4
  if (!is.na(t4)) {
    t4_mean = reference[str_detect(colnames(reference), "(^t4).+(mean)")]
    t4_sd = reference[str_detect(colnames(reference), "(^t4).+(sd)")]
    t4_zscore = (t4 - as.numeric(t4_mean[[1]])) / as.numeric(t4_sd[[1]])
  } else {
    t4_zscore = NA
  }
  
  # Trial 5
  if (!is.na(t5)) {
    t5_mean = reference[str_detect(colnames(reference), "(^t5).+(mean)")]
    t5_sd = reference[str_detect(colnames(reference), "(^t5).+(sd)")]
    t5_zscore = (t5 - as.numeric(t5_mean[[1]])) / as.numeric(t5_sd[[1]])
  } else {
    t5_zscore = NA
  }
  
  # List B
  if (!is.na(listB)) {
    listb_mean = reference[str_detect(colnames(reference), "(^listb).+(mean)")]
    listb_sd = reference[str_detect(colnames(reference), "(^listb).+(sd)")]
    listb_zscore = (listB - as.numeric(listb_mean[[1]])) / as.numeric(listb_sd[[1]])
  } else {
    listb_zscore = NA
  }
  
  # Trial 6
  if (!is.na(t6)) {
    t6_mean = reference[str_detect(colnames(reference), "(^t6).+(mean)")]
    t6_sd = reference[str_detect(colnames(reference), "(^t6).+(sd)")]
    t6_zscore = (t6 - as.numeric(t6_mean[[1]])) / as.numeric(t6_sd[[1]])
  } else {
    t6_zscore = NA
  }
  
  # Totals (T1 to T5)
  if (!is.na(total)) {
    total_mean = reference[str_detect(colnames(reference), "(^t1-t4).+(mean)|(^t1-t5).+(mean)|(^.total).+(mean)")]
    total_sd = reference[str_detect(colnames(reference), "(^t1-t4).+(mean)|(^t1-t5).+(sd)|(^.total).+(sd)")]
    total_zscore = (total - as.numeric(total_mean[[1]])) / as.numeric(total_sd[[1]])
  } else {
    total_zscore = NA
  }
  
  # Delayed free recall
  if (!is.na(delayed_free_recall)) {
    delayed_free_recall_mean = reference[str_detect(colnames(reference), "(freerecall).+(mean)")]
    delayed_free_recall_sd = reference[str_detect(colnames(reference), "(freerecall).+(sd)")]
    delayed_free_recall_zscore = (delayed_free_recall - as.numeric(delayed_free_recall_mean[[1]])) / as.numeric(delayed_free_recall_sd[[1]])
  } else {
    delayed_free_recall_zscore = NA
  }
  
  # Delayed cued recall
  if (!is.na(delayed_cued_recall)) {
    delayed_cued_mean = reference[str_detect(colnames(reference), "(cued).+(mean)")]
    delayed_cued_sd = reference[str_detect(colnames(reference), "(cued).+(sd)")]
    delayed_cued_zscore = (delayed_cued_recall - as.numeric(delayed_cued_mean[[1]])) / as.numeric(delayed_cued_sd[[1]])
  } else {
    delayed_cued_zscore = NA
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
  
  # Prepare output
  zscores <- list(
    Trial1 = c(t1_zscore, t1),
    Trial2 = c(t2_zscore, t2),
    Trial3 = c(t3_zscore, t3),
    Trial4 = c(t4_zscore, t4),
    Trial5 = c(t5_zscore, t5),
    Total = c(total_zscore, total),
    ListB = c(listb_zscore, listB),
    Trial6 = c(t6_zscore, t6),
    Delayed_FreeRecall = c(delayed_free_recall_zscore, delayed_free_recall),
    Delayed_CuedRecall = c(delayed_cued_zscore, delayed_cued_recall),
    Recognition_Hits = c(recog_correct_zscore, recog_correct),
    Recognition_FalseAlarms = c(recog_FA_zscore, recog_FA)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  out
}






