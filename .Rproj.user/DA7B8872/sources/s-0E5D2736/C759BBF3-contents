#' Computes Rey Auditory Verbal Learning Test (RAVLT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Lee2012_Singapore` or ` Wallace2017_ChineseAus`.
#' @param t1 Trial 1 score
#' @param t2 Trial 2 score
#' @param t3 Trial 3 score
#' @param t4 Trial 4 score
#' @param t5 Trial 5 score
#' @param listB List B score
#' @param t6 Trial 6 score (Short delay recall)
#' @param total Totals (Trials 1-5) score.
#' @param delayed_recall Delayed Free Recall score
#' @param recog_correct Recognition total number of correct items/hits.
#' @param recog_FA Recognition False Alarms.
#' @return A table of standardised scores and their descriptors.
#' @examples
#' out <- ravlt_norms(education=10, age=56, t1=5, t2=4, t3=6, t4=6, t5=10, total=15, listB=2, t6=6, delayed_recall=10, recog_correct=10, recog_FA=2);
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_extract
#' @importFrom dplyr select
#' @export

ravlt_norms <- function(education, age, male=TRUE,
                        source="Lee2012_Singapore",
                        t1=NA, t2=NA, t3=NA, t4=NA, t5=NA,
                        listB=NA, t6=NA, total=NA, delayed_recall=NA,
                        recog_correct=NA, recog_FA=NA){
  
  # Get normative samples
  url <- "https://github.com/zen-juen/NeuropsyNorms/blob/main/Database/RAVLT_Norms.xlsx?raw=true"
  destfile <- tempfile()
  download.file(url, destfile, mode = 'wb')
  data <- readxl::read_excel(destfile, sheet=source, col_names=FALSE)

  if (source == "Lee2012_Singapore") {
    print(paste("Lee et al. (2012) study ``N`` = 525 in Elderly Chinese individuals residing in Singapore aged between 54 - ≥75, education (in years) of 0 to >6. Tests were administered in different local languages (not taken into account in the norms reported). Scores are both age- and education-adjusted. Warning: groups may be even smaller after education and age group stratification."))
  } else if (source == "Wallace2017_ChineseAus"){
    print(paste("Wallace et al. (2017) study ``N`` = 145 in Chinese Australians aged between 55 - 87, education (in years) of < 12 and ≥12. Task instructions were given in English and interpreted into Chinese. Scores are age-, gender-, and education-adjusted. Warning: groups may be even smaller after gender, education and age group stratification."))
  } else {
    print(paste("Error: Other sources/norms not available at the moment."))
  }

  # Get education and age groups corresponding to normative sample
  demographic <- extract_demographic(education, age, male, source)
  education <- demographic[[1]]
  age <- demographic[[2]]
  gender <- demographic[[3]]
  
  # get reference group
  if (source == "Lee2012_Singapore") {
    df <- t(data)
    row.names(df) <- NULL
    colnames(df) <- df[1, ]
    df <- as.data.frame(df[-1, ])
    # Get reference
    ref_group <- df[df$`Education (Years)` == education & df$Age == age, ]
    
  } else if (source == "Wallace2017_ChineseAus"){
    colnames(data) <- data[1, ]
    df <- as.data.frame(data[-1, ])
    # Get reference
    ref_group <- df[df$`Education (Years)` == education & df$Age == age & df$Gender == gender, ]
    ref_group <- tidyr::pivot_wider(ref_group,
                                    names_from = Score,
                                    names_glue = "{.value}_{Score}",
                                    values_from = c(T1, T2, T3, T4, T5, `T1-T5`, ListB, T6, DelayedRecall, RecogHits))
  }

  colnames(ref_group) <- tolower(colnames(ref_group))
  
  # Compute standardised scores
  out <- .compute_ravlt_scores(ref_group, source,
                               t1, t2, t3, t4, t5,
                               listB, t6, total,
                               delayed_recall, recog_correct, recog_FA)
  extract_descriptors(out)
  }






# Utility functions -------------------------------------------------------

.compute_ravlt_scores <- function(reference, source,
                                  t1=NA, t2=NA, t3=NA, t4=NA, t5=NA,
                                  listB=NA, t6=NA, total=NA,
                                  delayed_recall=NA, recog_correct=NA, recog_FA=NA){

  # Precaution: Lee et al. (2012) does not compute norms for t2-t4; force NA if these entries are entered by user
  if (source == "Lee2012_Singapore") {
    t2 = NA
    t3 = NA
    t4 = NA
  } 
  if (source == "Wallace2017_ChineseAus") {
    recog_FA = NA
  } 
  
  # Trial 1
  if (!is.na(t1)) {
    exclude_colnames <- colnames(reference[stringr::str_detect(colnames(reference), "t1-t5")])
    ref_t1 <- reference[!(colnames(reference) %in% exclude_colnames)]
    t1_mean = ref_t1[stringr::str_detect(colnames(ref_t1), "(^t1).+(mean)")]
    t1_sd = ref_t1[stringr::str_detect(colnames(ref_t1), "(^t1).+(sd)")]
    t1_zscore = (t1 - as.numeric(t1_mean[[1]])) / as.numeric(t1_sd)
  } else {
    t1_zscore = NA
  } 
  
  # Trial 2
  if (!is.na(t2)) {
    t2_mean = reference[stringr::str_detect(colnames(reference), "(^t2).+(mean)")]
    t2_sd = reference[stringr::str_detect(colnames(reference), "(^t2).+(sd)")]
    t2_zscore = (t2 - as.numeric(t2_mean[[1]])) / as.numeric(t2_sd[[1]])
  } else {
    t2_zscore = NA
  }

  # Trial 3
  if (!is.na(t3)) {
    t3_mean = reference[stringr::str_detect(colnames(reference), "(^t3).+(mean)")]
    t3_sd = reference[stringr::str_detect(colnames(reference), "(^t3).+(sd)")]
    t3_zscore = (t3 - as.numeric(t3_mean[[1]])) / as.numeric(t3_sd[[1]])
  } else {
    t3_zscore = NA
  }
  
  # Trial 4
  if (!is.na(t4)) {
    t4_mean = reference[stringr::str_detect(colnames(reference), "(^t4).+(mean)")]
    t4_sd = reference[stringr::str_detect(colnames(reference), "(^t4).+(sd)")]
    t4_zscore = (t4 - as.numeric(t4_mean[[1]])) / as.numeric(t4_sd[[1]])
  } else {
    t4_zscore = NA
  }
  
  # Trial 5
  if (!is.na(t5)) {
    t5_mean = reference[stringr::str_detect(colnames(reference), "(^t5).+(mean)")]
    t5_sd = reference[stringr::str_detect(colnames(reference), "(^t5).+(sd)")]
    t5_zscore = (t5 - as.numeric(t5_mean[[1]])) / as.numeric(t5_sd[[1]])
  } else {
    t5_zscore = NA
  }
    
  # List B
  if (!is.na(listB)) {
    listb_mean = reference[stringr::str_detect(colnames(reference), "(^listb).+(mean)")]
    listb_sd = reference[stringr::str_detect(colnames(reference), "(^listb).+(sd)")]
    listb_zscore = (listB - as.numeric(listb_mean[[1]])) / as.numeric(listb_sd[[1]])
  } else {
    listb_zscore = NA
  }
    
  # Trial 6
  if (!is.na(t6)) {
    t6_mean = reference[stringr::str_detect(colnames(reference), "(^t6).+(mean)")]
    t6_sd = reference[stringr::str_detect(colnames(reference), "(^t6).+(sd)")]
    t6_zscore = (t6 - as.numeric(t6_mean[[1]])) / as.numeric(t6_sd[[1]])
  } else {
    t6_zscore = NA
  }

  # Totals (T1 to T5)
  if (!is.na(total)) {
    total_mean = reference[stringr::str_detect(colnames(reference), "(^t1-t5).+(mean)|(^.total).+(mean)")]
    total_sd = reference[stringr::str_detect(colnames(reference), "(^t1-t5).+(sd)|(^.total).+(sd)")]
    total_zscore = (total - as.numeric(total_mean[[1]])) / as.numeric(total_sd[[1]])
  } else {
    total_zscore = NA
  }
  
  # Delayed recall
  if (!is.na(delayed_recall)) {
    delayed_recall_mean = reference[stringr::str_detect(colnames(reference), "(^delay).+(mean)")]
    delayed_recall_sd = reference[stringr::str_detect(colnames(reference), "(^delay).+(sd)")]
    delayed_recall_zscore = (delayed_recall - as.numeric(delayed_recall_mean[[1]])) / as.numeric(delayed_recall_sd[[1]])
  } else {
    delayed_recall_zscore = NA
  }
  
  # Recognition Correct
  if (!is.na(recog_correct)) {
    recog_correct_mean = reference[stringr::str_detect(colnames(reference), "(^recogcorrect).+(mean)|(^recoghits).+(mean)")]
    recog_correct_sd = reference[stringr::str_detect(colnames(reference), "(^recogcorrect).+(sd)|(^recoghits).+(sd)")]
    recog_correct_zscore = (recog_correct - as.numeric(recog_correct_mean[[1]])) / as.numeric(recog_correct_sd)
  } else {
    recog_correct_zscore = NA
  }

  # Recognition FA
  if (!is.na(recog_FA)) {
    recog_FA_mean = reference[stringr::str_detect(colnames(reference), "(^recogfa).+(mean)|(^falsealarms).+(mean)")]
    recog_FA_sd = reference[stringr::str_detect(colnames(reference), "(^recogfa).+(sd)|(^falsealarms).+(sd)")]
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
    DelayedRecall = c(delayed_recall_zscore, delayed_recall),
    Recognition_Hits = c(recog_correct_zscore, recog_correct),
    Recognition_FalseAlarms = c(recog_FA_zscore, recog_FA)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  out
}





  
