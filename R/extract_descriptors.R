#' Extract Descriptors for Performance Classification
#'
#' Classifies scores based on https://www.medfriendly.com/standardscoretopercentileconversion.html/
#' @param scores A dataframe of raw and standardised scores.
#' @param type A character string. For now only conducts z-score conversion.
#' @return A dataframe of raw scores, z-scores, and descriptors for each test.
#' @export

extract_descriptors <- function(scores, type="z-scores") {

  if (type == "z-scores") {
    out <- as.data.frame(t(scores)) %>%     
      mutate(`Z-scores` = suppressWarnings(as.numeric(`Z-scores`))) %>% 
      mutate(Descriptor = ifelse(`Z-scores` >= 2.00, "Very Superior",
                                 ifelse(`Z-scores` >= 1.30 & `Z-scores` < 2.00, "Superior",
                                        ifelse(`Z-scores` >= 0.65 & `Z-scores` < 1.30, "Above Average/High Average",
                                               ifelse(`Z-scores` >= -0.70 & `Z-scores` < 0.65, "Average",
                                                      ifelse(`Z-scores` >= -1.35 & `Z-scores` < -0.70, "Below Average/Low Average",
                                                             ifelse(`Z-scores` >= -2.00 & `Z-scores` < -1.35, "Borderline Impaired",
                                                                    ifelse(`Z-scores` < -2.00, "Extremely Low/Impaired", NA)))))))) %>% 
      select(`Raw Scores`, `Z-scores`, Descriptor)
    
  } else {
    print(paste("Error: Other standardised score conversions not available at the moment."))
  }

    out  
}


  