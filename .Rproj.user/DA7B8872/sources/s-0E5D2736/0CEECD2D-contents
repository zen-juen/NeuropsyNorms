#' Extract Descriptors for Performance Classification
#'
#' Classifies scores based on https://www.medfriendly.com/standardscoretopercentileconversion.html/
#' @param scores A dataframe of raw and standardised scores.
#' @param type A character string. For now only conducts z-score conversion.
#' @return A dataframe of raw scores, z-scores, and descriptors for each test.
#' @importFrom dplyr select
#' @export

extract_descriptors <- function(scores, type="z-scores") {

  if (type == "z-scores") {
    out <- as.data.frame(t(scores))     
    out$`Z-scores` = suppressWarnings(as.numeric(out$`Z-scores`))
    out$Descriptor = ifelse(out$`Z-scores` >= 2.00, "Very Superior",
                            ifelse(out$`Z-scores` >= 1.30 & out$`Z-scores` < 2.00, "Superior",
                                  ifelse(out$`Z-scores` >= 0.65 & out$`Z-scores` < 1.30, "Above Average/High Average",
                                         ifelse(out$`Z-scores` >= -0.70 & out$`Z-scores` < 0.65, "Average",
                                                ifelse(out$`Z-scores` >= -1.35 & out$`Z-scores` < -0.70, "Below Average/Low Average",
                                                       ifelse(out$`Z-scores` >= -2.00 & out$`Z-scores` < -1.35, "Borderline Impaired",
                                                              ifelse(out$`Z-scores` < -2.00, "Extremely Low/Impaired", NA)))))))
    
    out <- dplyr::select(out, `Raw Scores`, `Z-scores`, Descriptor)
    
  } else {
    print(paste("Error: Other standardised score conversions not available at the moment."))
  }
    out  
}


  