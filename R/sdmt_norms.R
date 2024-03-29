#' Computes Symbol Digit Modality Test (SDMT) norms.
#'
#' @param education Education in years.
#' @param age Age in years.
#' @param male TRUE or FALSE.
#' @param source Can be `Kiely2014_Aus_Written` or ` Hsieh2007_China`.
#' @param written Written performance score.
#' @param verbal Verbal performance score.
#' @return A table of standardised scores and their descriptors.
#' @examples
#' out1 <- sdmt_norms(education="postsecondary", age=20, male=TRUE, written=50, verbal=50);
#' out2 <- sdmt_norms(education="tertiary", age=60, male=TRUE, written=50, verbal=50);
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr rename
#' @export

sdmt_norms <- function(education, age, male=TRUE,
                       source="Kiely2014_Aus_Written",
                       written=NA, verbal=NA){
  
  # Get normative sample
  url <- "https://github.com/zen-juen/NeuropsyNorms/blob/main/Database/SDMT_Norms.xlsx?raw=true"
  destfile <- tempfile()
  download.file(url, destfile, mode = 'wb')
  data <- readxl::read_excel(destfile, sheet=source, col_names=FALSE)
  
  if (source == "Kiely2014_Aus_Written") {
    print(paste("Kiely et al. (2014) study ``N`` = 15,165 English-speaking individuals residing in Australia (<16% per stratified group identify with non-English-speaking background) aged between 15 - 85+, with highest educational attainment ranging from Year 11 or less to tertiary degrees. Tests were administered in English. Scores are adjusted for age, education, and gender. Only written norms are provided."))
  } else if (source == "Hsieh2007_China") {
    print(paste("Hsieh & Tori (2007) study ``N`` = 324 in Mandarin-speaking individuals residing in China aged between 30 - 81. Tests were translated and administered in Mandarin. Scores are age-adjusted for persons between 30 - 81, and both age- and gender-adjusted for ages 61-81. Both written and verbal norms are provided."))
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
  if (source == "Kiely2014_Aus_Written") {

    df <- tidyr::pivot_longer(df,
        cols = `15-19`:`85+`,
        names_to = "Age group"
      )
    df <- dplyr::rename(df, Variable = Age)
    df <- tidyr::pivot_wider(df,
        names_from = Variable,
        names_glue = "{.value}_{Variable}"
      )
    ref_group <- df[df$`Age group` == age & df$Gender == gender & df$Education == education, ]
    
    
  } else if (source == "Hsieh2007_China") {
    if (!is.na(gender)) {
      ref_group <- df[df$Age == age & df$Gender == gender, ]
    } else {
      ref_group <- df[df$Age == age, ]
    }
  }
  colnames(ref_group) <- tolower(colnames(ref_group))
  
  # Compute standardised scores
  out <- .compute_sdmt_scores(ref_group, source, written, verbal)
  extract_descriptors(out)
}



# Utility functions -------------------------------------------------------

.compute_sdmt_scores <- function(reference, source,
                                 written=NA, verbal=NA){

  # Precaution: Kiely et al. (2014) only computes norms for written version. Force NA for verbal score if entered.
  if (source == "Kiely2014_Aus_Written") {
    verbal = NA
  }

  # Written
  if (source == "Kiely2014_Aus_Written") {
    if (reference$`age group` == "15-19" || reference$`age group` == "75-79" || reference$`age group` == "80-84" || reference$`age group` == "85+") {
      if (reference$education == "Tertiary" || reference$education == "Postsecondary") {
        print("Warning: No norms available for this stratified group.")
        written = NA
      }}}  # forca NA for specific stratfied groups to avoid error

  if (!is.na(written)) {
    written_mean = reference[stringr::str_detect(colnames(reference), "(written).+(mean)|(value).+(mean)")]
    written_sd = reference[stringr::str_detect(colnames(reference), "(written).+(sd)|(value).+(sd)")]
    written_zscore = (as.numeric(written_mean) - written) / as.numeric(written_sd)
  } else {
    written_zscore = NA
  } 
    
  # Verbal
  if (!is.na(verbal)) {
    verbal_mean = reference[stringr::str_detect(colnames(reference), "(verbal).+(mean)")]
    verbal_sd = reference[stringr::str_detect(colnames(reference), "(verbal).+(sd)")]
    verbal_zscore = (as.numeric(verbal_mean) - verbal) / as.numeric(verbal_sd)
  } else {
    verbal_zscore = NA
  }
  
  
  zscores <- list(
    Written = c(written_zscore, written),
    Verbal = c(verbal_zscore, verbal)
  )
  
  out <- as.data.frame(zscores)
  row.names(out) <- c("Z-scores", "Raw Scores")
  
  return(out)
}


