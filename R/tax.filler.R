#' Fill taxonomy based on previous hierarchy
#'
#' This function gets taxonomy df, each tax level per column, and fill the empty taxonomy levels based on the higher taxonomy hierarchy. Empty taxonomy are either NA or empty strings. Data frame has to be of character type and not factors.
#' @keywords
#' taxonomy, NA, fill
#' @export
#' @examples
#' tax.filler(tax, 3,6)

tax.filler <- function(df, start, end){
  # gets a dataframe with taxon names and fill the gapes based on previous taxonomy
  require(stringr)
  
  # If start and end are one clorser to each other
  nc <- ncol(df)
  nr <- nrow(df)
  
  # It need to be sorted to avoid filling gaps with empty values
  cols <- sort(seq(start, end))
  
  for (n in cols){
    c1 <- n-1
    c2 <- n
    for (i in 1:nr){
      if (as.character(df[i, c2]) %in% c('', "NA", NA)){
        if(is.na(df[i, c1])){ charac.c1 <- ""}else{
          charac.c1 <- as.character(df[i, c1])}
        # if statement necessary to avoid pasting uncultured over uncultured
        ifelse(str_detect(charac.c1, '(unc.)'),
               df[i, c2] <- charac.c1,
               df[i, c2] <- paste0(charac.c1, ' (unc.)')
        )
      }
    }
  }
  return(df)
}