#' Calculate revisit metrics
#' 
#' This function takes in data for a recorder and calculates the revisit metrics. A visit is defined as a unique combination of recording site and date. 
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#' @param recorder_col the name of the column that contains the recorder names
#' @param location_col the name of the column that contains the location. This is a character, such as a grid reference and should be representative of the scale at which recording is done over a single day, typically 1km-square is used.
#' @param summer_days Revisit metrics is only calculated for the summmer period is this parameter is provided. This is a three column data.frame: year, Jday_start (numeric Julian day of the first day of summer), Jday_end (numeric Julian day of the last day of summer). These are returned as an attribute from \code{summerData}.
#'     
#' @export
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' 
#' INSERT EXAMPLES
#' 
#' 
#' }
#' 
#' @return A data.frame with seven columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{visits} - }{}
#'  \item{\code{sites} - }{}
#'  \item{\code{PropRevisited} - }{}
#'  \item{\code{meanRevisits} - }{}
#'  \item{\code{SD_meanRevisits} - }{}
#' }

recorderVisits <-
  function(recorder_name,
           data,
           date_col = 'dates',
           recorder_col = 'recorders',
           location_col = 'kmsq',
           summer_days = NULL){
    
    # get the data for this recorder
    rec_data <- data[data[ ,recorder_col] == recorder_name, ]
    
    # create a unique visit column
    rec_data$visit <- paste(rec_data[ , recorder_col], rec_data[ , location_col],
                            sep = '_')
    
    # Find the number of unique visits per recorder
    visits <- length(unique(rec_data$visit))
    
    # Find the sites that have been visited more than once and make a temp df
    revisSites <- as.data.frame(rec_data %>% group_by(rec_data[ , location_col]) %>%
                                  tally())
    
    # Fins the number of unique sited visited by the recorder
    sites <- length(unique(revisSites[,1]))
    
    # Find the proportion of total sites visited that have been revisited (visited more than once) by the recorder
    sitesRevisited <- (nrow(subset(revisSites, n>1)))/(length(unique(rec_data[ , "km10grid"])))
    
    # Find the mean number of revisits per site per recorder
    meanRevisits <- mean((subset(revisSites, n>1))$n)
    SD_meanRevisits <- sd((subset(revisSites, n>1))$n)
    
    
    # Create empty dataframe to be populated
    output = data.frame(recorder = recorder_name, 
                        visits = visits,
                        sites = sites,
                        PropRevisited = sitesRevisited,
                        meanRevisits = meanRevisits,
                        SD_meanRevisits = SD_meanRevisits)
    
    return(output)
  }