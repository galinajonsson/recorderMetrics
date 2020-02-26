#' Calculate revisit metrics
#' 
#' This function takes in data for a recorder and calculates the revisit metrics. A visit is defined as a unique combination of recording site and date. 
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#' @param recorder_col the name of the column that contains the recorder names
#' @param location_col the name of the column that contains the location. This is a character, such as a grid reference and should be representative of the scale at which recording is done over a single day, typically 1km-square is used.
#' 
#' @export
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Run for a single recorder
#' RV <- recorderVisits(recorder_name = 3007,
#'                      data = cit_sci_data,
#'                      date_col = 'date',
#'                      recorder_col = 'recorder',
#'                      location_col = 'location')
#'                    
#'                    
#'                    
#' # Run the metric for all recorders
#' RV_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = recorderVisits,
#'                  data = cit_sci_data,
#'                  date_col = 'date',
#'                  recorder_col = 'recorder',
#'                  location_col = 'location')
#'
#' # summarise as one table
#' RV_all_sum <- do.call(rbind, RV_all)
#'
#' }
#' 
#' @return A data.frame with six columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{visits} - }{the total number of visits per recorder}
#'  \item{\code{sites} - }{The total number of sites visited by the recorder}
#'  \item{\code{PropRevisited} - }{The proportion of total sites visited that have been revisited, i.e. visited more than once, by the recorder}
#'  \item{\code{meanRevisits} - }{The mean number of visits per site per recorder for sites visited more than once.}
#'  \item{\code{SD_meanRevisits} - }{The standard deviation of the mean number of visits per site per recorder for sites visited more than once.}
#' }

recorderVisits <-
  function(recorder_name,
           data,
           date_col = 'dates',
           recorder_col = 'recorders',
           location_col = 'location'){
    
    # check date column
    if(!inherits(data[, date_col], 'Date')){
      stop('Your date column is not a date')
    }
    
    # check name
    if(!recorder_name %in% data[,recorder_col]) {
      stop(paste(recorder_name, 'does not appear in the recorder column of your data'))
    }
    
    # check location
    if(!inherits(data[, location_col], 'Character')) {
      stop('Your location column is not a character')
    }
    
    # get the data for this recorder
    rec_data <- data[data[ ,recorder_col] == recorder_name, ]
    
    # create a unique visit column
    rec_data$visit <- paste(rec_data[ , recorder_col], rec_data[ , location_col],
                            sep = '_')
    
    # Find the number of unique visits per recorder
    visits <- length(rec_data$visit)
    
    # Find the sites that have been visited more than once and make a temp df
    revisSites <- as.data.frame(rec_data %>% group_by(rec_data[ , location_col]) %>%
                                  tally())
    
    # Fins the number of unique sited visited by the recorder
    sites <- length(unique(rec_data[ , location_col]))
    
    # Find the proportion of total sites visited that have been revisited (visited more than once) by the recorder
    sitesRevisited <- (nrow(subset(revisSites, n>1)))/(length(unique(rec_data[ , location_col])))
    
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