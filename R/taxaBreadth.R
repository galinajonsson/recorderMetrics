#' Metrics identifying the breadth and proportion of taxa recorded 
#' 
#' These metrics describe the 'experience' the recorder has had recording species within the group.
#'
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param sp_col the name of the column that contains the species names
#' @param recorder_col the name of the column that contains the recorder names
#' @param set_taxa_breadth optional integer that defines the taxa breadth and is applied to all recorders. Default is FALSE, i.e. total number of species recorded is found for each individual recorder
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' TB <- taxaBreadth(recorder_name = 3007,
#' data = cit_sci_data,
#' sp_col = 'species',
#' recorder_col = 'recorder')
#'
#' head(TB)
#' 
#' # Run for more than one recorder, this can be slow 
#' TB_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = taxaBreadth, 
#'                  data = cit_sci_data, 
#'                  sp_col = 'species',
#'                  recorder_col = 'recorder')
#'
#' # summarise as one table
#' TB_all_sum <- do.call(rbind, TB_all)
#'
#' hist(TB_all_sum$taxa_prop, breaks = 40)
#'
#' 
#' } 
#' 
#' @return A data.frame with four columns is returned.
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{taxa_breadth} - }{The total number of species recorded by this recorder}
#'  \item{\code{taxa_prop} - }{The proportion of species recorded by this species. Calculated as \code{taxa_breadth} divided by the total number of species recorded in \code{data}.}
#'  \item{\code{n} - }{The total number of observations made by this recorder}
#' }
#'

taxaBreadth <- function(recorder_name,
                         data,
                         sp_col = 'preferred_taxon',
                         recorder_col = 'recorders',
                        set_taxa_breadth = NULL){
  
  data_rec <- data[data[,recorder_col] == recorder_name, c(sp_col, recorder_col)]
  
  if(is.null(set_taxa_breadth)){
  
  return(data.frame(recorder = recorder_name,
                    taxa_breadth = length(unique(data_rec[ ,sp_col])),
                    taxa_prop = length(unique(data_rec[ ,sp_col]))/length(unique(data[ ,sp_col])),
                    n = nrow(data_rec)))
    
  } else if(!is.null(set_taxa_breadth)){
    
    return(data.frame(recorder = recorder_name,
                      taxa_breadth = set_taxa_breadth,
                      taxa_prop = length(unique(data_rec[ ,sp_col]))/set_taxa_breadth,
                      n = nrow(data_rec)))
    
  } 
}
