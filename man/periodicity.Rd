% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/periodicity.R
\name{periodicity}
\alias{periodicity}
\title{Calculate periodicity metrics}
\usage{
periodicity(recorder_name, data, date_col = "date_start",
  recorder_col = "recorders", day_limit = 5)
}
\arguments{
\item{recorder_name}{the name of the recorder for whom you want to calculate the metrics}

\item{data}{the data.frame of recording information}

\item{date_col}{the name of the column that contains the date. This must be formatted as a date}

\item{recorder_col}{the name of the column that contains the recorder names}

\item{day_limit}{the threshold number of days a recorder must be active before these metrics are estimated. If the number of active days for the recorder is less than this number then the function will return NA values.}
}
\value{
A data.frame with seven columns
\itemize{
 \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
 \item{\code{periodicity} - }{The median number of days elapsed between each pair of sequential active days. This describes the regularity with which people record.}
 \item{\code{periodicity_variation} - }{The standard deviation of the times elapsed between each pair of sequential active days}
 \item{\code{median_streak} - }{The median length of streaks, including streak lengths of 1}
 \item{\code{sd_streak} - }{The standard deviation of streaks lengths, including streak lengths of 1}
 \item{\code{max_streak} - }{the length of this recorders longest streak}
 \item{\code{n_days} - }{The number of dates on which this recorder made observations}
}
}
\description{
This function takes in data for a recorder and calculates the periodicity metrics. All metrics are calculated within years.
}
\details{
In this function a streak is defined as a series of consecutive days on which a person made observations. A streak of 1 is a single days recording in isolation. 2 is 2 days back-to-back, 3 is 3 days in a row and so on.
}
\examples{
\dontrun{

# load example data
head(cit_sci_data)

# run for one recorder
P <- periodicity(recorder_name = 3007,
                data = cit_sci_data,
                date_col = 'date',
                recorder_col = 'recorder',
                day_limit = 5)

# Run the metric for all recorders
P_all <- lapply(unique(cit_sci_data$recorder),
               FUN = periodicity,
               data = cit_sci_data,
               date_col = 'date',
               recorder_col = 'recorder',
               day_limit = 5)

# summarise as one table
P_all_sum <- do.call(rbind, P_all)

hist(P_all_sum$max_streak)
}

}
