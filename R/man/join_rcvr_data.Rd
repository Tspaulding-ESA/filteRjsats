% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_receiver.R
\name{join_rcvr_data}
\alias{join_rcvr_data}
\title{Add Receiver Data to a Completely Filtered Detection File}
\usage{
join_rcvr_data(final_file, rcvr_data)
}
\arguments{
\item{final_file}{a dataframe of detections retrieved from second_filter}

\item{rcvr_data}{a dataframe of receiver metadata retrieved from get_rcvr_data}
}
\value{
A dataframe to which receiver metadata has been added.
}
\description{
This function takes any acoustic receiver detection dataframe which contains
receiver serial numbers in a \code{ReceiverSN} field, such as that produced by
\verb{second_filter_*()}, and joins it with receiver metadata generated by
the \code{get_rcvr_data()} or \code{format_receiver()} functions.
}
\examples{
# Join receiver metadata to detection data
join_rcvr_data(fish_detects, cft_rcvrs)
}
