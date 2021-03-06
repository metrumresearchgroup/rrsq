% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qview.R
\name{get_queue_data}
\alias{get_queue_data}
\title{Function to grab a dataframe of the jobs in the queue for the given user.}
\usage{
get_queue_data(queue_obj, .user)
}
\arguments{
\item{queue_obj}{The RSimpleQueue object currently in use.}

\item{.user}{The user as a character string -- only this user's jobs will be returned.}
}
\description{
Function to grab a dataframe of the jobs in the queue for the given user.
}
