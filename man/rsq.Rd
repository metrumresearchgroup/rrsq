% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSimpleQueue.R
\docType{class}
\name{RSimpleQueue}
\alias{RSimpleQueue}
\title{Create a new rsq server manager}
\description{
The Babylon generator creates a new 'Babylon'-object, which contains logic to interact
with a running bbq server from the babylon ecosystem.
}
\section{Initialization}{

A new rsq object is initialized with the new() method:

`rsq <- RSimpleQueue$new()`

and can optionally take some initialization arguments:

* `host` - the hostname the server is running on - defaults to 'http://localhost'
* `port` - port the server is listening on - defaults to 8999.
* `verbose` - whether internal status information should be displayed
* `mustWork` - on initialization, check and confirm a rsq server is listening on the host/port configuration set
}

\section{Methods}{

* `submit_job(...)` - check ?submit_models for arguments, the server is automatically set internally
* `get_jobs(status, STATUSES)` - get information about models that have been submitted
* `get_job(id)` - get information about a model
* `poll(.ids, until, timeout, interval, print, parse)` - poll a vector of models by ID name until completion
    * ids - vector of job ids
    * until - status criteria to poll against, default to COMPLETED or ERROR
    * timeout - length of time to poll before stopping
    * print - whether to print the status of all jobs each poll
}

\examples{
\dontrun{
rsq <- RSimpleQueue$new()
rsq$get_jobs()

# get all ids
bbq$get_jobs \%>\% map_dbl("ID")

# find all queued models
bbq$get_jobs(status = "QUEUED") \%>\% map_dbl("ID")

bbq$poll(1:5) # poll for models 1-5 to complete

# get all outputs for completed runs
rsq$get_jobs(status = "COMPLETED") \%>\% map_chr(~ .x$Result$Output)
}
}
\keyword{datasets}
