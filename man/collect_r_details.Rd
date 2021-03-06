% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/details.R
\name{collect_r_details}
\alias{collect_r_details}
\title{Information about running R}
\usage{
collect_r_details(.envs = NULL, .pkgs = NULL)
}
\arguments{
\item{.envs}{environment variables to retain}

\item{.pkgs}{packages to load in new session, if null defaults to all loaded in current session}
}
\description{
Information about running R
}
\details{
.envs will naturally grab all R_<var> variables, such as R_LIBS_SITE
to set
}
