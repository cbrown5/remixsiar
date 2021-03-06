% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterative_grp.R
\name{iterative_grp}
\alias{iterative_grp}
\title{Iteratively apply \code{group_sources} or \code{rm_sources}}
\usage{
iterative_grp(nsources, mingrps, s_means, s_sds, s_names, method = "group")
}
\arguments{
\item{nsources}{\code{integer} giving number of sources}

\item{mingrps}{\code{integer} the minimum number of groups}

\item{s_means}{\code{matrix} giving tracer means (columns) for each source}

\item{s_sds}{\code{matrix} giving tracer SD (columns) for each source}

\item{s_names}{\code{character} giving source names}

\item{method}{\code{character} giving method}
}
\value{
a \code{list} of outputs from \code{group_sources}
}
\description{
Iteratively apply \code{group_sources} or \code{rm_sources}
}
\details{
Method can either be "group" or "rm". Group aggregates 
similar sources, rm removes one of them.
}
\author{
Christopher J. Brown
}
