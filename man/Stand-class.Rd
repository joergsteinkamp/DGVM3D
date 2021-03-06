% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{Stand-class}
\alias{Stand-class}
\title{One model stand consisting of several patches}
\description{
One model stand consisting of several patches
}
\section{Slots}{

\describe{
\item{\code{patches}}{list of patches in one stand}

\item{\code{area}}{the area of each patch}

\item{\code{year}}{the year of the current patch vegetation}

\item{\code{hexagon}}{a \code{\link{TriangBody-class}} Hexagon definition used for all patches}

\item{\code{layout}}{either 'linear' or 'square'}

\item{\code{composition}}{either 'spatial' or 'temporal'. Has no effect yet.}

\item{\code{patch.pos}}{the position of the hexagon centers of each patch}
}}

