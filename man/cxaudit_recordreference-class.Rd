% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxaudit_recordreference.R
\docType{class}
\name{cxaudit_recordreference-class}
\alias{cxaudit_recordreference-class}
\alias{cxaudit_recordreference}
\title{Reference class representing an audit record reference}
\description{
Represents an abbreviated reference to an audit record. The class is intended
for references and not for creating and for registering an audit record.

The \code{getproperties} method returns a list of the record reference named
properties.

The \code{getproperty} method returns the value of specified property \code{x}. If \code{x}
is not defined as a property, \code{NULL} is returned.
}
\section{Fields}{

\describe{
\item{\code{.attr}}{Internal storage of record properties and attributes}
}}

\section{Methods}{

\describe{
\item{\code{getproperties()}}{Return list of record properties}

\item{\code{getproperty(x)}}{Return a named record property}

\item{\code{initialize()}}{Initialize record representation}
}}

