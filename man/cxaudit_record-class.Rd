% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxaudit_record.R
\docType{class}
\name{cxaudit_record-class}
\alias{cxaudit_record-class}
\alias{cxaudit_record}
\title{Reference class representing an audit record}
\description{
Represents an audit record.

The class is initialized with a vector or list of named properties.
\itemize{
\item \code{event} on the object being audited
\item \code{type} of audited object
\item \code{class} of the audited object type
\item \code{reference} as a common reference to the object
\item \code{object} as a digest referring to the object
\item \code{label} as a human readable label to the audited event
\item \code{actor} meaning the user or service performing or initiating the event
}

The \code{event} is one of the supported key words \code{create}, \code{read}, \code{update},
\code{delete}, \code{execute}, \code{fail}, \code{commit}, \code{lock}, \code{unlock}, \code{sign}, \code{connect} and
\code{disconnect}. The events \code{create}, \code{read}, \code{update}, and \code{delete} refer to
the content of the object and its state. The \code{lock} and \code{unlock} events are
special states that are audited separately and are synonymous  to
\emph{read-only} and \emph{write enabled}, respectively.

The \code{reference} is intended as a short programmatic reference to an audited
item such that \code{reference} represents the same audited object across the
process. A simple example can be \code{ADAE} to represent the Adverse Event data set
and \code{T_AE} as representing a standard Adverse Event summary table.

The \code{object} is a digest reference to the object itself and is used to track
audited events across environments. As an example, a file can use the file's
digest, hash, checksum, etc. to effectively track the version of that file
across environments and actions that are registered in the audit trail.

The default \code{object} is the SHA-1 digest of the string
\verb{<type>:<class>:<reference>}.

The special properties \code{id} and \code{datetime} are assigned when the the audit
record is stored and are only available for saved audit records.

The \code{getproperties} method returns a list of the record reference named
properties.

The \code{getproperty} method returns the value of specified property \code{x}. If \code{x}
is not defined as a property, \code{NULL} is returned.

The \code{setproperty} method permits setting the record properties \code{object} and
\code{label} after the audit record has been created as long as the properties
\code{id} and \code{datetime} are not defined (key indicators of a saved audit record).

One or more additional attributes can be recorded as part of the audit record.
An attribute is a key/value pair.

An attribute \code{x} can be associated with the audit record. If the attribute
\code{value} is \code{NULL}, all entries associated with the attribute \code{x} is dropped.
The optional attribute \code{label} is a human readable version of \code{x}. A
qualifying keyword \code{qualfier} can be associated with the attribute value,
such as \code{new}, \code{old}, etc. Note that the attribute name \code{x} does not have to
be unique.

The \code{getattributes} method returns a character vector of named elements
representing the record attributes.

The \code{getattribute} method returns the value of specified attribute \code{x}. If \code{x}
is not defined as an attribute, \code{NULL} is returned.

If the audit record represents a saved audit record, one or more \emph{linked}
audit record references represents audit records that were saved to the
audit trail in conjunction with the current record. The \code{links} method returns
an un-ordered list of audit record references. A new record has no links.
}
\section{Fields}{

\describe{
\item{\code{.attr}}{Internal storage of record properties and attributes}
}}

\section{Methods}{

\describe{
\item{\code{getattribute(x)}}{Return a named record attribute}

\item{\code{getattributes()}}{Return list of record attributes}

\item{\code{getproperties()}}{Return list of record properties}

\item{\code{getproperty(x)}}{Return a named record property}

\item{\code{initialize(x)}}{Initialize record representation}

\item{\code{setattribute(x, value = NULL, label = NULL, qualifier = NULL)}}{Set record attributes}

\item{\code{setproperty(x, value)}}{Set named record property}
}}

