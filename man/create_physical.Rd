% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical-element.R
\name{create_physical}
\alias{create_physical}
\title{Create Physical Element}
\usage{
create_physical(
  file_path,
  number_of_headers = "1",
  record_delimiter = "\\\\n",
  attribute_orientation = "column",
  field_delimiter = c(",", " ", "\\\\t", ":"),
  data_url = NULL
)
}
\arguments{
\item{file_path}{The file path of the data set being documented. The file size
and authentication checksums will be generated from this input.}

\item{number_of_headers}{Number of header lines preceding the data. A default
of "1" is assigned if no input is given. Please refrain from inputting any other value
if possible as it is bad practice to do so.}

\item{record_delimiter}{Character used to delimit records. If no value is
inputted the default value of '\\n' is assigned.}

\item{attribute_orientation}{The orientation of the attributes. A default of
"column" will be assigned if no input is given.}

\item{field_delimiter}{Character used to delimit each field. The options
provided are commas (","), space (" "), tab ("\\t"), or colon (":"). The
default is comma, as csv's are most common.}

\item{data_url}{(Optional). A url, if possible, of which the data file can be
downloaded.}
}
\value{
A complete set of information on the physical format.
}
\description{
Create the information of the physical format of the dataset based off of EML standards.
}
\examples{
create_physical(file_path = "User/data/example.csv", 
             number_of_headers = "1",
             record_delimiter = "\\\\r\\\\n", 
             attribute_orientation = "column", 
             field_delimiter = ",",
             data_url = "https://mydata.org/etc")
             
create_physical(file_path = "User/data/example.csv",
             data_url = "https://mydata.org/etc")
}
