% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DeepS_find_laser.R
\name{DeepS_find_laser}
\alias{DeepS_find_laser}
\title{This function detects double green lasers on a towed benthic sledge}
\usage{
DeepS_find_laser(MP4_file, para, freq)
}
\arguments{
\item{MP4_file}{is the video file assigned MP4_file <- file.choose() for example}

\item{para}{is a parallelization option to optimize the computing time FALSE for this version}

\item{freq}{is the frame frequency used. A shorter number increases the computing time.}
}
\value{
a result is saved also in user/documents directory as laser_position_$yourvideonames$.txt at each iteration.
}
\description{
This function detects double green lasers on a towed benthic sledge
}
