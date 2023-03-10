% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Instruction.R
\name{Instruction}
\alias{Instruction}
\title{R6 Class for recipe instructions}
\description{
An \code{Instruction} is an R6 object, and it can be created using
\code{Instruction$new()}. It only has one field, \code{text}, that contains the
instruction detail.
}
\section{Fields}{

\describe{
\item{\code{text}}{Scalar character vector; the wording of one step of a recipe.}
}}

\seealso{
\link{Recipe}

Other recipe components: 
\code{\link{Equipment}},
\code{\link{Image}},
\code{\link{Ingredient}}
}
\concept{recipe components}
