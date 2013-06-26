\name{chooseByPriority}
\alias{chooseByPriority}


\title{
Chooses an item from a vector according to a defined priority order
}

\description{
Chooses a single item of x according to the priority order choice_order
}

\usage{
chooseByPriority(x, choice_order)
}

\arguments{
  \item{x}{
a numeric vector
}
  \item{choice_order}{
a numeric vector giving the order of priorities.
Items near the front of the vector have higher priority.
}
}

\value{
returns the value of choice_order with the smallest index that is in x. 
NA if no element of x occurs in choice_order.
}


\note{
Used by \code{\link{addToCohort}} to select a single entry per patient.
}

\seealso{
\code{\link{addToCohort}}, \code{\link{addCodelistToCohort}}
}

\examples{
##
chooseByPriority(1:4, 3:5) # returns 3
chooseByPriority(1:4, 5:7) # returns NA
}
