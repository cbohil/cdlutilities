\name{cataccuracy}
\alias{cataccuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute categorization task accuracy rate
%%  ~~function to do ... ~~
}
\description{
Compute accuracy by block (any number of categories)
}
\usage{
catAccuracy(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data needs to be a list of data frames.  Each data set needs columns labeled "block", "cat", & "resp"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# data needs to be a list of data frames
# each data set needs columns labeled "block", "cat", & "resp"
warning("catAccuracy needs to be generalized to send in labels for block, cat & resp")

catAccuracy = function(data=cat2_data) {
  accuracy <- lapply(data, function(data) {lapply(split(data, data$block), function(data) {length(which(data$cat == data$resp))/nrow(data)})})
  accuracy <- matrix( unlist(accuracy), nrow=length(accuracy) , byrow = TRUE)
  return(accuracy)
  }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
