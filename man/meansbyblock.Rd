\name{meansbyblock}
\alias{meansbyblock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute means for each block of trials
%%  ~~function to do ... ~~
}
\description{
Compute mean values for each level of a grouping variable (e.g., block)
}
\usage{
meansByBlock(data)
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
# compute mean values for each level of a grouping variable (e.g., block)

meansByBlock = function(data=cat2_data) {
  data <- as.data.frame(data)
  meansOut <- aggregate(data, list(data$block), mean)
  return(meansOut)
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
