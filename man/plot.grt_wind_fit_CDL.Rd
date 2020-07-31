\name{plot.grt_wind_fit_CDL}
\alias{plot.grt_wind_fit_CDL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot GRT model decision bounds (means, medians, all bounds, x- or y- dimension bounds)
%%  ~~function to do ... ~~
}
\description{
Plots decion bounds from GRTwInd model analysis

Can specify different types of plots
- means, medians, allbounds, xbounds, ybounds

requires model first to be fit in grtools
}
\usage{
plot.grt_wind_fit_CDL(model, bnd_type="means", labels=c("dim A", "dim B"), ellipse_width=0.8, main = NULL, line_width=1, lgnd_vals=c('A1B1', 'A2B1', 'A1B2', 'A2B2'), lgnd_symbols=3,
    lgnd_position='topleft', marginals=TRUE, hideticks=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
model is the name of the GRT wIND model output you wish to plot
}
  \item{bnd_type}{
bnd_type can = "means", "medians", "allbounds", "xbounds", "ybounds"
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

plot.grt_wind_fit_CDL(fullmodel)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
