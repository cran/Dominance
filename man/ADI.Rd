\name{ADI}
\alias{ADI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Average Dominance Index}
\description{computes the Average Dominance Index}
\usage{
ADI(data, bytes, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  either a data.frame f.e imported from a data sheet containing\cr  
                                "Name","item.number"\cr 
                                "action.from.","action.to","kind.of.action"\cr
                                "name.of.action","action.number","classification","weighting"\cr   
  \cr                                 
  or only "action.from.","action.to","kind.of.action"if exists actions and items\cr
  \cr 
  actions: with  "name.of.action","action.number","classification","weighting

  items  with "Name","item.number" 
  }
  \item{bytes}{
  a string where each enabled action is set to 1 and each disabled action is set to 0
  }
 \item{...}{
  dots :  actions with "name.of.action","action.number","classification","weighting"\cr   
   \cr 
   Classification 1 for aggresive behaviour 2 for non agressive behaviour like retreat\cr
   \cr 
   Weigting the factor which should be used to calsulate the behavior (1 for aggressive -1 for none aggressiv)\cr
   \cr 
   Setting a aggressiv behaviour to 2 means it is count double\cr
   \cr 
   vcolors as much colors as items, colors will returned as sorted ADI colors means color 1 = item rank 1, color 2 = item rank 2, and so on\cr
   \cr 

}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
ADI returns a list with\cr
ADI - the Average Dominance index\cr
Colors - the colors supported by vcolors sorted by ADI of the items\cr
ADI_count_matrix - the counts from which the ADI was calulated\cr
}
\references{

The Construction of Dominance Order: Comparing Performance of Five Methods Using an Individual-Based Model C. K. Hemelrijk, J. Wantia and L. Gygax, Behaviour Vol. 142, No. 8 (Aug., 2005), pp. 1037-1058

\url{http://www.jstor.org/stable/4536286}


On using the DomWorld model to evaluate dominance ranking methods , de Vries, Han,  Behaviour, Volume 146, Number 6, 2009 , pp. 843-869(27)

\url{http://dx.doi.org/10.1163/156853909X412241}
}
\author{
 Knut Krueger, Prof. Dr. Konstanze Krueger
}
%%\note{
%%  ~~further notes~~
%%}
%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## you can eihter use:
data=data.frame   ("action.from"=c(1,4,2,3,4,3,4,3,4,3,4,3,4,3,4),
                   "action.to"=c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
                   "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),stringsAsFactors=FALSE)
items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
                   "item.number"=c(1:6),stringsAsFactors=FALSE)
actions=data.frame("name.of.action"= c("leading","following","approach","bite","threat to bite",
                   "kick","threat to kick", "chase","retreat"),
                   "action.number"=c(1:9),
                   "classification"=c(1,2,1,1,1,1,1,1,2) ,
                   "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
## all  encounters  without leading and following
bytes= "001111111"  
ADI(data,items=items,actions=actions,bytes)
## or you can use a complete f.e Excel sheet
## you can save this data as basic excel sheet to work with
data(data_ADI)
bytes= "001111111"  
ADI(data_ADI,bytes)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
