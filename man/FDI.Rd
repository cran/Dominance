\name{FDI}
\alias{FDI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{!!!!!        ADI Manual TODO Change it to FDI}
\description{computes the Average Dominance Index}
\usage{
FDI(data_sheet, bytes, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_sheet}{
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
    Classification 1 if "action.from"" wins; Classification 2 if "action.to" wins \cr
   \cr 
   Weigting the factor which should be used to calculate the behavior (1 for "action.from"" wins -1 for "action.to" wins")\cr
   \cr 
   Setting a behaviour to 2 means it is count double\cr
   \cr 
  vcolors as much colors as items, colors will returned as sorted FDI colors means color 1 = item rank 1, color 2 = item rank 2, and so on\cr
   \cr 
  workbook :  the XlConnect Workbook for the Excel file to be changed\cr 
  note: The workbook must be opened before \cr
   \cr 
   sheet: the sheet name ( FDI will be added to be sure not to delete any data\cr
   \cr 
   savecounts: save also the counts of actions as sheet (availalbe only with workbook\cr
   \cr 

}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
FDI returns a list with\cr
FDI - the Average Dominance index\cr
Colors - the colors supported by vcolors sorted by FDI of the items\cr
FDI_count_matrix - the counts from which the FDI was calulated\cr
}
\references{

TODO
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
data_sheet=data.frame   ("action.from"=c(1,4,2,3,4,3,4,3,4,3,4,3,4,3,4),
                         "action.to"=c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
                         "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),stringsAsFactors=FALSE)
items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
                   "item.number"=c(1:6),stringsAsFactors=FALSE)
actions=data.frame("name.of.action"= c("leFDIng","following","approach","bite","threat to bite",
                   "kick","threat to kick", "chase","retreat"),
                   "action.number"=c(1:9),
                   "classification"=c(1,2,1,1,1,1,1,1,2) ,
                   "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
## all  encounters  without leFDIng and following
bytes= "001111111"  
FDI(data_sheet,items=items,actions=actions,bytes)
## or you can use a complete f.e Excel sheet
## you can save this data as basic excel sheet to work with
data(data_ADI)
bytes= "001111111"  
FDI(data_ADI,bytes)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
