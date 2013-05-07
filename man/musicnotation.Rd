\name{Musicnotation}
\alias{Musicnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Music Notation Graph}
\description{computes Music Notatation graphs}
\usage{
Musicnotation(data, ...)
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
 \item{...}{

  colors:  a factor of colors as much as actions

 lwd: line width if lwd_arrows is not used also for line width arrows
%% TODO check this it not working -> no show_items all items will be shown  
 show_items: items to be shown - 
  
  angel_arrows: The angel aof the arrow head default 20

  length_arrows: the lenght of the arrow default 0.05
  
  lwd_arrows: the line width of the arrows default 1
  
 
  actions_colors: a vector of colors for actions f.e to show one special action

  starting_time: 
  
  ending_time:   builds the graph with data bewteen starting and ending time 
  
  sort_dominace:  sort the graph by ADI if sort_dominance is present

  user_colors:   a vector of colors as much as items to show differetn colors for items

  color_bits:  a vector of colors as much as items 1 shows the horse colored 0 in black (defined with actions_colors)

} 
}
%% \details{
%%  ~~ If necessary, more details than the description above ~~
%% }
\value{
returns the ADI
}
\references{
Chase, I. D. (2006). Music notation: a new method for visualizing social interaction in animals and humans. Front Zool, 3, 18. 

\url{http://dx.doi.org/10.1186\%2F1742-9994-3-18}
}
\author{
Knut Krueger
}
%% \note{
%%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
## you can eihter use:
dataM=data.frame  ("action.from"=c(1,2,3,4,5,1,1,1,1,1,1,1,1,3,4),
                   "action.to"=  c(2,3,4,5,6,2,3,4,5,6,3,4,3,4,3),
                   "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
                   "Time"=c("03:15:00","03:17:30","03:20:00","03:20:30","03:21:00","03:21:30","03:22:00","03:22:30","03:23:00","03:23:30","03:25:00","03:25:30","03:26:00","03:26:30","03:27:00"),stringsAsFactors=FALSE)


items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
                   "item.number"=c(1:6),stringsAsFactors=FALSE)
actions=data.frame("name.of.action"= c("leading","following","approach","bite","threat to bite", "kick","threat to kick", "chase","retreat"),
                   "action.number"=c(1:9),
                   "classification"=c(1,2,1,1,1,1,1,1,2) ,
                   "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
## all  encounters  without leading and following
bytes= "001111111"  
## set colors for special encounters
color= c("green","green","red","red","red","red","red","red")    

Musicnotation(data=dataM,actions=actions,items=items,sort_dominance=TRUE)
## or you can use a complete f.e Excel sheet
## you can save this data as basic excel sheet to work with
data(data_Musicnotation)
Musicnotation(data=data_Musicnotation,sort_dominance=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
