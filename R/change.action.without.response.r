#' Function change.action.without.response
#' 
#' A package to calculate Dominance Indices, print Soical Network Graphs and Music Notation Graphs.
#' @name change.action.without.response
#' 
#' @param data.set A data frame used f.e at ADI or FDI
#' @param action the action which is to be searched for
#' @param newaction  newaction must be: data.frame("name.of.action"="test","action.number"=1,"classification"=2,"weighting"=3)\cr
#' or "change.only" if action is present and row kind.of.action should be changed'
#' @param response the number of the action responing to param action

#' @return the changed data.set
#' 

#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' @keywords  ~FDI ~ADI
#'@export change.action.without.response
#'
change.action.without.response <-
  function(data.set,action,response,newaction)
  {
    

    if (length(newaction) < 4)   {
      if (newaction != "change.only") {
        
        print('newaction must be: data.frame("name.of.action"="test","action.number"=1,"classification"=2,"weighting"=3)')
        print('or "change.only" if action is present and row kind.of.action should be changed')
        return()
      }
    }
   
    if (is.na(data.set$action.number[newaction$action.number]) & (newaction[1]=="change.only")){
      print('to add action newaction must be: data.frame("name.of.action"="test","action.number"=1,"classification"=2,"weighting"=3)')
      return()
    }
    if (!is.na(data.set$action.number[newaction$action.number]) & (newaction[1] !="change.only")){
      print('action  is already present: to change kind.of.actions only newaction must be"change.only"')
      return() 
    }
    
    
    #set the classification if not exist
    if (is.na(data.set$action.number[newaction$action.number]) & (newaction[1] !="change.only")){
      if (!is.na(data.set$action.number[newaction$action.number] ) & (data.set$action.number[newaction$action.number] != max(data.set$action.number,na.rm=TRUE)+1)){
        print(paste('new kind.of.actions must be',  max(data.set$action.number,na.rm=TRUE)+1 , 'and willbe set to ', max(data.set$action.number,na.rm=TRUE)+1, sep=" "))
        newaction$action.number = max(data.set$action.number,na.rm=TRUE)+1
      }

      data.set$action.number[newaction$action.number] = newaction$action.number
      data.set$name.of.action[newaction$action.number] = as.character(newaction$name.of.action)
      data.set$classification[newaction$action.number] = newaction$classification
      data.set$weighting[newaction$action.number] = newaction$weighting
    }
    max_items<- 0
    max_items <- max(data.set$action.from,na.rm=TRUE)
    count_max <- length(data.set$action.from)-1 #if last entry is kind of action
    if (max_items < max(data.set$action.to,na.rm=TRUE))
      max_items <- max(data.set$action.to,na.rm=TRUE)
    if (max(data.set$item.number,na.rm=TRUE) < max_items)
    {
      stop('Error: max count of items does not match max item.numbers')
    }
    max_items <-  max(data.set$item.number,na.rm=TRUE)       # because some items may not be used in actions
    
    for (x in 1:count_max)
    {  
      #  print(x)
      if ((data.set$kind.of.action[x] ==action) & (data.set$kind.of.action[x+1] != response))
      data.set$kind.of.action[x] = newaction$action.number
    }
    if ((data.set$kind.of.action[x+1] ==action) & (x == count_max)) #if last entry is kind of action
      data.set$kind.of.action[x+1] = newaction$action.number

    return(data.set)
  }
