#TODO Check Excel file einbauen
#TODO Check and eleminate NA items
ADI <-
function(data_sheet,bytes,...)
{
#--------------------- ?bergabe parameter ----------
args = list(...)

if ("workbook" %in% names(args)){
  wb <- args$workbook
}


if ("sheet" %in% names(args)) {
  sheet_new <- paste(args$sheet, "ADI",sep="-")
  sheet_new_counts <- paste(args$sheet, "counts",sep="-")
}

if ("savecounts" %in% names(args)){
  savecounts <- args$savecounts
  if ((savecounts != TRUE) && (savecounts != FALSE)){
    warning("Error: savecounts must be TRUE or FALSE, default FALSE")
    return
  }  
}
else
  savecounts<- "FALSE"

if ("countmatrix" %in% names(args)){
  countmatrix <- args$countmatrix
  if ((countmatrix != TRUE) && (countmatrix != FALSE)){
    warning("Error: countmatrix must be TRUE or FALSE, default FALSE")
    return
  }  
}
else
  countmatrix <- "FALSE"


# as we build the package for reading a complete excel sheet we must build one data.frame to compute singel frames
if (("actions" %in% names(args)) &  ("items" %in% names(args))) 
{ 
  actions <- args$actions
  items <- args$items   
  data_length = length(data_sheet$action.from)
  tempNA= c(1:data_length)
  tempNA[1:data_length] =NA
  tempString_NA= c(1:data_length)
  tempString_NA[1:data_length] = "<NA>"

  data_temp=data.frame("action.from"=data_sheet$action.from,"action.to"=data_sheet$action.to,"kind.of.action"=data_sheet$kind.of.action,
                      "Name"=tempString_NA,"item.number"=tempNA,
                      "name.of.action"=tempString_NA,
                      "action.number"=tempNA,
                     "classification"=tempNA,
                      "weighting"=tempNA,stringsAsFactors=FALSE)

  data_temp$Name[1:length(items$Name)] = items$Name [1:length(items$Name)]
  data_temp$item.number[1:length(items$item.number)] =items$item.number
  data_temp$name.of.action[1:length(actions$name.of.action)] =actions$name.of.action
  data_temp$action.number[1:length(actions$action.number)] = actions$action.number
  data_temp$classification[1:length(actions$classification)] = actions$classification
  data_temp$weighting[1:length(actions$weighting)] = actions$weighting

  data_sheet = data_temp  # compute with the complete frame

}
if ("vcolors" %in% names(args))
  vcolors <- args$vcolors
else
  vcolors <-""

if  (countmatrix == FALSE){
  
 results <- search.win.lose(data_sheet,bits=bytes)
 win_lose_results <- results$data.win.lose
 items <- results$items    

 if (max(data_sheet$item.number,na.rm=TRUE) < length(vcolors))
           {
                    warning("Error max count of colors does not match")
              break;
      }
#----------------------------------------------------------------------------------
#
#
#TODO pruefen ob die Anzal der Tiere mit der laenge des namensvektors uebereinstimmt
#
#
#----------------------------------------------------------------------------------

  # Create Data matrix
  ADI_Rownames =c(1:items+3)
  ADI_Rownames[1:items] = as.vector(data_sheet$Name[1:items])
  ADI_Rownames[items+1] = "results.ADI"
  ADI_Rownames[items+2] = "id"
  ADI_Rownames[items+3] = "rank" 
  tempdata <- matrix(0,nrow=items,ncol=items+2, dimnames = list(ADI_Rownames[1:items],ADI_Rownames[1:(items+2)]))
  # set diagonal left/up to right/down to NA
  for (I in (1:items))
    tempdata[I,I] <- NA
  # Fill matrix  
  for (I in (1: length(win_lose_results$wins)))
  { 
  tempdata[as.integer(win_lose_results$wins[I]),as.integer(win_lose_results$loses[I])] <-  
  tempdata[as.integer(win_lose_results$wins[I]),as.integer(win_lose_results$loses[I])]+ 1
  }
}  #if  (countmatrix == FALSE)
else
{
  tempdata <- data_sheet
  items =length(tempdata[,1])
  ADI_Rownames =c(1:items+3)
  ADI_Rownames[1:items] = rownames(data_sheet)
  ADI_Rownames[items+1] = "results.ADI"
  ADI_Rownames[items+2] = "id"
  ADI_Rownames[items+3] = "rank" 
  
}
   tempdata 
  result.data <- matrix(0,nrow=items,ncol=items+2,   #Items + ADI + RAnge
        dimnames = list(ADI_Rownames[1:items],ADI_Rownames[1:(items+2)]))

  # matrix[down,right] 
  for (column in (1:items))
    result.data[column,column] <- NA

   for (column in (1:items))
       for (Row in (1:items))
      {
      if (Row != column)
        result.data[column,Row] <- tempdata[column,Row]/ (tempdata[column,Row]+tempdata[Row,column])
      }  
      
   count_points <- 0
      
   for (Row in (1:items))
   {   count.points <- 0
       count.relations <- 0

       for (column in (1:items))
      {                                                                        
        if (!is.na(result.data[column,Row]))
        {
            count.points <- count.points + result.data[Row,column] 
            count.relations <- count.relations +1
         }   
      }  
      
      if (count.relations > 0)
      result.data[Row,column+1] <- count.points/count.relations
    }  
    
#--------------------------- sort matrix    -----------
test<-0
test2<-0 
id=c(1:items)
result.data[,items+2]=id

test <- as.data.frame(result.data)
test <- test[order(test$results.ADI,decreasing = TRUE),]
position=c(1:items)
test2<-test
vcolors2<- vcolors
for (X in (1:length(test[,1]-1)))
  for (J in (1:length(test[,1]-1)))
  if  (rownames(test[X,]) == colnames(test[J])){
      test2[,X]<-test[,J]
      colnames(test2[X])<- colnames(test[J])
      if (vcolors[1] != ""){
       vcolors2[X] <- vcolors[J]
      }
      }


test2 <- rename.vars(test2, colnames(test2), c(rownames(test),colnames(test2[length(colnames(test2))-1]),colnames(test2[length(colnames(test2))])),info=FALSE)
test2[,items+3]= test2[,items+3]= data.frame("rank"=position)

result.data <- as.matrix(test2)
#--------------------------- end sort matrix    -----------

#     result.data <- result.data[order(result.data_sheet$results.ADI) , ] 
#----------------------------------------------------------------------

if ((exists("wb") )  && (exists("sheet_new"))) {
  createSheet(wb, name = sheet_new)
  writeWorksheet(wb,result.data,sheet=sheet_new,rownames="Row Names")
  if(savecounts ==TRUE){
    createSheet(wb, name = sheet_new_counts)
    writeWorksheet(wb,tempdata[,1:items],sheet=sheet_new_counts,rownames="Row Names") 
    
  }
  saveWorkbook(wb)
  #delete warnings until Problem 
  #1: In names(res)[1] <- colname :
  #  number of items to replace is not a multiple of replacement length
  # when adding rowmanes with writeWorksheet is solved
  assign("last.warning", NULL, envir = baseenv())  
  
  
}
#else
#  print('Remarks: No changes to excel sheet: missing wb or sheet')

   return(list("ADI"=result.data,"Colors"=vcolors2,"ADI_count_matrix"=tempdata[,1:items]))

rm(test2)
rm(test)
rm(tempdata)   
rm(vcolors2)
rm(position)
rm(id)
}
