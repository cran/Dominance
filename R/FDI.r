#TODO Check Excel file einbauen
#TODO Check and eleminate NA items
#TODO Vcolor ausgeben
#Fehler
#Attache Paket: 'Dominance'
#The following object is masked _by_ '.GlobalEnv':
# 
# FDI

FDI <-
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
    
    
    if (countmatrix == FALSE) {
      FDIResult =  ADI(data_sheet,bytes)
      FDI = FDIResult[3]$ADI_count_matrix
      vcolors = FDIResult[2]$Colors
    }  
    else {
      vcolors = ""
      FDI = data_sheet
    }  
    
    items= length(FDI[1,])
    FDI_Rownames =c(1:(items+2))  # add additional FDI Rownames
    FDI_Rownames[1:items] = rownames(FDI)
    FDI_Rownames[items+1] = "Sum_Bi"
    FDI_Rownames[items+2] = "Sum_bij"
    
    
    
    FDI_Colnames =c(1:(items+4)) # add additional FDI Colnames
    FDI_Colnames[1:items] = colnames(FDI)
    FDI_Colnames[items+1] = "Sum_Li"
    FDI_Colnames[items+2] = "Sum_lij"
    FDI_Colnames[items+3] = "FDI" 
    FDI_Colnames[items+4] = "id"
    FDI_Colnames[items+5] = "rank"   
    
    tempdata <- matrix(NA,nrow=items+2,ncol=items+5, dimnames = list(FDI_Rownames,FDI_Colnames)) #create matrix
    
    for (I in (1: items)) {   #fill matrix with ADI count matrix
      tempdata[I,] =  c(FDI [I,] ,0,0,0,0,0)
    }
    
    
    # Fill matrix  
    for (I in (1: items))  #calculate Sum_Bi
    {
      tempdata[ items+1,I] =sum(tempdata[,I], na.rm = TRUE)
    }   
        
    for (I in (1: items)) #calculate Sum_Li
    {
      tempdata[I, items+1] =sum(tempdata[I,], na.rm = TRUE)
    }   
      
    
    for (I in (1: items))  #calculate Sum_Lij
    { Lij_temp =0 
      
      for (J in (1: items)) 
      {
        if ((  tempdata[I,J] > 0) &  ( I != J )) {
          tempdata[I,J]
          Lij_temp =Lij_temp + tempdata[J, items+1]
        }
      }   
      tempdata[I, items+2] =Lij_temp
    }   
    
    for (J in (1: items))  #calculate Sum_Bij
    { bij_temp =0 
      
      for (I in (1: items)) 
      {
        if ((  tempdata[I,J] > 0) &  ( I != J )) {
          bij_temp =bij_temp + tempdata[items+1,I]
        }
      }   
      tempdata[items+2,J] =bij_temp
    }  
    

    for (I in (1: items)) { #calculate FDI 
      tempdata[I,items+3] = ((tempdata[items+1,I] + tempdata[items+2,I]+1)/ (tempdata[I,items+1]+tempdata[I,items+2]+1))
    
    }
    #--------------------------- sort matrix    -----------
    
    #print(tempdata)
    test<-0
    test2<-0 
    id=c(1:items)
    tempdata[1:items,items+4]=id
    
    test <- as.data.frame(tempdata)
    test <- test[order(test$FDI,decreasing = FALSE),]  #TODO Order increasing but without SUM_BI BIJ
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
    
    
    test2 <- rename.vars(test2, colnames(test2),c(rownames(test),colnames(test2[length(colnames(test2))-2]),colnames(test2[length(colnames(test2))-1]),colnames(test2[length(colnames(test2))])),info=FALSE)
    test2[1:items,items+5]= test2[1:items,items+5]= data.frame("rank"=position)
    
   tempdata <- as.matrix(test2)
    #--------------------------- end sort matrix    -----------
    
    #----------------------------------------------------------------------
    
    if ((exists("wb") )  && (exists("sheet_new"))) {
      createSheet(wb, name = sheet_new)
      writeWorksheet(wb,tempdata,sheet=sheet_new,rownames="Row Names")
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
    
    
    FDI <-  tempdata
    
    rm(tempdata)
    rm(I)
    rm(J)   
    rm(bij_temp)
    rm(Lij_temp)
    rm(FDI_Colnames)
    rm(FDI_Rownames)   
    return (FDI)
    
    
    #  TODO   save Workbook
  }