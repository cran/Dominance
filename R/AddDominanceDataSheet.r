#' Function AddDominanceDataSheet
#' 
#' adds a Sheet with  Dominance data to a loaded xlsx workbook
#' @name AddDominanceDataSheet
#' @param  workbook the previous opened workbook
#' @param sheetname the sheet name
#' @param DATASheet the data of the new sheet
#' @param filename the file name to be opened   
#' @param XLSX either XLConnect or openxls default: openxls
#' @param  overwrite overwrite existing file - default = no

#' @return returns a list with\cr
#' WB the Workbook\cr
#' the opened DataSheet\cr
#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' 
#' @examples { #you can eihter use:

#' }
#' @export AddDominanceDataSheet
#' 

AddDominanceDataSheet <- function(workbook,filename,sheetname,DATASheet,XLSX,overwrite=FALSE) {
  
  sheetname <- paste(sheetname,format(Sys.time(), "%H%M%S"),sep="-") #for security reasons
  if (file.exists(filename) && overwrite==FALSE) {
    stop(paste("filename:",getwd(),"/",filename," exists. Please provide another filename or set overwrite=TRUE",sep = ""))  
  }  
  
  if ((XLSX == "openxlsx") | (XLSX == "XLConnect")) {
  #  if ((XLSX == "openxlsx") && (!available("openxlsx"))) 
  #    stop ("package openxlsx not available")
  #  if ((XLSX == "XLConnect") && (!available("XLConnect"))) 
  #    stop ("package XLConnect not available")
    
     if (XLSX == "openxlsx") {
      requireNamespace("openxlsx") 
      print("saving workbook with new sheet - be patient")
      
      openxlsx::addWorksheet(workbook, sheetName = sheetname)
      openxlsx::writeData(workbook, sheet = sheetname,DATASheet,rowNames=TRUE,colNames=TRUE)
      openxlsx::saveWorkbook(workbook,filename, overwrite=overwrite) 
      # alt: rownames=c(na.omit(data_ADI$item.number),na.omit(data_ADI$item.number)))
     }
    
     if (XLSX == "XLConnect") {
      requireNamespace("XLConnect") 
       print(paste("Saving data sheet: ",sheetname, "with workbook: ", filename))
       XLConnect::createSheet(workbook)
       XLConnect::writeWorksheet(workbook, DATASheet, sheet = sheetname)
       XLConnect::saveWorkbook(workbook,filename)        
     }
    }
    else
    { stop ("XLSX must be either openxlsx XLConnect") 
    }  

    return()
    
  }    
