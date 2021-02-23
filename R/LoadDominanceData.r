#' Function LoadDominanceData
#' 
#' Load Dominance data with available xlsx packages and set packages
#' @name LoadDominanceData
#' @param sheetname the sheet name
#' @param filename the file name to be opened   
#' @param XLSX either XLConnect or openxls or RcmdrMisc package default: openxls

#' @return returns a list with\cr
#' WB the Workbook\cr
#' the opened DataSheet\cr
#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' 
#' @examples { #you can eihter use:

#' }
#' @export LoadDominanceData



LoadDominanceData <- function(filename,sheetname,XLSX) {
    if ((XLSX == "openxlsx") | (XLSX == "XLConnect") | (XLSX == "RcmdrMisc")) {
      #if ((XLSX == "openxlsx") && (!available("openxlsx"))) 
      #  stop ("package openxlsx not available")
      #if ((XLSX == "XLConnect") && (!available("XLConnect"))) 
      #  stop ("package XLConnect not available")
      #if ((XLSX == "RcmdrMisc") && (!available("RcmdrMisc"))) 
      #  stop ("package RcmdrMisc not available")
      
     if (XLSX == "openxlsx") {
      requireNamespace("openxlsx") 
      print("Loading workbook - be patient")
      wbt <- openxlsx::loadWorkbook(filename)
      names(wbt)
      print("Loading data sheet")
      dataT <- openxlsx::readWorkbook(wbt, sheet = sheetname)  
      
     }  
     
     if (XLSX == "XLConnect") {
      requireNamespace("XLConnect") 
      wbt <- XLConnect::loadWorkbook(filename)
      print("Loading workbook")
      dataT <- XLConnect::readWorksheet(wbt, sheet = sheetname)  
      print("Loading data sheet")
     }
#      if (XLSX == "RcmdrMisc") {
       # requireNamespace("RcmdrMisc") 
#        print("Loading data sheet")
#        dataT <- readXL(excelfile, rownames=FALSE, header=TRUE, na="", sheet=sheetname, stringsAsFactors=TRUE)
#     }    
    }
    else
    { stop ("XLSX must be either openxlsx XLConnect") 
    }  

    return(list("openedwb"=wbt,"data_sheet"=dataT))
    
  }    
