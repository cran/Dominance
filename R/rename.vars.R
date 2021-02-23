# $Id: rename.vars.R 2072 2016-02-03 20:00:57Z warnes $\cr
#' Function rename.vars\cr
#' Source code from gdata as gdata is unmaintained since 2017-06-06\cr
#' Rename variables in a dataframe
#' @name rename.vars
#' @param  data	 frame to be modified.\cr  
#' @param  from 	 character vector containing the current name of each variable to be renamed.\cr
#' @param  to character vector containing the new name of each variable to be renamed.\cr
# @param names character vector containing the names of variables to be removed.\cr
#' @param info boolean value indicating whether to print details of the removal/renaming. Defaults to TRUE.\cr
#' 
#' 
#' @return returns the updated data frame with variables listed in from renamed to the corresponding element of to.\cr
#' @author Don MacQueen (package gdata), \email{macq\@llnl.gov.}
#' 
#' @examples { data <- data.frame(x=1:10,y=1:10,z=1:10)
#' names(data)
#' data <- rename.vars(data, c("x","y","z"), c("first","second","third"))
#' names(data)
#'
# data <- remove.vars(data, "second")
# names(data)

#' }
#' @export rename.vars
#' 
rename.vars <- 
function (data, from = "", to = "", info = TRUE) 
{
  dsn <- deparse(substitute(data))
  dfn <- names(data)
  if (length(from) != length(to)) {
    cat("--------- from and to not same length ---------\n")
    stop()
  }
  if (length(dfn) < length(to)) {
    cat("--------- too many new names ---------\n")
    stop()
  }
  chng <- match(from, dfn)
  frm.in <- from %in% dfn
  if (!all(frm.in)) {
    cat("---------- some of the from names not found in", 
        dsn, "\n")
    stop()
  }
  if (length(to) != length(unique(to))) {
    cat("---------- New names not unique\n")
    stop()
  }
  dfn.new <- dfn
  dfn.new[chng] <- to
  if (info) 
    cat("\nChanging in", dsn)
  tmp <- rbind(from, to)
  dimnames(tmp)[[1]] <- c("From:", "To:")
  dimnames(tmp)[[2]] <- rep("", length(from))
  if (info) {
    print(tmp, quote = FALSE)
    cat("\n")
  }
  names(data) <- dfn.new
  data
}

