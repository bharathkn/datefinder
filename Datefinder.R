setwd("X:/Workspace")
#declaring the abbreviated months
month <- c("jan", "feb", "mar", "may", "jun",
           "jul", "aug", "sep", "oct", "nov", "dec")
days <- as.character(c(1:31))
year <- as.character(c(1880:2019))

#defining the fucntion to carryout operations
date_finder <- function(string){

#stripping all the punctuation marks i.e (",", ".")
punc_remove <- gsub(pattern="(/)|(th)|[[:punct:]]", replacement=" ", x=string)
punc_remove
#converting the string to lower to do logical calculations
string_rm <- tolower(read.table(text=punc_remove, sep="", as.is=TRUE))
string_rm

#using %in% and an OR operator to find the dates and return indices
selection_index <- which(as.logical((string_rm %in% month)+(string_rm %in% tolower(month.name))
              +(string_rm %in% days)+(string_rm %in% year)), arr.ind = TRUE)

#if the indices returned are more than 3 or less than 3, O/P=None
date_selector <- ifelse(
                          (length(selection_index)<3) 
                         +(length(selection_index)>3),print("None"),
                            print(string_rm[selection_index])
                        )
}
#add your string in the quotes to find the date
date_finder("INSERT YOUR STRING HERE")
