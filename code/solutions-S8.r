# 8.1
grep("<dd>Cause:",raw) -> ii ## check for cause field
raw[-ii] ## OK these really are missing - code as NA
x <- gsub(".*<dd>Cause: ","",raw) ## cut text before actaul cause record
cause <- gsub("</dd>.*","",x) ## cut text from end of record
cause[-ii] <- NA ## set missings to NA
mean(tolower(cause)=="shooting",na.rm=TRUE) ## prop shooting

