dir.base <- "/Users/niuniu/Desktop/Amey/NL"
setwd(dir.base)
library(zoo)
library(ggplot2)

sl <- read.csv(file="shoes_lost_data.csv",
               stringsAsFactors = F)

str(sl)
dim(sl) #9958 15
var.names <- names(sl)

for (col in 1:ncol(sl)){
  sl[,col] <- ifelse(sl[,col]=="", NA, sl[,col])
  
  print(paste("[", var.names[col], "]",
              " U: ", length(unique(sl[,col])),
              " NM: ", sum(is.na(sl[, col])),
              " LMin: ", min(nchar(sl[, col], allowNA = FALSE)),
              " LMax: ", max(nchar(sl[, col], allowNA = FALSE)),
              sep=""))
}

table(sl$Order.Type, useNA="always")
table(sl$Type, useNA="always")
table(sl$Bogie, useNA="always")
table(sl$Side, useNA="always")
table(sl$Lost...Cracked, useNA="always")

str(sl$Created.on)
table(nchar(sl$Created.on))
head(sl$Created.on[nchar(sl$Created.on)==7])

cdate <- as.Date(sl$Created.on, format="%d/%m/%y")

min(cdate, na.rm=T)
max(cdate, na.rm=T)

ts.sl <- data.frame(year.mon = names(table(as.yearmon(cdate))),
                    freq = as.numeric(table(as.yearmon(cdate))))

p <- ts(ts.sl$freq, frequency = 12, start = c(2009, 5)) # 2nd Quarter of 1959
plot(p, main="Monthly shoe loss frequency, 2009 - 2015",
     ylab="frequency", xlab="Time")
grid()
