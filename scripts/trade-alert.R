# VIX - Realized Crossover Alert

# Runs on an amazon nano instance near real-time, send first alert 
# during trading day. 

# R program will check every x minutes.  

# Alert counter is reset at end of day.

# One daily batch process will run to save down quotes, and also give EOD alert
# http://stackoverflow.com/questions/27026193/real-time-stock-price-r

install_dep <- function() {
  install.packages("gdata")
  install.packages("pander")
  install.packages("qmao", repos="http://R-Forge.R-project.org")  
}

library(qmao)
library(matrittr)

library(RJSONIO)

format(Sys.time(), tz = "America/New_York", usetz =  T)

# logrets <- read.csv("SPY.csv")
# last 4 days
# tail(logrets, 4)

qmao::getQuote.google("SPY")

# Send Text Message -------------------------------------------------------
# Enter the recipient's T-Mobile phone number -- without punctuation -- followed by "@tmomail.net" in the "To" field. ...



# Old Way -----------------------------------------------------------------


# qmao::getQuote.BATS("SPY", what = "bbo")
# Symbol = "SPY"
# exch = c("bzx", "byx", "edgx", "edga", "opt")
# exch = c("bzx")
# fromJSON(paste("http://www.batstrading.com/json", exch, 
#                "book", Symbol, sep = "/"))
# print(getQuote("SPY", src="BATS", what="ladder"), header=FALSE)
# plot(getQuote("SPY", src="BATS"))

