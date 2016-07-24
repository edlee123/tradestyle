# app graphing

install_all <- function() {
  install.packages("ggvis")  
}



# Alex Vol Strategy ETF
library(magrittr)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(tidyr)
library(lubridate)
library(ggvis)

rm(list=ls(all=TRUE))

list.files(path = "./R", full.names = T) %>% lapply(., FUN = source)

datastart= '2003-01-01'

tickers <- c("XIV", 
             "VXX",
             "XIV",
             "TLT",
             "GLD",
             "EDV",
             "EMB",
             "SPY",
             "SVXY",
             "^VIX")   

bills = c("^IRX")

# Factor Attribution ------------------------------------------------------

# FF Factors
periodicity = 'days'

factors = data.ff('F-F_Research_Data_Factors', periodicity, force.download = F, 
                  clean = F)
factors_mom = data.ff('F-F_Momentum_Factor', periodicity, force.download = F, 
                      clean = F)

factors$carhart = merge(factors$data, factors_mom$data) /100

# Utility Functions -------------------------------------------------------

rename.xts <- function(dfxts, colmap) {
  targetcols <- names(colmap)
  if (sum(targetcols %in% colnames(dfxts)) < length(colmap)) {
    stop("Some columns to be renamed are not in the xts")
  }
  colnames(dfxts)[which(colnames(dfxts) %in% targetcols)] = colmap %>% as.character()
  dfxts
}

#' data frame of start dates
px_starts <- function(xtsenv) {
  if(is.null(xtsenv$prices)){stop("px_starts: input env doesn't have prices")}
  colstart <- function(vector) min(which(!is.na(vector)))
  date_ind = sapply(xtsenv$prices, function(x)colstart(x))
  dts <- index(xtsenv$prices)[date_ind]
  names(dts) <- names(xtsenv$prices)
  data.frame(ticker = names(dts), startdates = dts) %>% 
    spread(key = "ticker", value = "startdates")
}

#' @details Implied - Realized > threshold
#' @details will short VIX when above realized..
vol_signal <- function (adjdata, rollavgdays = 5, threshold = 1.5) {
  adjdata$lgrets = log(adjdata$prices / stats::lag(adjdata$prices) )
  colmap = c("SPY" = "rv1d", "VIX.Adjusted" = "vix")
  dailyrealized <- adjdata$lgrets$SPY %>% abs * sqrt(252) * 100
  sig = merge(dailyrealized, adjdata$VIX$VIX.Adjusted) %>%
    rename.xts(colmap = colmap) 
  
  sig$vix_diff_rv = sig$vix - sig$rv1d
  sig$diff_avg5d = rollapply(data = sig$vix_diff_rv, width = rollavgdays, 
                             FUN = function(x) mean(x, na.rm =T))
  sig$sig = ifelse(sig$diff_avg5d > threshold, 1, -1)  
  sig
}

clone_env <- function(e1) {
  as.environment(as.list(e1, all.names=TRUE))
}

# Get Data ----------------------------------------------------------------

XIVmore = read.csv("XIV2004-2010.csv")

XIVmore_xts = xts(XIVmore %>% dplyr::select(-Date, -Day) %>%
                    dplyr::mutate(Volume = NA),
                  order.by = lubridate::parse_date_time(XIVmore$Date, 
                                                        orders ="mdy",
                                                        tz = "GMT"))

head(XIVmore_xts)

pxdata <- new.env() # Initialize a data environment
getSymbols(tickers, src = 'yahoo', 
           from = datastart, env = pxdata, auto.assign = T)


billsdata <- new.env()
getSymbols(bills, src = 'yahoo', 
           from = datastart, env = billsdata, auto.assign = T)

bills90rollmean <- rollapply(billsdata$IRX$IRX.Adjusted, width = 90, FUN = mean)
# plot(billsdata$IRX$IRX.Adjusted)
# plot(bills90rollmean)
xivbind <- XIVmore_xts[ ,c(1,2,3,4,8,7)]
colnames(xivbind) = colnames(pxdata$XIV)
pxdata$XIV = rbind(xivbind, pxdata$XIV)
pxdata$XIV$XIV.Close = pxdata$XIV$XIV.Adjusted

bt.prep(pxdata) # populate prices data frames etc.
adjdata <- adjust_px(pxdata)

adjdata$factors = factors$carhart  #

startdt <- px_starts(adjdata) %$% EDV 
daterange <- paste0(startdt, "::")

bt.prep(adjdata, dates = daterange) # truncate backtest to earliest XIV date

volsignal <- vol_signal(adjdata, threshold = 2) # have to optimize the threshold

# Interactive Plot --------------------------------------------------------

abline_data <- function (domain, intercept, slope) {
  data.frame(x = domain, y = domain * slope + intercept)
}

untick <- function (x) {
  # Hack to remove backticks from names
  stopifnot(all(sapply(x, is.name)))
  str_replace_all(as.character(x), "`", "")
}

layer_abline <- function (.vis, domain, intercept = 0, slope = 1) {
  df <- abline_data(domain, intercept, slope)
  names(df) <- with(.vis$cur_props, untick(c(x.update$value, y.update$value)))
  layer_paths(.vis, data = df)
}

library(stringr)

df = data.frame(dt = volsignal %>% index ,  val = as.vector(volsignal$diff_avg5d), label = "ind")
# dfvix = data.frame(dt = volsignal %>% index, val = as.vector(volsignal$vix), label = "vix")
gpdat = rbind(df, dfvix)
gpdat = rbind(df)

saveRDS(gpdat, file = "gpdat.rds")
gpdat = readRDS("gpdat.rds")

gpdat %>% 
  ggvis(~dt, ~ val, stroke = ~label) %>%
  layer_lines(strokeWidth := 0.25)  %>%
  layer_points(size := 3, size.hover := 200,
               fillOpacity := 0.01, fillOpacity.hover := 0.25) %>%
  add_tooltip(function(x) 
    paste( as.Date(x$dt/86400000, origin = '1970-01-01'), 
    round(x$val,2) %>% as.character, sep = "<br/>") )


layer_points(fill=input_slider( 1, len(gpdat$dt), step=1, 
                                map=function(x) factor(x == gpdat$dt))) %>%
  
input_slider(min(gpdat$dt), max(gpdat$dt), step=1, 
             map=function(x) factor(x == gpdat$dt))

input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment")
  
as.Date(dat$time/86400000, origin='1970-01-01') 
  
#   tdat %>% ggvis(~tdat %>% ggvis(~time, ~val, stroke = ~group) %>% layer_lines(strokeWidth := 1) %>%
#                    layer_points(size = 1, fill = ~group) %>% add_tooltip(getData)time, ~val, stroke = ~group) %>% layer_lines(strokeWidth := 1) %>%
#   layer_points(size = 1, fill = ~group) %>% add_tooltip(getData)

# Plot Indicator Plot -----------------------------------------------------


