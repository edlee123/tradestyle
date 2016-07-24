library(magrittr)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(tidyr)
library(lubridate)
library(ggplot2)

# Style analyze many vanilla etfs and load into app.

rm(list=ls(all=TRUE))

list.files(path = "./R", full.names = T) %>% lapply(., FUN = source)

datastart= '2003-01-01'

tickers <- c("BRK-B",
             "IEP",
             "USMV",
             "SPLV",
             "ALFA",
             "SMH",
             "WIP",
             "EEM",
             "TLT",
             "XHB",
             "GLD",
             "EMB",
             "SPY",
             "XLF",
             "JNK",
             "QQQ",
             "IVE",
             "IWF",
             "IWM",
             "IYR",
             "PDP",
             "VTV",
             "MOM",
             "SPHQ"
             )   


# Factor Attribution ------------------------------------------------------

# FF Factors
periodicity = 'days'

factors = data.ff('F-F_Research_Data_Factors', periodicity, force.download = F, 
                  clean = F)
factors_mom = data.ff('F-F_Momentum_Factor', periodicity, force.download = F, 
                      clean = F)

factors$carhart = merge(factors$data, factors_mom$data) / 100

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


clone_env <- function(e1) {
  as.environment(as.list(e1, all.names=TRUE))
}


pxdata <- new.env() # Initialize a data environment
getSymbols(tickers, src = 'yahoo', 
           from = datastart, env = pxdata, auto.assign = T)


bt.prep(pxdata) # populate prices data frames etc.
adjdata <- adjust_px(pxdata)

backtest_asset <- function(tick, adjdata){
  cleanstrat <- clone_env(adjdata)
  bt.prep(cleanstrat)  # remove NA from prices
  st = min(which( !is.na(cleanstrat$prices[, tick] )))
  truncind = st:dim(cleanstrat$prices)[1]
  # cleanstrat$dates = cleanstrat$dates[truncind, ] 
  cleanstrat$execution.price = cleanstrat$execution.price[truncind, ]
  cleanstrat$prices = cleanstrat$prices[truncind, ] 
  cleanstrat$weight = cleanstrat$weight[truncind, ] 
  cleanstrat$weight[, tick] = 1 
  bt.run.share(cleanstrat, trade.summary=T, 
                                    clean.signal = F) 
}

models_etf <- lapply(tickers, FUN = function(x)backtest_asset(x, adjdata))
names(models_etf) = tickers


pregen_rollingstyle <- function(models) {
  for (i in names(models)) {
    obj <- style_roll.xts(models[[i]]$equity, factors$carhart, 250)
    temp <- models[[i]] 
    temp[["style"]] = obj
    models[[i]] <- temp
  }
  models
}
message("Running rolling style analyses.")
models_etf2 = pregen_rollingstyle(models_etf)
saveRDS(models_etf2, "./data/models2.rds")

# out = plotbt.strategy.sidebyside(models_etf2, return.table=T, make.plot = F)
# Test that graphs will truncate appropriately (not too early)
# testmodel = backtest_asset("EMB",adjdata)
# testroll = pregen_rollingstyle(list(a = testmodel))
# style_roll_detail_plot_lattice(testroll$a$style)
# ind = 5
# names(models_etf2)[ind]
# style_roll_detail_plot_lattice(models_etf2[[ind]]$style)

