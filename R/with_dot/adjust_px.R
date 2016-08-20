#' Adjusts prices for dividends / or interest
#' Takes in an environment of px data and outputs the environment of px
adjust_px <- function(env) {
  message("Adjusting prices for dividends/income on:")
  tickers <- env$.getSymbols %>% names
  for (i in tickers) {
    print(i)
    z = try(adjustOHLC(env[[i]], use.Adjusted = T))
    
    if (class(z)[1] == "xts") {
      env[[i]] = z
    } else {
      print(paste(i, " ", z))
      env[[i]] <- NULL
    }
  }
  env
}
