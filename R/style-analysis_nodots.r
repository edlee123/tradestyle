
############################################################################### 
# New fund regression calculator
# http://www.bogleheads.org/forum/viewtopic.php?t=11506&amp;highlight=regression
#
# Factor loadings?
# http://www.bogleheads.org/forum/viewtopic.php?t=14629
#
# Efficient Frontier: Rolling Your Own: Three-Factor Analysis
# http://www.efficientfrontier.com/ef/101/roll101.htm
#
# Kenneth R French: Data Library
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
############################################################################### 
# alpha: how much 'extra' return did the fund have that could not be accounted for by the model. this is almost never large or statistically significant.
# B(MKT) = market factor: most 100% stock funds have a market factor near 1.0. higher values may indicate leverage. lower values may indicate cash and or bonds.
# B(SMB) = size factor (small minus big): positive values indicate the fund's average holding is smaller than the market
# B(HML) = value factor (high minus low): positive values indicate the fund's average holding is more 'value' oriented than the market (based on book to market ratio)
# R2: measures how well the fund returns match the model (values close to 1.0 indicate a good statistical fit)
############################################################################### 
#' renamed factor_rolling_regression to style_roll (shorter)

style_roll_xts <- function(equity, factors, window.len = 36, silent = F, 
                           custom.stats.fn = NULL) {

  #*****************************************************************
  # Facto Loadings Regression over whole period
  #****************************************************************** 
  prices = equity
  dates = index(equity)
  
  # compute simple returns, xts knows to align the dates when subtracting xts
  # http://stackoverflow.com/questions/32443445/how-to-subtract-rows-in-xts
  hist.returns = ROC(prices, type = 'discrete')	
  hist.returns_excess = hist.returns - factors$RF
  
  yout = hist.returns_excess
  xout = factors[, -which(names(factors) == 'RF')]
  x = coredata(xout) # carhart facotrs
  ticker = names(yout)
  
  align_ret_factor <- merge(xout, yout)
  
  carhart_reg <- function(ticker, align_ret_factor) {
    reg_formula <- as.formula(paste0(ticker, " ~ ." ))
    fit <- lm(reg_formula, data = align_ret_factor)
    stderr = summary(fit) %$% coefficients %>% as.data.frame 
    stderrout = stderr$`Std. Error`
    names(stderrout) = paste0("SE_",rownames(stderr))
    list(r2 = summary(fit)$r.squared,
         est = fit$coefficients,
         std.err = stderrout, # of coefficients
         lmres = fit)
  }
  
  # Overall regression results
  res <- carhart_reg(ticker, align_ret_factor)
  r2 = res$r2
  est = res$est
  std.err = res$std.err
  lmres = res$lmres

  full_sample_res = list()
  full_sample_res$estimate = c(est, r2)
  full_sample_res$std.error = c(std.err, NA) # we don't have standard error for R^2
  
  #*****************************************************************
  # Factor Loadings Regression over Month window
  #****************************************************************** 
  colnames = c('alpha', colnames(x), 'R2')
  alldat <- merge(xout, yout) 
  ind <- complete.cases(alldat)
  trimdat <- alldat[ind, ]
  nperiods = nrow(trimdat)
  # initialize output
  nperiods = nrow(trimdat)
  estimate = make_xts(matrix(NA, nr = nperiods, len(colnames)), dates)
  colnames(estimate) = colnames
  
  if( !is.null(custom.stats.fn) ) {
    temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
    fl$custom = make_xts(matrix(NA, nr = nperiods, len(temp)), dates)	
  } 	
  
  # TODO better to use rollapply
  rollcarhart2 <- function(i) {
    window.index = (i - window.len + 1) : i
    reglist <- carhart_reg(ticker, trimdat[window.index,])
    xts(x = t(as.matrix(c(reglist$est, "r2" = reglist$r2, "std.err" = reglist$std.err))) , index(trimdat)[i] )
  }
  
  # main loop
  out <- lapply(window.len:nperiods, function(x)rollcarhart2(x)) %>% 
    do.call(rbind, .)
  
  fl = list()
  fl$estimate = merge(out[,1:5], out$r2)
  fl$std.error = out[, 7:11]
  fl$std.error$std.err.SE_r2 = NA
  
  return(list(full_sample_res = full_sample_res, fl = fl, window.len=window.len,
              y=yout, x=xout, RF=factors$RF,  olsstat = lmres))
}



style_roll <- function(
  xtsenv, 
  ticker = xtsenv$symbolnames[-grep('factor', xtsenv$symbolnames)], 
  window.len = 36,
  silent = F,
  custom.stats.fn = NULL
) 
{
  ticker = ticker[1]
  
  #*****************************************************************
  # Facto Loadings Regression over whole period
  #****************************************************************** 
  prices = xtsenv$prices
  dates = index_xtsenv$prices)
  
  # compute simple returns	 
  # xts knows to align the dates when subtracting xts
  # http://stackoverflow.com/questions/32443445/how-to-subtract-rows-in-xts
  hist.returns = ROC(prices[,ticker], type = 'discrete')	
  hist.returns_excess = hist.returns - xtsenv$factors$RF
  
  yout = hist.returns_excess
  xout = xtsenv$factors[, -which(names(xtsenv$factors) == 'RF')]
  x = coredata(xout) # carhart facotrs

  align_ret_factor <- merge(xout, yout)

  carhart_reg <- function(ticker, align_ret_factor) {
    reg_formula <- as.formula(paste0(ticker, " ~ ." ))
    #  use R lm instead of SIT::ols_fn.
    #  na.omit = T by default
    fit <- lm(reg_formula, data = align_ret_factor)
    stderr = summary(fit) %$% coefficients %>% as.data.frame 
    stderrout = stderr$`Std. Error`
    names(stderrout) = paste0("SE_",rownames(stderr))
    list(r2 = summary(fit)$r.squared,
    est = fit$coefficients,
    std.err = stderrout,
    lmres = fit)
  }
  res <- carhart_reg(ticker, align_ret_factor)
  r2 = res$r2
  est = res$est
  std.err = res$std.err
  lmres = res$lmres
  
  # Facto Loadings - fl
  full_sample_res = list()
  full_sample_res$estimate = c(est, r2)
  full_sample_res$std.error = c(std.err, NA) # we don't have standard error for R^2
  
  #*****************************************************************
  # Facto Loadings Regression over Month window
  #****************************************************************** 
  colnames = c('alpha', colnames(x), 'R2')
  alldat <- merge(xout, yout) 
  ind <- complete.cases(alldat)
  trimdat <- alldat[ind, ]
  nperiods = nrow(trimdat)
  # initialize output
  nperiods = nrow(trimdat)
  estimate = make_xts(matrix(NA, nr = nperiods, len(colnames)), dates)
  colnames(estimate) = colnames
  
  if( !is.null(custom.stats.fn) ) {
    temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
    fl$custom = make_xts(matrix(NA, nr = nperiods, len(temp)), dates)	
  } 	
  
  # TODO better to use rollapply
  rollcarhart2 <- function(i) {
    window.index = (i - window.len + 1) : i
    reglist <- carhart_reg(ticker, trimdat[window.index,])
    xts(x = t(as.matrix(c(reglist$est, "r2" = reglist$r2, "std.err" = reglist$std.err))) , index(trimdat)[i] )
  }
  
  # main loop
  out <- lapply(window.len:nperiods, function(x)rollcarhart2(x)) %>% 
    do.call(rbind, .)
  
  fl = list()
  fl$estimate = merge(out[,1:5], out$r2)
  fl$std.error = out[, 7:11]
  fl$std.error$std.err.SE_r2 = NA
  
  return(list(full_sample_res = full_sample_res, fl = fl, window.len=window.len,
              y=yout, x=xout, RF=xtsenv$factors$RF,  olsstat = lmres))
}

#' This is rolling plot of Carhart loadings
#' @details Uses lattice graphics (faster). style_roll_detail_plot_ggplot is 
#' slower but prettier.
style_roll_detail_plot_lattice <- function(obj) {
  
  n <- ncol(obj$fl$estimate) 
  dates <- index(obj$fl$estimate)
  par(mar = c(1, 1, 1, 1))
  layout(matrix(1:(2 * n), nc = 2, byrow = T))
  
  # Function to plot coefficient's rolling error bounds
  polyprep_xts <- function(ubnd_xts, lbnd_xts, xlabel = "X", ylabel = "Y") {
    all <- merge(ubnd_xts, lbnd_xts)
    xx <- c(index(all), rev(index(all))) # order polygon points anticlockwise
    yy <- c(coredata(lbnd_xts), rev(coredata(ubnd_xts)))  # anticlocklise
    plot(as.Date(index(ubnd_xts)), coredata(ubnd_xts), type = "l", col = "blue", 
         bty = "L", xlab = xlabel, main = ylabel)
    polygon(xx, yy, col = "grey", border = "red", lty = 2)
  }
  
  # Plot a row per cofficient
  for (i in 1:n) {
    par(mar = c(4, 2, 2, 1))
    
    est <- obj$fl$estimate[, i]
    est.std.error <- ifna(obj$fl$std.error[, i], 0)
    ubnd_xts <- est + est.std.error
    lbnd_xts <- est - est.std.error
    
    # Plot Estimate's Error Bounds
    yaxes <- if (i == 1) "Alpha" else names(est)
    polyprep_xts(ubnd_xts, lbnd_xts, xlabel = "Date", ylabel = yaxes)
    
    # Plot Rolling estimate
    lines(index(obj$fl$estimate[, i]), obj$fl$estimate[, i], col = "blue")
    abline(h = 0, col = "blue", lty = "dashed")
    
    # Plot Histogram of Rolling Estimates
    hist(obj$fl$estimate[, i], col = "grey", border = "gray", las = 1, xlab = "", 
         ylab = "", main = yaxes)
    abline(v = obj$full_sample_res$estimate[i], col = "blue", lwd = 2)
  }
  
}


# detail plot for each factor and histogram
# TODO ggplot
style_roll_detail_plot_ggplot <- function(obj) {
  
  n = ncol(obj$fl$estimate)
  dates = index(obj$fl$estimate)

  coefplot <- function(est, est.std.error) {
    y = est %>% as.data.frame()
    coefname = colnames(y)[1]
    colnames(y)[1] = "val"
    y$dt = index(est)
    p2 <- ggplot(y, aes(x= dt, y = val)) +
      geom_line() +
      geom_ribbon(
        aes(ymin= est - est.std.error,ymax=est + est.std.error),
        alpha=0.3) +
      ylab(coefname) +
      xlab("Date")
    p2
  }
  
  coefhist <- function(est) {
    y = est %>% as.data.frame()
    coefname = colnames(y)[1]
    qplot( est %>% coredata, bins = 30) + xlab(coefname)
  }
  
  grphlist <- lapply(1:n, function(x) coefplot(obj$fl$estimate[,x],
                                    ifna(obj$fl$std.error[,x], 0))
    )
  
  histlist <- lapply(1:n, function(x) coefhist( obj$fl$estimate[,x]) )
  
  multiplot( plotlist = c(grphlist, histlist), cols = 2, layout = 
               matrix(c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12), 
                      ncol = 2, byrow=TRUE))
  
}


# style plot for 2 given factors
style_roll_style_plot <- function(obj, 
                                                 xfactor='HML', yfactor='SMB',
                                                 xlim = c(-1.5, 1.5), ylim = c(-0.5, 1.5)
) {
  # Style chart	
  i = which(colnames(obj$fl$estimate) == xfactor)
  x = coredata(obj$fl$estimate[,i])
  x.e = ifna(coredata(obj$fl$std.error[,i]), 0)
  
  x.all = obj$full_sample_res$estimate[i]
  x.all.e = obj$full_sample_res$std.error[i]
  
  xlab = colnames(obj$fl$estimate)[i]
  
  i = which(colnames(obj$fl$estimate) == yfactor)
  y = coredata(obj$fl$estimate[,i])
  y.e = ifna(coredata(obj$fl$std.error[,i]), 0)
  
  y.all = obj$full_sample_res$estimate[i]
  y.all.e = obj$full_sample_res$std.error[i]
  
  ylab = colnames(obj$fl$estimate)[i]
  
  # plot
  layout(1)
  plot(x,y, xlab=xlab, ylab = ylab, type='n', las=1,
       xlim = range(c(x + x.e, x - x.e, xlim), na.rm=T),
       ylim = range(c(y + y.e, y - y.e, ylim), na.rm=T),
       main = paste('Style, last =', ylab, round(last(y),2), xlab, round(last(x),2))
  )		
  grid()
  abline(h=0)
  abline(v=0)
  
  
  col = col_add_alpha('pink',250)
  rect(x - x.e, y - y.e, x + x.e, y + y.e, col=col, border=NA)
  
  points(x,y, col='red', pch=20)
  points(last(x),last(y), col='black', pch=3)	
  points(x.all,y.all, col='blue', pch=15)
  
  legend('topleft', spl('Estimates,Last estimate,Overall estimate'),
         pch = c(20,3,15),
         col = spl('red,black,blue'),
         pt.bg = spl('red,black,blue'),
         bty='n'
  ) 
}


# re-construct historical perfromance based on factor loadings
# compare fund perfromance to the
# - re-constructed portfolio based on the regression over whole period
# - re-constructed portfolio based on the rolling window regression
style_roll_bt_plot <- function(obj) {
  # setup
  ticker = colnames(obj$y)
  n = ncol(obj$fl$estimate)-1
  nperiods = nrow(obj$fl$estimate)
  
  # fund, alpha, factors, RF
  ret = cbind(obj$RF, obj$y, 1, obj$x)
  colnames(ret)[1:3] = spl('RF,fund,alpha')
  prices = bt_apply_matrix(1+ifna(ret,0),cumprod)
  
  xtsenv <- new.env()
  xtsenv$symbolnames = colnames(prices)		
  
  for(i in colnames(prices)) {
    xtsenv[[i]] = prices[,i]
    colnames(xtsenv[[i]]) = 'Close'
  }
  
  bt_prep(xtsenv, align='keep.all')
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 	
  
  # create models
  models = list()
  
  xtsenv$weight[] = NA
  xtsenv$weight$fund = 1
  xtsenv$weight$RF = 1
  xtsenv$weight[1:obj$window.len,] = NA
  models[[ticker]] = bt_run_share(xtsenv, clean.signal = F)
  
  xtsenv$weight[] = NA
  xtsenv$weight[,3:(n+2)] = t(repmat(obj$full_sample_res$estimate[1:n], 1, nperiods))
  xtsenv$weight$RF = 1
  xtsenv$weight[1:obj$window.len,] = NA
  models$all.alpha = bt_run_share(xtsenv, clean.signal = F)
  
  xtsenv$weight[] = NA
  xtsenv$weight[,3:(n+2)] = t(repmat(obj$full_sample_res$estimate[1:n], 1, nperiods))
  xtsenv$weight$RF = 1
  xtsenv$weight$alpha = NA
  xtsenv$weight[1:obj$window.len,] = NA
  models$all = bt_run_share(xtsenv, clean.signal = F)
  
  xtsenv$weight[] = NA
  xtsenv$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
  xtsenv$weight$RF = 1
  xtsenv$weight[1:obj$window.len,] = NA
  models$est.alpha = bt_run_share(xtsenv, clean.signal = F)
  
  xtsenv$weight[] = NA
  xtsenv$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
  xtsenv$weight$RF = 1
  xtsenv$weight$alpha = NA	
  xtsenv$weight[1:obj$window.len,] = NA
  models$est = bt_run_share(xtsenv, clean.signal = F)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 	
  # Plot perfromance
  layout(1)
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
  mtext('Cumulative Performance', side = 2, line = 1)

}
