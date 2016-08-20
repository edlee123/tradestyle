###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Backtest Summary Report Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

#' Default summary of the system. 
#' Custom summary functions like this can be passed into bt_detail_summary
#'
sys_summary <- function (bt, trade.summary) {
  
  out = list()
  out$Period = join( format( range(index_xts(bt$equity)), '%b%Y'), ' - ')
  
  out$CAGR = compute_cagr(bt$equity)
  out$Sharpe = compute_sharpe(bt$ret) / 100 # TODO check this!
  out$DVR = compute_DVR(bt) / 100
  out$Volatility = compute_risk(bt$ret)
  
  out$MaxDD = compute_max_drawdown(bt$equity)
  out$AvgDD = compute_avg_drawdown(bt$equity)
  
  if( !is.null(trade.summary) ) {
    out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
  }
  
  out$VaR = compute_var(bt$ret)
  out$CVaR = compute_cvar(bt$ret)
  
  out$Exposure = compute_exposure(bt$weight)		
  out
}

#' Use the morningstar definition of sharpe ratio.
#' http://corporate.morningstar.com/US/documents/MethodologyDocuments/MethodologyPapers/StandardDeviationSharpeRatio_Definition.pdf
sys_summary2 <- function (bt, trade.summary) {
  
  out = list()
  out$Period = join( format( range(index_xts(bt$equity)), '%b%Y'), ' - ')
  
  out$CAGR = compute_cagr(bt$equity)
  monthlyret <- monthlyReturn(bt$equity) # simple monthly ret
  # TODO get save RF rate, retrieve and merge it, and pass to below
  out$Sharpe = SharpeRatio.annualized(monthlyret, Rf = 0, scale = 12) / 100 
  out$VolFromMonthlyRet = StdDev.annualized(monthlyret, scale = 12) # ann monthly simple rets
  out$VolFromDailyRet = compute_risk(bt$ret) # ann daily simple ret (not log ret)
  out$MaxDrawdownMonthlyRet = -maxDrawdown(monthlyret)
  out$MaxDrawdownDailyRet = compute_max_drawdown(bt$equity)
  out$AvgDD = compute_avg_drawdown(bt$equity)
  # below not that important. straight line performance i.e. sharpe x R^2 of wealth to straightline
  out$DVR = compute_DVR(bt) / 100 
  
  if( !is.null(trade.summary) ) {
    out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
  }
  
  out$VaR = compute_var(bt$ret)
  out$CVaR = compute_cvar(bt$ret)
  
  out$Exposure = compute_exposure(bt$weight)		
  out
}

#' Use monthly stats only except for 
sys_summary3 <- function (bt, trade.summary) {
  
  out = list()
  out$Period = join( format( range(index_xts(bt$equity)), '%b%Y'), ' - ')
  
  out$CAGR = compute_cagr(bt$equity)
  monthlyret <- monthlyReturn(bt$equity) # simple monthly ret
  # TODO get save RF rate, retrieve and merge it, and pass to below
  out$Sharpe = SharpeRatio.annualized(monthlyret, Rf = 0, scale = 12) / 100 
  out$Vol = StdDev.annualized(monthlyret, scale = 12) # ann monthly simple rets
  out$MaxDrawdown = -maxDrawdown(monthlyret)
  out$AvgDrawdown = compute_avg_drawdown(bt$equity)
  
  # below not that important. straight line performance i.e. sharpe x R^2 of wealth to straightline
  out$DVR = compute_DVR(bt) / 100 
  
  if( !is.null(trade.summary) ) {
    out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
  }
  # out$SemiDeviation = SemiDeviation(monthlyret)
  out$VaR = compute_var(bt$ret)
  out$CVaR = compute_cvar(bt$ret)
  out$Exposure = compute_exposure(bt$weight)		
  names(out)[length(out)-1] = "CVaR 95% 1-Day"
  names(out)[length(out)-2] = "VaR 95% 1-Day"
  out
}


trade_summary <- function (bt) {
  if( !is.null(bt_trade_summary) ) trade.summary = bt_trade_summary
  out = list()
  if( !is.null(trade.summary) ) {
    out$Win.Percent = trade.summary$stats['win.prob', 'All']
    out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
    out$Avg.Win = trade.summary$stats['win.avg.pnl', 'All']
    out$Avg.Loss = trade.summary$stats['loss.avg.pnl', 'All']
    
    out = lapply(out, function(x) if(is.double(x)) round(100 * x, 1) else x)		
    
    out$Best.Trade = max(as.double(trade.summary$trades[, 'return']))
    out$Worst.Trade = min(as.double(trade.summary$trades[, 'return']))
    
    out$WinLoss.Ratio = round( 
      -trade.summary$stats['win.avg.pnl', 'All'] / 
        trade.summary$stats['loss.avg.pnl', 'All'], 2)
    out$Avg.Len = round(trade.summary$stats['len', 'All'],2)
    out$Num.Trades = trade.summary$stats['ntrades', 'All']			
  }
  out
}

###############################################################################
#' @title  Backtest Detail summary
#' @details This is the key function used to output summary statistics of a bt.
#' @details Modified this so it can take in different summary functions.
#' @details TODO (Ed): check this sharpe ratio makes sense.
#' @details looks like he got this from http://mysimplequant.blogspot.com/p/trading-system-performance-indicators.html
#' @export 
bt_detail_summary <- function
(
	bt,		# backtest object
	trade.summary = NULL,
	system_summary_fn = sys_summary3
) 
{	
	out.all = list()

	# System Section	
	
	out <- system_summary_fn(bt, trade.summary)  # can pass in a custom fn if
	out.all$System = lapply(out, function(x) if(is.double(x)) round(100*x,2) else x)
		
	# Trade Section	
	
	out.all$Trade = trade_summary(bt)
		
	# Period Section
	out = list()
		out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T) / len(bt$ret)
		out$Best.Day = bt$best
		out$Worst.Day = bt$worst
		
		month.ends = endpoints(bt$equity, 'months')
		mret = ROC(bt$equity[month.ends,], type = 'discrete')
		out$Win.Percent.Month = sum(mret > 0, na.rm = T) / len(mret)
		out$Best.Month = max(mret, na.rm = T)
		out$Worst.Month = min(mret, na.rm = T)
		
		year.ends = endpoints(bt$equity, 'years')
		mret = ROC(bt$equity[year.ends,], type = 'discrete')
		out$Win.Percent.Year = sum(mret > 0, na.rm = T) / len(mret)
		out$Best.Year = max(mret, na.rm = T)
		out$Worst.Year = min(mret, na.rm = T)
	out.all$Period = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
	
	return(out.all)
}

###############################################################################
# Rotational Trading: how to reduce trades and improve returns by Frank Hassler
# http://engineering-returns.com/2011/07/06/rotational-trading-how-to-reducing-trades-and-improve-returns/
# Custom Summary function to replicate tables from Engineering Returns
#' @export 
###############################################################################
engineering_returns_kpi <- function
(
	bt,		# backtest object
	trade.summary = NULL
) 
{	
	if( !is.null(bt_trade_summary) ) trade.summary = bt_trade_summary
	
	out = list()
	out$Period = join( format( range(index(bt$equity)), '%b%Y'), ' - ')
		
	out$Cagr = compute_cagr(bt$equity)
	out$Sharpe = compute_sharpe(bt$ret) / 100	
	out$DVR = compute_DVR(bt) / 100	
	out$R2 = compute_R2(bt$equity) / 100	

	out$Volatility = compute_risk(bt$ret)
	out$MaxDD = compute_max_drawdown(bt$equity)
	out$Exposure = compute_exposure(bt$weight)						
			
	if( !is.null(trade.summary) ) {
		out$Win.Percent = trade.summary$stats['win.prob', 'All']
		out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']		
		out$Profit.Factor = trade.summary$stats['profitfactor', 'All'] / 100
	}
		

	# format
	out = lapply(out, function(x) if(is.double(x)) round(100*x,2) else x)
				
	if( !is.null(trade.summary) ) out$Num.Trades = trade.summary$stats['ntrades', 'All']			
			
	return( list(System=out))
}
	
###############################################################################
# Plot strategy perfromance side by side		
#' @export 
###############################################################################
plotbt_strategy_sidebyside <- function
( 
	... , 
	perf.metric = spl('System,Trade,Period'), 
	perf.fn = 'bt_detail_summary',
	return.table = FALSE,
	make.plot = TRUE
) 
{
	models = variable_number_arguments( ... )
	out = list()
	
	for( i in 1:len(models) ) {
		out[[ names(models)[i] ]] = match.fun(perf.fn)(models[[ i ]])[[ perf.metric[1] ]]
	}
	temp = list2matrix(out, keep.names=F)
	if(make.plot) plot_table( temp, smain = perf.metric[1] )
	
	if(return.table) return(temp)
}


###############################################################################
# Plot equity curves for each strategy(model)
#' @export 
###############################################################################
plotbt <- function
(
	...,				# variable arguments
	dates = NULL,		# dates subset
	plottype = spl('line,12M'),
	xfun=function(x) { x$equity },
	main = NULL,
	plotX = T,
	log = '',
	x.highlight = NULL,
	LeftMargin = 0 
) 
{    		
	models = variable_number_arguments( ... )					
	plottype = plottype[1]
   	n = length(models)    	

   	# get data
   	temp = list()
   	for( i in 1:n ) {
		msg = try( match.fun(xfun)( models[[i]] ) , silent = TRUE)
		if (class(msg)[1] != 'try-error') {
			temp[[i]] = msg
		}
	}

	
	# prepare plot
	#last.date = index(last(temp[[1]]))
	#prev.year.last.date = last.date - 365
	#nlag = max( 1, nrow(temp[[1]]) - which.min(abs(index(temp[[1]]) - prev.year.last.date)) )
	nlag = max( 1, compute_annual_factor(temp[[1]]) )
	
	# nlag = len(last(temp[[1]], '12 months'))
   	yrange=c();   	   	
   	for( i in 1:n ) {
   		itemp = temp[[i]]
   		
   		if(!is.null(dates)) {
   			itemp = itemp[dates]
   			if(itemp[1] != 0) itemp = itemp / as.double(itemp[1])
   		}
   		
   		if( plottype == '12M' ) {
   			itemp = 100 * (itemp / mlag(itemp, nlag ) - 1)
   		}
   		temp[[i]] = itemp
   		
		yrange = range(yrange, itemp ,na.rm = T)		
   	}
   	  	
   	# plot
	plota(temp[[1]], main = main, plotX = plotX, type = 'l', col = 1, 
		ylim = yrange,log = log, LeftMargin = LeftMargin, x.highlight = x.highlight)
		
	if( n > 1 ) {
		for( i in 2:n ) plota_lines(temp[[i]], col = i)
	}
	
	if( plottype == '12M' ) legend('topright', legend = '12 Month Rolling', bty = 'n')
	plota_legend(names(models), paste('', 1:n, sep=''), temp)	
}

###############################################################################
# Plot Transition Map
#' @export 
###############################################################################
plotbt_transition_map <- function
(
	weight,
	name = '',
	col = rainbow(ncol(weight), start=0, end=.9),
	x.highlight = NULL,
	sort.asssets = T
) 
{
	par(mar=c(2, 4, 1, 1), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
	
	
	weight[is.na(weight)] = 0
	
	# arrange so that most consient holdings are at the bottom 
	if(sort.asssets) weight = weight[, sort.list(colSums(weight!=0), decreasing=T)]
	
	plota_stacked(index_xts(weight), weight, col = col, type='s', flip.legend=T, main = iif(nchar(name) > 0, paste('Transition Map for', name), ''), x.highlight = x.highlight)	
}
	
###############################################################################
# Plot Pie Chart for holdings
#' @export 
###############################################################################
plotbt_holdings <- function
(
	weight, 
	smain = format(index_xts(last(weight)), '%d-%b-%Y') 
) 
{
	par(mar=c(2, 2, 2, 2), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
	icols=rainbow(ncol(weight), start=0, end=.9)
	
	# sync order of assets with plotbt_transition_map
	# arrange so that most consient holdings are at the bottom 
	weight = weight[, sort.list(colSums(weight!=0, na.rm=T), decreasing=T), drop=F]
	
	temp = 100 * as.vector(last(weight))
	atemp = abs(temp)	
	
	if(sum(atemp)>0) {
		pie(atemp, labels = paste(round(temp,0), '% ', colnames(weight), sep=''), 
			col = icols, cex =0.8,
			main = paste('Allocation for ', smain, sep='')
		)
	}
}

###############################################################################
# Plot Pie Chart for holdings throught out time
#' @export 
###############################################################################
plotbt_holdings_time <- function(weight, smain='') 
{
	weight = as.matrix( apply(abs(weight), 2, sum, na.rm = T) )
	if( sum(abs(weight)) > 0 ) plotbt_holdings( t(weight) / sum(abs(weight), na.rm = T), smain = paste0(smain, ' in time'))
}



###############################################################################
# Plot monthly return table
#' @title Tabulate calendar returns
#' @details TODO (Ed): better not to use index to avoid confusion with xts::index
#' @export 
plotbt_monthly_table <- function(equity, make.plot = TRUE, smain = '') 
{
	equity = map2monthly(equity)

	dates = index_xts(equity)
	equity = coredata(equity)	
	
  # just keep both versions for now	
  if(T) {		
  	# find period ends
  	month.ends = date_month_ends(dates)
  	year.ends =  date_year_ends(dates[month.ends])
  		year.ends = month.ends[year.ends]
  	nr = len(year.ends) + 1
  		
  } else {		
  	# find period ends
  	month.ends = unique(c(endpoints(dates, 'months'), len(dates)))
  		month.ends = month.ends[month.ends>0]
  	year.ends =  unique(c(endpoints(dates[month.ends], 'years'), len(month.ends)))
  		year.ends = year.ends[year.ends>0]
  		year.ends = month.ends[year.ends]
  	nr = len(year.ends) + 1	
  }
  	
	# create plot matrix
	temp = matrix( double(), nr, 12 + 2)
		rownames(temp) = c(date_year(dates[year.ends]), 'Avg')
		colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Year,MaxDD')
	
	# compute yearly profit and drawdown
	ind = c(1, year.ends)
	for(iyear in 2:len(ind)) {
		iequity = equity[ ind[(iyear-1)] : ind[iyear] ]
		iequity = ifna( ifna_prev(iequity), 0)
		
		temp[(iyear-1), 'Year'] = xts::last(iequity, 1) / iequity[1] -1
		temp[(iyear-1), 'MaxDD'] = min(iequity / cummax(iequity) - 1, na.rm = T)
	}

	# compute monthly profit
	ind = month.ends
		monthly.returns = c(NA, diff(equity[ind]) / equity[ind[-len(ind)]])
		
		ind = date_month(range(dates[ind]))
		monthly.returns = c( rep(NA, ind[1]-1), monthly.returns, rep(NA, 12-ind[2]) )
		temp[1:(nr - 1), 1:12] = matrix(monthly.returns, ncol=12, byrow = T)
			
	# compute averages
	temp = ifna(temp, NA)
	temp[nr,] = apply(temp[-nr,], 2, mean, na.rm = T)		

	if(make.plot) {
		#higlight
		highlight = temp
			highlight[] = iif(temp > 0, 'lightgreen', iif(temp < 0, 'red', 'white'))
			highlight[nr,] = iif(temp[nr,] > 0, 'green', iif(temp[nr,] < 0, 'orange', 'white'))
			highlight[,13] = iif(temp[,13] > 0, 'green', iif(temp[,13] < 0, 'orange', 'white'))
			highlight[,14] = 'yellow'
	}
	
	
	temp[] = plota_format(100 * temp, 1, '', '')
	
	# plot
	if(make.plot) plot_table(temp, highlight = highlight, smain = smain)
	
	return(temp)
}	
	


###############################################################################
# Convert list of lists to matrix
#' @export 
###############################################################################
list2matrix <- function
(
	ilist,
	keep.names = TRUE
) 
{   
	if ( is.list( ilist[[1]] ) ) {
		inc = 1
		if( keep.names ) inc = 2
		
		out = matrix('', nr = max(unlist(lapply(ilist, len))), nc = inc * len(ilist) )
		colnames(out) = rep('', inc * len(ilist))
		
		for( i in 1:len(ilist) ) {
			nr = len(ilist[[i]])
			colnames(out)[inc * i] = names(ilist)[i]
			
			if(nr > 0){ 
				if( keep.names ) {
					out[1:nr,(2*i-1)] = names(ilist[[i]])
				} else {
					rownames(out) = names(ilist[[i]])
				}
				out[1:nr,inc*i] = unlist(ilist[[i]])
			}
		}
		return(out)
	} else {
		return( as.matrix(unlist(ilist)) )
	}
}





###############################################################################
# Custom Backtest Report
#' @export 
###############################################################################
plotbt_custom_report <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  x.highlight = NULL
) 
{	
  # create layout	
  ilayout = 
    '1,1
  1,1
  2,2
  3,3
  4,6
  4,6
  5,7
  5,8'
  plota_layout(ilayout)
  
  models = variable_number_arguments( ... )
  
  # Main plot
  plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('12 Month Rolling', side = 2, line = 1)
  
  plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute_drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
  mtext('Drawdown', side = 2, line = 1)
  
  model = models[[1]]
  name=ifnull(names(models),'')[1]
  
  # Additional Info
  plotbt_transition_map(model$weight, x.highlight = x.highlight, name=name)
  temp = plotbt_monthly_table(model$equity, smain=name)		
  plotbt_holdings_time(model$weight, smain=name)	
  
  if ( !is.null(model$trade.summary) ) {
    plot_table( list2matrix(bt_detail_summary(model, model$trade.summary)), keep_all.same.cex = TRUE, smain=name)
  } else {
    plot_table( list2matrix(bt_detail_summary(model)), keep_all.same.cex = TRUE, smain=name)
  }
  
  if( len(models) > 1 ) plotbt_strategy_sidebyside(models)
  
  if ( trade.summary & !is.null(model$trade.summary)) {	
    ntrades = min(20, nrow(model$trade.summary$trades))
    
    temp = last(model$trade.summary$trades, ntrades)
    if( ntrades == 1 ) temp = model$trade.summary$trades
    print( temp )
    print( model$trade.summary$stats )
    
    #layout(1)
    layout(c(1,rep(2,10)))
    
    # make dummy table with name of strategy		
    make_table(1,1)
    a = matrix(names(models)[1],1,1)
    cex = plot_table_helper_auto_adjust_cex(a)
    draw_cell(a[1],1,1, text.cex=cex,frame.cell=F)		
    
    plot_table( temp )
  }	
}	

# split plotbt_custom_report into 3 functions
#' @export 
plotbt_custom_report_part1 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  x.highlight = NULL
) 
{	
  layout(1:3)
  
  models = variable_number_arguments( ... )
  model = models[[1]]
  
  # Main plot
  plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('12 Month Rolling', side = 2, line = 1)
  
  plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute_drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
  mtext('Drawdown', side = 2, line = 1)
}

#' @export 
plotbt_custom_report_part2 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  x.highlight = NULL	
) 
{	
  models = variable_number_arguments( ... )
  model = models[[1]]
  name=ifnull(names(models),'')[1]
  
  # create layout	
  if( len(models) > 1 )
    ilayout = 
    '1,1,3,4
  2,2,5,5
  2,2,6,6'
  else
    ilayout = 
    '1,1,1,3
  2,2,4,4
  2,2,5,5'
  
  plota_layout(ilayout)
  
  
  # Additional Info
  plotbt_transition_map(model$weight, x.highlight = x.highlight, name=name)
  temp = plotbt_monthly_table(model$equity, smain=name)	
  if( len(models) > 1 ) 
    plotbt_holdings_time(model$weight, smain=name)
  
  plot_table(to_percent(t(last(models[[1]]$weight))), smain=name)
  
  if ( !is.null(model$trade.summary) ) {
    plot_table( list2matrix(bt_detail_summary(model, model$trade.summary)), keep_all.same.cex = TRUE, smain=name)
  } else {
    plot_table( list2matrix(bt_detail_summary(model)), keep_all.same.cex = TRUE, smain=name)	
  }
  
  if( len(models) > 1 ) 
    plotbt_strategy_sidebyside(models)
  else
    plotbt_holdings_time(model$weight, smain=name)
  
}	

#' @export 
plotbt_custom_report_part3 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE 
) 
{	
  
  models = variable_number_arguments( ... )
  model = models[[1]]
  
  if ( trade.summary & !is.null(model$trade.summary)) {	
    ntrades = min(20, nrow(model$trade.summary$trades))
    
    temp = last(model$trade.summary$trades, ntrades)
    if( ntrades == 1 ) temp = model$trade.summary$trades
    print( temp )
    print( model$trade.summary$stats )
    
    #layout(1)
    layout(c(1,rep(2,10)))
    
    # make dummy table with name of strategy		
    make_table(1,1)
    a = matrix(names(models)[1],1,1)
    cex = plot_table_helper_auto_adjust_cex(a)
    draw_cell(a[1],1,1, text.cex=cex,frame.cell=F)		
    
    plot_table( temp )		
  }	
}
