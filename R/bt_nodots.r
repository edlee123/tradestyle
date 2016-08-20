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
# Backtest Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Align dates, faster version of merge function
#' @export 
###############################################################################
bt_merge <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL	# subset of dates
) 
{
	align = align[1]
	symbolnames = b$symbolnames
	nsymbols = len(symbolnames) 
	
	# count all series
	ncount = sapply(symbolnames, function(i) nrow(b[[i]]))
		all.dates = double(sum(ncount))		
		
	# put all dates into one large vector
	itemp = 1
	for( i in 1:nsymbols ) {
		all.dates[itemp : (itemp + ncount[i] -1)] = attr(b[[ symbolnames[i] ]], 'index')
		itemp = itemp + ncount[i]
	}
		
	# find unique
	temp = sort(all.dates)
	unique.dates = c(temp[1], temp[-1][diff(temp)!=0])
	
	# trim if date is supplied	
	if(!is.null(dates)) { 
		class(unique.dates) = c('POSIXct', 'POSIXt')	
		temp = make_xts(integer(len(unique.dates)), unique.dates) 		
		unique.dates = attr(temp[dates], 'index')
	}
		
	# date map
	date.map = matrix(NA, nr = len(unique.dates), nsymbols)
	itemp = 1
	for( i in 1:nsymbols ) {
		index = match(all.dates[itemp : (itemp + ncount[i] -1)], unique.dates)
		sub.index = which(!is.na(index))
		date.map[ index[sub.index], i] = sub.index
		itemp = itemp + ncount[i]
	}
	
	# trim logic
	index = c()
	if( align == 'remove.na' ) { 
		index = which(count(date.map, side=1) < nsymbols )
	} 
	# keep all
#	else {
#		index = which(count(date.map, side=1) < max(1, 0.1 * nsymbols) )
#	}
	
	if(len(index) > 0) { 
		date.map = date.map[-index,, drop = FALSE]
		unique.dates = unique.dates[-index] 
	}
	
	class(unique.dates) = c('POSIXct', 'POSIXt')	
	return( list(all.dates = unique.dates, date.map = date.map))
}


###############################################################################
# Prepare backtest data enviroment
#
# it usually contains:
# * b$symbolnames
# * b$universe
# * b$prices
# * b - asset hist data
#
#' @export 
###############################################################################
# Will handle OHLC, and look for certain column names e.g. Open, High etc.
bt_prep <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL,	# subset of dates
	fill.gaps = F,	# fill gaps introduced by merging
	basic = F		# control if xts object are created
) 
{    
	# setup
	if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
	symbolnames = b$symbolnames
	nsymbols = len(symbolnames) 
	
	if( nsymbols > 1 ) {
		# merge
		out = bt_merge(b, align, dates)
		
		for( i in 1:nsymbols ) {
			temp = coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE]
			b[[ symbolnames[i] ]] = iif(basic, temp, make_xts( temp, out$all.dates))
		
			# fill gaps logic
			map.col = find_names('Close,Volume,Open,High,Low,Adjusted', b[[ symbolnames[i] ]])
			if(fill.gaps & !is.na(map.col$Close)) {	
				close = coredata(b[[ symbolnames[i] ]][,map.col$Close])
					n = len(close)
					last.n = max(which(!is.na(close)))
				close = ifna_prev(close)
				if(last.n + 5 < n) close[last.n : n] = NA
				b[[ symbolnames[i] ]][, map.col$Close] = close
					index = !is.na(close)	

				if(!is.na(map.col$Volume)) {
					index1 = is.na(b[[ symbolnames[i] ]][, map.col$Volume]) & index
					b[[ symbolnames[i] ]][index1, map.col$Volume] = 0
				}
				
				#for(j in colnames(b[[ symbolnames[i] ]])) {
				for(field in spl('Open,High,Low')) {
				j = map.col[[field]]
				if(!is.null(j)) {
					index1 = is.na(b[[ symbolnames[i] ]][,j]) & index
					b[[ symbolnames[i] ]][index1, j] = close[index1]
				}}

				j = map.col$Adjusted
				if(!is.null(j)) {
					b[[ symbolnames[i] ]][index, j] = ifna_prev(b[[ symbolnames[i] ]][index, j])
				}
				
				
				#for(j in setdiff(1:ncol( b[[ symbolnames[i] ]] ), unlist(map.col))) {
				#	b[[ symbolnames[i] ]][index, j] = ifna_prev(b[[ symbolnames[i] ]][index, j])				
				#}				
			}
		}	
	} else {
		if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]	
		out = list(all.dates = index_xts(b[[ symbolnames[1] ]]) )
		if(basic) b[[ symbolnames[1] ]] = coredata( b[[ symbolnames[1] ]] )
	}

	# dates
	b$dates = out$all.dates
		   
	# empty matrix		
	dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
		colnames(dummy.mat) = symbolnames
		if(!basic) dummy.mat = make_xts(dummy.mat, out$all.dates)
		
	# weight matrix holds signal and weight information		
	b$weight = dummy.mat
	
	# execution price, if null use Close	
	b$execution.price = dummy.mat
		
	# populate prices matrix
	for( i in 1:nsymbols ) {
		if( has.Cl( b[[ symbolnames[i] ]] ) ) {
			dummy.mat[,i] = Cl( b[[ symbolnames[i] ]] );
		}
	}
	b$prices = dummy.mat	
}




# matrix form
#' @export 
bt_prep_matrix <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL,	# subset of dates
	basic = F		# control if xts object are created	
)
{    
	align = align[1]
	nsymbols = len(b$symbolnames)
	
	# merge
	if(!is.null(dates)) { 	
		temp = make_xts(1:len(b$dates), b$dates)
		temp = temp[dates] 
		index = as.vector(temp)
		
		for(i in b$fields) b[[ i ]] = b[[ i ]][index,, drop = FALSE]
		
		b$dates = b$dates[index]
	}
 
	if( align == 'remove.na' ) { 
		index = which(count(b$Cl, side=1) < nsymbols )
	} else {
		index = which(count(b$Cl,side=1) < max(1,0.1 * nsymbols) )
	}
	
	if(len(index) > 0) { 
		for(i in b$fields) b[[ i ]] = b[[ i ]][-index,, drop = FALSE]
		
		b$dates = b$dates[-index]
	}
	
	# empty matrix		
	dummy.mat = iif(basic, b$Cl, make_xts(b$Cl, b$dates))
		
	# weight matrix holds signal and weight information		
	b$weight = NA * dummy.mat
	
	b$execution.price = NA * dummy.mat
	
	b$prices = dummy.mat
}


bt_prep_matrix_test <- function() {
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load_packages('quantmod')
	# example csv file holds returns
	# Date ,A,B
	# Jan-70,0.01,0.02
	returns = read_xts('Example.csv', date.fn=function(x) paste('1',x), format='%d %b-%y')
	prices = bt_apply_matrix(1 + returns, cumprod)
	
	data <- new.env()
		data$symbolnames = colnames(prices)
		data$dates = index(prices)
		data$fields = 'Cl'
		data$Cl = prices				
		
	bt_prep_matrix(data)
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# Buy & Hold	
	data$weight[] = NA
		data$weight[] = 1
	buy.hold = bt_run_share(data)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 		
	plotbt(buy.hold, plotX = T, log = 'y', LeftMargin = 3)	    	
			mtext('Cumulative Performance', side = 2, line = 1)
}

###############################################################################
# Remove symbols_from enviroment
#' @export 
###############################################################################
bt_prep_remove_symbols_min_history <- function
(
	b, 					# enviroment with symbols time series
	min.history = 1000	# minmum number of observations
) 
{
	bt_prep_remove_symbols(b, which( count(b$prices, side=2) < min.history ))
}


#' @export 
bt_prep_remove_symbols <- function
(
	b, 					# enviroment with symbols time series
	index				# index of symbols to remove
) 
{
	if( len(index) > 0 ) {
		if( is.character(index) ) index = match(index, b$symbolnames)
		 
		b$prices = b$prices[, -index]
		b$weight = b$weight[, -index]
		b$execution.price = b$execution.price[, -index]
		
		env_rm(b$symbolnames[index], b)
		b$symbolnames = b$symbolnames[ -index]
	}
}


###############################################################################
#' Trim data enviroment
#'
#' This function will remove weights that are smaller than given threshold
#'
#' @param b original enviroment with symbols time series
#' @param dates dates to keep from original enviroment
#'
#' @return updated enviroment with symbols time series
#'
#' @examples
#' \dontrun{ 
#' bt_prep_trim(data, endpoints(data$prices, 'months'))
#' bt_prep_trim(data, '2006::')
#' }
#' @export 
bt_prep_trim <- function
(
	b, 				# enviroment with symbols time series
	dates = NULL	# subset of dates
) 
{	
	if(is.null(dates)) return(b)
	
	# convert dates to dates.index
	dates.index = dates2index(b$prices, dates)	
	
	data.copy <- new.env()
	for(s in b$symbolnames) data.copy[[s]] = b[[s]][dates.index,,drop=F]
		 
	data.copy$symbolnames = b$symbolnames
	data.copy$dates = b$dates[dates.index]
	
	data.copy$prices = b$prices[dates.index,,drop=F]
	data.copy$weight = b$weight[dates.index,,drop=F]
	data.copy$execution.price = b$execution.price[dates.index,,drop=F]
	return(data.copy)
}
 

###############################################################################
# Helper function to backtest for type='share'
#' @export 
###############################################################################
bt_run_share <- function
(
	b,					# enviroment with symbols time series
	prices = b$prices,	# prices
	clean.signal = T,	# flag to remove excessive signal
	
	trade.summary = F, 	# flag to create trade summary
	do.lag = 1, 		# lag signal
	do.CarryLastObservationForwardIfNA = TRUE, 	
	silent = F,
	capital = 100000,
	commission = 0,
	weight = b$weight,
	dates = 1:nrow(b$prices)
) 
{
	# make sure that prices are available, assume that
	# weights account for missing prices i.e. no price means no allocation
	prices[] = bt_apply_matrix(coredata(prices), ifna_prev)	

	weight = mlag(weight, do.lag - 1)
	do.lag = 1

	if(clean.signal)
		weight[] = bt_exrem(weight)
	
	weight = (capital / prices) * weight
	
	bt_run(b,
		trade.summary = trade.summary,
		do.lag = do.lag,
		do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA,
		type='share',
		silent = silent,
		capital = capital,
		commission = commission,
		weight = weight,
		dates = dates)	
}


###############################################################################
# Run backtest
#
# Inputs are assumed as if they were computed at point in time (i.e. no lags)
#
# For 'weight' back-test, the default action is to lage weights by one day,
# because weights are derived using all the information avalaible today, 
# so we can only implement these weights tomorrow:
#   portfolio_returns = lag(weights,1) * returns = weights * ( p / lag(p,1) - 1 )
# user can specify a different lag for weights, by changing the do.lag parameter.
#
# For example, for the end of the month strategy: if we open position at the close
# on the 30th, hold position on the 31st and sell it at the close on the 1st. If our
# weights have 0 on the 30th, 1 on the 31st, 1 on the 1st, and 0 on the 2nd, we
# can specify do.lag = 0 to get correct portfolio_returns
#
# Alternatively, if our weights have 0 on the 29th, 1 on the 30st, 1 on the 31st, and 0 on the 1nd, we
# can leave do.lag = 1 to get correct portfolio_returns
#
# For 'share' back-test, the portfolio_returns:
#   portfolio_returns = lag(shares,1) * ( p - lag(p,1) ) / ( lag(shares,1) * lag(p,1) )
# 
###############################################################################
# some operators do not work well on xts
# weight[] = apply(coredata(weight), 2, ifna_prev)
#' @export 
###############################################################################
bt_run <- function
(
	b,					# enviroment with symbols time series
	trade.summary = F, 	# flag to create trade summary
	do.lag = 1, 		# lag signal
	do.CarryLastObservationForwardIfNA = TRUE, 
	type = c('weight', 'share'),
	silent = F,
	capital = 100000,
	commission = 0,
	weight = b$weight,
	dates = 1:nrow(b$prices)	
) 
{
	# convert dates to dates.index
	dates.index = dates2index(b$prices, dates) 
	
	# setup
	type = type[1]

	# create signal
 weight[] = ifna(weight, NA)

	# lag
 if(do.lag > 0)
		weight = mlag(weight, do.lag) # Note k=1 implies a move *forward*  

	# backfill
	if(do.CarryLastObservationForwardIfNA)
		weight[] = apply(coredata(weight), 2, ifna_prev)

	weight[is.na(weight)] = 0

	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1
		trade = ifna(tstart | tend, FALSE)
	
	# prices
	prices = b$prices
	
	# execution.price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price)
		prices1 = coredata(b$prices)
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		prices[] = prices1
	}
		
	# type of backtest
	if( type == 'weight') {
		ret = prices / mlag(prices) - 1
		ret[] = ifna(ret, NA)
		ret[is.na(ret)] = 0			
	} else { # shares, hence provide prices
		ret = prices
	}
	
	#weight = make_xts(weight, b$dates)
	temp = b$weight
		temp[] = weight
	weight = temp
	
	
	# prepare output
	bt = bt_summary(weight, ret, type, b$prices, capital, commission)
		bt$dates.index = dates.index
	bt = bt_run_trim_helper(bt, dates.index)

	if( trade.summary ) bt_trade_summary = bt_trade_summary(b, bt)

	if( !silent ) {
		# print last signal / weight observation
		cat('Latest weights :\n')
		print(round(100*last(bt$weight),2))
		cat('\n')

		cat('Performance summary :\n')
		cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    	cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
		cat('\n')    
	}
	    
	return(bt)
}

# trim bt object, used internally
#' @export 
bt_run_trim_helper = function(bt, dates.index) {
	n.dates = len(dates.index) 
	for(n in ls(bt)) {
		if( !is.null(dim(bt[[n]])) ) {
			if( nrow(bt[[n]]) > n.dates )
				bt[[n]] = bt[[n]][dates.index,,drop=F]
		} else if( len(bt[[n]]) > n.dates )
			bt[[n]] = bt[[n]][dates.index]			
	}
	
	bt$equity = bt$equity / as.double(bt$equity[1])
	bt$best = max(bt$ret)
	bt$worst = min(bt$ret)
	bt$cagr = compute_cagr(bt$equity)
	
	bt
}


###############################################################################
#tic(11)		
#for(j in 1:10)
#	a = as.vector(prices)
#toc(11)		
#
#tic(11)		
#for(j in 1:10)
#	a = coredata(prices)
#toc(11)		
# Interestingly coredata is a lot faster
#
###############################################################################
# Backtest summary
#' @export 
###############################################################################
bt_summary <- function
(
	weight, 	# signal / weights matrix
	ret, 		# returns for type='weight' and prices for type='share'
	type = c('weight', 'share'),
	close.prices,
	capital = 100000,
	commission = 0	# cents / share commission
) 
{
	# cents / share commission
   	#   trade cost = abs(share - mlag(share)) * commission$cps
	# fixed commission per trade to more effectively to penalize for turnover
   	#   trade cost = sign(abs(share - mlag(share))) * commission$fixed
	# percentage commission
	#   trade cost = price * abs(share - mlag(share)) * commission$percentage
	
	# Todo
	# - ability to set different commissions at start/end of the trade
	# - add percentage.fixed, same logic as fixed, but applied to capital
	#   trade cost = sign(abs(share - mlag(share))) * 
	#		price * abs(pmax(share, mlag(share))) * commission$percentage.fixed
	# - ability to set commissions for each asset; hence modeling different tax
	#   rates for stock and equities; these special commissions/taxes should
	#   only be applied at the year end and depend on asset and holding period
	#   only for trades that were completed in the given year => probably a separate function
		
	
	if( !is.list(commission) ) {
		if( type == 'weight') 
			commission = list(cps = 0.0, fixed = 0.0, percentage = commission)	
		else 
			commission = list(cps = commission, fixed = 0.0, percentage = 0.0)	
	}
	
	type = type[1]
    n = nrow(ret)
	     	
    bt = list()
    	bt$weight = weight
    	bt$type = type
    	
	# for commission calculations, un lag the signal
	com.weight = mlag(weight,-1)	
    	
	if( type == 'weight') { 
		temp = ret[,1]
			temp[] = rowSums(ret * weight) - 
				rowSums(abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
				- rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
		bt$ret = temp
    	#bt$ret = make_xts(rowSums(ret * weight) - rowSums(abs(weight - mlag(weight))*commission, na.rm=T), index_xts(ret))    	
    	#bt$ret = make_xts(rowSums(ret * weight), index_xts(ret))    	
    } else {
    	bt$share = weight
    	bt$capital = capital
    	prices = ret
    		
    	# backfill prices
		#prices1 = coredata(prices)
		#prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]				
		#prices[] = prices1
		prices[] = bt_apply_matrix(coredata(prices), ifna_prev)	
		close.prices[] = bt_apply_matrix(coredata(close.prices), ifna_prev)	
		
		# new logic
		#cash = capital - rowSums(bt$share * mlag(prices), na.rm=T)
		cash = capital - rowSums(bt$share * mlag(close.prices), na.rm=T)
		
			# find trade dates
			share.nextday = mlag(bt$share, -1)
			tstart = bt$share != share.nextday & share.nextday != 0
			tend = bt$share != 0 & bt$share != share.nextday
				trade = ifna(tstart | tend, FALSE)
				tstart = trade
			
			index = mlag(apply(tstart, 1, any))
				index = ifna(index, FALSE)
index[1] = T					
							
			totalcash = NA * cash
				totalcash[index] = cash[index]
			totalcash = ifna_prev(totalcash)
				totalcash = ifna(totalcash,0)	# check this

		
		# We can introduce transaction cost to portfolio_returns as
		# abs(bt$share - mlag(bt$share)) * 0.01
		
		portfolio.ret = (totalcash  + rowSums(bt$share * prices, na.rm=T)
							- rowSums(abs(com.weight - mlag(com.weight)) * commission$cps, na.rm=T)
							- rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
							- rowSums(prices * abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
		 				) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1		
				
		#portfolio.ret = (totalcash + rowSums(bt$share * prices, na.rm=T) ) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1				
		
		bt$weight = bt$share * mlag(prices) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) )
		

		
		bt$weight[is.na(bt$weight)] = 0		
		#bt$ret = make_xts(ifna(portfolio.ret,0), index_xts(ret))
		temp = ret[,1]
			temp[] = ifna(portfolio.ret,0)
temp[1] = 0
		bt$ret = temp

    }
    	
    bt$best = max(bt$ret)
    bt$worst = min(bt$ret)
    
	bankrupt = which(bt$ret <= -1)
	if(len(bankrupt) > 0) bt$ret[bankrupt[1]:n] = -1
        
    bt$equity = cumprod(1 + bt$ret)
    bt$cagr = compute_cagr(bt$equity)
    	
    return(bt)    
}

bt_summary_test <- function() {
    #*****************************************************************
    # Load historical data
    #******************************************************************
    load_packages('quantmod')

    data <- new.env()
    getSymbols('EEM', src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
    
    bt_prep(data, align='keep.all', dates='2013:08::2013:08:10')
	    buy.date = '2013:08:05'
		sell.date = '2013:08:06'
    
    #*****************************************************************
    # Code Strategies
    #******************************************************************    
    # set dummy prices
	coredata(data$prices) <-c(10,20,40,60,20,160,60)
	    prices = data$prices
	
	# weight back-test	
	data$weight[] = NA
	    data$weight[buy.date] = 1
	    data$weight[sell.date] = 0
	    commission = list(cps = 0.0, fixed = 0.0, percentage = 1/100)
	model3 = bt_run(data, commission = commission, silent = T)
	
	model3$ret
	#There is 1% drop on 5th due to buying stock, and on the 6th return is 0.49 = 0.5 - 0.01 (commission)

	
	# share back-test
	data$weight[] = NA
	    data$weight[buy.date] = 1
	    data$weight[sell.date] = 0
	    commission = list(cps = 0.0, fixed = 0.0, percentage = 1/100)
	model3 = bt_run_share(data, commission = commission, capital = 100000, silent = T)

	model3$ret
	#There is 1% drop on 5th due to buying stock, and on the 6th return is 
	#0.485 = (2500 * 60 - 2500 * 60 * 0.01) /  (2500 * 40) - 1
	#i.e. return = (share * price + cash - total.commission) / (share * mlag(price) + cash) - 1

}

###############################################################################
# Remove all leading NAs in model equity
#' @export 
###############################################################################
bt_trim <- function
(
	...,
	dates = '::'
) 
{	
	models = variable_number_arguments( ... )

	for( i in 1:len(models) ) {
		bt = models[[i]]
		
		n = len(bt$equity)
		first = which.max(!is.na(bt$equity) & bt$equity != 1)
		if(first > 1 && !is.na(bt$equity[(first-1)]))
			first = first - 1
		if (first < n) {
			index = first:n

			dates.range = range(dates2index(bt$equity[index],dates))
			index = index[dates.range[1]] : index[dates.range[2]]

			bt$dates.index = bt$dates.index[index]
			bt$equity = bt$equity[index]
				bt$equity = bt$equity / as.double(bt$equity[1])
			bt$ret = bt$ret[index]
			bt$weight = bt$weight[index,,drop=F]
			if (!is.null(bt$share)) bt$share = bt$share[index,,drop=F]

		    bt$best = max(bt$ret)
			bt$worst = min(bt$ret)
			bt$cagr = compute_cagr(bt$equity)
		}
		
		models[[i]] = bt
	}
	return (models)
}


bt_trim_test <- function() {
    #*****************************************************************
    # Load historical data
    #******************************************************************
    load_packages('quantmod')
        
    data <- new.env()
    getSymbols(spl('SPY,GLD'), src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
    bt_prep(data, align='keep.all')

    #*****************************************************************
    # Code Strategies
    #******************************************************************
    models = list()
    
    data$weight[] = NA
        data$weight$SPY[] = 1
    models$SPY = bt_run_share(data, clean.signal=F)

    data$weight[] = NA
        data$weight$GLD[] = 1
    models$GLD = bt_run_share(data, clean.signal=F)
    
    
    strategy_performance_snapshoot(bt_trim(models), T)
}


# bt_run - really fast with no bells or whisles
# working directly with xts is alot slower, so use coredata
#' @export 	
bt_run_weight_fast <- function
(
	b,					# enviroment with symbols time series
	do.lag = 1, 		# lag signal
	do.CarryLastObservationForwardIfNA = TRUE
) 
{
    # Signal => weight
    weight = ifna(coredata(b$weight), NA)
    
    # lag
    if(do.lag > 0) weight = mlag(weight, do.lag) # Note k=1 implies a move *forward*  
	
	# backfill
	if(do.CarryLastObservationForwardIfNA) weight[] = apply(coredata(weight), 2, ifna_prev)
    
	weight[is.na(weight)] = 0

	# returns
	prices = coredata(b$prices)
	ret = prices / mlag(prices) - 1
		ret[] = ifna(ret, 0)
	ret = rowSums(ret * weight)
	
	# prepare output
    list(weight = weight, ret = ret, equity = cumprod(1 + ret))
}

###############################################################################
# Portfolio turnover	
# http://wiki.fool.com/Portfolio_turnover
# sales or purchases and dividing it by the average monthly value of the fund's assets
#' @export 
###############################################################################
compute_turnover <- function
(	
	bt,		# backtest object
	b, 		# environment with symbols time series
	exclude.first.trade = T # first trade is the reason for 100% turnover
							# i.e. going from cash to fully invested
) 
{ 
	year.ends =  unique(c(endpoints(bt$weight, 'years'), nrow(bt$weight)))	
		year.ends = year.ends[year.ends>0]	
		nr = len(year.ends)
	period.index = c(1, year.ends)

	# find first investment date
	first = which.max(!is.na(bt$equity) & bt$equity != 1)
	if(first > 1 && !is.na(bt$equity[(first-1)]))
		first = first - 1
	
	
	if( bt$type == 'weight') {    	    	
		portfolio.value = rowSums(abs(bt$weight), na.rm=T)
		portfolio_turnover = rowSums( abs(bt$weight - mlag(bt$weight)), na.rm=T) 
		portfolio_turnover[ rowSums( !is.na(bt$weight) & !is.na(mlag(bt$weight)) ) == 0 ] = NA
	} else {
		prices = mlag(b$prices[bt$dates.index,,drop=F])
		
		if( is.null(bt$cash) ) {
			# logic from bt_summary function				
			cash = bt$capital - rowSums(bt$share * prices, na.rm=T)
		
			# find trade dates
			share.nextday = mlag(bt$share, -1)
			tstart = bt$share != share.nextday & share.nextday != 0
			
			index = mlag(apply(tstart, 1, any))
				index = ifna(index, FALSE)
								
			totalcash = NA * cash
				totalcash[index] = cash[index]
			totalcash = ifna_prev(totalcash)
		} else
			totalcash = bt$cash
		
		portfolio.value = totalcash + rowSums(bt$share * prices, na.rm=T)
		
		portfolio_turnover = rowSums( prices * abs(bt$share - mlag(bt$share)), na.rm=T)
		portfolio_turnover[ rowSums( !is.na(bt$share) & !is.na(mlag(bt$share)) & !is.na(prices) ) == 0 ] = NA
	}
	
	if(exclude.first.trade) portfolio_turnover[first] = 0
	
	portfolio_turnover[1:2] = 0
	temp = NA * period.index			
	for(iyear in 2:len(period.index)) {
		temp[iyear] = sum( portfolio_turnover[ (1+period.index[iyear-1]) : period.index[iyear] ], na.rm=T) / 
						mean( portfolio.value[ (1+period.index[iyear-1]) : period.index[iyear] ], na.rm=T)			
	}
	
	if(exclude.first.trade)
		turnover = mean(temp[period.index > first], na.rm=T)
	else
		turnover = mean(temp[period.index >= first], na.rm=T)
	
	ifna(turnover,0)
}



# debug	
# write_xts(make_xts(bt$cash, index(bt$weight)), 'cash.csv')
# write_xts(make_xts(bt$share, index(bt$weight)), 'share.csv')
# write_xts(prices, 'price.csv')
#
# portfolio.value = make_xts(portfolio.value,index(prices))
# portfolio_turnover = make_xts(portfolio_turnover,index(prices))
# iyear='1998'
# mean(portfolio.value[iyear])
# sum(portfolio_turnover[iyear])
# sum(portfolio_turnover[iyear]) / mean(portfolio.value[iyear])
	


###############################################################################
# Compute Portfolio Maximum Deviation
#' @export 
###############################################################################
compute_max_deviation <- function
(
	bt,
	target.allocation
)
{
	weight = bt$weight[-1,]
	max(abs(weight - repmat(target.allocation, nrow(weight), 1)))
}


###############################################################################
# Backtest Trade summary
#' @export 
###############################################################################
bt_trade_summary <- function
(
	b, 		# enviroment with symbols time series
	bt		# backtest object
)
{    
	if( bt$type == 'weight') weight = bt$weight else weight = bt$share
	
	out = NULL
	
	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1	
		tstart[1, weight[1,] != 0] = T
		n = nrow(weight)
		tend[n, weight[n,] != 0] = T
		#tend[1, ] = F
		trade = ifna(tstart | tend, FALSE)
	
	# prices
	prices = b$prices[bt$dates.index,,drop=F]
	
	# execution price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price[bt$dates.index,,drop=F])
		prices1 = coredata(b$prices[bt$dates.index,,drop=F])
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		
		# backfill pricess
		prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]				
		prices[] = prices1
			
		# get actual weights
		weight = bt$weight
	
		# extract trades
		symbolnames = b$symbolnames
		nsymbols = len(symbolnames) 	
		
			ntrades = max(sum(tstart,na.rm=T), sum(tend,na.rm=T))
		trades = matrix(NA,nr=ntrades,nc=7)
			colnames(trades) = spl('date,symbol,weight,entry.date,exit.date,entry.price,exit.price')
		itrade = 1
		for( i in 1:nsymbols ) {	
			tstarti = which(tstart[,i])
			tendi = which(tend[,i])
			
#cat(colnames(data$prices)[i], len(tstarti), len(tendi), '\n')			
			
			if( len(tstarti) > 0 ) {
				#if( len(tendi) < len(tstarti) ) tendi = c(tendi, nrow(weight))
				if( len(tendi) > len(tstarti) ) tstarti = c(1, tstarti)
				
				ntrade = len(tstarti)
				ntrade.index = itrade:(itrade+ntrade-1)
				trades[ntrade.index,] = 
					cbind((tstarti+1), i, coredata(weight[(tstarti+1), i]), 
						tstarti, tendi, 
						coredata(prices[tstarti, i]), coredata(prices[tendi,i])
					)
				itrade = itrade + ntrade 
			}
		}
		
		
		# prepare output		
		out = list()
		out$stats = cbind(
			bt_trade_summary_helper(trades),
			bt_trade_summary_helper(trades[trades[, 'weight'] >= 0, ]),
			bt_trade_summary_helper(trades[trades[, 'weight'] <0, ])
		)
		colnames(out$stats) = spl('All,Long,Short')
		
		dates = index(weight)
		dates0 = format(dates, '%Y-%m-%d')
		index = order(dates[trades[,'entry.date']])
		
		temp = matrix('',nr=nrow(trades),nc=8)
			colnames(temp)=spl('symbol,weight,entry.date,exit.date,nhold,entry.price,exit.price,return')		
		temp[,'symbol'] = symbolnames[trades[index,'symbol']]
		temp[,'weight'] = round(100*trades[index,'weight'],1)
		temp[,'entry.date'] = dates0[trades[index,'entry.date']]
		temp[,'exit.date'] = dates0[trades[index,'exit.date']]
		temp[,'nhold'] = as.numeric(dates[trades[index,'exit.date']] - dates[trades[index,'entry.date']])
		temp[,'entry.price'] = round(trades[index,'entry.price'], 2)
		temp[,'exit.price'] = round(trades[index,'exit.price'], 2)
		temp[,'return'] = round(100*trades[index,'weight'] * (trades[index,'exit.price']/trades[index,'entry.price'] - 1),2)
				
		out$trades = temp				
	}
	
	return(out)
}



###############################################################################
# Backtest Trade summary
#' @export 
###############################################################################
bt_trade_summary_old <- function
(
	b, 		# enviroment with symbols time series
	bt		# backtest object
)
{    
	if( bt$type == 'weight') weight = bt$weight else weight = bt$share
	
	out = NULL
	
	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1	
		tstart[1, weight[1,] != 0] = T
		n = nrow(weight)
		tend[n, weight[n,] != 0] = T
		#tend[1, ] = F
		trade = ifna(tstart | tend, FALSE)
	
	# prices
	prices = b$prices[bt$dates.index,,drop=F]
	
	# execution price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price[bt$dates.index,,drop=F])
		prices1 = coredata(b$prices[bt$dates.index,,drop=F])
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		
		# backfill pricess
		prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]				
		prices[] = prices1
			
		# get actual weights
		weight = bt$weight
	
		# extract trades
		symbolnames = b$symbolnames
		nsymbols = len(symbolnames) 	

		trades = c()
		for( i in 1:nsymbols ) {	
			tstarti = which(tstart[,i])
			tendi = which(tend[,i])
			
#cat(colnames(data$prices)[i], len(tstarti), len(tendi), '\n')			
			
			if( len(tstarti) > 0 ) {
				#if( len(tendi) < len(tstarti) ) tendi = c(tendi, nrow(weight))
				if( len(tendi) > len(tstarti) ) tstarti = c(1, tstarti)
				
				trades = rbind(trades, 
								cbind(i, weight[(tstarti+1), i], 
								tstarti, tendi, tendi-tstarti,
								coredata(prices[tstarti, i]), coredata(prices[tendi,i])
								)
							)
			}
		}
		colnames(trades) = spl('symbol,weight,entry.date,exit.date,nhold,entry.price,exit.price')

		
		# prepare output		
		out = list()
		out$stats = cbind(
			bt_trade_summary_helper(trades),
			bt_trade_summary_helper(trades[trades[, 'weight'] >= 0, ]),
			bt_trade_summary_helper(trades[trades[, 'weight'] <0, ])
		)
		colnames(out$stats) = spl('All,Long,Short')
		
		temp.x = index_xts(weight)
		
		trades = data.frame(coredata(trades))
			trades$symbol = symbolnames[trades$symbol]
			trades$nhold = as.numeric(temp.x[trades$exit.date] - temp.x[trades$entry.date])
			trades$entry.date = temp.x[trades$entry.date]
			trades$exit.date = temp.x[trades$exit.date]			
			trades$return = round(100*(trades$weight) * (trades$exit.price/trades$entry.price - 1),2)			
			trades$entry.price = round(trades$entry.price, 2)
			trades$exit.price = round(trades$exit.price, 2)			
			trades$weight = round(100*(trades$weight),1)		

		out$trades = as.matrix(trades)				
	}
	
	return(out)
}


bt_trade_summary_test <- function() {
test = list(
	weight1 = matrix(c(0,0,0,1),nc=1),
	weight2 = matrix(c(0,1,0,0),nc=1),
	weight3 = matrix(c(1,1,1,1),nc=1),
	weight4 = matrix(c(1,0,0,0),nc=1),
	weight5 = matrix(c(1,2,0,1,2),nc=1)
)

	for(i in 1:len(test)) {
	
	weight = test[[i]]

	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1
		#tstart[1, weight[1,] != 0] = T
		n = nrow(weight)
		tend[n, weight[n,] != 0] = T
		tend[1, ] = NA
		
	trade = ifna(tstart | tend, FALSE)

	tstarti = which(tstart)
	tendi = which(tend)
			
	if( len(tendi) > len(tstarti) ) tstarti = c(1, tstarti)
			
	cat(len(tstarti), len(tendi), '\n')
	
	#data.frame(weight, weight1, tstart, tend)
	}
	
}


# helper function
# [Why Every Trader Should Know and Understand This Formula](http://www.priceactionlab.com/Blog/2015/02/why-every-trader-should-know-and-understand-this-formula/)
#' @export 
bt_trade_summary_helper <- function(trades) 
{		
	if(nrow(trades) <= 0) return(NA)
	
	out = list()
		tpnl = trades[, 'weight'] * (trades[, 'exit.price'] / trades[,'entry.price'] - 1)
		tlen = trades[, 'exit.date'] - trades[, 'entry.date']
		
	out$ntrades = nrow(trades)
	out$avg.pnl = mean(tpnl)
	out$len = mean(tlen)
		
	out$win.prob = len(which( tpnl > 0 )) / out$ntrades
	out$win.avg.pnl = mean( tpnl[ tpnl > 0 ])
	out$win.len = mean( tlen[ tpnl > 0 ])
		
	out$loss.prob = 1 - out$win.prob
	out$loss.avg.pnl = mean( tpnl[ tpnl < 0 ])
	out$loss.len = mean( tlen[ tpnl < 0 ])
		
	#Van Tharp : Expectancy = (PWin * AvgWin) - (PLoss * AvgLoss)			
	out$expectancy = (out$win.prob * out$win.avg.pnl + out$loss.prob * out$loss.avg.pnl)/100
			
	# Profit Factor is computed as follows: (PWin * AvgWin) / (PLoss * AvgLoss)
	out$profitfactor = -(out$win.prob * out$win.avg.pnl) / (out$loss.prob * out$loss.avg.pnl)			
			
	return(as.matrix(unlist(out)))
}		

###############################################################################
# Change data periodicity in the given bt enviroment
#
# example of mapping to first day of the month
# date.map.fn = function(x) as.Date(format(x, '%Y-%m-1'),'%Y-%m-%d')
#
#' @export 
###############################################################################
bt_change_periodicity <- function
(
	b,			# enviroment with symbols time series
	
	# convert data to given periodicity
	periodicity = 'months',
	period.ends = NULL,
	date.map.fn = NULL 
)
{
	require(xts)
	b1 = env()
	for(n in ls(b))
		if( is.xts( b[[n]] ) ) {
			if(!is.null(periodicity))
				period.ends = endpoints(b[[n]], periodicity)
				
			temp = b[[n]][period.ends,]				
				
			if(!is.null(date.map.fn))
				index(temp) = date.map.fn(index(temp))
				
			colnames(temp) = colnames(b[[n]])
			b1[[n]] = temp
		} else			
			b1[[n]] = b[[n]]		
		
		if(!is.null(b$dates))
			b1$dates = index(b1$prices)		
	b1
}

###############################################################################
# Apply given function to bt enviroment
# for example, to compute 10 month moving average each quater 
# bt_apply_matrix(prices, function(x) mean(last(x,10)), periodicity='months', apply.periodicity='quarters') 
#
# Make sure not to use a rolling window functions if apply.periodicity is given
#
###############################################################################
#' @export 
bt_apply <- function
(
	b,			# enviroment with symbols time series
	xfun=Cl,	# user specified function
	...			# other parameters
)
{
	out = b$weight
	out[] = NA
	
	symbolnames = b$symbolnames
	nsymbols = length(symbolnames) 
	xfun = match.fun(xfun)
	
	for( i in 1:nsymbols ) {	
		msg = try( xfun( coredata(b[[ symbolnames[i] ]]),... ) , silent=TRUE)
		if (class(msg)[1] == 'try-error')
			warning(i, msg, '\n')					
		else
			out[,i] = msg
	}
	return(out)
}

#' @export 
bt_apply_matrix <- function
(
	b,			# matrix
	xfun=Cl,	# user specified function
	...			# other parameters
)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	xfun = match.fun(xfun)
	
	for( i in 1:nsymbols ) {	
		msg = try( xfun( coredata(b[,i]),... ) , silent=TRUE)
		if (class(msg)[1] == 'try-error')
			warning(i, msg, '\n')					
		else
			out[,i] = msg
			
	}
	return(out)
}





# following function can handle different periodicity and apply.periodicity
# make sure not to use a rolling window functions if apply.periodicity is given!!!
#' @export 
bt_apply_ex <- function
(
	b,			# enviroment with symbols time series
	xfun=Cl,	# user specified function
	...,		# other parameters
	
	# convert data to given periodicity before applying xfun
	periodicity = NULL,
	period.ends = NULL,
	
	# apply xfun only on selected periodicity
	apply.periodicity = NULL,
	apply.period.ends = NULL,
	
	fill.gaps = F	# fill gaps introduced by having different periodicity
)
{
	temp = bt_apply_setup_helper(b$weight, xfun, periodicity, period.ends, apply.periodicity, apply.period.ends)
	period.ends = temp$period.ends
	apply.period.ends = temp$apply.period.ends
	map = temp$map

	out = b$weight
	out[] = NA
	
	symbolnames = b$symbolnames
	nsymbols = length(symbolnames) 
	xfun = match.fun(xfun)
	
	# check how many results xfun returns

if(is.null(apply.period.ends)) {		
	if(is.null(period.ends)) 
		for( i in 1:nsymbols ) {	
			msg = try( xfun( coredata(b[[ symbolnames[i] ]]),... ) , silent=TRUE)
			if (class(msg)[1] != 'try-error')
				out[,i] = msg
			else
				warning(i, msg, '\n')		
		}
	else
		for( i in 1:nsymbols ) {	
			msg = try( xfun( coredata(b[[ symbolnames[i] ]][period.ends,]),... ) , silent=TRUE)
			if (class(msg)[1] != 'try-error')
				out[period.ends,i] = msg
			else
				warning(i, msg, '\n')		
		}
} else {
	if(is.null(period.ends)) 
		for( i in 1:nsymbols ) {
			x = coredata(b[[ symbolnames[i] ]])
			for( j in apply.period.ends ) { 
				msg = try( xfun( x[1:j,,drop=F],... ) , silent=TRUE)	
				if (class(msg)[1] != 'try-error')
					out[j,i] = msg
				else
					warning(i, msg, '\n')
			}
		}
	else # i.e. run quaterly on the monthly data
		for( i in 1:nsymbols ) {	
			x = coredata(b[[ symbolnames[i] ]][period.ends,])
			for( j in apply.period.ends ) {
				msg = try( xfun( x[1:map[j]],... ) , silent=TRUE)				
				if (class(msg)[1] != 'try-error')
					out[j,i] = msg
				else
					warning(i, msg, '\n')		
			}
		}
}		
	if(fill.gaps) bt_apply_matrix(out, ifna_prev) else out
}

# make sure not to use a rolling window functions if apply.periodicity is given!!!
#' @export 
bt_apply_matrix_ex <- function
(
	b,			# matrix
	xfun=Cl,	# user specified function
	...,		# other parameters
	
	# convert data to given periodicity before applying xfun
	periodicity = NULL,
	period.ends = NULL,
	
	# apply xfun only on selected periodicity
	apply.periodicity = NULL,
	apply.period.ends = NULL,
	
	fill.gaps = F	# fill gaps introduced by having different periodicity
)
{
	temp = bt_apply_setup_helper(b, xfun, periodicity, period.ends, apply.periodicity, apply.period.ends)
	period.ends = temp$period.ends
	apply.period.ends = temp$apply.period.ends
	map = temp$map
				
	out = b
	out[] = NA
	nsymbols = ncol(b)
	xfun = match.fun(xfun)
	
if(is.null(apply.period.ends)) {
	if(is.null(period.ends)) 
		for( i in 1:nsymbols ) {	
			msg = try( xfun( coredata(b[,i]),... ) , silent=TRUE)
			if (class(msg)[1] != 'try-error')
				out[,i] = msg
			else
				warning(i, msg, '\n')		
		}
	else
		for( i in 1:nsymbols ) {	
			msg = try( xfun( coredata(b[period.ends,i]),... ) , silent=TRUE)
			if (class(msg)[1] != 'try-error')
				out[period.ends,i] = msg
			else
				warning(i, msg, '\n')		
		}
} else {
	if(is.null(period.ends)) 
		for( i in 1:nsymbols ) {
			x = coredata(b[,i])
			for( j in apply.period.ends ) { 
				msg = try( xfun( x[1:j],... ) , silent=TRUE)	
				if (class(msg)[1] != 'try-error')
					out[j,i] = msg
				else
					warning(i, msg, '\n')
			}
		}
	else # i.e. run quaterly on the monthly data
		for( i in 1:nsymbols ) {	
			x = coredata(b[period.ends,i])			
			for( j in apply.period.ends ) {
				msg = try( xfun( x[1:map[j]],... ) , silent=TRUE)				
				if (class(msg)[1] != 'try-error')
					out[j,i] = msg
				else
					warning(i, msg, '\n')		
			}
		}
}
	if(fill.gaps) bt_apply_matrix(out, ifna_prev) else out
}



bt_apply_setup_helper <- function(m, xfun, periodicity, period.ends, apply.periodicity, apply.period.ends) {
	if(!is.null(periodicity) && is.null(period.ends))
		period.ends = endpoints(m, periodicity)
	if(!is.null(apply.periodicity) && is.null(apply.period.ends))
		apply.period.ends = endpoints(m, apply.periodicity)
		
	if(!is.null(apply.period.ends)) 
		apply.period.ends = apply.period.ends[apply.period.ends > 0]
	if(!is.null(period.ends)) 
		period.ends = period.ends[period.ends > 0]

	map = NULL		
	if(!is.null(apply.period.ends) && !is.null(period.ends)) {
		map = array(NA, nrow(m))
			map[period.ends] = 1:len(period.ends)
			map = ifna_prev_map)
			map = ifna(map,1)
	}
	
	list(period.ends = period.ends, apply.period.ends = apply.period.ends, map = map)
}




# following function can handle multiple return arrays. i.e. ATR
# make sure not to use a rolling window functions if apply.periodicity is given!!!
#' @export 
bt_apply_ex2 <- function
(
	b,			# enviroment with symbols time series
	xfun=Cl,	# user specified function
	...,		# other parameters
	
	# convert data to given periodicity before applying xfun
	periodicity = NULL,
	period.ends = NULL,
	
	# apply xfun only on selected periodicity
	apply.periodicity = NULL,
	apply.period.ends = NULL,
	
	fill.gaps = F	# fill gaps introduced by having different periodicity
)
{
	temp = bt_apply_setup_helper(b$weight, xfun, periodicity, period.ends, apply.periodicity, apply.period.ends)
	period.ends = temp$period.ends
	apply.period.ends = temp$apply.period.ends
	map = temp$map

	temp = b$weight
	temp[] = NA
	out = env(out = temp, n=1, name='out')
	index = 1:nrow(temp)
	
	symbolnames = b$symbolnames
	nsymbols = length(symbolnames) 
	xfun = match.fun(xfun)
	
	if(is.null(apply.period.ends)) {	
		for( i in 1:nsymbols )
			if(is.null(period.ends)) 
				set_result_helper(b[[ symbolnames[i] ]], index, xfun, out, i, ...)
			else
				set_result_helper(b[[ symbolnames[i] ]][period.ends,], period.ends, xfun, out, i, ...)
	} else {	
		for( i in 1:nsymbols ) {
			x = coredata(iif(is.null(period.ends), b[[ symbolnames[i] ]], b[[ symbolnames[i] ]][period.ends,]))
			for( j in apply.period.ends )
				if(is.null(period.ends))
					set_result_helper(x[1:j,,drop=F], j, xfun, out, i, ...)
				else # i.e. run quaterly on the monthly data
					set_result_helper(x[1:map[j],,drop=F], j, xfun, out, i, ...)
		}
	}

	bt_apply_fill_gaps_helper(out, fill.gaps)
}



# out = bt_apply2(data, function(x) ATR(HLC(x)))
# out$atr
# make sure not to use a rolling window functions if apply.periodicity is given!!!
#' @export 
bt_apply_matrix_ex2 <- function
(
	b,			# matrix
	xfun=Cl,	# user specified function
	...,		# other parameters
	
	# convert data to given periodicity before applying xfun
	periodicity = NULL,
	period.ends = NULL,
	
	# apply xfun only on selected periodicity
	apply.periodicity = NULL,
	apply.period.ends = NULL,
	
	fill.gaps = F	# fill gaps introduced by having different periodicity
)
{
	temp = bt_apply_setup_helper(b, xfun, periodicity, period.ends, apply.periodicity, apply.period.ends)
	period.ends = temp$period.ends
	apply.period.ends = temp$apply.period.ends
	map = temp$map
				
	temp = b
	temp[] = NA
	out = env(out = temp, n=1, name='out')
	index = 1:nrow(temp)
	
	nsymbols = ncol(b)
	xfun = match.fun(xfun)
	
	if(is.null(apply.period.ends)) {	
		for( i in 1:nsymbols )
			if(is.null(period.ends)) 
				set_result_helper(b[,i], index, xfun, out, i, ...)
			else
				set_result_helper(b[period.ends,i], period.ends, xfun, out, i, ...)
	} else {	
		for( i in 1:nsymbols ) {
			x = coredata(iif(is.null(period.ends), b[,i], b[period.ends,i]))
			for( j in apply.period.ends )
				if(is.null(period.ends)) {
					set_result_helper(x[1:j], j, xfun, out, i, ...)		
				} else # i.e. run quaterly on the monthly data
					set_result_helper(x[1:map[j]], j, xfun, out, i, ...)		
		}
	}
	
	bt_apply_fill_gaps_helper(out, fill.gaps)
}

set_result_helper = function(x, j, xfun, out, i, ...) {
	msg = try( xfun( iif(is.xts(x), coredata(x), x), ... ) , silent=TRUE)
	if (class(msg)[1] == 'try-error')
		warning(i, msg, '\n')			
	else {
		nresult = iif(is.null(dim(msg)), 1, ncol(msg))
		if(nresult != out$n) {				
			temp = out[[ out$name[1] ]]
			rm(list = out$name, envir = out)
			out$name = iif(is.null(dim(msg)), names(msg), colnames(msg))
			out$n = nresult	
			for(result.name in out$name)
				out[[result.name]] = temp							
		}
				
		if(out$n == 1)
			out$out[j,i] = msg
		else
			for(result.name in out$name)
				out[[result.name]][j,i] = iif(len(j) == 1, msg[result.name], msg[,result.name])
	}
}

bt_apply_fill_gaps_helper = function(out, fill.gaps) {
	if(out$n == 1) {
		if(fill.gaps)
    		bt_apply_matrix(out$out, ifna_prev)
		else
			out$out
	} else {
		if(fill.gaps)
			for(result.name in out$name)
				out[[result.name]] = bt_apply_matrix(out[[result.name]], ifna_prev)
		rm(list = c('n','name'), envir = out)
		out
	}		
}


# test for bt_apply functions
bt_apply_test = function() {
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load_packages('quantmod,quadprog,corpcor,lpSolve')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

	data = env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt_prep(data, align='remove.na', dates='1990::') 

	t2 = bt_apply_ex2(data, function(x) ATR(HLC(x)))
	#t2$atr
	#ls(t2)



	prices = data$prices
	
	t01 = bt_apply_matrix(prices, SMA, 100)	
	t02 = bt_apply(data, function(x) SMA(Cl(x),100))
	t11 = bt_apply_matrix_ex(prices, SMA, 100)	
	t12 = bt_apply_ex(data, function(x) SMA(Cl(x),100))
	t21 = bt_apply_matrix_ex2(prices, SMA, 100)	
	t22 = bt_apply_ex2(data, function(x) SMA(Cl(x),100))
	print(all.equal(t01, t02))
	print(all.equal(t01, t11))
	print(all.equal(t01, t12))
	print(all.equal(t01, t21))
	print(all.equal(t01, t22))

	t11 = bt_apply_matrix_ex(prices, SMA, 10, periodicity='months')
	t12 = bt_apply_ex(data, function(x) SMA(Cl(x),10), periodicity='months')
	t21 = bt_apply_matrix_ex2(prices, SMA, 10, periodicity='months')
	t22 = bt_apply_ex2(data, function(x) SMA(Cl(x),10), periodicity='months')
	print(all.equal(t11, t12))
	print(all.equal(t11, t21))
	print(all.equal(t11, t22))


	# Make sure not to use a rolling window functions if apply.periodicity is given
	# bt_apply_matrix(prices, function(x) mean(mlast(x,10)), periodicity='months', apply.periodicity='quarters') 
	t11 = bt_apply_matrix_ex(prices, function(x) mean(mlast(x,100)), apply.periodicity='quarters')
	t12 = bt_apply_ex(data, function(x) mean(mlast(Cl(x),100)), apply.periodicity='quarters')
	t21 = bt_apply_matrix_ex2(prices, function(x) mean(mlast(x,100)), apply.periodicity='quarters')
	t22 = bt_apply_ex2(data, function(x) mean(mlast(Cl(x),100)), apply.periodicity='quarters')
	print(all.equal(t11, t12))
	print(all.equal(t11, t21))
	print(all.equal(t11, t22))


	t11 = bt_apply_matrix_ex(prices, function(x) mean(mlast(x,10)), periodicity='months', apply.periodicity='quarters') 
	t12 = bt_apply_ex(data, function(x) mean(mlast(Cl(x),10)), periodicity='months', apply.periodicity='quarters') 
	t21 = bt_apply_matrix_ex2(prices, function(x) mean(mlast(x,10)), periodicity='months', apply.periodicity='quarters') 
	t22 = bt_apply_ex2(data, function(x) mean(mlast(Cl(x),10)), periodicity='months', apply.periodicity='quarters') 
	print(all.equal(t11, t12))
	print(all.equal(t11, t21))
	print(all.equal(t11, t22))



	load_packages('rbenchmark')

	test01 = function() { t01 = bt_apply_matrix(prices, SMA, 100)	}
	test02 = function() { t02 = bt_apply(data, function(x) SMA(Cl(x),100)) }
	test11 = function() { t11 = bt_apply_matrix_ex(prices, SMA, 100)	}
	test12 = function() { t12 = bt_apply_ex(data, function(x) SMA(Cl(x),100)) }
	test21 = function() { t21 = bt_apply_matrix_ex2(prices, SMA, 100)	 }
	test22 = function() { t22 = bt_apply_ex2(data, function(x) SMA(Cl(x),100)) }

	
  	library(rbenchmark)
	benchmark(
   		test01(),
   		test02(),
   		test11(),
   		test12(),
   		test21(),
   		test22(),
       columns = c("test", "replications", "elapsed", "relative"),
       order = "relative",
       replications = 50
	)
	
}


###############################################################################
# Remove excessive signal
# http://www.amibroker.com/guide/afl/exrem.html
#' @export 
###############################################################################
exrem <- function(x) {        
    temp = c(0, ifna(ifna_prev(x),0))
        itemp = which(temp != mlag(temp))
    x[] = NA
    x[(itemp-1)] = temp[itemp]    
    return(x)
}

exrem_test <- function() {
	exrem(c(NA,1,1,0,1,1,NA,0))
}

#' @export 
bt_exrem <- function(weight)
{
    bt_apply_matrix(weight, exrem)
}


###############################################################################
# Backtest Test function
###############################################################################
bt_test <- function()
{
	load_packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	bt_prep(data, align='keep.all', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 

	prices = data$prices    
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt_run(data)	

	# MA Cross
	sma = bt_apply(data, function(x) { SMA(Cl(x), 200) } )	
	data$weight[] = NA
		data$weight[] = iif(prices >= sma, 1, 0)
	sma.cross = bt_run(data, trade.summary=T)			

	#*****************************************************************
	# Create Report
	#****************************************************************** 
		
					
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt_custom_report_part1( sma.cross, buy.hold)			
dev.off()	


png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt_custom_report_part2( sma.cross, buy.hold)			
dev.off()	
	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt_custom_report_part3( sma.cross, buy.hold)			
dev.off()	




	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt_custom_report(sma.cross, buy.hold, trade.summary=T)
	dev.off()	

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	
	data$weight[] = NA
		data$weight$SPY = 1
	temp = bt_run(data)

	data$weight[] = NA
		data$weight$SPY = 2
	temp = bt_run(data)

	data$weight[] = NA
		data$weight$SPY = 1
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	temp = bt_run(data, type='share', capital=capital)

	data$weight[] = NA
		data$weight$SPY = 2
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	temp = bt_run(data, type='share', capital=capital)
	
}

###############################################################################
# Analytics Functions
###############################################################################
# CAGR - geometric return
#' @export 
###############################################################################
# Note xts::last may be masked if you use dplyr 
compute_cagr <- function(equity, nyears = NA) 
{
	if(is.numeric(nyears))
		as.double( last(equity,1)^(1/nyears) - 1 )
	else 
		as.double( xts::last(equity,1)^(1/compute_nyears(equity)) - 1 )
}

#' @export 
compute_nyears <- function(x) 
{
	as.double(diff(as.Date(range(index_xts(x)))))/365
}

#' @export 
compute_raw_annual_factor = function(x) {
	round( nrow(x) / compute_nyears(x) )
}

# 252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
#' @export 
compute_annual_factor = function(x) {
	possible.values = c(252,52,26,13,12,6,4,3,2,1)
	index = which.min(abs( compute_raw_annual_factor(x) - possible.values ))
	round( possible.values[index] )
}

#' @title Sharpe Ratio
#' @export 
#' @details Key function to calculate sharpe ratio. 
#'  ED: standard way is 
#'  arithmetic monthly / annualzed monthly vol (geom).
compute_sharpe <- function(x) 
{ 
	temp = compute_annual_factor(x)
	x = as.vector(coredata(x))
	return(sqrt(temp) * mean(x)/sd(x) ) 
}

# http://alumnus.caltech.edu/~amir/mdd-risk.pdf
# The Calmar Ratio is equal to the compounded annual growth rate divided by the maximum drawdown.
# The maximum drawdown is typically measured over a three year period.
# Calmar Ratio = CAGR / MAXDD
#' @export 
compute_calmar <- function(x)
{
    compute_cagr(x) / compute_max_drawdown(x)
}

# R2 equals the square of the correlation coefficient
#' @export 
compute_R2 <- function(equity) 
{
	x = as.double(index_xts(equity))
	y = equity
	#summary(lm(y~x))
	return( cor(y,x)^2 )
}

# http://cssanalytics.wordpress.com/2009/10/15/ft-portfolio-with-dynamic-hedging/
# DVR is the Sharpe Ratio times the R-squared of the equity curve
#' @export 
compute_DVR <- function(bt) 
{
	return( compute_sharpe(bt$ret) * compute_R2(bt$equity) )
}

#' @export 
compute_risk <- function(x) 
{ 
	temp = compute_annual_factor(x)
	x = as.vector(coredata(x))
	return( sqrt(temp)*sd(x) ) 
}

#' @export 
compute_drawdown <- function(x) 
{ 
	return(x / cummax(c(1,x))[-1] - 1)
}

#' @export 
compute_max_drawdown <- function(x) 
{ 
	as.double( min(compute_drawdown(x)) )
}

#' @export 
compute_avg_drawdown <- function(x) 
{ 
	drawdown = c( 0, compute_drawdown(coredata(x)), 0 )
	dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
	dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
	drawdowns = apply( cbind(dstart, dend), 1, function(x) min(drawdown[ x[1]:x[2] ], na.rm=T) )
	mean(drawdowns)
}

#' @export 
compute_cdar <- function(x, probs=0.05) 
{ 
	drawdown = c( 0, compute_drawdown(coredata(x)), 0 )
	dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
	dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
	drawdowns = apply( cbind(dstart, dend), 1, function(x) min(drawdown[ x[1]:x[2] ], na.rm=T) )
	if(len(drawdowns)>2)
		mean( drawdowns[ drawdowns < quantile(drawdowns, probs=probs) ] )
	else
		min(drawdowns)
}	

#' @export 
compute_exposure <- function(weight) 
{ 
	sum( apply(weight, 1, function(x) sum(x != 0) ) != 0 ) / nrow(weight) 
}

#' @export 
compute_var <- function(x, probs=0.05) 
{ 
	quantile( coredata(x), probs=probs)
}

#' @export 
compute_cvar <- function(x, probs=0.05) 
{ 
	x = coredata(x)
	mean( x[ x < quantile(x, probs=probs) ] )
}

#' @export 
compute_stats <- function(data, fns, do.na.omit = T) 
{
	out = matrix(double(), len(fns), len(data))
		colnames(out) = names(data)
		rownames(out) = names(fns)
if( do.na.omit )
	for(c in 1:len(data)) {
		for(r in 1:len(fns)) {
			out[r,c] = match.fun(fns[[r]])( fast_na_omit(data[[c]]) )
		}
	}
else
	for(c in 1:len(data)) {
		for(r in 1:len(fns)) {
			out[r,c] = match.fun(fns[[r]])( data[[c]] )
		}
	}

	return(out)
}

###############################################################################
# Example to illustrate a simeple backtest
#' @export 
###############################################################################
bt_simple <- function(data, signal, silent = F) 
{
	# lag singal
	signal = Lag(signal, 1)

	# back fill
    signal = na.locf(signal, na.rm = FALSE)
	signal[is.na(signal)] = 0

	# calculate Close-to-Close returns
	ret = ROC(Cl(data), type='discrete')
	ret[1] = 0
	
	# compute_stats	
    n = nrow(ret)
    bt <- list()
    	bt$ret = ret * signal
    	bt$best = max(bt$ret)
    	bt$worst = min(bt$ret)
    	bt$equity = cumprod(1 + bt$ret)
    	bt$cagr = bt$equity[n] ^ (1/nyears(data)) - 1
    
    # print
    if( !silent) {
	cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
    }
    	    	
	return(bt)
}

bt_simple_test <- function()
{
	load_packages('quantmod')
	
	# load historical prices from Yahoo Finance
	data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

	# Buy & Hold
	signal = rep(1, nrow(data))
    buy.hold = bt_simple(data, signal)
        
	# MA Cross
	sma = SMA(Cl(data),200)
	signal = ifelse(Cl(data) > sma, 1, 0)
    sma.cross = bt_simple(data, signal)
        
	# Create a chart showing the strategies perfromance in 2000:2009
	dates = '2000::2009'
	buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
	sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

	chartSeries(buy.hold.equity, TA=c(addTA(sma.cross.equity, on=1, col='red')),	
	theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )	
}

###############################################################################
#' Remove small weights
#'
#' This function will remove weights that are smaller than given threshold
#'
#' @param weight weight matrix
#' @param long.min.weight minimum weight for long positions, \strong{defaults to 0.1 }
#' @param short.min.weight minimum weight for short positions, \strong{defaults to long.min.weight }
#'
#' @return updated weight matrix
#'
#' @examples
#' \dontrun{ 
#' weight = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
#' weight = matrix(weight, nrow=2, byrow=TRUE)
#' print(bt_apply_min_weight(weight, 0.1))
#' }
#' @author Ivan Popivanov and Michael Kapler
#' @export 
###############################################################################
# Possible use
#   if(!missing(min.weight)) {
#      for(i in names(obj$weights)) {
#         obj$weights[[i]] = apply.min.weight(obj$weights[[i]], min.weight)
#      }    
#   }
###############################################################################
bt_apply_min_weight <- function
(
	weight, 
	long.min.weight = 0.1, 
	short.min.weight = long.min.weight
)
{
	# make sure weight is a matrix
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
    
	# in each row, compute total pos/neg weights
	pos = apply(weight, 1, function(row) sum(row[row > 0]))
	neg = rowSums(weight) - pos
    
	# setup
	pos.mat = iif(weight >= long.min.weight, weight, 0)
	neg.mat = iif(weight <= -short.min.weight, weight, 0)
    
	# re-scale
	pos.mat = pos.mat * ifna(pos / rowSums(pos.mat), 1)
	neg.mat = neg.mat * ifna(neg / rowSums(neg.mat), 1)
    
	return(pos.mat + neg.mat)
} 

test_bt_apply_min_weight <- function()
{
	data = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
	mm = matrix(data=data, nrow=2, byrow=TRUE)
	print(bt_apply_min_weight(mm, 0.1))
	print(bt_apply_min_weight(mm, 0.2))
   
	data = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
	mm = matrix(data=data, nrow=1, byrow=TRUE)
	print(bt_apply_min_weight(mm, 0.1))
	print(bt_apply_min_weight(mm, 0.2))
   
	data = c(0.1, 0.6, 0.2, 0.1, 0, -0.2, -0.5, -0.3, -0.1, 0)
	mm = matrix(data=data, nrow=1, byrow=TRUE)
	print(bt_apply_min_weight(mm, 0.1))
	print(bt_apply_min_weight(mm, 0.2))
}

###############################################################################
#' Round weights
#' 
#' Similar idea to bt_apply_min_weight
#' 
#' @export 
###############################################################################
bt_apply_round_weight <- function
(
	weight, 
	long.round.weight = 5/100, 
	short.round.weight = long.round.weight
)
{
	# make sure weight is a matrix
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
    
	# in each row, compute total pos/neg weights
	pos = apply(weight, 1, function(row) sum(row[row > 0]))
	neg = rowSums(weight) - pos
    
	# setup
	pos.mat = iif(weight >= 0, round(weight / long.round.weight) * long.round.weight, 0)
	neg.mat = iif(weight <= 0, round(weight / short.round.weight) * short.round.weight, 0)
    
	# re-scale
	pos.mat = pos.mat * ifna(pos / rowSums(pos.mat), 1)
	neg.mat = neg.mat * ifna(neg / rowSums(neg.mat), 1)
    
	return(pos.mat + neg.mat)
} 

###############################################################################
#' Print starting dates for time series
#' 
#' @export 
###############################################################################
bt_start_dates <- function
(
	b 					# enviroment with symbols time series
) 
{
	temp = lapply(b, function(x) index(x[1]) )
		temp$dates = NULL
		temp$prices = NULL
		temp$weight = NULL
		temp$execution.price = NULL
		temp$symbolnames = NULL
	temp = temp[order( sapply(temp, function(x) x) )]
	
	out = t(t( sapply(temp, function(x) as.character(x)) ))
    colnames(out) = 'Start'
  out
}

#' @export 
bt_end_dates <- function
(
	b 					# enviroment with symbols time series
) 
{
	temp = lapply(b, function(x) index(last(x)) )
		temp$dates = NULL
		temp$prices = NULL
		temp$weight = NULL
		temp$execution.price = NULL
		temp$symbolnames = NULL
	temp = temp[order( sapply(temp, function(x) x) )]
	
	out = t(t( sapply(temp, function(x) as.character(x)) ))
    colnames(out) = 'Start'
  out
}
	

###############################################################################
#' Append today's quotes
#' 
#' data.today = getQuote_yahoo_today(ls(data))
#' 	print(data.today)
#' bt_append_today(data, data.today)
#' 
#' @export 
###############################################################################
bt_append_today <- function(b, data.today) {
	date.column = find_names('Date',data.today)
	valid.index = which(!is.na(data.today[,date.column,with=F]))	
	data.today = data.today[valid.index]
	
	data = make_stock_xts(read_xts(data.today, date.column=date.column,format='%m/%d/%Y', decreasing=NULL))	
		tickers = data.today$Symbol
		Yesterday = data.today$Yesterday	
	
	# todo, better logic for merging Intraday and EOD data
	for(i in 1:len(tickers)) {
		if(is.null(b[[ tickers[i] ]])) next
		if( last(index(data[i,])) > last(index(b[[ tickers[i] ]])) )
			b[[ tickers[i] ]] = rbind(data[i,], b[[ tickers[i] ]])
		#b[[ tickers[i] ]] = extend_data(env[[ s ]], data[[ gsub('\\^', '', map[[ s ]][i]) ]], scale=T)
	}
}
