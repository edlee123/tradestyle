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
# Stop functionality for Backtests
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Timed Exit: exit trade after nlen bars
#' @export 
###############################################################################
bt_exrem_time_exit <- function(signal, nlen, create.weight = T) {
	signal[is.na(signal)] = FALSE
	
	signal.index = which(signal)
		nsignal.index = len(signal.index)
		nperiods = len(signal)	
		signal.index.exit = iif(signal.index + nlen - 1 > nperiods, nperiods, signal.index + nlen)
				
	if(!create.weight) {
		for(i in 1:nsignal.index) {
			if( signal[ signal.index[i] ] ) {
				signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
			}
		}	
		return(signal)
	} else {
		temp = signal * NA

		for(i in 1:nsignal.index) {
			if( signal[ signal.index[i] ] ) {
				signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
				temp[ signal.index.exit[i] ] = 0
			}
		}	
				
		temp[signal] = 1
		return(temp)
	}
}



###############################################################################
# Enforce minimum holding period before taking another signal
#' @export 
###############################################################################
bt_min_holding_period <- function(x, nlen) {
	x = coredata(x)
	
	enter = x != 0
    enter[is.na(enter)] = FALSE
    enter.index = which(enter)
    
    for(t in enter.index)
	    if( enter[ t ] ) {
			index = t + nlen
			enter[ t : index ] = FALSE
		    x[ t : index ] = x[t]
		}
	return(x)
}


###############################################################################
# Matrix versions of time/price/time.price stops
#' @export 
###############################################################################
bt_time_stop <- function(weight, nlen)
{
	# same as bt_exrem_time_exit function!!!
    bt_apply_matrix(weight, bt_ts_time_stop, nlen)
}


# based on bt_apply_matrix
#' @export 
bt_price_stop <- function(b, price, pstop)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	
	if(is.null(dim(pstop))) pstop = rep_row(pstop, nrow(b))
	
	for( i in 1:nsymbols )
		out[,i] = bt_ts_price_stop(coredata(b[,i]), coredata(price[,i]), coredata(pstop[,i]))
	return(out)
}


# based on bt_apply_matrix
#' @export 
bt_time_price_stop <- function(b, nlen, price, pstop)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	
	if(is.null(dim(pstop))) pstop = rep_row(pstop, nrow(b))
	
	for( i in 1:nsymbols )
		out[,i] = bt_ts_time_price_stop(coredata(b[,i]), nlen, coredata(price[,i]), coredata(pstop[,i]))
	return(out)
}


###############################################################################
# enter signal: weight != 0
# exit signal : weight == 0 or 1 to -1 flip signal
# no signal   : is.na(weight)
#' @export 
###############################################################################
bt_ts_trade_index <- function(x)
{
    # index of enter signals
    enter = x != 0
    enter[is.na(enter)] = FALSE
# do not consider trade that happend today    
enter[length(x)] = FALSE
    
    enter.index = which(enter)
    

    # index of exit signals corresponding to enter signals
    # capute both x == 0 and 1 to -1 flip signal
    temp = ifna_prev(x)
		temp0 = mlag(temp)    	
    exit = temp0 != 0 & temp != temp0
    exit[ !exit ] = NA
    exit = ifna_prevx_rev(exit)
    
    list(enter = enter, enter.index = enter.index, exit = exit)
}


###############################################################################
# enter signal: weight != 0
# true/false vector, true indicating start of trade
#' @export 
###############################################################################
bt_ts_enter_state <- function(x)
{
    # enter signals
    enter = x != 0
    	enter[is.na(enter)] = FALSE
    enter
}

###############################################################################
# Exit position if holding periods > nlen
#' @export 
###############################################################################
bt_ts_time_stop <- function(x, nlen)
{
	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] )
		if( exit[ t ] < t + nlen )
			enter[ t : exit[ t ] ] = FALSE
		else {
			enter[ t : (t + nlen) ] = FALSE
		    x[ (t + nlen) ] = 0
		}		    
    return(x)
}


time_stop_test <- function() {
	bt_ts_time_stop(c(1,1,1,0,1,1,NA,1,0),2)
	bt_ts_time_stop(c(1,0,1,1,1,1,1,1,1),3)
}


###############################################################################
# Exit long  position if price.today < price.enter - stop
# Exit short position if price.today > price.enter + stop
#
# price SHOULD NOT contain any non-leading NA's!!!
#
#' @export 
###############################################################################
bt_ts_price_stop <- function(x, price, pstop)
{
	price = coredata(price)
	pstop = coredata(pstop)

	if(length(pstop) == 1) pstop = rep(pstop, len(x))

	# faster which
	dummy = 1:length(x)

	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit

 
	# loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		if( x[ t ] > 0 )
			temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
		else
			temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			enter[ t : iexit ] = FALSE
		    x[ iexit ] = 0				
		} else
			enter[ t : exit[ t ] ] = FALSE			
	}
    return(x)
}


price_stop_test <- function() {
	bt_ts_price_stop(c(1,1,1,1,1,1,NA,1,0), 
				c(1,1,0.9,0.7,1,1,1,1,0),
				0.2
				)
				
	bt_ts_price_stop(-c(1,1,1,1,1,1,NA,1,0), 
				c(1,1,0.9,1.7,1,1,1,1,0),
				0.2
				)
	
}


###############################################################################
# Exit position if either time stop or price stop
#
# price SHOULD NOT contain any non-leading NA's!!!
#
#' @export 
###############################################################################
bt_ts_time_price_stop <- function(x, nlen, price, pstop)
{
	price = coredata(price)
	pstop = coredata(pstop)

	if(length(pstop) == 1) pstop = rep(pstop, len(x))

	# faster which
	dummy = 1:length(x)
	
	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit
	

	# loop over all enter signals and apply time and price stop    
    for(t in enter.index)
	if( enter[ t ] ) {
		if( x[ t ] > 0 )
			temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
		else
			temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			
			if( iexit < t + nlen ) {
				enter[ t : iexit ] = FALSE
			    x[ iexit ] = 0				
			} else {
				enter[ t : (t + nlen) ] = FALSE
			    x[ (t + nlen) ] = 0
			}			
		} else
			if( exit[ t ] < t + nlen )
				enter[ t : exit[ t ] ] = FALSE
			else {
				enter[ t : (t + nlen) ] = FALSE
			    x[ (t + nlen) ] = 0
			}
	}
    return(x)
}

time_price_stop_test <- function() {
	bt_ts_time_price_stop(c(1,1,1,1,1,1,NA,1,0), 
				4,
				c(1,1,0.9,0.7,1,1,1,1,0),
				0.2
				)
				
	bt_ts_time_price_stop(-c(1,1,1,1,1,1,NA,1,0), 
				4,
				c(1,1,0.9,1.7,1,1,1,1,0),
				0.2
				)
	
}







###############################################################################
# Price stop with user defined fn
###############################################################################
# stop.fn is expected to return a boolean array of size tend - tstart + 1
# with TRUE(s) idicating that stop was activated
#
# price SHOULD NOT contain any non-leading NA's!!!
#
# time comparison with xts is usually tricky; hence it is always wise to 
# provide all inputs in after coredata
#
#' @export 
custom_stop_fn <- function(x, price, stop.fn, ...)
{
	price = coredata(price)

	if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)

	# faster which
	dummy = 1:length(x)
		
	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		# temp = stop.fn(x[ t ], price, t, exit[ t ], ...)
		temp = stop.fn(x[ t ], price, t, exit[ (t + 1) ], ...)
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			enter[ t : iexit ] = FALSE
		    x[ iexit ] = 0				
		} else
			enter[ t : exit[ t ] ] = FALSE			
	}
    return(x)
}
	
# expect list(state - T/F vector, remove - T/F - if remove signals, value - value to set)
#
# in terms of custom_stop_fn, list(state = state, remove = T, value = 0 i.e. exit)
#' @export 
custom_stop_fn_list <- function(x, price, stop.fn, ...)
{
	price = coredata(price)

	if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)

	# faster which
	dummy = 1:length(x)
		
	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		# temp = stop.fn(x[ t ], price, t, exit[ t ], ...)
		out = stop.fn(x[ t ], price, t, exit[ (t + 1) ], ...)
		temp = out$state
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			if(out$clean.signal) enter[ t : iexit ] = FALSE
		    x[ iexit ] = out$value				
		} else
			enter[ t : exit[ t ] ] = FALSE			
	}
    return(x)
}




custom_stop_fn_full <- function(x, price, stop.fn, ...)
{
	price = coredata(price)

	if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)

	# get index of trades
	temp = bt_ts_trade_index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		# temp = stop.fn(x[ t ], price, t, exit[ t ], ...)
		out = stop.fn(x, price, t, exit[ (t + 1) ], ...)
			x = out$x
			if(out$clean.signal) enter[ t : out$tlast ] = FALSE				
	}
    return(x)
}


	





# note that this is a custom function because HHV (i.e. cummax) and is path dependent
custom_trailing_stop_test <- function(weight, price, tstart, tend, sma, nstop) {
	index = tstart : tend
	if(weight > 0) {
		# trailing stop
		temp = price[ index ] < cummax(0.9 * sma[ index ])
		
		# profit target
		temp = temp | price[ index ] > cummax(1.1 * sma[ index ])				
	} else {
		temp = price[ index ] > cummax(1.1 * sma[ index ])
	}
	
	# time stop
	if( tend - tstart > nstop ) temp[ (nstop + 1) ] = T
	
	return( temp )	
}


###############################################################################
# Tests
###############################################################################
custom_stop_fn_test <- function() {
	signal = c(1,1,1,1,1,1,NA,1,0)
	price = c(1,1,0.9,0.7,1,1,1,1,0)
	custom_stop_fn(signal, price,
				custom_trailing_stop_test,
				sma = ifna(SMA(price, 2), price),
				nstop = 20
				)
				

	signal = -c(1,1,1,1,1,1,NA,1,0)
	price = c(1,1,0.9,1.7,1,1,1,1,0)
	custom_stop_fn(signal, price,
				custom_trailing_stop_test,
				sma = ifna(SMA(price, 2), price),
				nstop = 4
				)
	
}


