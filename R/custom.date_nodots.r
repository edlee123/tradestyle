###############################################################################
# Common functions used internally
###############################################################################
len = function(x) length(x)
mlast = function(m, n=1) m[(nrow(m)-n+1), ,drop=F]
spl = function(s, delim = ',') unlist(strsplit(s,delim))
rep_row = function(m, nr) matrix(m, nr=nr, nc=len(m), byrow=T)
trim = function(s) {
  s = sub(pattern = '^\\s+', replacement = '', x = s)
  sub(pattern = '\\s+$', replacement = '', x = s)
}
index_xts = function(x) {
  temp = attr(x, 'index')
  class(temp) = c('POSIXct', 'POSIXt')  
  type = attr(x, '.indexCLASS')[1]
  if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
    temp = as.Date(temp)
  return(temp)
}

###############################################################################
#' Custom Date function with Business Date logic
#'
#' @param expr expression that describes the dates to be extracted
#' @param dates dates
#' @param calendar RQuantLib calendar name to use to determine business_days, \strong{defaults to NULL}
#'
#' @return list with days.since and days.till arrays
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' custom_date_bus('last day in Apr', dates, 'UnitedStates/NYSE')
#' custom_date_bus('last day in Apr', dates, 'Canada/TSX')
#' }
#' @export 
###############################################################################
custom_date_bus = function(expr, dates, calendar = NULL) {
  apply_business_days(dates, function(x) custom_date(expr, x), calendar)
}


###############################################################################
#' Custom Date function
#'
#' @param expr expression that describes the dates to be extracted
#' @param dates dates
#'
#' @return list with days.since and days.till arrays
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' custom_date('last day in Apr', dates)
#' }
#' @export 
###############################################################################
custom_date = function(expr, dates) {
  if( xts::is.xts(dates) ) dates = index(dates)
  dates = as.Date(dates)
  
  # split tokens with *in*, *every*, *of*, *of every*, *in every*
  expr = gsub('the ', '', tolower(expr))
  expr = gsub(' in every ', ' every ', expr)
  expr = gsub(' of every ', ' every ', expr)	
  tokens = trim(spl(spl(spl(expr, ' in '), ' every '), ' of '))
  
  stack = list(
    splits = date_all(dates), 
    dates.index = 1:len(dates)
  )
  selected = rep_row(c(1,len(dates)), len(dates))
  selected.n = 1
  
  
  # process in reverse order
  for(token in rev(tokens[nchar(tokens) > 0])) {
    selected0 = selected[1:selected.n, , drop=F]
    selected.n = 0
    for(i in 1:nrow(selected0)) {
      temp = custom_date_token(token, dates, stack, selected0[i,])
      selected[(selected.n+1):(selected.n+nrow(temp)),] = temp
      selected.n = selected.n + nrow(temp)
    }
  }
  selected[1:selected.n,1]
}


###############################################################################
# Helper functions to support custom_date logic
#
# 1,2,3,4,5
# 5 = last day
# 4 = last but 1
# 	1st from/to last
# 	1 before last 
# 3 = last but 2
# 	2nd from/to last
# 	2 before last 
#
# Q | 1st Q | Apr | last day | 3rd Mon | 2nd from/to/before last Mon
###############################################################################
custom_date_token = function(expr, dates, stack, selected) {
  # split into tokens and remove empty ones
  tokens = trim(spl(tolower(expr), ' '))
  tokens = tokens[nchar(tokens) > 0]
  n.tokens = len(tokens)
  
  # last token is freq
  freq = custom_date_map(tokens[n.tokens])
  periods = date_periodicity_map(freq$freq)
  
  if( is.null(periods) )
    warning('unknown freq', freq$freq)
  
  if( periods == 'days' ) {
    temp = cbind(selected[1]:selected[2], selected[1]:selected[2])
    rownames(temp) =  stack$splits$dayofweek[selected[1]:selected[2]]
  } else
    temp = custom_date_extract(selected[1], selected[2], periods, stack) 
  
  if( !is.null(freq$pick) )
    temp = temp[rownames(temp) == freq$pick,,drop=F]
  
  # done
  if( n.tokens == 1 ) return(temp)
  
  # 1/2/3st | first | last | 1/2/3nd from/to/before last
  if( n.tokens == 2 ) {
    if( tokens[1] == 'last' )
      return(mlast(temp))
    if( tokens[1] == 'first' )
      return(temp[1,,drop=F])
  }
  
  # parse_number
  #load_packages('stringr')
  offset = stringr::str_match(tokens[1], '^[0-9]+')[1]
  if( is.na(offset) ) warning('unknown offset', tokens[1])
  offset = as.numeric(offset)
  
  if( offset > nrow(temp) ) {
    if( n.tokens == 2 )
      mlast(temp)			
    else
      temp[1,,drop=F]
  } else {
    if( n.tokens == 2 )		
      temp[offset,,drop=F]
    else
      temp[nrow(temp)-offset,,drop=F]
  }
}


month_map_abbr = 1:12
names(month_map_abbr) = spl('jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec')
month_map = 1:12
names(month_map) = spl('january,february,march,april,may,june,july,august,september,october,november,december')


day_map_abbr = 0:6
names(day_map_abbr) = spl('sun,mon,tue,wed,thu,fri,sat')
day_map = 0:6
names(day_map) = spl('sunday,monday,tuesday,wednesday,thursday,friday,saturday')


custom_date_map = function(token) {
  if( !is.na(month_map_abbr[token]) )
    return(list(freq='months', pick = month_map_abbr[token]))
  if( !is.na(month_map[token]) )
    return(list(freq='months', pick = month_map[token]))
  
  if( !is.na(day_map_abbr[token]) )
    return(list(freq='day', pick = day_map_abbr[token]))
  if( !is.na(day_map[token]) )
    return(list(freq='day', pick = day_map[token]))
  
  #load_packages('stringr')
  # Q1	
  match = stringr::str_match(token, '^q[1-4]{1}')[1]
  if( !is.na(match) ) return(list(freq='quarter', pick = substring(match,2,2) ))
  
  match = stringr::str_match(token, '^m[0-9]+')[1]
  if( !is.na(match) ) return(list(freq='month', pick = substring(match,2,3) ))
  
  match = stringr::str_match(token, '^s[1-2]{1}')[1]
  if( !is.na(match) ) return(list(freq='semiannual', pick = substring(match,2,2) ))	
  
  list(freq=token)				
}


custom_date_extract = function(i0, i1, freq, stack) {
  label = stack$splits[[freq]][i0 : i1]
  label.n = len(label)
  
  temp = unique(c( 0, stack$dates.index[1:label.n][diff( label ) != 0], label.n ))
  temp.n = len(temp)
  
  temp = cbind(1 + temp[1:(temp.n - 1)], temp[2:temp.n])
  rownames(temp) =  label[temp[,1]]
  (i0 - 1) + temp
}


###############################################################################
# Tests for custom_date logic
###############################################################################
custom_date_test = function () {  
  
  dates = seq(Sys.Date()-1000, Sys.Date(), 1)
  dates[custom_date('last day in Apr', dates)]
  dates[custom_date('first day in Apr', dates)]
  dates[custom_date('last day in first week in Apr', dates)]
  
  dates[custom_date('last Mon in Apr', dates)]
  dates[custom_date('last Fri in Apr', dates)]
  
  
  dates[custom_date('first day in Apr', dates)]
  dates[custom_date('1st day in Apr', dates)]
  
  dates[custom_date('10th day in Apr', dates)]
  dates[custom_date('50th day in Apr', dates)]
  
  dates[custom_date('10th to last day in Apr', dates)]
  
  dates[custom_date('3rd Mon in Q', dates)]
  dates[custom_date('3rd Mon in 1st Q', dates)]
  dates[custom_date('3rd Mon in Q1', dates)]
  dates[custom_date('3rd Mon in last M in Q1', dates)]
  
  # Options Expiration is third Friday of the expiration month
  # the expiration months are the first month of each quarter - January, April, July, October. 
  format(dates[custom_date('3rd Fri in Q', dates)], '%Y %b %d %w')
  
  
  
  dates = seq(as.Date('1-jan-2010','%d-%b-%Y'), as.Date('29-apr-2015','%d-%b-%Y'), 1)
  dates[custom_date('last day in Apr', dates)]
  
  dates = seq(as.Date('1-jan-2010','%d-%b-%Y'), as.Date('30-apr-2015','%d-%b-%Y'), 1)
  dates[custom_date('last day in Apr', dates)]
  
  dates = seq(as.Date('1-jan-2010','%d-%b-%Y'), as.Date('29-apr-2015','%d-%b-%Y'), 1)
  dates[custom_date_bus('last day in Apr', dates)]
  
  
  
  # Book on creating packages
  # http://r-pkgs.had.co.nz/tests.html
  # http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/
  #load_packages('devtools_testthat')
  
  dates = seq(as.Date('1-jan-2010','%d-%b-%Y'), as.Date('20-oct-2015','%d-%b-%Y'), 1)
  dates[custom_date('last day in Apr', dates)]
  
  expect_identical(
    dates[custom_date('last day in Apr', dates)],	
    as.Date(
      c("2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", "2014-04-30", "2015-04-30")
      ,'%Y-%m-%d')
  )
  
}


custom_date_debug = function () {
  
  dates[custom_date('last day in first week in Apr', dates)]
  # Execution plan
  # 1. split monthly and pick Apr
  # 2. split weekly and pick first
  # 2. split daily and find last day
  
  
  dates = seq(Sys.Date()-1000, Sys.Date(), 1)
  custom_date('last day in Apr', dates)
  custom_date('3rd Mon in 1st Q', dates)
  custom_date('Mon in 3rd W in 1st Q', dates)
  
  
  dates = seq(Sys.Date()-1000, Sys.Date(), 1)
  stack = env(splits, dates.index = 1:len(dates))
  
  i0 = 1
  i1 = len(dates)
  freq = 'month'
  
  temp = custom_date_extract(i0, i1, freq, stack) 
  temp = temp[rownames(temp) == month_map_abbr['apr'],]
  dates[temp[,1]] 
  dates[temp[,2]]
  
  i0 = temp[1,1]
  i1 = temp[1,2]
  freq = 'week'
  
  temp1 = custom_date_extract(i0, i1, freq, stack) 
  temp1 = temp1[1,,drop=F]
  dates[temp1[,1]] 
  dates[temp1[,2]]
  
  
  # debug helper
  #gall <<- environment() 
  #list2vars(gall)
  #if(index == 1511)
  #matrix(1,1,1)[1,20]
  
}	
