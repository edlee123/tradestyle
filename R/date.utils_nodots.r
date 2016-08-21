

###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{ 
#' date_dayofweek(Sys.Date())
#' }
#' @export 
#' @rdname DateFunctions
###############################################################################
date_dayofweek <- function(dates) { 
  as.POSIXlt(dates)$wday
}


#' @export 
#' @rdname DateFunctions
date_day <- function(dates) { 
  as.POSIXlt(dates)$mday
}


# wday 0-6 day of the week, starting on Sunday.
# %U Week of the year as decimal number (00-53) using Sunday as the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1). The US convention.
#' @export 
#' @rdname DateFunctions
date_week <- function(dates) { 
  dates = as.POSIXlt(dates)
  offset = (7 + dates$wday - dates$yday %% 7) %%7
  (dates$yday +  offset)%/% 7
}


#' @export 
#' @rdname DateFunctions
date_month <- function(dates) { 
  as.POSIXlt(dates)$mon + 1
}


quarter_map = c(1,1,1,2,2,2,3,3,3,4,4,4)

#' @export 
#' @rdname DateFunctions
date_quarter <- function(dates) { 	
  quarter_map[date_month(dates)] 
}


semiannual_map = c(1,1,1,1,1,1,2,2,2,2,2,2)

#' @export 
#' @rdname DateFunctions
date_semiannual = function (dates) {
  semiannual_map[date_month(dates)] 
}


#' @export 
#' @rdname DateFunctions
date_year = function (dates) {
  as.POSIXlt(dates)$year + 1900
}


#' @export 
#' @rdname DateFunctions
date_all = function(dates) 
{
  dates = as.POSIXlt(dates)
  offset = (7 + dates$wday - dates$yday %% 7) %%7
  
  list(
    dayofweek = dates$wday,
    mday = dates$mday,
    yday = dates$yday,
    weeks = (dates$yday +  offset)%/% 7,
    months = dates$mon + 1,		
    quarters = quarter_map[dates$mon + 1],
    semiannual = semiannual_map[dates$mon + 1],
    years = dates$year + 1900	
  )
}


# Test above functionality
date_period_test = function() {
  # test date functions
  # ?DateTimeClasses
  # ?strptime 
  date_dayofweek0 <- function(dates) { 
    return(as.double(format(dates, '%w')))
  }
  date_day0 <- function(dates) { 
    return(as.double(format(dates, '%d')))
  }
  date_week0 <- function(dates) { 
    return(as.double(format(dates, '%U')))
  }
  date_month0 <- function(dates) { 
    return(as.double(format(dates, '%m')))
  }
  # (((1:12)-1) %/% 3)+1  
  date_quarter0 <- function(dates) { 	
    (((date_month(dates))-1) %/% 3)+1
  }
  date_year0 = function (dates) {
    return(as.double(format(dates, '%Y')))
  }
  
  
  #------------------------------------------
  
  dates1 = seq(Sys.Date()-100000, Sys.Date(), 1)
  all.equal(diff(date_week0(dates1))!=0 , diff(date_week(dates1))!=0 )
  
  dates = seq(Sys.Date()-10000, Sys.Date(), 1)
  
  library(rbenchmark)
  benchmark(
    test1 = diff(date_week0(dates))!=0, 
    test2 = diff(date_week(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )
  
  #------------------------------------------
  
  all.equal(diff(date_dayofweek0(dates1))!=0 , diff(date_dayofweek(dates1))!=0 )
  
  benchmark(
    test1 = diff(date_dayofweek0(dates))!=0, 
    test2 = diff(date_dayofweek(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )
  
  #------------------------------------------
  
  all.equal(diff(date_day0(dates1))!=0 , diff(date_day(dates1))!=0 )
  
  benchmark(
    test1 = diff(date_day0(dates))!=0, 
    test2 = diff(date_day(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )   	
  
  #------------------------------------------
  
  all.equal(diff(date_month0(dates1))!=0 , diff(date_month(dates1))!=0 )
  
  benchmark(
    test1 = diff(date_month0(dates))!=0, 
    test2 = diff(date_month(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )   	
  
  #------------------------------------------
  
  all.equal(diff(date_quarter0(dates1))!=0 , diff(date_quarter(dates1))!=0 )
  
  benchmark(
    test1 = diff(date_quarter0(dates))!=0, 
    test2 = diff(date_quarter(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )   	
  
  #------------------------------------------
  
  all.equal(diff(date_year0(dates1))!=0 , diff(date_year(dates1))!=0 )
  
  benchmark(
    test1 = diff(date_year0(dates))!=0, 
    test2 = diff(date_year(dates))!=0, 
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 200
  )   	   	  	
}


###############################################################################
#' Dates Index Functions
#'
#' @param dates collection of dates
#' @param last.date flag to include last date, \strong{defaults to TRUE}
#'
#' @return location of the week/month/year ends
#'
#' @examples
#' \dontrun{ 
#' date_week_ends(seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
#' @rdname DateFunctionsIndex
###############################################################################
date_week_ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 100*date_year(dates) + date_week(dates) ) != 0)
  ends_add_last_date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date_month_ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 100*date_year(dates) + date_month(dates) ) != 0)
  ends_add_last_date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date_quarter_ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 10*date_year(dates) + date_quarter(dates) ) != 0)
  ends_add_last_date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date_semiannual_ends = function(dates, last.date=T) 
{ 
  ends = which(diff( 10*date_year(dates) + date_semiannual(dates) ) != 0)
  ends_add_last_date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date_year_ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( date_year(dates) ) != 0)
  ends_add_last_date(ends, len(dates), last.date)
}


# helper function to add last date
ends_add_last_date = function(ends, last.date, action=T) 
{
  if(action)
    unique(c(ends, last.date))
  else
    ends
}


###############################################################################
#' Dates Mapping Functions
#'
#' @param periodicity periodicity (i.e. weeks, months)
#'
#' @return standard periodicity
#'
#' @examples
#' \dontrun{ 
#' date.freq.map('week')
#' }
#' @export 
#' @rdname DateFunctionsMap
###############################################################################
date_periodicity_map = function(periodicity) {
  switch(periodicity,
         days = 'days',
         day = 'days',
         daily = 'days',
         d = 'days',
         
         weeks = 'weeks',
         week = 'weeks',
         weekly = 'weeks',
         w = 'weeks',
         
         months = 'months',
         month = 'months',
         monthly = 'months',
         m = 'months',
         
         quarters = 'quarters',
         quarter = 'quarters',
         quarterly = 'quarters',
         q = 'quarters',
         
         semiannual = 'semiannual',
         semiannually = 'semiannual',
         s = 'semiannual',
         
         years = 'years',
         year = 'years',
         yearly = 'years',
         annual = 'years',
         annually = 'years',
         y = 'years',
         
         # default
         NULL)  
}

#' @export 
#' @rdname DateFunctionsMap
date_ends_fn = function(periodicity) {
  switch(date_periodicity_map(periodicity),
         weeks = date_week_ends,
         months = date_month_ends,
         quarters = date_quarter_ends,
         semiannual = date_semiannual_ends,
         years = date_year_ends,
         
         # default
         NULL)  
}


###############################################################################
#' Apply Business Date logic to provided Dates function 
#'
#' @param dates dates, dates are assumed to contain only business dates
#' @param dates.fn dates function that returns index of selected dates, \strong{defaults to NULL}
#' @param calendar RQuantLib calendar name to use to determine business_days, \strong{defaults to NULL}
#'
#' @return index of business_days
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' apply_business_days(dates, date_week_ends, 'UnitedStates/NYSE')
#' apply_business_days(dates, date_week_ends, 'Canada/TSX')
#' }
#' @export 
###############################################################################
apply_business_days = function(dates, dates.fn = NULL, calendar = NULL, base = T) {
  # assume that if xts is given; it is sourced from historical data and
  # does not contain holidays; we just need to check boundary cases
  if( xts::is.xts(dates) ) {
    dates = index(dates)
    apply_business_days_internal(dates, dates.fn, calendar, base)
  } else {
    ok.index = business_days(dates = dates, calendar = calendar, return.index=T)	
    index = apply_business_days_internal(dates[ok.index], dates.fn, calendar, base)
    if(base)
      (1:len(dates))[ok.index][index]
    else
      index
  }
}

apply_business_days_internal = function(dates, dates.fn = NULL, calendar = NULL, base = T) {
  if( xts::is.xts(dates) ) dates = index(dates)
  dates = as.Date(dates)
  n = len(dates)
  
  # getHolidayList	
  holidays = NULL
  if( !is.null(calendar) ) {	  	
    if( requireNamespace('RQuantLib', quietly = T) )
      holidays = RQuantLib::getHolidayList(calendar, dates[1] - 60, dates[1] - 1)
    else
      warning('RQuantLib could not be loaded')
  }
  before = business_days(dates[1] - 60, dates[1] - 1, holidays)
  n.before = len(before) 
  
  if( !is.null(holidays) ) 
    holidays = RQuantLib::getHolidayList(calendar, dates[n] + 1, dates[n] + 60)   
  after = business_days(dates[n] + 1, dates[n] + 60, holidays)
  
  dates = c(before, dates, after)
  
  if( !is.null(dates.fn) ) 
    index = dates.fn(dates)
  else
    index = 1:len(dates)
  
  # map back to original dates
  if( base ) {
    index = index[index > n.before & index <= (n.before + n)]
    index = index - n.before
    return(index)
  }
  
  # till / since logic (business_days_location_end)
  original.dates.index = (n.before + 1) : (n.before + n)
  temp.cum = cumsum(rep(1, len(dates)))
  temp = temp.cum * NA
  temp[index] = temp.cum[index]
  days.since = temp.cum - ifna_prev(temp)
  days.till = temp[ifna_prevx_rev(temp)] - temp.cum
  list(days.since = days.since[original.dates.index], days.till = days.till[original.dates.index])	
}




###############################################################################
#' Business Date Since / Till function 
#'
#' @param dates dates
#' @param dates.fn dates function that returns index of selected dates, \strong{defaults to date_month_ends}
#' @param calendar RQuantLib calendar name to use to determine business_days, \strong{defaults to NULL}
#'
#' @return list with days.since and days.till arrays
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' business_days_location_end(dates, date_week_ends, 'UnitedStates/NYSE')
#' business_days_location_end(dates, date_week_ends, 'Canada/TSX')
#' }
#' @export 
###############################################################################
business_days_location_end = function(dates, dates.fn = date_month_ends, calendar = NULL) { 
  apply_business_days(dates, dates.fn, calendar, F)
}


#' out is result of the business_days_location_end
#' @export 
date_ends_index <- function(out, timing) {
  if(timing <= 0)
    which(out$days.till == (-timing))
  else
    which(out$days.since == (timing))
}


###############################################################################
#' Date Ends function 
#'
#' @param dates dates
#' @param periodicity periodicity
#' @param by take every *by* item, \strong{defaults to 1}
#' @param skip skip periods, \strong{defaults to 0}
#' @param last.date flag to include last date, \strong{defaults to TRUE}
#' @param calendar RQuantLib calendar name to use to determine business_days, \strong{defaults to NULL}
#'
#' @return index of date_ends
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' date_ends(dates, 'year', calendar = 'UnitedStates/NYSE')
#' date_ends(dates, 'year', calendar = 'Canada/TSX')
#' date_ends(dates, 'bi-weekly')
#' }
#' @export 
###############################################################################
date_ends = function(dates, periodicity, by=1, skip=0, last.date=T, calendar = NULL) {
  periodicity = trim(tolower(periodicity))
  
  # bi- means 'every two', as in every two [weeks/months/years, etc]
  # biweekly = every two weeks / bimonthly = every two months
  # semi- to mean 'twice every' (as semiannually - twice per year)
  # semiweekly = twice a week / semimonthly = twice a month
  bi.flag = substr(periodicity,1,2) == 'bi'
  #semi.flag = substr(periodicity,1,4) == 'semi'	
  
  if(bi.flag) periodicity = substr(periodicity,3,1000)
  by = if(bi.flag) 2 else by
  #if(semi.flag) periodicity = substr(periodicity,5,1000)
  periodicity = trim(gsub('-','',periodicity))
  
  ends = apply_business_days(dates, calendar = calendar,
                             dates.fn = function(x) {			
                               fn = date_ends_fn(periodicity)
                               if( is.null(fn) )
                                 xts::endpoints(xts::xts(1:len(x), x), periodicity)			
                               else
                                 fn(x, last.date=F)		
                             })
  
  if( xts::is.xts(dates) ) dates = index(dates)		
  ends = ends_add_last_date(ends, len(dates), last.date)		
  
  if( skip > 0) ends = ends[-c(1:skip)]
  if( by > 1) ends = ends[seq(1, len(ends), by=by)]
  
  ends		
}

#' last calendar day of period
#' date_end('2014-01-13')
#' @export 
#' @rdname DateFunctionsIndex 
date_end <- function(date = Sys.Date(), periodicity = 'months', date.format = '%Y-%m-%d') {
  date = as.Date(paste(date), date.format)
  temp = seq(date, date + 40, 1)
  temp[date_ends_fn(periodicity)(temp)[1]]
}

#' index of dates apart by a given n
#' one.year = date_ends_n(prices, 365)
#' @export 
#' @rdname DateFunctionsIndex 
date_ends_n = function(dates, n, start=1, less = T, silent=T) {
  if (xts::is.xts(dates))
    dates = index(dates)
  
  days = cumsum(as.numeric(diff(dates)))	
  
  nperiods = len(dates)
  periods = c(start)	
  while( last(periods) < (nperiods-1) )
    if(less)
      periods = c(periods, last(which((days - days[last(periods)]) < n)))
  else
    periods = c(periods, 2 + last(which((days - days[last(periods)]) < n)))
  
  if(!less) periods[periods > nperiods] = nperiods
  
  if(!silent) 
    if(less) print( max(diff(dates[periods])) )
  else print( diff(dates[periods]) )
  periods
}

###############################################################################
#' Extract Business Days
#'
#' @param from start date (ignored if dates are provided)
#' @param to start date (ignored / optional if dates are provided)
#' @param holidays list of holidays, \strong{defaults to NULL}
#' @param dates dates, \strong{defaults to NULL}
#' @param calendar RQuantLib calendar name to use to determine business_days, \strong{defaults to NULL}
#' @param return.index flag to return index of valid dates instead of dates, \strong{defaults to FALSE}
#'
#' @return business_days
#'
#' @examples
#' \dontrun{ 
#' library(RQuantLib)
#' from = as.Date('10Jun2013','%d%b%Y')
#' to = as.Date('10Jan2014','%d%b%Y')
#' holidays = getHolidayList('UnitedStates/NYSE', from, to)  
#' holidays = getHolidayList('Canada/TSX', from, to)  
#' business_days(from, to, holidays = holidays)
#'
#' business_days(dates = dates, calendar = 'UnitedStates/NYSE')
#' }
#' @export 
###############################################################################
business_days = function(
  from = Sys.Date(), 
  to = as.Date(from) + 31, 
  holidays = NULL, 
  dates = NULL, 
  calendar = NULL,
  return.index = F
) {
  if( is.null(dates) ) 
    dates = seq(as.Date(from), as.Date(to), 1)
  else if( xts::is.xts(dates) ) 
    dates = index(dates)
  dates = as.Date(dates)
  
  rm.index = date_dayofweek(dates) == 6 | date_dayofweek(dates) == 0
  
  # getHolidayList	
  if( !is.null(calendar) ) {	  	
    if( requireNamespace('RQuantLib', quietly = T) )
      holidays = RQuantLib::getHolidayList(calendar, dates[1], dates[len(dates)])
    else
      warning('RQuantLib could not be loaded')
  }
  
  if( !is.null(holidays) ) {
    holidays = as.Date(holidays)
    rm.index = rm.index | !is.na(match(dates, holidays))        
  }
  
  if( return.index )
    !rm.index
  else
    dates[!rm.index]
}


###############################################################################
#' Business Date Functions
#'
#' @param from start date
#' @param holidays list of holidays, \strong{defaults to NULL}
#' @param fn.ends function that return periods ends, \strong{defaults to date_month_ends}
#'
#' @return number of business_days
#'
#' @examples
#' \dontrun{ 
#' from = as.Date('27Dec2013','%d%b%Y')
#' holidays = getHolidayList('UnitedStates/NYSE', from-40, from+10)  
#' business_days_till_end(from, holidays)
#' business_days_since_end(from, holidays)
#' }
#' @export 
#' @rdname BusinessDateFunctions
###############################################################################
business_days_till_end <- function(from, holidays = NULL, fn.ends = date_month_ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business_days(from - 10, from, holidays)
  from = dates[len(dates)]
  
  dates = business_days(from, from + 40, holidays)
  index = match.fun(fn.ends)(dates, F)
  index[1] - 1
}


#' @export 
#' @rdname BusinessDateFunctions
business_days_since_end <- function(from, holidays = NULL, fn.ends = date_month_ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business_days(from - 10, from, holidays)
  from = dates[len(dates)]
  
  dates = business_days(from - 40, from + 10, holidays)
  index = match.fun(fn.ends)(dates, F)
  
  last.index = index[len(index)]
  if( dates[last.index] == from) return(0)
  
  from.index = sum(dates <= from)
  if( dates[last.index] < from) return(from.index - last.index)
  
  last.index = index[(len(index) - 1)]
  return(from.index - last.index)
}


#' @export 
#' @rdname BusinessDateFunctions
next_business_day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business_days(from + offset, from + 10, holidays)
  dates[1]
}


#' @export last_business_day
#' @rdname BusinessDateFunctions
last_business_day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business_days(from - 10, from - offset, holidays)
  dates[1]
}



###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references 
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{ 
#' third_friday_month(2012,1)
#' }
#' @export 
###############################################################################
third_friday_month <- function(years, months)
{ 
  helper <- function(year, month) {
    day = date_dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
    day = c(20,19,18,17,16,15,21)[1 + day]
    as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1]
  }
  if(len(years) > 1 && len(months) > 1) {
    out = c()
    for(month in months)
      out = c(out, helper(years,month))
    as.Date(out)
  } else
    helper(years,months) 
} 

# Special days
# http://www.cboe.com/AboutCBOE/xcal2014.pdf
# third_friday_month(2010:2013, 4)  
# key.date = map_spx_expiration(data$prices)  
# VIX settles 30 days prior to SPY
# key.date = map_spx_expiration(data$prices, offset=30)   
# na.omit(key.date['2014'])
#' @export
map_spx_expiration <- function(data, backfill = T, offset = 0) {
  dates = as.Date(index(data))
  
  # 3rd Friday of the month is last trading day for equity options
  years = date_year(range(dates))
  friday = third_friday_month(years[1]:(years[2]+1), 1:12)
  friday.future = friday[friday > dates[len(dates)]]
  friday = friday[friday <= dates[len(dates)]]
  
  key.date.index = match(friday, dates)
  na.index = which(is.na(key.date.index))
  
  # backfill NA's
  if(backfill && len(na.index)>0)
    key.date.index[na.index] = match(friday[na.index]-1, dates)
  
  if(offset != 0) {
    friday = c(dates[key.date.index], friday.future)
    offset.date = friday - offset
    key.date.index = match(offset.date, dates)
  }
  
  key.date.index = na.omit(key.date.index)
  
  key.date = NA * data[,1]
  key.date[key.date.index,] = T
  key.date
}

