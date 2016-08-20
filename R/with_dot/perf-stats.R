


###############################################################################
# Plot monthly return table
#' @export 
###############################################################################
#' @import xts
plotbt.monthly.table <- function(equity, make.plot = TRUE, smain = '') 
{
  equity = map2monthly(equity)
  
  dates = index.xts(equity)
  equity = coredata(equity)	
  
  # just keep both versions for now	
  
  month.ends = date.month.ends(dates)
  year.ends =  date.year.ends(dates[month.ends])
  year.ends = month.ends[year.ends]
  nr = len(year.ends) + 1
    

  # create plot matrix
  temp = matrix( double(), nr, 12 + 2)
  rownames(temp) = c(date.year(dates[year.ends]), 'Avg')
  colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Year,MaxDD')
  
  # compute yearly profit and drawdown
  index = c(1, year.ends)
  for(iyear in 2:len(index)) {
    iequity = equity[ index[(iyear-1)] : index[iyear] ]
    iequity = ifna( ifna.prev(iequity), 0)
    
    temp[(iyear-1), 'Year'] = xts::last(iequity, 1) / iequity[1] -1
    temp[(iyear-1), 'MaxDD'] = min(iequity / cummax(iequity) - 1, na.rm = T)
  }
  
  # compute monthly profit
  index = month.ends
  monthly.returns = c(NA, diff(equity[index]) / equity[index[-len(index)]])
  
  index = date.month(range(dates[index]))
  monthly.returns = c( rep(NA, index[1]-1), monthly.returns, rep(NA, 12-index[2]) )
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

  temp[] = plota.format(100 * temp, 1, '', '')
  
  if(make.plot) plot.table(temp, highlight = highlight, smain = smain)
  
  return(temp)
}	
