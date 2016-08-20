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
# Examples for the R/Finance Presentation
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################




#############################################################################
# Cluster Risk Parity
# R/Finance 2013
#############################################################################


# Examine Weights and Risk Contributions
bt_cluster_risk_parity_weights_test <- function()
{
    #*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load_packages('quantmod')

	tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
	
	# map logic
	map = spl('Gold GLD,US Dollar UUP,S&P500 SPY,Nasdaq QQQ,Small Cap IWM,EmergingM EEM,InternationalM EFA,Real Estate IYR,Oil USO,Treasurys TLT')
		names(map) = tickers

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
	# map logic
	for(i in ls(data)) data[[ map[i] ]] = data[[i]]
	rm(list=tickers, envir=data)

	bt_prep(data, align='remove.na', dates = '2011:12::2012')	

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	periodicity = 'months'
	lookback.len = 250
	cluster.group <<- cluster_group_kmeans_90
		
	obj = portfolio_allocation_helper(data$prices, 
		periodicity = periodicity, lookback.len = lookback.len,
		min.risk.fns = list(
			EW=equal_weight_portfolio,
			RP=risk_parity_portfolio(),
			ERC=equal_risk_contribution_portfolio,
						
			G2.EW = distribute_weights(equal_weight_portfolio, cluster.group),
			G2.RP=distribute_weights(risk_parity_portfolio(), cluster.group),
						
			G2.MV=distribute_weights(min_var_portfolio, cluster.group),
			G2.ERC=distribute_weights(equal_risk_contribution_portfolio, cluster.group)
		),
		adjust2positive.definite = F,
		custom.stats.fn = portfolio_allocation_custom_stats_clusters
	) 			

	
	#*****************************************************************
	# Create Plots
	#****************************************************************** 		
	clusters = coredata(obj$clusters$EW)[13,]
	
	# re-map, so that Fixed Income  is red, Currency is blue, Equity is green, Commodity is purple
	temp = clusters
		temp[] = 0
	temp[clusters == clusters[names(clusters) == 'Treasurys TLT']] = 1
	temp[clusters == clusters[names(clusters) == 'US Dollar UUP']] = 2
	temp[clusters == clusters[names(clusters) == 'EmergingM EEM']] = 3
	temp[clusters == clusters[names(clusters) == 'Gold GLD']] = 4
	clusters = temp

		
png(filename = 'plot1.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')			

	layout(matrix(1:2,nc=2))
	
	plot_cluster_weights(coredata(obj$weights$ERC)[13,], clusters, 
		main='ERC Weights')
	
	plot_cluster_weights(coredata(obj$risk.contributions$ERC)[13,], clusters, 
		main='ERC Risk Contributions')

dev.off()	
	
png(filename = 'plot2.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')			

	layout(matrix(1:2,nc=2))
	
	plot_cluster_weights(coredata(obj$weights$G2.ERC)[13,], clusters, 
		main='Cluster ERC Weights')
	
	plot_cluster_weights(coredata(obj$risk.contributions$G2.ERC)[13,], clusters, 
		main='Cluster ERC Risk Contributions')

dev.off()	
	
}	

	

###############################################################################
# Back test for 10 Major Asset Classes
###############################################################################		
bt_cluster_risk_parity_10_major_assets <- function()
{		
	#*****************************************************************
	# Define universes
	#****************************************************************** 		
	tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
	dates='2004:12::'							
	name = 'ETFs AAA'
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	load_packages('quantmod')	
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
		
	bt_prep(data, align='keep.all', dates=dates, fill.gaps=T)
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	periodicity = 'weeks'
	lookback.len = 250
	cluster.group <<- cluster_group_kmeans_90
	
	obj = portfolio_allocation_helper(data$prices, 
		periodicity = periodicity, lookback.len = lookback.len,
		min.risk.fns = list(
						EW=equal_weight_portfolio,
						RP=risk_parity_portfolio(),
						ERC=equal_risk_contribution_portfolio,
						
						Dynamic.EW = distribute_weights(equal_weight_portfolio, cluster.group),
						Dynamic.RP=distribute_weights(risk_parity_portfolio(), cluster.group),
						Dynamic.ERC=distribute_weights(equal_risk_contribution_portfolio, cluster.group)
			),
			adjust2positive.definite = F,
		custom.stats.fn = portfolio_allocation_custom_stats
	) 			
	
	models = create_strategies(obj, data, dates=(lookback.len):nrow(data$prices))$models
	  
	#*****************************************************************
	# Create Reports
	#****************************************************************** 		
	title = paste(name, '(' ,periodicity, ',' , lookback.len, 'days )')
			
	stats = bt_summary_report(models, title, data, obj,
		control = list(
			plot.weight.transition.maps = F,
			plot.risk.contribution.transition.maps = F)
		)  
				
}    

   
###############################################################################
# Back test for stocks in Dow 30 index
###############################################################################		
bt_cluster_risk_parity_dow_30 <- function()
{		
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load_packages('quantmod')	
	dates='1995::'							
	name = 'Dow Jones 30'	
	data = load_dow_jones(align='keep.all', dates=dates)
		sectors = data$sectors
		tickers = data$symbolnames
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 		
	periodicity = 'weeks'
	lookback.len = 250
	cluster.group <<- cluster_group_kmeans_90

	obj = portfolio_allocation_helper(data$prices, 
		periodicity = periodicity, lookback.len = lookback.len,
		min.risk.fns = list(EW=equal_weight_portfolio,
						RP=risk_parity_portfolio(),
						ERC=equal_risk_contribution_portfolio,
						
						Static.EW = distribute_weights(equal_weight_portfolio, static_group(as.numeric(sectors))),
						Static.RP=distribute_weights(risk_parity_portfolio(), static_group(sectors)),
						Static.ERC=distribute_weights(equal_risk_contribution_portfolio, static_group(sectors)),

						Dynamic.EW = distribute_weights(equal_weight_portfolio, cluster.group),
						Dynamic.RP=distribute_weights(risk_parity_portfolio(), cluster.group),
						Dynamic.ERC=distribute_weights(equal_risk_contribution_portfolio, cluster.group)
			),
			adjust2positive.definite = F,
		custom.stats.fn = portfolio_allocation_custom_stats
	) 			

	models = create_strategies(obj, data, dates=(lookback.len):nrow(data$prices))$models
	
	#*****************************************************************
	# Create Reports
	#****************************************************************** 		
	title = paste(name, '(' ,periodicity, ',' , lookback.len, 'days )')
			
	stats = bt_summary_report(models, title, data, obj,
		control = list(
			plot.weight.transition.maps = F,
			plot.risk.contribution.transition.maps = F)
		)  
}		
	
	
    
    

    
    	
	

###############################################################################
# Helper Data functions
###############################################################################
load_dow_jones <- function(align='remove.na', dates = NULL) {	
	#*****************************************************************
	# Find Sectors for each company in DOW 30
	#****************************************************************** 
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
	tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
	
	sector.map = c()
	for(i in 1:len(tickers)) {
		sector.map = rbind(sector.map, 
				cbind(sector_spdr_components(tickers[i]), tickers.desc[i])
			)
	}
	colnames(sector.map) = spl('ticker,sector')

	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load_packages('quantmod')	
	tickers = dow_jones_components()
	
	sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
		names(sectors) = tickers
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	
	bt_prep(data, align=align,dates=dates)
	
	# re-order sectors, because bt_prep can change the order of tickers
	data$sectors = sectors[data$symbolnames]

	return(data)
}

###############################################################################
# Helper Back-test functions
###############################################################################
# compute portfolio allocation additional stats
portfolio_allocation_custom_stats_clusters <- function(x,ia) {
	risk.contributions = portfolio_risk_contribution(x, ia)
	clusters = cluster.group(ia)
	return(list(
		# vectors
		risk.contributions = risk.contributions,
		clusters = clusters,
		
		# numbers
		ncluster = max(clusters)		
	))
}
 

# create back-test report
bt_summary_report <- function(models, title, data, obj=NULL,
	control = list(
		plot.weight.transition.maps = F,
		plot.risk.contribution.transition.maps = !is.null(obj)
	)
) {
	#*****************************************************************
	# Setup
	#****************************************************************** 					
	if(is.null(control$plot.weight.transition.maps)) control$plot.weight.transition.maps = F
	if(is.null(control$plot.risk.contribution.transition.maps)) control$plot.risk.contribution.transition.maps = obj!=NULL

    filename = title
    filename.pdf = paste(filename, '.pdf', sep='')
    filename.csv = paste(filename, '.csv', sep='')
	
    # put all reports into one pdf file
	pdf(file = filename.pdf, width=8.5, height=11)
	
	

	#*****************************************************************
	# Create Plots
	#****************************************************************** 					
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt_strategy_sidebyside(models, return.table=T) 

	#*****************************************************************
	# Create Table
	#****************************************************************** 							
	# Composite Diversification Indicator
	cdi = custom_composite_diversification_indicator(obj, plot.main = F, plot_table = F)	
		out = rbind(colMeans(cdi, na.rm=T), out)
		rownames(out)[1] = 'Composite Diversification Indicator(CDI)'	
	
	# Portfolio Turnover
	y = 100 * sapply(models, compute_turnover, data)
		out = rbind(y, out)
		rownames(out)[1] = 'Portfolio Turnover'		
	
	# Bar chart in descending order of the best algos
	performance_barchart_helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,T,T,F,F,T))			
	
	
	#*****************************************************************
	# Create Plots
	#****************************************************************** 					
	
	# Plot transition maps	
	if(control$plot.weight.transition.maps) {
		#layout(1:len(models))
		layout(1:4)
		for(m in names(models)) {
			plotbt_transition_map(models[[m]]$weight, name=m)
				legend('topright', legend = m, bty = 'n')
		}
	}
		
	# Plot transition maps for Risk Contributions
	if(control$plot.risk.contribution.transition.maps) {
		dates = index(data$prices)[obj$period.ends]
		layout(1:4)
		for(m in names(models)) {
			plotbt_transition_map(make_xts(obj$risk.contributions[[m]], dates), 
			name=paste('Risk Contributions',m))
				legend('topright', legend = m, bty = 'n')
		}	
	}	
	
	# close pdf file
    dev.off()    	
	
	
	#*****************************************************************
	# save summary & equity curves into csv file
	#****************************************************************** 
	load_packages('abind')
	append=FALSE
	
	cat(title, '\n', file=filename.csv, append=append)
	write.table(out, sep=',',  row.names = , col.names = NA, file=filename.csv, append=TRUE)	
	cat('\n\n', file=filename.csv, append=TRUE)
	
	if(F) {	
		out = abind(lapply(models, function(m) m$equity))
			colnames(out) = names(models)
		write_xts(make_xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)	
	}	
	
	return(out) 
}
 

	



#############################################################################
# Helper plot functions
#############################################################################
# pie.labels_function override to allow trnaparent background
pie_labels_fix <- function (x, y, angles, labels, radius = 1, ...) {
	par(xpd = TRUE)
	    xylim <- par("usr")
	    plotdim <- par("pin")
	    yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
	    xc <- cos(angles) * radius + x
	    yc <- sin(angles) * yradius + y
	    text(xc, yc, labels, ...)
    par(xpd = FALSE)
}

# plot pie charts weight / clusters	
plot_cluster_weights <- function(weight, clusters, main='') {
	load_packages('RColorBrewer,plotrix')
	
	clusters = sort(clusters)
	weight = weight[names(clusters)]
	weight.cluster = tapply(weight,clusters,sum)
	
	
	counts = tapply(names(clusters),clusters,len)
		ncluster = len(counts)
 
	require(RColorBrewer)
	colors = colorRampPalette(brewer.pal(iif(ncluster>9,9,ncluster),'Set1'))(ncluster)
		#  plot(1:len(colors), col=colors, lwd=40)

	cols = c()
	for(i in 1:ncluster) cols = c(cols, col_add_alpha(colors[i], seq(200,100,length.out = counts[i])))	
	

	if(F) {
		plot(-1:1 ,-1:1, main=main, type='n', yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', axes = F)
		bisect.angles = floating.pie(0,0,weight, col=cols, border='white', radius=0.9, cex=0.8)
		pie.labels(0,0,bisect.angles,names(weight),radius=1,bg=0,border=F, srt=bisect.angles)
	}
	
	par(mar = c(2,2,2,2)) 
	pie(weight, col=cols, border='white', radius=0.9, main=main)
	
	require(plotrix)
	bisect.angles = floating.pie(0,0,weight.cluster,radius=0.5,col=colors,border='white')
	pie_labels_fix(0,0,bisect.angles,paste(round(100*weight.cluster,0),'%',sep=''),radius=0.2)
}

