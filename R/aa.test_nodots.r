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
# Test cases for Asset Allocation Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Test AA functions, introduction
###############################################################################
aa_test <- function()
{
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------

	ia = aa_test_create_ia()
	
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# visualize input assumptions
	plot_ia(ia)
	
dev.off()	

png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		

	# display each asset in the Risk - Return plot 
	layout(1)
	par(mar = c(4,4,2,1), cex = 0.8)
	x = 100 * ia$risk
	y = 100 * ia$expected.return
	
	plot(x, y, xlim = range(c(0, x)), ylim = range(c(0, y)),
		xlab='Risk', ylab='Return', main='Risk vs Return', col='black')
	grid();
	text(x, y, ia$symbols,	col = 'blue', adj = c(1,1), cex = 0.8)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier
	ef = portopt(ia, constraints, 50, 'Efficient Frontier')

	
	
		
png(filename = 'plot3.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	plot_ef(ia, list(ef))	

dev.off()
	
	#--------------------------------------------------------------------------
	# Plot multiple Efficient Frontiers
	#--------------------------------------------------------------------------
	
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = portopt(ia, constraints, 50, 'Max Loss', min_maxloss_portfolio)	
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio_maxloss, F)	
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio_mad, F)	

dev.off()	
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	layout( matrix(1:4, nrow = 2) )
	plot_transition_map(ef.risk)
	plot_transition_map(ef.maxloss)
	plot_transition_map(ef.mad)

dev.off()	

	
}

###############################################################################
# Test AA functions, long/short 130:30
###############################################################################
# Workingimplementation of 130:30
# Asset Allocation and Risk Assessment with Gross Exposure Constraints for Vast Portfolios by J. Fan, Zhang J., Yu K. (2008)
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1307423
#
# Note 3 on Page 8
# To get 130 long, 30 short
#--------------------------------------------
# One alternative
# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
#
# Transfrom the covariance Q into
# | Q    0*Q |
# | 0*Q  0*Q |
#--------------------------------------------
# Another alternative
# Split x into x.long and x.short, x_long and x_short >= 0
# SUM(x.long) - SUM(x.short) = 1.6
#
# Transfrom the covariance Q into
# | Q -Q |
# |-Q  Q |
#--------------------------------------------
# The problem is that 1.6 is not always inforced because
# minimum variance can be achived at a lower leverage
###############################################################################
aa_long_short_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# -0.5 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = -0.5, ub = 0.8)
		
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
	#--------------------------------------------------------------------------

	# -0.5 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = -0.5, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		

	# adjust prior constraints, add v.i
	constraints = add_variables(n, constraints)

	# -v.i <= x.i <= v.i
	#   x.i + v.i >= 0
	constraints = add_constraints(rbind(diag(n), diag(n)), rep(0, n), type = '>=', constraints)
	#   x.i - v.i <= 0
	constraints = add_constraints(rbind(diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
	
	# SUM(v.i) = 1.6
	constraints = add_constraints(c(rep(0, n), rep(1, n)), 1.6, type = '=', constraints)

	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		# keep only portfolio weights
		ef.risk$weight = ef.risk$weight[,(1:n)]			
		
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[,(1:n)]
		
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
		
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 1.6
	#--------------------------------------------------------------------------
	ia.ls = aa_test_ia_add_short(ia)
	
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new_constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
		
	# SUM (x.long - x.short) = 1
	constraints = add_constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 1.6
	constraints = add_constraints(c(rep(1,n), rep(1,n)), 1.6, type = '=', constraints)		

	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()
	

	#--------------------------------------------------------------------------
	# Create 200:100
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 3
	#
	# The problem is that 3 is not always inforced because
	# minimum variance can be achived at a lower leverage	
	#--------------------------------------------------------------------------
	
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new_constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
		
	# SUM (x.long - x.short) = 1
	constraints = add_constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 3
	constraints = add_constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)		

	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 200:100 using binary[0/1] variables and Branch and Bound algorithm
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 3
	#
	# Solve using branch and bound: add a binary var b1:bn, xL < b, xS < (1-b)
	#--------------------------------------------------------------------------
		
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new_constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
	
	# SUM (x.long - x.short) = 1
	constraints = add_constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 3
	constraints = add_constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)		
				
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add_variables(n, constraints)
	
	# index of binary variables b.i
	constraints$binary.index = (2*n+1):(3*n)
	
	# binary variable b.i : x.long < b, x.short < (1 - b)
	# x.long < b
	constraints = add_constraints(rbind(diag(n), 0*diag(n), -diag(n)), rep(0, n), type = '<=', constraints)

	# x.short < (1 - b)
	constraints = add_constraints(rbind(0*diag(n), diag(n), diag(n)), rep(1, n), type = '<=', constraints)
	
	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		

png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()
	
}

###############################################################################
# Test AA functions, Cardinality Constraints
###############################################################################
# Minimum Invesment Constraint
# Pre-determined Number of Asstes Constraint
###############################################################################
aa_cardinality_test <- function()
{

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)
		
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	aa.plot_ef.summary.test <- function(ef)
	{
		layout(1:2)
		par(mar = c(4,4,2,1), cex = 0.8)
		y = iif(ef$weight > 0.000001, ef$weight, NA) 
		plot(as.vector(sort(100 * y)), pch=20, xaxt='n', ylim = c(0, 80),
			xlab='', ylab='Weight', main='Portfolio Weights')
			abline(h=0, col = 'red')
			abline(h=10, col = 'red')
			
		plot(100* ef$risk, rowSums(!is.na(y), na.rm = T), pch=20, type='b', 
			xlab='Risk', ylab='Number of Assets', main='Number of Assets')
	
	}
	
	aa.plot_ef.summary.test(ef.risk)

dev.off()	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
		
dev.off()	


	#--------------------------------------------------------------------------
	# Minimum Investment Constraint is 10%
	# Add binary[0/1] variables
	# 0.1 * b <= x.i <= 0.8 * b
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new_constraints(n,rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add_variables(n, constraints)
		
	# index of binary variables b.i
	constraints$binary.index = (n+1):(2*n)
		
	# 0.1 * b <= x.i <= 0.8 * b
	# x.i >= 0.1 * b 
	constraints = add_constraints(rbind(diag(n), -0.1 * diag(n)), rep(0, n), type = '>=', constraints)

	# x.i <= 0.8 * b
	constraints = add_constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
	
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		ef.risk$weight = ef.risk$weight[, 1:n]
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n]
		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	aa.plot_ef.summary.test(ef.risk)

dev.off()	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()	

	
	#--------------------------------------------------------------------------
	# Limit number of assets to 3
	# Add binary[0/1] variables
	# 0.00001 * b <= x.i <= 0.8 * b
	# SUM b.i = 3
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new_constraints(n, rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add_variables(n, constraints)
	
	# index of binary variables b.i
	constraints$binary.index = (n+1):(2*n)
		
	# 0.00001 * b <= x.i <= 0.8 * b
	# x.i >= 0.00001 * b 
	constraints = add_constraints(rbind(diag(n), -0.00001 * diag(n)), rep(0, n), type = '>=', constraints)

	# x.i <= 0.8 * b
	constraints = add_constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
	
	# SUM b = 3
	constraints = add_constraints(c(rep(0,n), rep(1,n)), 3, type = '=', constraints)
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		ef.risk$weight = ef.risk$weight[, 1:n]
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n]

		
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
				
	aa.plot_ef.summary.test(ef.risk)		

dev.off()	
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.mad)
	
dev.off()	
	
}


###############################################################################
# Test AA functions, Average Correlation
# Forecast-Free Algorithms: A New Benchmark For Tactical Strategies
# http://cssanalytics.wordpress.com/2011/08/09/forecast-free-algorithms-a-new-benchmark-for-tactical-strategies/
#
# Follow up FAQ: Forecast-Free Algorithms and Minimum Correlation Algorithm
# http://cssanalytics.wordpress.com/2011/08/15/follow-up-faq-forecast-free-algorithms-and-minimum-correlation-algorithm/
###############################################################################
aa_avg_cor_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	

	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.cor.insteadof.cov = portopt(ia, constraints, 50, 'Cor instead of Cov', min_cor_insteadof_cov_portfolio)
	ef.avgcor = portopt(ia, constraints, 50, 'AvgCor', min_avgcor_portfolio)

	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	layout(1:2)
	plot_ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio_avgcor, F)	
	
dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	layout( matrix(1:4, nrow = 2) )
	plot_transition_map(ef.risk)
	plot_transition_map(ef.avgcor)
	plot_transition_map(ef.cor.insteadof.cov)

dev.off()	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# visualize input assumptions
	plot_ia(ia)
	
dev.off()		
		

	#--------------------------------------------------------------------------
	# Double check that NonLinear Optimization finds global maximums by
	# creating random portfolios that satisfy constraints. 
	# Plot Average Correlation Efficient Frontier and random portfolios, check
	# that all portfolios lie below the efficient frontier.
	#--------------------------------------------------------------------------	
	# Generate random portfolios
	ef.random = list()
		ef.random$name = 'Random'
		ef.random$weight = randfixedsum(1000000, n, 1, 0, 0.8)
		
		ef.random$risk = portfolio_avgcor(ef.random$weight, ia)		
		ef.random$return = portfolio_return(ef.random$weight, ia)		
		
		
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot Average Correlation and random portfolios
	layout(1)
	plot(100*ef.random$risk, 100*ef.random$return, type='p', pch=20,
			xlim = 100*range(0, ef.random$risk, ef.avgcor$risk),
			ylim = 100*range(0, ef.random$return, ef.avgcor$return),
			main = 'Average Correlation Efficient Frontier vs Random Portfolios',
			xlab = 'portfolio_avgcor',
			ylab = 'Return'			
		)
	lines(100*portfolio_avgcor(ef.avgcor$weight, ia), 100*ef.avgcor$return, type='l', lwd=2,col = 'red')
	
dev.off()			
	

	
}



###############################################################################
# Test AA functions, Equal-Risk-Contribution (ERC) Portfolio
#
# Unproxying weight constraints by Pat Burns
# http://www.portfolioprobe.com/2011/04/13/unproxying-weight-constraints/
#
# Analytical Solution for the Equal-Risk-Contribution Portfolio
# http://www.wilmott.com/messageview.cfm?catid=34&amp;threadid=38497
#
# Equally-weighted risk contributions: a new method to build risk balanced diversified portfolios by S. Maillard, T. Roncalli and J. Teiletche (2008)
# http://www.thierry-roncalli.com/download/erc-slides.pdf
#
# On the property of equally-weighted risk contributions portfolios by S. Maillard, T. Roncalli and J. Teiletche (2008)
# http://www.thierry-roncalli.com/download/erc.pdf
#
# Matlab code for Equal Risk Contribution Portfolio by Farid Moussaoui
# http://mfquant.net/erc_portfolio.html
###############################################################################
aa_erc_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)
		
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier
	ef.risk = portopt(ia, constraints, 50, 'Risk')

	# plot	
	layout( 1:3 )
	plot_ef(ia, list(ef.risk), portfolio_risk, F)	
	plot_transition_map(ef.risk)	
	plot_transition_map(portfolio_risk_contribution(ef.risk$weight, ia), 
			ef.risk$risk, name='Risk Contribution')
	
	#--------------------------------------------------------------------------
	# Look at some portfolios
	#--------------------------------------------------------------------------			
	# 1/n
	x = rep(1/ia$n,ia$n)
	round(100*portfolio_risk_contribution(x, ia),1)
			
	# construct ERC Equal-Risk-Contribution Portfolio	
	x = find_erc_portfolio(ia, constraints)
	round(100*portfolio_risk_contribution(x, ia),1)
	
	#--------------------------------------------------------------------------
	# Replicate some examples from erc-slides.pdf
	#--------------------------------------------------------------------------			
	s = (c(1,2,3,4)/10)
	cor = 0.5 + 0*diag(4)
		diag(cor) = 1
	cov = cor * (s %*% t(s))
		
	weight = rep(1/4,4)
	weight = c(100,0,0,0)/100
	weight = c(48,24,16,12)/100	

	ia$n = 4
	ia$cov=cov
	round(100*portfolio_risk(weight, ia),1)
	round(100*portfolio_risk_contribution(weight, ia),1)
	
	
	s = c(12,10,11,13,12)/100
	cor = 0.6 + 0*diag(5)
		diag(cor) = 1
	cov = cor * (s %*% t(s))
	
	weight = c(23.96,6.43,16.92,28.73,23.96)/100
	weight = c(19.2,23,20.8,17.7,19.2)/100

	ia$n = 5
	ia$cov=cov
	round(100*portfolio_risk(weight, ia),1)
	round(100*portfolio_risk_contribution(weight, ia),1)
}


###############################################################################
# Test AA functions, Gini mean difference Efficient Frontier
#
# Gini mean difference
# The mean difference is also known as the absolute mean difference and the Gini mean difference 
# http://en.wikipedia.org/wiki/Mean_difference
#
# The Generation of Mean Gini Efficient Sets by J. Okunev (1991)
# Can be made more efficient by solving for dual
###############################################################################
aa_gini_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia_rebal()	
		ia$risk = apply(coredata(ia$hist.returns),2,sd)		
		ia$correlation = cor(coredata(ia$hist.returns), use='complete.obs',method='pearson')
		ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
					
	
	n = ia$n		

	# 0 <= x.i <= 1 
	constraints = new_constraints(n, lb = 0, ub = 1)
		
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	
	#x = min_gini_portfolio(ia, 	constraints)
	#portfolio_gini_coefficient(x, ia)

	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.gini = portopt(ia, constraints, 50, 'GINI', min_gini_portfolio)
		

	#--------------------------------------------------------------------------
	# Create Plots
	#--------------------------------------------------------------------------
		
png(filename = 'plot1g.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		

	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.gini), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.gini), portfolio_gini_coefficient, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.gini)
		
dev.off()	
		
		

	#require(fBasics)		
	#col = seqPalette(n, 'Greys')
	#plot_transition_map(ef.risk, col=col)





	ia = list()
	ia$n = 3
	ia$hist.returns = matrix(0,3,3)
		ia$hist.returns[1,] = c(10,9,6)/100
		ia$hist.returns[2,] = c(15,8,12)/100
		ia$hist.returns[3,] = c(12,7,15)/100
	
}





###############################################################################
# Test AA functions, CVaR Efficient Frontier
###############################################################################
aa_cvar_test <- function()
{

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	

# Expected shortfall (CVaR)
# http://www.investopedia.com/articles/04/092904.asp
ia$parameters.alpha = 0.95
	
		
	
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min_maxloss_portfolio)
	ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min_mad_portfolio)
	ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min_cvar_portfolio)
	ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min_cdar_portfolio)


png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio_cvar, F)	
	plot_ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio_cdar, F)	

dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	layout( matrix(1:4, nrow = 2) )
	plot_transition_map(ef.risk)
	plot_transition_map(ef.cvar)
	plot_transition_map(ef.cdar)

dev.off()

	return()
	

	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio_risk, F)
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio_maxloss, F)
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio_cvar, F)
	plot_ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio_cdar, F)
	
		
	layout( matrix(1:4, nrow = 2) )
	plot_transition_map(ef.maxloss)
	plot_transition_map(ef.mad)	
	plot_transition_map(ef.cvar)
	plot_transition_map(ef.cdar)
		

}

###############################################################################
# Test AA functions, Omega Efficient Frontier
###############################################################################
aa_omega_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# Omega - http://en.wikipedia.org/wiki/Omega_ratio
	ia$parameters.omega = 13/100 
		ia$parameters.omega = 12/100 
		# convert annual to monthly
		ia$parameters.omega = ia$parameters.omega / 12


	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )
	
	# weights
	rownames(ef.risk$weight) = paste('Risk','weight',1:50,sep='_')
	plot_omega(ef.risk$weight[c(1,10,40,50), ], ia)
	
	# assets
	temp = diag(n)
	rownames(temp) = ia$symbols
	plot_omega(temp, ia)
		
	# portfolio
	plot_ef(ia, list(ef.risk), portfolio_omega, T, T)			

dev.off()	
		
	#--------------------------------------------------------------------------
	# Create Efficient Frontier in Omega Ratio framework
	#--------------------------------------------------------------------------
		
	# Create maximum Omega Efficient Frontier
	ef.omega = portopt_omega(ia, constraints, 50, 'Omega')
	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )

	# weights
	plot_omega(ef.risk$weight[c(1,10,40,50), ], ia)

	# weights
	rownames(ef.omega$weight) = paste('Omega','weight',1:50,sep='_')	
	plot_omega(ef.omega$weight[c(1,10,40,50), ], ia)
		
	# portfolio
	plot_ef(ia, list(ef.omega, ef.risk), portfolio_omega, T, T)			
		
dev.off()	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk,ef.omega), portfolio_risk, F)			
	plot_ef(ia, list(ef.risk,ef.omega), portfolio_omega, F)			

	plot_transition_map(ef.risk)
	plot_transition_map(ef.omega)
	
dev.off()	
				
}


###############################################################################
# Test AA functions, Downside Risk
###############################################################################
aa_downside_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8
	constraints = new_constraints(n, lb = 0, ub = 0.8)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		

	# Set target return (or Minimum Acceptable Returns (MAR))
	# and consider only returns that are less than the target 
	ia$parameters.mar = 0/100 
		# convert annual to monthly
		ia$parameters.mar = ia$parameters.mar / 12

		
	# create efficient frontier(s)
	ef.mad = portopt(ia, constraints, 50, 'MAD', min_mad_portfolio)
	ef.mad.downside = portopt(ia, constraints, 50, 'S-MAD', min_mad_downside_portfolio)
	
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.risk.downside = portopt(ia, constraints, 50, 'S-Risk', min_risk_downside_portfolio)
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.mad.downside, ef.mad), portfolio_mad, F)			
	plot_ef(ia, list(ef.mad.downside, ef.mad), portfolio_mad_downside, F)			
		
	plot_transition_map(ef.mad)
	plot_transition_map(ef.mad.downside)

dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.downside, ef.risk), portfolio_risk, F)			
	plot_ef(ia, list(ef.risk.downside, ef.risk), portfolio_risk_downside, F)			

	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.downside)

dev.off()		
}

	
###############################################################################
# Test AA functions, Multiple Risk Measures Efficient Frontier
###############################################################################
aa_multiple_risk_measures_test <- function()
{
	# Following linear risk constraints are implemented
	# add_constraint_maxloss
	# add_constraint_mad
	# add_constraint_cvar
	# add_constraint_cdar
	

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
		
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min_maxloss_portfolio)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.maxloss), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk, ef.maxloss), portfolio_maxloss, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.maxloss)

dev.off()
	
	#--------------------------------------------------------------------------
	# Add MaxLoss <= 12 constraint
	#--------------------------------------------------------------------------
	
	constraints = add_constraint_maxloss(ia, 12/100, '<=', constraints)	
		
	ef.risk.maxloss = 		portopt(ia, constraints, 50, 'Risk+MaxLoss')
		ef.risk.maxloss$weight = ef.risk.maxloss$weight[, 1:n]
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio_risk, F)	
	plot_ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio_maxloss, F)	

	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.maxloss)

dev.off()
	
	return()		
	
	
	
	#--------------------------------------------------------------------------
	# Other Examples
	#--------------------------------------------------------------------------
	
	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)			
	
	# Alpha for CVaR and DVar
	ia$parameters.alpha = 0.95
				
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min_maxloss_portfolio)
	ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min_mad_portfolio)
	ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min_cvar_portfolio)
	ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min_cdar_portfolio)
	
	
	#--------------------------------------------------------------------------
	# Limit Max Loss
	#--------------------------------------------------------------------------
	layout(1)
	plot_ef(ia, list(ef.risk, ef.maxloss), portfolio_maxloss, F)
	
	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)			
	constraints = add_constraint_maxloss(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot_ef(ia, list(ef.risk), portfolio_maxloss, F)
	plot_ef(ia, list(ef.risk.new), portfolio_maxloss, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.new, ef.risk,ef.maxloss), portfolio_maxloss, F)	
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.maxloss), portfolio_risk, F)
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.new)
	
	#--------------------------------------------------------------------------
	# Limit MAD
	#--------------------------------------------------------------------------
	layout(1)
	plot_ef(ia, list(ef.risk, ef.mad), portfolio_mad, F)
	
	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)				
	constraints = add_constraint_mad(ia, 2.9/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot_ef(ia, list(ef.risk), portfolio_mad, F)
	plot_ef(ia, list(ef.risk.new), portfolio_mad, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.new, ef.risk,ef.mad), portfolio_mad, F)	
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.mad), portfolio_risk, F)
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.new)
	
	#--------------------------------------------------------------------------
	# Limit CVaR
	#--------------------------------------------------------------------------
	layout(1)
	plot_ef(ia, list(ef.risk, ef.cvar), portfolio_cvar, F)
	
	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)					
	constraints = add_constraint_cvar(ia, 8/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot_ef(ia, list(ef.risk), portfolio_cvar, F)
	plot_ef(ia, list(ef.risk.new), portfolio_cvar, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.new, ef.risk,ef.cvar), portfolio_cvar, F)	
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.cvar), portfolio_risk, F)
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.new)

	#--------------------------------------------------------------------------
	# Limit CVaR
	#--------------------------------------------------------------------------
	layout(1)
	plot_ef(ia, list(ef.risk, ef.cdar), portfolio_cdar, F)
	
	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)						
	constraints = add_constraint_cdar(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot_ef(ia, list(ef.risk), portfolio_cdar, F)
	plot_ef(ia, list(ef.risk.new), portfolio_cdar, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.new, ef.risk,ef.cdar), portfolio_cdar, F)	
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.cdar), portfolio_risk, F)
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.new)


	#--------------------------------------------------------------------------
	# Limit both Max Loss and CDaR
	#--------------------------------------------------------------------------
	layout(1:2)
	plot_ef(ia, list(ef.risk, ef.maxloss), portfolio_maxloss, F)
	plot_ef(ia, list(ef.risk, ef.cdar), portfolio_cdar, F)

	# constraints
	constraints = new_constraints(n, lb = 0, ub = 0.8)
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)							
	constraints = add_constraint_maxloss(ia, 15/100, '<=', constraints)		
	constraints = add_constraint_cdar(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk), portfolio_maxloss, F)
	plot_ef(ia, list(ef.risk.new), portfolio_maxloss, F)
	plot_ef(ia, list(ef.risk), portfolio_cdar, F)
	plot_ef(ia, list(ef.risk.new), portfolio_cdar, F)


	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio_maxloss, F)		
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio_cdar, F)	
	plot_ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio_risk, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.new)
	plot_transition_map(ef.maxloss)
	plot_transition_map(ef.cdar)


}


###############################################################################
# Test AA functions to control risk and return at the same time
###############################################################################
aa_control_risk_return_test <- function()
{
	#*****************************************************************
	# Load data
	#******************************************************************	
	tickers = spl('EEM,EFA,GLD,IWM,IYR,QQQ,SPY,TLT')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
	for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt_prep(data, align='keep.all', dates='2012:12::')

	#*****************************************************************
	# Create Input Assumptions
	#******************************************************************	
	prices = data$prices
	n=ncol(prices)

	# make sure that there is no na's in returns; othwerwise MAD will complain
	ret = na.omit(prices/mlag(prices)-1)
	ia = create_historical_ia(ret,252)

	#*****************************************************************
	# Create Efficient Frontier
	#******************************************************************		
	# 0 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)
	constraints = add_constraints(diag(n), type='>=', b=0, constraints)
	constraints = add_constraints(diag(n), type='<=', b=1, constraints)
	 
	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)

	# create efficient frontier
	ef = portopt(ia, constraints, 50, 'Efficient Frontier')

	# plot
	risk.fn = portfolio_risk
	plot_ef(ia, list(ef), risk.fn, transition.map=F)

	#*****************************************************************
	# Plot example portfolios
	#******************************************************************			
	weight = min_var_portfolio(ia,constraints)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='red')
	
	weight = max_sharpe_portfolio()(ia,constraints)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='orange')
	
	weight = max_return_portfolio(ia,constraints)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='green')

	weight = risk_parity_portfolio()(ia,constraints)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='green')

	#*****************************************************************
	# Find portfolio for given return
	#******************************************************************				
	target.return = 24/100	
	constraints1 = add_constraints(ia$expected.return,type='>=', b=target.return, constraints)
	weight = min_var_portfolio(ia,constraints1)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='orange')

	#*****************************************************************
	# Find portfolio for given risk
	#******************************************************************				
	# map between risk and mad
	# plot(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia))
	# approx(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia), 10/100, method='linear')$y
	target.risk = 12/100
	target.mad = approx(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia), target.risk, method='linear')$y
	
	constraints1 = add_constraint_mad(ia, type='<=', value=target.mad, constraints)	
	weight = max_return_portfolio(ia,constraints1)	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='orange')
	
	#*****************************************************************
	# Find portfolio for given return and given risk
	#******************************************************************				
	target.return = 24/100
	target.risk = 12/100
	# map between risk and mad
	# plot(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia))
	# approx(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia), 10/100, method='linear')$y	
	# this is not very precise, so extra adjusment might be necessary
	target.mad = approx(portfolio_risk(ef$weight,ia), portfolio_mad(ef$weight,ia), target.risk, method='linear')$y
	target.mad = target.mad	# - 0.0002

	constraints1 = add_constraints(ia$expected.return,type='>=', b=target.return, constraints)		
	constraints1 = add_constraint_mad(ia, type='>=', value=target.mad, constraints1)	

	f.obj.return = c(ia$expected.return, rep(0, nrow(constraints1$A) - ia$n))	
	f.obj.mad = constraints1$A[, ncol(constraints1$A)]
	weight = lp_obj_portfolio(ia, constraints1, f.obj.return + f.obj.mad )	
	points(100 * risk.fn(weight,ia), 100 * portfolio_return(weight,ia), pch=15, col='orange')
	
		
	# diagnostics
	100 * portfolio_mad(weight, ia)	
	100 * target.mad
	100 * portfolio_risk(weight, ia)	
	100 * portfolio_return(weight, ia)	
}



###############################################################################
# Test AA functions: Solutions to Instability of mean-variance efficient portfolios
# Resampling and Shrinkage
###############################################################################
aa_solutions2instability_test <- function()
{
	#--------------------------------------------------------------------------
	# All methods provide:
	# 1. Better Diversification
	# 2. Efficient Portfolios are immune to small changes in input assumptions
	#--------------------------------------------------------------------------

	#--------------------------------------------------------------------------
	# Create Resampled Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia_rebal()
	n = ia$n		

	# -1 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk', equally.spaced.risk = T)
	ef.risk.resampled = portopt.resampled(ia, constraints, 50, 'Risk Resampled', 
						nsamples = 200, sample.len= 10)	
						
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
									
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(c(1,1,2,3), nrow = 2, byrow=T) )
	plot_ef(ia, list(ef.risk, ef.risk.resampled), portfolio_risk, F)	
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.resampled)
			
dev.off()	

	#--------------------------------------------------------------------------
	# Create Efficient Frontier using Ledoit-Wolf Covariance Shrinkage Estimator from tawny package
	#--------------------------------------------------------------------------
	
	# load / check required packages
	load_packages('tawny')

	ia.original = ia
	
	ia$cov = tawny::cov_shrink(ia$hist.returns)	
	ef.risk.cov_shrink = portopt(ia, constraints, 50, 'Risk Ledoit-Wolf', equally.spaced.risk = T)
		
	ia = ia.original
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(c(1,1,2,3), nrow = 2, byrow=T) )
	plot_ef(ia, list(ef.risk, ef.risk.cov_shrink), portfolio_risk, F)	
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.cov_shrink)
	
dev.off()
	
	#--------------------------------------------------------------------------
	# Create Resampled Efficient Frontier(using Ledoit-Wolf Covariance Shrinkage Estimator)
	# As described on page 8 of
	# Resampling vs. Shrinkage for Benchmarked Managers by M. Wolf (2006)
	#--------------------------------------------------------------------------

	ef.risk.resampled.shrink = portopt.resampled(ia, constraints, 50, 'Risk Ledoit-Wolf+Resampled', 
						nsamples = 200, sample.len= 10, shrinkage.fn=tawny::cov_shrink)	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
												
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(c(1:4), nrow = 2, byrow=T) )
	plot_ef(ia, list(ef.risk, ef.risk.resampled, ef.risk.resampled.shrink), portfolio_risk, F)	
	plot_transition_map(ef.risk)
	plot_transition_map(ef.risk.resampled)
	plot_transition_map(ef.risk.resampled.shrink)

dev.off()	

}

###############################################################################
# Test AA functions, Arithmetic vs Geometric Efficient Frontier
###############################################################################
aa_arithmetic_geometric_test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia_rebal()
	n = ia$n		

	# -1 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Arithmetic', equally.spaced.risk = T)	
	
	# compute historical geometrical returns
	ef.risk.geometric = ef.risk
		ef.risk.geometric$name = 'Geometric'
		ef.risk.geometric$return = portfolio_geometric_return(ef.risk$weight, ia)		
		
		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
				
	# Plot multiple Efficient Frontiers and Transition Maps
	plot_ef(ia, list(ef.risk, ef.risk.geometric), portfolio_risk, T)			

dev.off()	
	


	#--------------------------------------------------------------------------
	# Following DIVERSIFICATION, REBALANCING, AND THE GEOMETRIC MEAN FRONTIER by W. Bernstein and D. Wilkinson (1997)
	# paper's notation : A(1,0) and A(1,1) page 8, 14
	#--------------------------------------------------------------------------
	# A(1,0)
	ef.risk.A10 = ef.risk
		ef.risk.A10$name = 'A(1;0)'
		ef.risk.A10$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 0) )
	# A(1,1)
	ef.risk.A11 = ef.risk
		ef.risk.A11$name = 'A(1;1)'
		ef.risk.A11$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 1) )
							
	# G(1,0)
	ia.G = ia
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm(x[1], x[2], 1, 0) )
	ef.risk.G10 = portopt(ia.G, constraints, 50, 'G(1;0)',equally.spaced.risk = T)	
		ef.risk.G10$return = apply( cbind(ef.risk.G10$return, ef.risk.G10$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 0) )
	# G(1,1)
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm(x[1], x[2], 1, 1) )
	ef.risk.G11 = portopt(ia.G, constraints, 50, 'G(1;1)',equally.spaced.risk = T)	
		ef.risk.G11$return = apply( cbind(ef.risk.G11$return, ef.risk.G11$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 1) )
									
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	# Plot multiple Efficient Frontiers
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A10), portfolio_risk, F)
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A11), portfolio_risk, F)
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G10), portfolio_risk, F)
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G11), portfolio_risk, F)
	
dev.off()		

	#--------------------------------------------------------------------------
	# Use A4 method to convert between Arithmetic and Geometric means
	#--------------------------------------------------------------------------
	# A	
	ef.risk.A4 = ef.risk
		ef.risk.A4$name = 'Risk A4'
		ef.risk.A4$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom4(x[1], x[2]) )

	# G
	ia.G = ia
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm4(x[1], x[2]) )
	ef.risk.G4 = portopt(ia.G, constraints, 50, 'Risk G4',equally.spaced.risk = T)	
		ef.risk.G4$return = apply( cbind(ef.risk.G4$return, ef.risk.G4$risk), 1, 
								function(x) aritm2geom4(x[1], x[2]) )

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
								
	# Plot multiple Efficient Frontiers						
	layout( matrix(1:2, nrow = 2) )
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A4), portfolio_risk, F)
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G4), portfolio_risk, F)

dev.off()		
	
	#--------------------------------------------------------------------------
	# Create True Geometric Efficient Frontier
	#--------------------------------------------------------------------------
	ef.true.geometric = ef.risk
		ef.true.geometric$name = 'True Geometric'
		constraints$x0 = ef.risk$weight[1,]

	for(i in 1:len(ef.risk$risk)) {
		cat('i =', i, '\n')
		ef.true.geometric$weight[i,] = max_geometric_return_portfolio(ia, constraints, ef.risk$risk[i], ef.risk$risk[i])
			constraints$x0 = ef.true.geometric$weight[i,]
	}
	
	ef.true.geometric$return = portfolio_geometric_return(ef.true.geometric$weight, ia)		

png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot multiple Efficient Frontiers						
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk.geometric, ef.risk, ef.true.geometric), portfolio_risk, T, T)			
	plot_ef(ia, list(ef.true.geometric, ef.risk, ef.risk.geometric), portfolio_risk, T, T)			
	
dev.off()			
	
	#--------------------------------------------------------------------------
	# Double check that NonLinear Optimization finds global maximums by
	# creating random portfolios that satisfy constraints. 
	# Plot True Geometric Efficient Frontier and random portfolios, check
	# that all portfolios lie below the efficient frontier.
	#--------------------------------------------------------------------------	
	# Generate random portfolios
	ef.random = list()
		ef.random$name = 'Random'
		ef.random$weight = randfixedsum(100000, n, 1, 0, 1)
		
		ef.random$risk = portfolio_risk(ef.random$weight, ia)		
		ef.random$return = portfolio_geometric_return(ef.random$weight, ia)		
		
		
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot True Geometric Efficient Frontier and random portfolios
	layout(1)
	plot(100*ef.random$risk, 100*ef.random$return, type='p', pch=20,
			xlim = 100*range(ef.random$risk, ef.true.geometric$risk),
			ylim = 100*range(ef.random$return, ef.true.geometric$return),
			main = 'True Geometric Efficient Frontier vs Random Portfolios',
			xlab = 'portfolio_risk',
			ylab = 'Return'			
		)
	lines(100*ef.true.geometric$risk, 100*ef.true.geometric$return, type='l', lwd=2,col = 'red')
		
dev.off()		

	return()	


	# compute Unrebalanced returns
	ef.risk.unrebalanced = ef.risk
		ef.risk.unrebalanced$name = 'Unrebalanced'
		ef.risk.unrebalanced$return = portfolio_unrebalanced_return(ef.risk$weight, ia)			
	plot_ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.unrebalanced), portfolio_risk, T)			
	
	
	# To check that Geometric returns are not additive, feed geometric.returns to optimizer
	# and observe resulting frontier below the True Geometric frontier
	ia.G = ia
	ia.G$expected.return = ia$geometric.return
	ef.risk.geometric1 = portopt(ia.G, constraints, 50, 'Geometric1',equally.spaced.risk = T)	
	plot_ef(ia, list(ef.risk, ef.risk.geometric,ef.risk.geometric1), portfolio_risk, T)			

	
	# Find maximum Geometric Mean portfolio	
	x=max_geometric_return_portfolio(ia, constraints, 0, 1)
	lines( portfolio_risk(t(x), ia), portfolio_geometric_return(t(x), ia), type='p', pch=20, col = 'blue')
}

###############################################################################
# Test AA functions, Periodic table
# Construct Periodic table, like in Single Country Index Returns
# http://us.ishares.com/content/stream.jsp?url=/content/en_us/repository/resource/single_country_periodic_table.pdf&mimeType=application/pdf
###############################################################################
aa_periodic_table_test <- function()
{
	#--------------------------------------------------------------------------
	# Get Historical Data
	#--------------------------------------------------------------------------
	# Country IA are based on monthly data
	ia = aa_test_create_ia_country('1990::')
		hist.returns = ia$hist.returns
		
	# convert returns to prices
	hist.prices = cumprod(1 + hist.returns)
	
	# extract annual prices
	period.ends = endpoints(hist.prices, 'years')
		hist.prices = hist.prices[period.ends, ]
		
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
		hist.returns = hist.returns['2000::']

				
	#--------------------------------------------------------------------------
	# Create Periodic table
	#--------------------------------------------------------------------------

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	plot_periodic_table1(hist.returns)
	
dev.off()			
			
	#--------------------------------------------------------------------------
	# Create Periodic table, another version
	#--------------------------------------------------------------------------
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	plot_periodic_table2(hist.returns)
	
dev.off()			

}

###############################################################################
# Test AA functions, Decompose Manager's Style
###############################################################################
#--------------------------------------------------------------------------
# Style Summary Plot
#--------------------------------------------------------------------------
aa_style_summary_plot <- function(name, style.weights, style.r.squared, window.len)
{
	layout( matrix(c(1,2,2,3,3,3), nrow=2, byrow=T) )
	
	#Latest weights	
	weight = last(style.weights)
	plot_table(t(round(100*weight)))
	
	# R2
	plota(100*style.r.squared, type='l', LeftMargin = 3, main=paste(window.len, 'months window Linear Least Squares Regression R^2'))
		
	# Style History
	plot_transition_map(style.weights, index(style.weights), xlab='', name=name)
}	
	

aa_style_test <- function()
{

	#--------------------------------------------------------------------------
	# Get Historical Data
	#--------------------------------------------------------------------------
	load_packages('quantmod')

	# load historical prices from Yahoo Finance
	symbols = spl('FMILX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')	
	symbols = spl('FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')	
	
	symbol.names = spl('Fund,Australia,Canada,France,Germany,Japan,UK,USA')
	
	getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
			
	# align dates for all symbols & convert to frequency 
	hist.prices = merge(FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY)		
		period.ends = endpoints(hist.prices, 'months')
		hist.prices = Ad(hist.prices)[period.ends, ]
		
		index(hist.prices) = as.Date(paste('1/', format(index(hist.prices), '%m/%Y'), sep=''), '%d/%m/%Y')
		colnames(hist.prices) = symbol.names
	
	# remove any missing data	
	hist.prices = na.omit(hist.prices['1990::2010'])
	
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
		
	#load 3-Month Treasury Bill from FRED
	TB3M = quantmod::getSymbols('TB3MS', src='FRED', auto.assign = FALSE)	
	TB3M = processTBill(TB3M, timetomaturity = 1/4)
		index(TB3M) = as.Date(paste('1/', format(index(TB3M), '%m/%Y'), sep=''), '%d/%m/%Y')
		TB3M = ROC(Ad(TB3M), type = 'discrete')
		colnames(TB3M) = 'Cash'
		
	hist.returns = na.omit( merge(hist.returns, TB3M) )

	#--------------------------------------------------------------------------
	# Style Regression  over 36 Month window, unconstrainted
	#--------------------------------------------------------------------------
	# setup
	ndates = nrow(hist.returns)
	n = ncol(hist.returns)-1
	window.len = 36
		
	style.weights = hist.returns[, -1]
		style.weights[] = NA
	style.r.squared = hist.returns[, 1]
		style.r.squared[] = NA
	
	# main loop
	for( i in window.len:ndates ) {
		window.index = (i - window.len + 1) : i
		
		fit = lm_constraint( hist.returns[window.index, -1], hist.returns[window.index, 1] )	
			style.weights[i,] = fit$coefficients
			style.r.squared[i,] = fit$r.squared
	}
 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	aa_style_summary_plot('Style UnConstrained', style.weights, style.r.squared, window.len)

dev.off()	

			
	#--------------------------------------------------------------------------
	# Style Regression  over Window, constrainted
	#--------------------------------------------------------------------------
	# setup
	load_packages('quadprog')

	style.weights[] = NA
	style.r.squared[] = NA

	# Setup constraints
	# 0 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# main loop
	for( i in window.len:ndates ) {
		window.index = (i - window.len + 1) : i
		
		fit = lm_constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )	
			style.weights[i,] = fit$coefficients
			style.r.squared[i,] = fit$r.squared
	}
 	
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	aa_style_summary_plot('Style Constrained', style.weights, style.r.squared, window.len)
	
dev.off()			

	#--------------------------------------------------------------------------
	# Style Regression  over Window, constrained + limits on allocation
	#--------------------------------------------------------------------------
	# setup
	style.weights[] = NA
	style.r.squared[] = NA

	# Setup constraints
	temp = rep(0, n)
		names(temp) = colnames(hist.returns)[-1]
	lb = temp
	ub = temp
	ub[] = 1
			
	lb['Australia'] = 0
	ub['Australia'] = 5

	lb['Canada'] = 0
	ub['Canada'] = 5
		
	lb['France'] = 0
	ub['France'] = 15

	lb['Germany'] = 0
	ub['Germany'] = 15

   	lb['Japan'] = 0
	ub['Japan'] = 15

   	lb['UK'] = 0
	ub['UK'] = 25
	
   	lb['USA'] = 30
	ub['USA'] = 100
	     
   	lb['Cash'] = 2
	ub['Cash'] = 15
       
	# 0 <= x.i <= 1
	constraints = new_constraints(n, lb = lb/100, ub = ub/100)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# main loop
	for( i in window.len:ndates ) {
		window.index = (i - window.len + 1) : i
		
		fit = lm_constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )	
			style.weights[i,] = fit$coefficients
			style.r.squared[i,] = fit$r.squared
	}
 	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	aa_style_summary_plot('Style Constrained+Limits', style.weights, style.r.squared, window.len)

dev.off()		

	#--------------------------------------------------------------------------
	# Look at Manager's Tracking Error
	#--------------------------------------------------------------------------
	manager.returns = hist.returns[, 1]
		manager.returns = manager.returns[window.len:ndates,]
	implied.returns = as.xts( rowSums(style.weights * hist.returns[, -1]), index(hist.returns))
		implied.returns = implied.returns[window.len:ndates,]

	tracking.error = manager.returns - implied.returns
	alpha = 12*mean(tracking.error)
	covar.alpha = 12* cov(tracking.error)
		
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
			
	layout(1:2)
	plota(cumprod(1+manager.returns), type='l')
		plota_lines(cumprod(1+implied.returns), col='red')
		plota_legend('Fund,Style', 'black,red')
			
	par(mar = c(4,4,2,1))
	hist(100*tracking.error, xlab='Monthly Tracking Error',
		main= paste('Annualized Alpha =', round(100*alpha,1), 'Std Dev =', round(100*sqrt(covar.alpha),1))
	)
	
dev.off()		

	
	# Biulding Managers IA to create Efficient Frontier
	# For error calculations we can either use most recent window or full sample
	# error = managers.hist.returns - style %*% t(assets.hist.returns)
	# managers.alpha = 12 * mean(error)
	# managers.covar.alpha = 12 * cov(error)	
	# 
	# Long-term component + Short-term component
	# managers.expected.return = style %*% t(assets.expected.return) + managers.alpha
	# managers.cov = style %*% assets.covar %*% t(style) + managers.covar.alpha

}


###############################################################################
# Test AA functions, Black-Litterman model
###############################################################################
aa_black_litterman_test <- function()
{
	#--------------------------------------------------------------------------
	# Visualize Market Capitalization History
	#--------------------------------------------------------------------------

	hist.caps = aa_test_hist_capitalization()	
	hist.caps.weight = hist.caps/rowSums(hist.caps)
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	plot_transition_map(hist.caps.weight, index(hist.caps.weight), xlab='', name='Market Capitalization Weight History')

dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		
	layout( matrix(1:9, nrow = 3, byrow=T) )
	col = plota_colors(ncol(hist.caps))
	for(i in 1:ncol(hist.caps)) {
		plota(hist.caps[,i], type='l', lwd=5, col=col[i], main=colnames(hist.caps)[i])
	}

dev.off()	

			

	#--------------------------------------------------------------------------
	# Compute Risk Aversion, prepare Black-Litterman input assumptions
	#--------------------------------------------------------------------------
	ia = aa_test_create_ia_country()
	
	ir = get_fedfunds_rate()	
		period = join( format(range(index(ia$hist.returns)), '%Y:%m'), '::')
	
	# The implied risk aversion coefficient can be estimated by dividing
	# the expected excess return by the variance of the portfolio
	risk.aversion = bl_compute_risk_aversion( ia$hist.returns$USA, ir[period]/ia$annual.factor )
	risk.aversion = bl_compute_risk_aversion( ia$hist.returns$USA )

	# the latest weights
	cap.weight = last(hist.caps.weight)	
			
	ia.bl = ia
	ia.bl$expected.return = bl_compute_eqret( risk.aversion, ia$cov, cap.weight, last(ir[period]) )
	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	layout( matrix(c(1,1,2,3), nrow=2, byrow=T) )
	pie(coredata(cap.weight), paste(colnames(cap.weight), round(100*cap.weight), '%'), 
		main = paste('Country Market Capitalization Weights for', format(last(index(ia$hist.returns)),'%b %Y'))
		, col=plota_colors(ia$n))
	
	plot_ia(ia.bl, T)
		
dev.off()	
	
	#--------------------------------------------------------------------------
	# Create Efficient Frontier(s)
	#--------------------------------------------------------------------------
	n = ia$n
	
	# -1 <= x.i <= 1
	constraints = new_constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add_constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Historical', equally.spaced.risk = T)		
	ef.risk.bl = portopt(ia.bl, constraints, 50, 'Black-Litterman', equally.spaced.risk = T)	

png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia, list(ef.risk), portfolio_risk, T, T)			
	plot_ef(ia.bl, list(ef.risk.bl), portfolio_risk, T, T)			
	
dev.off()
	
	#--------------------------------------------------------------------------
	# Create Views
	#--------------------------------------------------------------------------
	temp = matrix(rep(0, n), nrow = 1)
		colnames(temp) = ia$symbols
		
	# Relative View
	# Japan will outperform UK by 2%
	temp[,'Japan'] = 1
	temp[,'UK'] = -1

	pmat = temp
	qmat = c(0.02)
	
	# Absolute View
	# Australia's expected return is 12%
	temp[] = 0
	temp[,'Australia'] = 1
	
	pmat = rbind(pmat, temp)	
	qmat = c(qmat, 0.12)

	# compute posterior distribution parameters
	post = bl_compute_posterior(ia.bl$expected.return, ia$cov, pmat, qmat, tau = 0.025 )
	#bl_compute_optimal(risk.aversion, post$expected.return, post$cov)

	# create Black-Litterman input assumptions with Views	
	ia.bl.view = ia.bl
		ia.bl.view$expected.return = post$expected.return
		ia.bl.view$cov = post$cov
		ia.bl.view$risk = sqrt(diag(ia.bl.view$cov))
		
	# create efficient frontier(s)
	ef.risk.bl.view = portopt(ia.bl.view, constraints, 50, 'Black-Litterman + View(s)', equally.spaced.risk = T)	

png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot_ef(ia.bl, list(ef.risk.bl), portfolio_risk, T, T)			
	plot_ef(ia.bl.view, list(ef.risk.bl.view), portfolio_risk, T, T)			
	
dev.off()	
				
}


###############################################################################
# Historical Country Capitalizations from worldbank.org
# Select Countries, Series. Type in "capitalization" and select Years
# http://databank.worldbank.org/ddp/home.do?Step=12&id=4&CNO=2
#
# Alternative Source : "World Federation of Exchanges"
# http://www.world-exchanges.org/statistics/time-series
#
# How to invest in the world with a few ETFs _ Decision Science News
# http://www.decisionsciencenews.com/2011/12/29/youve-got-the-whole-world-in-your-portfolio/
###############################################################################
aa_test_hist_capitalization <- function()
{
	symbols = spl('Australia	Canada	France	Germany	Japan	United Kingdom	United States', '\t')
	
	# Market capitalization of listed companies (current US$) in 1,000,000,000	
	data = 
'1988	138.0	242.0	245.0	252.0	3910.0	771.0	2790.0
1989	141.0	291.0	365.0	365.0	4390.0	827.0	3510.0
1990	109.0	242.0	314.0	355.0	2920.0	849.0	3060.0
1991	149.0	267.0	348.0	393.0	3130.0	988.0	4090.0
1992	145.0	243.0	351.0	348.0	2400.0	927.0	4490.0
1993	204.9	326.5	456.1	463.5	2999.8	1151.6	5136.2
1994	218.9	315.0	451.3	470.5	3719.9	1210.2	5067.0
1995	245.2	366.3	522.1	577.4	3667.3	1407.7	6857.6
1996	312.0	486.3	591.1	671.0	3088.9	1740.2	8484.4
1997	295.8	567.6	674.4	825.2	2216.7	1996.2	11308.8
1998	328.9	543.4	991.5	1094.0	2495.8	2374.3	13451.4
1999	427.7	800.9	1475.5	1432.2	4546.9	2933.3	16635.1
2000	372.8	841.4	1446.6	1270.2	3157.2	2577.0	15104.0
2001	375.1	700.8	1174.4	1071.7	2251.8	2164.7	13854.6
2002	378.8	575.3	967.0	691.1	2126.1	1864.3	11098.1
2003	585.5	894.0	1355.9	1079.0	3040.7	2460.1	14266.3
2004	776.4	1177.5	1559.1	1194.5	3678.3	2815.9	16323.7
2005	804.1	1480.9	1758.7	1221.3	4736.5	3058.2	16970.9
2006	1095.9	1700.7	2428.6	1637.8	4726.3	3794.3	19425.9
2007	1298.4	2186.6	2771.2	2105.5	4453.5	3858.5	19947.3
2008	675.6	1002.2	1492.3	1108.0	3220.5	1852.0	11737.6
2009	1258.5	1681.0	1972.0	1297.6	3377.9	2796.4	15077.3
2010	1454.5	2160.2	1926.5	1429.7	4099.6	3107.0	17139.0'
	
	hist.caps = matrix( as.double(spl( gsub('\n', '\t', data), '\t')), 
				nrow = len(spl(data, '\n')), byrow=TRUE)
				
				
	load_packages('quantmod')
	symbol.names = symbols
	
	hist.caps = as.xts( hist.caps[,-1] , 
							as.Date(paste('1/1/', hist.caps[,1], sep=''), '%d/%m/%Y')
						) 
	colnames(hist.caps) = symbols
	
	return(hist.caps)

}


# Get Monthly Federal funds rate from http://www.federalreserve.gov/releases/h15/data.htm
get_fedfunds_rate <- function()
{
	# download Monthly History of Fed Funds rates
	url = 'http://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=40afb80a445c5903ca2c4888e40f3f1f&lastObs=&from=&to=&filetype=csv&label=include&layout=seriescolumn'
	txt = readLines(url)

	txt = txt[-c(1 : grep('Time Period', txt))]
	hist.returns = matrix( spl(txt), nrow = len(txt), byrow=TRUE)
	
	load_packages('quantmod')
	
	hist.returns = as.xts( as.double(hist.returns[,-1]) / 100, 
							as.Date(paste(hist.returns[,1], '-1', sep=''), '%Y-%m-%d')
						) 
						
	return(hist.returns)
}



aa_test_create_ia_country <- function(dates = '1990::2010')
{
	#--------------------------------------------------------------------------
	# Load historical prices and compute simple returns
	#--------------------------------------------------------------------------
	load_packages('quantmod,quadprog')

	# load historical prices from Yahoo Finance
	symbols = spl('EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')	
	symbol.names = spl('Australia,Canada,France,Germany,Japan,UK,USA')
	
	getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
			
	# align dates for all symbols & convert to frequency 
	hist.prices = merge(EWA,EWC,EWQ,EWG,EWJ,EWU,SPY)		
		period.ends = endpoints(hist.prices, 'months')
		hist.prices = Ad(hist.prices)[period.ends, ]
		colnames(hist.prices) = symbol.names
	annual.factor = 12
	
	# remove any missing data	
	hist.prices = na.omit(hist.prices[dates])
	
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
	ia = create_historical_ia(hist.returns, annual.factor)
	
	return(ia)	
}




###############################################################################
# Create Input Assumptions used in 
# DIVERSIFICATION, REBALANCING, AND THE GEOMETRIC MEAN FRONTIER by W. Bernstein and D. Wilkinson (1997)
# www.effisols.com/basics/rebal.pdf
###############################################################################
aa_test_create_ia_rebal <- function()
{
	symbols = spl('SP500	SmallUS   	Europe	Pacific	Japan  	Gold   	20Y_Treas	5Y_Treas	TBills', '\t')
		symbols = trim(symbols)
	
	data = 
'1970	0.0403	-0.1743	-0.0935	-0.13	-0.156	0.0871	0.121	0.1685	0.0652
1971	0.1432	0.165	0.2803	0.1082	0.6107	-0.0373	0.1324	0.0874	0.0439
1972	0.1898	0.0443	0.1582	0.6678	1.1447	0.602	0.0567	0.0517	0.0384
1973	-0.1466	-0.309	-0.0773	-0.2392	-0.1595	0.9184	-0.011	0.0461	0.0693
1974	-0.2647	-0.1995	-0.2277	-0.4059	-0.1392	0.1094	0.0435	0.0568	0.0801
1975	0.372	0.5282	0.439	0.6342	0.1723	-0.2407	0.0919	0.0782	0.058
1976	0.2384	0.5738	-0.0637	0.0572	0.2637	-0.3258	0.1676	0.1288	0.0508
1977	-0.0718	0.2538	0.2392	0.0334	0.1722	0.3549	-0.0065	0.014	0.0513
1978	0.0656	0.2346	0.243	0.2397	0.5182	0.0934	-0.0118	0.0349	0.072
1979	0.1844	0.4346	0.1467	0.5216	-0.1461	1.6133	-0.0121	0.041	0.1038
1980	0.3242	0.3988	0.1452	0.6149	0.2939	0.6427	-0.0396	0.039	0.1126
1981	-0.0491	0.1388	-0.1045	-0.1547	0.1041	-0.2514	0.0186	0.0944	0.1472
1982	0.2141	0.2801	0.0569	-0.2818	-0.0023	0.4786	0.4037	0.291	0.1053
1983	0.2251	0.3967	0.2238	0.3421	0.2779	0.0259	0.0069	0.0741	0.088
1984	0.0623	-0.0667	0.0126	-0.0724	0.1701	0.2922	0.1554	0.1403	0.0978
1985	0.3216	0.2466	0.7979	0.1729	0.4413	-0.0887	0.3096	0.2034	0.0773
1986	0.1847	0.0685	0.4446	0.4839	0.9185	0.3593	0.2445	0.1513	0.0615
1987	0.0523	-0.093	0.041	0.042	0.4187	0.3753	-0.027	0.029	0.0546
1988	0.1681	0.2287	0.1635	0.3056	0.3534	-0.1846	0.0968	0.0609	0.0636
1989	0.3149	0.1018	0.2906	0.1585	0.0217	0.2538	0.181	0.1327	0.0838
1990	-0.0317	-0.2156	-0.0337	-0.1015	-0.3618	-0.2373	0.062	0.0974	0.0782
1991	0.3055	0.4463	0.1366	0.3661	0.0882	-0.042	0.1926	0.1531	0.056
1992	0.0766	0.2335	-0.0425	0.0701	-0.2111	-0.1598	0.0941	0.072	0.0351
1993	0.099	0.21	0.2979	0.8035	0.2505	0.8287	0.1824	0.1124	0.029
1994	0.012	0.031	0.0266	-0.141	0.2217	-0.1193	-0.0778	-0.0513	0.0391
1995	0.3753	0.3448	0.2213	0.1295	0.0069	0.0191	0.3069	0.1905	0.0551
1996	0.2295	0.1765	0.2895	0.2054	-0.155	0.0706	-0.0127	0.0661	0.0502'

	hist.returns = matrix( as.double(spl( gsub('\n', '\t', data), '\t')), 
				nrow = len(spl(data, '\n')), byrow=TRUE)
				
				
	load_packages('quantmod')
	
	hist.returns = as.xts( hist.returns[,-1] , 
							as.Date(paste('1/1/', hist.returns[,1], sep=''), '%d/%m/%Y')
						) 
	colnames(hist.returns) = symbols
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
	ia = create_historical_ia(hist.returns, 1, symbols)
	
	return(ia)
}



###############################################################################
# Create Input Assumptions used in aa_test functions
###############################################################################
aa_test_create_ia <- function()
{
	#--------------------------------------------------------------------------
	# Load historical prices and compute simple returns
	#--------------------------------------------------------------------------
	load_packages('quantmod,quadprog')

	# load historical prices from Yahoo Finance
	symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')	
	symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold')
	
	getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
			
	# align dates for all symbols & convert to monthly 
	hist.prices = merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)		
		month.ends = endpoints(hist.prices, 'months')
		hist.prices = Ad(hist.prices)[month.ends, ]
		colnames(hist.prices) = symbols
		
	# remove any missing data	
	hist.prices = na.omit(hist.prices['1995::2010'])
	
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
	ia = create_historical_ia(hist.returns, 12, symbols, symbol.names)
	
	return(ia)	
}


###############################################################################
# Create Historical Input Assumptions given symbols and dates
###############################################################################
aa_test_create_ia_custom <- function(symbols, symbol.names = symbols, dates = NULL)
{
	#--------------------------------------------------------------------------
	# Load historical prices and compute simple returns
	#--------------------------------------------------------------------------
	load_packages('quantmod,quadprog')

	data <- new.env()
	getSymbols(symbols, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt_prep(data, align='remove.na', dates=dates)

	# convert to monthly frequency 
	hist.prices = data$prices
		period.ends = endpoints(hist.prices, 'months')
		hist.prices = hist.prices[period.ends, ]
		colnames(hist.prices) = symbol.names
	annual.factor = 12
	
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
	ia = create_historical_ia(hist.returns, annual.factor, symbol.names, symbol.names)
	
	return(ia)	
}




###############################################################################
# Add short (negative copy) input assumptions to given ia
###############################################################################
aa_test_ia_add_short <- function(ia)
{
	ia$symbols = c(ia$symbols,ia$symbols)
	ia$n = 2*ia$n
	ia$hist.returns = cbind(ia$hist.returns, -ia$hist.returns)
	
	ia$expected.return = c(ia$expected.return, -ia$expected.return)
	ia$risk = c(ia$risk, ia$risk)

	# Transfrom correlation & covariance
	# | cov -cov |
	# |-cov  cov |		
	ia$correlation = cbind( rbind(ia$correlation, -ia$correlation), rbind(-ia$correlation, ia$correlation) )	
	ia$cov = cbind( rbind(ia$cov, -ia$cov), rbind(-ia$cov, ia$cov) )
	
	return(ia)
}


	
