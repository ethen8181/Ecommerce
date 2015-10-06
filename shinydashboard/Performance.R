# Function for tabName = "Performance"

# @date  : pass in the date character in the format of YYYY-MM-DD
# @url   : the url for the given webpage, please exclude the host name of the page
# @metric: or so called the measure to indicate the performance 
PerformanceData <- function( date, metric, url, table.id )
{
	GoogleQuery <- function( table.id )
	{
		query_list <- Init( start.date  = date,
	                    	end.date    = date,
		                    dimensions  = "ga:hour",
		                    metrics     = metric,
		                    filter      = paste0( "ga:pagePath==", url ),
		                    table.id    = paste0( "ga:", table.id ) )

		# Create the Query Builder object so that the query parameters are validated
		# Extract the data and store it in a data-frame
		ga_data <- GetReportData( QueryBuilder(query_list), token )		
	}

	# the second table.id does not have record before 2015.9.4
	# querying it will lead to error 
	if( as.Date(date) >= add_date )
	{	
		data_list <- lapply( table.id, function(x)
		{
			GoogleQuery( table.id = x )
		})
		data <- do.call( rbind, data_list )

		# all different measurement column will be assign with the name metric 
		final <- cbind( date, aggregate( list( metric = data[,2] ), list( hour = data$hour ), FUN = sum ) )
	}else
	{
		final <- cbind( date, GoogleQuery( table.id = table.id[1] ) )
		setnames( final, names(final)[3], "metric" )
	}
	return(final)
}

# ---------------------------------------------------------------------------------------------
# testing code for Performance - uniqueview

# add_date <- as.Date("2015-09-04")
# date <- "2015-09-01"
# metric <- "ga:uniquePageviews"
# testing url 
# url <- "/p/E130119WNF000750"

# before <- PerformanceData( "2015-09-22", metric, url, table.id )
# after  <- PerformanceData( "2015-09-28", metric, url, table.id )
# data   <- rbind( after, before )

# test <- t.test( metric ~ date, data = data, paired = TRUE )

# if there're no data to compare, the p.value will return as nan ( not a number )
# if( !is.nan(test$p.value) )
# {	
# 	if( test$p.value < .05 )
# 	{
# 		if( test$estimate > 0 )
# 	 	{
# 	 		string <- "Good job, showing improvements!!"
# 	 	}else
# 		    string <- "The Performance is decreasing ....."	
# 	}else
# 	    string <- "Can Do Better ^^"	
# 
# 	summary <- tapply( data$metric, data$date, sum )
# 	title <- paste0( "Total UU = ", names(summary[1]), ":  ", summary[1], ",  ", 
# 	                                names(summary[2]), ":  ", summary[2] )
# 
# 	chart_title <- substitute( atop( string, italic(title) ), 
# 				               list( string = string,
# 				                     title  = title  ) )	              
# }
# 
# plot <- ggplot( data, aes( hour, metric, group = date, color = date ) ) +
#         geom_line() + 
#         geom_point( size = 3 ) +
#         scale_color_economist() + 
#         theme_economist()
# 
# if( !is.nan(test$p.value) )
# 	plot <- plot + ggtitle( as.expression(chart_title) )

# ---------------------------------------------------------------------------------------------

# "ga:pageviews,ga:uniquePageviews,ga:bounceRate,ga:exitRate,ga:avgSessionDuration"
# "ga:transactionsPerSession"

        




