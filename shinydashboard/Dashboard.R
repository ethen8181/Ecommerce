# Function for tabName = "Dashboard"
library(dplyr)

# @date = pass in the date to obtain the total revenue, total UU (uniquepageviews) and avg. session duration 
DailyData <- function( date, table.id )
{
	GoogleQuery <- function( table.id )
	{
		query_list <- Init( start.date  = date,
			                end.date    = date,
			                metrics     = "ga:transactionRevenue,ga:uniquePageviews,ga:avgSessionDuration",
			                max.results = 10000,
			                table.id    = paste0( "ga:", table.id ) )

		# Create the Query Builder object so that the query parameters are validated
		# Extract the data and store it in a data-frame
		ga_data <- GetReportData( QueryBuilder(query_list), token )
	}	
	
	if( as.Date(date) >= as.Date("2015-09-04") )
	{
		data_list <- lapply( table.id, function(x)
		{
			GoogleQuery(x)
		})
		data <- do.call( rbind, data_list )

		final <- data.frame( transactionRevenue = sum(data$transactionRevenue),
							 uniquePageviews    = sum(data$uniquePageviews),
							 avgSessionDuration = mean(data$avgSessionDuration) )
	}else
		final <- GoogleQuery(table.id[1])
	return(final)
}

# date <- "2015-09-22"
# daily_data <- DailyData( date, table.id )


## not used yet !!!!!!!!!
DailySource <- function( date )
{
	query_list <- Init( start.date  = date,
		                end.date    = date,
		                dimensions  = "ga:sourceMedium",
		                metrics     = "ga:transactionRevenue,ga:uniquePageviews",
		                max.results = 10000,
		                table.id    = paste0( "ga:", table.id ) )

	ga_data <- GetReportData( QueryBuilder(query_list), token )	
}


# ---------------------------------------------------------------------------------------------
# start from here 

MonthlyData <- function( start.date, end.date, table.id )
{
	if( as.Date("2015-09-04") )

	data_list <- lapply( table.id, function(x)
	{
		
	})
	data <- do.call( rbind, data_list ) 
}

GoogleQuery <- function( table.id )
{
	query_list <- Init( start.date  = start.date,
			            end.date    = end.date,
			            metrics     = "ga:transactionRevenue,ga:transactions,ga:uniquePageviews,ga:avgSessionDuration",
			            max.results = 10000,
			            table.id    = paste0( "ga:", table.id ) )

	ga_data <- GetReportData( QueryBuilder(query_list), token, split_daywise = TRUE )				
}

library(dygraphs)
library(xts)
start.date <- floor_date( Sys.Date() - 1, "month" )
end.date <- Sys.Date() - 1

# !!!!! bug, no data for the second table.id 
monthly_data <- MonthlyData( start.date, end.date, table.id )
day <- seq( start.date, end.date, "day" )

test1 <- ga_data
test2 <- cbind( day, test1 )

ggplot( test2, aes( day, transactionRevenue ) ) + geom_line()


dygraph( (xts(test2[,2], day ) )


