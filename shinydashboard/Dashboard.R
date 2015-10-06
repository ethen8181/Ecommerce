# Function for tabName = "Dashboard"

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
	
	if( as.Date(date) >= add_date )
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

# -------------------------------------------------------------------------------
# testing code
# add_date <- as.Date("2015-09-04")
# date <- "2015-10-01"
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

MonthlyData <- function( start.date, end.date, table.id )
{
	GoogleQuery <- function( start.date, end.date, table.id )
	{
		query_list <- Init( start.date  = start.date,
				            end.date    = end.date,
				            metrics     = "ga:transactionRevenue,ga:transactions,ga:uniquePageviews,ga:avgSessionDuration",
				            max.results = 10000,
				            table.id    = paste0( "ga:", table.id ) )

		# split_daywise = TRUE returns the data in decreasing order,
		# with the latest date in the first row !!!!!!!!
		ga_data <- GetReportData( QueryBuilder(query_list), token, split_daywise = TRUE )				
	}

	if( month(start.date) == month(end.date) )
	{
		days <- seq( start.date, end.date, "day" )

		# if the end.date happened before add_date query the first table.id
		# if the add_date is in the interval of the start and end date,
		# query the data separately  
		# else query both table.id
		if( as.Date(end.date) < add_date )
		{
			final <- cbind( days = rev(days), 
				            GoogleQuery( start.date, end.date, table.id[1] ) )

		}else if( add_date %within% as.interval( ymd(start.date), ymd(end.date) ) )
		{
			# midpoint for separating the days	
			mid <- as.numeric( ( add_date - 1 ) - start.date, units = "days" ) + 1

			# data1 : start.date to one day before the add_date, and only the first table.id
			data1 <- cbind( days = days[ mid:1 ], 
				            GoogleQuery( start.date, add_date - 1, table.id[1] ) )

			# data2 : add_date to end date and both table.id 
			data_list <- lapply( table.id, function(x)
			{			
				cbind( days = days[ length(days):(mid+1) ], 
				       GoogleQuery( add_date, end.date, x ) )
			})
			data2 <- data.table( do.call( rbind, data_list ) )
			
			data2 <- data2[ , .( transactionRevenue = sum(transactionRevenue),
					             transactions       = sum(transactions),
					             uniquePageviews    = sum(uniquePageviews),
					             avgSessionDuration = mean(avgSessionDuration) ), by = days ]

			# combine the two dataset
			final <- rbind( data1, data2 )
			final <- final[ order(final$days, decreasing = TRUE ), ]
		}else 
		{
			data_list <- lapply( table.id, function(x)
			{			
				cbind( days = rev(days), GoogleQuery( start.date, end.date, x ) )
			})
			data <- data.table( do.call( rbind, data_list ) )

			# combine the dataset, be sure to take the mean for avg.session
			final <- data[ , .( transactionRevenue = sum(transactionRevenue),
					            transactions       = sum(transactions),
					            uniquePageviews    = sum(uniquePageviews),
					            avgSessionDuration = mean(avgSessionDuration) ), by = days ]
		}	
	}else
		final <- NA # fix ?? 

	return(final)
}

# -------------------------------------------------------------------------
# testing code

start.date1 <- floor_date( Sys.Date() - 1, "month" )
end.date1   <- Sys.Date() - 1

start.date2 <- as.Date("2015-09-01")
end.date2   <- as.Date("2015-09-06")

start.date2 <- floor_date( rollback(end.date1), "month" )
end.date2   <- rollback(end.date1)


# column order : transactionRevenue, transaction, uniquePageviews, avgSessionDuration
monthly_data1 <- MonthlyData( start.date1, end.date1, table.id )
monthly_data2 <- MonthlyData( start.date2, end.date2, table.id )

# buggy ?? start from here 
names1 <- with( monthly_data1[1], paste0( year(days), "_", month(days) ) )

monthly_transactionRevenue1 <- data.frame( day = day(monthly_data1$days) )
monthly_transactionRevenue1[names1] <- monthly_data1$transactionRevenue

names2 <- with( monthly_data2[1], paste0( year(days), "_", month(days) ) )

monthly_transactionRevenue2 <- data.frame( day = day(monthly_data2$days) )
monthly_transactionRevenue2[names2] <- monthly_data2$transactionRevenue

monthly_transactionRevenue <- merge( monthly_transactionRevenue1, monthly_transactionRevenue2, all = TRUE )


ggplot( melt( monthly_transactionRevenue, id.vars = "day" ), aes( day, value, color = variable ) ) + 
geom_line( size = 1 ) + 
geom_point( size = 3 )





