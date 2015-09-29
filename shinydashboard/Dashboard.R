
# Function for tabName = "Dashboard"

# @date = pass in the date to obtain the total revenue, total UU (uniquepageviews) and avg. session duration 
DailyData <- function( date )
{
	query_list <- Init( start.date  = date,
		                end.date    = date,
		                metrics     = "ga:transactionRevenue,ga:uniquePageviews,ga:avgSessionDuration",
		                max.results = 10000,
		                table.id    = paste0( "ga:", table.id ) )

	# Create the Query Builder object so that the query parameters are validated
	# Extract the data and store it in a data-frame
	# split_daywise = TRUE
	ga_data <- GetReportData( QueryBuilder(query_list), token )
}

# date <- "2015-09-01"
# daily_data <- DailyData("2015-09-01")


