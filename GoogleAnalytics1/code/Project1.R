library(grid)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(data.table)

# environment setting 
rm( list = ls() )
Sys.setlocale( "LC_TIME", "English" )
setwd("C:/Users/ASUS/Ecommerce/GoogleAnalytics1")

# base data Analytics.csv
data <- data.table( read.csv( "Analytics.csv", stringsAsFactors = FALSE, 
                              colClasses = c( rep( "character", 2 ), rep( "numeric", 5 ) ) ) )

# -------------------------------------------------------------------------------------------
# the first 9 rows are meaningless and blank lines are seen as NA value
# rename the column name of the new data 
newdata <- data.table( read.csv( file.choose(), stringsAsFactors = FALSE, 
                                 na.strings = "", skip = 9, header = FALSE ) )

newdata <- newdata[ complete.cases(newdata), ]
setnames( newdata, names(data) )

# preprocess the Revenue, Tax, ShippingFee and Refund column : convert them to numeric  
columns <- which ( names(data) %in% c( "Revenue", "Tax", "ShippingFee", "Refund" ) )
# define the preprocessing function
preprocess <- function(x)
{
    string <- str_extract( x, "([:digit:]+\\,)*([:digit:]+)" )
    as.numeric( gsub( "\\,", "", string ) )
}
newdata <- newdata %>% modifyList( lapply( newdata[ , columns, with = FALSE ], preprocess ) )

# combine the new and original dataset
# use unique to prevent accidently loading in data of the same day 
data <- unique( rbind( data, newdata ) )

# extract date from the transaction code 
date <- gsub( "ON[0-9]{5}([0-9]{6}).*", "20\\1", data$TransactionCode )
data$Date <- as.Date( date, format = "%Y%m%d" )

# ------------------------------------------------------------------------------------------
# categorize the MediaSource
sourcecategory <- read.csv( "code/sourcecategory.csv", stringsAsFactors = FALSE )
# remove possible whitespace in the beginning and end of source before matching 
data$AggregatedSource <- sourcecategory$aggregatecategory[ match(data$MediaSource, str_trim(sourcecategory$sources) ) ]
# all the ones that are not matched are categorized as all other
data$AggregatedSource[ is.na(data$AggregatedSource) ] <- "all other"

# ------------------------------------------------------------------------------------------
# count the number of transaction from different sources in each day
# use table so that days that have zero transcation from source will also show up 
countSource <- data.table( with( data, table( Date, AggregatedSource ) ) )
setnames( countSource, c( "Date", "AggregatedSource", "Count" ) )
countSource$Date <- as.Date( countSource$Date, format = "%Y-%m-%d" )

transactionperday <- ggplot( countSource, aes( Date, Count, color = AggregatedSource ) ) + geom_line( size = 1 ) +
		             ggtitle( "Number of Transaction Per Day" )

# ------------------------------------------------------------------------------------------
# count the total revenue from different sources of the latest date
latestdate <- unique(data$Date)[ order( unique(data$Date), decreasing = TRUE ) ][1]

data$Revenue <- as.numeric(data$Revenue)
revenuedata  <- data[ data$Date == latestdate, .( TotalRevenue = sum(Revenue) ), by = list(AggregatedSource) ] %>%
                arrange( desc(TotalRevenue) )
# sort the order of the bar according to source
lastestrevenue <- ggplot( revenuedata, aes( reorder( AggregatedSource, -TotalRevenue ), TotalRevenue, fill = AggregatedSource ) ) + 
	              geom_bar( stat = "identity" ) + 
	              geom_text( aes( label = TotalRevenue ), vjust = -.2, size = 3, fontface = "bold" ) +
                  scale_fill_discrete( limits = revenuedata$AggregatedSource ) +
	              labs( title = sprintf( "Each Source's Total Revenue on %s", latestdate ), 
	                    x = "source" )

# ------------------------------------------------------------------------------------------
# revenue distribution : how much revenue does each transaction generate
revenuedistribution <- ggplot( data, aes(Revenue) ) + geom_histogram( fill = "royalblue3" ) + 
					   ggtitle( "Revenue Distribution" )

# ------------------------------------------------------------------------------------------
# pie chart of the number of transaction from different sources of the latest date ( in percentage )
sourcesdata <- data[ data$Date == latestdate, .( Source = .N ), by = list(AggregatedSource) ] %>% 
		       arrange( desc(Source) )

breaks <- with( sourcesdata, cumsum(Source) - Source/2 )
labels <- round( with( sourcesdata, Source/sum(Source) ), 2 ) * 100

transactionpercentage <- ggplot( sourcesdata, aes( 1, Source, fill = AggregatedSource ) ) + 
						 geom_bar( stat = "identity", color = "black" ) +
						 guides( fill = guide_legend( override.aes = list( colour = NA ) ) ) + coord_polar("y") +
						 scale_y_continuous( breaks = breaks, labels = paste0( as.character(labels), "%" ) ) +
						 ggtitle( sprintf( "Each Source's Percentage of Transaction on %s", latestdate ) ) +
                         scale_fill_discrete( limits = sourcesdata$AggregatedSource ) +
						 theme( axis.title  = element_blank(),
						        axis.ticks  = element_blank(), 
						        axis.text.y = element_blank(), 
						        axis.text.x = element_text( color = "black", face = "bold" ) ) 

# ------------------------------------------------------------------------------------------
# print the plot 
define_region <- function( row, col )
{
    viewport( layout.pos.row = row, layout.pos.col = col )
} 
# Open a new page on grid device
grid.newpage() 
# define the layout of the new grid
pushViewport( viewport( layout = grid.layout( 4, 5 ) ) )
# print each plot onto the grid with the specified row and col width
print( lastestrevenue       , vp = define_region( 1:2, 1:3 ) )
print( transactionpercentage, vp = define_region( 1:2, 4:5 ) )
print( transactionperday    , vp = define_region( 3:4, 1:3 ) )
print( revenuedistribution  , vp = define_region( 3:4, 4:5 ) )

# ------------------------------------------------------------------------------------------
# write out files section

# write out the count the number of transaction from different sources in each day to excel file
# aggregated version
countSourceSpread <- spread( countSource, key = AggregatedSource, value = Count )
countSourceSpread$TotalTransaction <- rowSums( countSourceSpread[ , -1, with = FALSE ] )
write.csv( countSourceSpread, file = "aggregatedcategory.csv", row.names = FALSE )

#  non-aggregated version
data$Category <- sourcecategory$category[ match(data$MediaSource, str_trim(sourcecategory$sources) ) ]
# all the ones that are not matched are categorized as other
data$Category[ is.na(data$Category) ] <- "other"

countCategory <- data.table( with( data, table( Date, Category ) ) )
setnames( countCategory, "N", "Count" )

countCategorySpread <- spread( countCategory, key = Category, value = Count )
write.csv( countCategorySpread, file = "category.csv", row.names = FALSE )

# overide the base Analytics data, remember to exclude the added column
# Date, AggregatedSource, Category
write.csv( data %>% select( 1:7 ), file = "Analytics1.csv", row.names = FALSE )


