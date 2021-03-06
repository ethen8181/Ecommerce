# Google Analytics Project1
Ethen Liu  
2015/9/1  

## Project Scenario

Everyday, digital markerters need to download transaction data from Google Analytics and extract the information needed to generate his daily report. Sure, if he's really good with Excel then spending some time everyday will still do the job. The setback  for doing all the work in Excel is that the processing time will significantly scale up when the dataset starts to get larger. Therefore, the main purpose of this project is to solve the digital markerter's problem. After a simple step of running the code and passing in the dataset, the code will write out the partial, tidy data that the Digital Markerter want into a csv file, and also create some simple visualizations of the insights that can be drawn from the data. Insights includes : For this platform, which media sources are bringing more transaction and revenue? How much money do most people spent per transaction on this platform? 

## Dataset

All the datasets are stored in the "data" folder. Each dataset's in-depth description is provided below. The numbers in the dataset have been shadowed or revalued, but the gist remains the same.

- `Analytics.csv` "base" file that stores the historical data. To be explicit, suppose the "Analyticsnew.csv" contains transaction data on August 18, then the "base" file will contain  transaction data starting from day one when Google Analytics began to keep track of the transaction that happend on this website and its reports are available for download to the digital marketers to the day before August 18. This data is clean and does not requires cleaning when you read it in.
    - This data consists of transaction records from August 15, 2015 to August 19, 2015. There are six columns in total, namely TransactionCode, MediaSource, Revenue, Tax, ShippingFree, Refund and Amount. Desciptions of what each variable (columns) stands for are given below.
    - "MediaSource" Keeps track of which media was the traffic coming from.
    - "Amount" column records the number of products that the consumer purchased for that transaction.
    - Given the names of the columns, all other columns should be self-descriptive. 
    

- `Analyticsnew.csv` The new transaction data that the digital marketer downloaded everyday, well, not always everyday. On some occasions, he only needs to do it once every two to three days. Some preprocessing is required when reading in the file, steps are elaborated in the following. 
    - The first 6 rows are meaningless, they're simply gibberish when downloaded from Google Analytics.
    - The csv file does not contain the header ( it probably does, but shows up as gibberish ), rename them manually using column names from the base data. 
    - All blank lines in the file are seen as NA value, and rows that contains NA values are removed after you've finished reading the new dataset.
    

All other datasets in the "data" folder are files that have been written out after the information needed were peeled out from the two datasets above. Note that for those two datasets, they both still require some preprocessing steps after reading them, such as converting characters to date, numberic and so on. Details are further described in the Data Preprocessing section.

## I. Environment Setting

Setting includes, load in the required libraries, clean up the global environment, set the local time to English ( this will prevent dates from printing out in Chinese ) and set the working directory.


```r
library(grid)
library(tidyr)
library(ggplot2)
library(gridExtra)
suppressMessages( library(dplyr) )
suppressWarnings( library(stringr) )
suppressMessages( library(data.table) )

rm( list = ls() )
Sys.setlocale( "LC_ALL", "C" )
setwd("/Users/ethen/Ecommerce/GoogleAnalytics1")
```

## II. Data Preprocessing

**Step1:** `data` Read in the "base" data.


```r
data <- data.table( read.csv( "data/Analytics.csv", stringsAsFactors = FALSE ) )
head( data, 3 )
```

```
##         TransactionCode                 MediaSource Revenue Tax
## 1: ON700381508185801258            google / organic    8926  10
## 2: ON400781508198401403 findprice.com.tw / referral    7998  10
## 3: ON100011508167701056 findprice.com.tw / referral    6190  10
##    ShippingFee Refund Amount
## 1:         400      0      3
## 2:           0      0      2
## 3:           0      0      1
```


**Step2:** `newdata` Read in the new dataset downloaded from Google Analytics( .csv version ). In this case, it's the file "Analyticsnew.csv" in the data folder. Note that, after all the necessary tasks are done, the new dataset will be appended to the base dataset "Analytics.csv", which means you can delete the new dataset afterwards. But for reproducibility of this code, the data is written into "Analytics1.csv".


```r
newdata <- data.table( read.csv( "data/Analyticsnew.csv", stringsAsFactors = FALSE, 
                                 na.strings = "", skip = 7, header = FALSE ) )
newdata <- newdata[ complete.cases(newdata), ]
setnames( newdata, names(data) )
head( newdata, 3 )
```

```
##         TransactionCode                 MediaSource     Revenue      Tax
## 1: ON400831508213301597  m.feebee.com.tw / referral NT$8,200.00 NT$10.00
## 2: ON400781508198401403 findprice.com.tw / referral NT$7,998.00 NT$10.00
## 3: ON300341508243501760    feebee.com.tw / referral NT$7,980.00 NT$10.00
##    ShippingFee  Refund Amount
## 1:   NT$100.00 NT$0.00      6
## 2:     NT$0.00 NT$0.00      2
## 3:     NT$0.00 NT$0.00      2
```

- **Note:** Four columns, including Revenue, Tax, ShippingFee and Refund contains symbols, punctuation marks and therefore can't be directly converted to numeric values as they should be. So we'll next handle this issue.

**Step3:** `Preprocess` Define a function. Given the specified column of the dataset, first extract all the numeric digits and convert the string to numeric value. A caution worth noticing is the fact that numbers starting from 1,000 contains a comma to seperate the last three values, remember to exclude that as well. There might be a better way to do this, so I'm open to different solutions. Now, we're good to go! Select the columns from the newdata that requires this preprocessing step, and use the Preprocess function on all of them. 


```r
Preprocess <- function(x)
{
    string <- str_extract( x, "([:digit:]+\\,)*([:digit:]+)" )
    as.numeric( gsub( "\\,", "", string ) )
}
# apply function to columns
columns <- which ( names(data) %in% c( "Revenue", "Tax", "ShippingFee", "Refund" ) )
newdata <- newdata %>% modifyList( lapply( newdata[ , columns, with = FALSE ], Preprocess ) )
head( newdata, 3 )
```

```
##         TransactionCode                 MediaSource Revenue Tax
## 1: ON400831508213301597  m.feebee.com.tw / referral    8200  10
## 2: ON400781508198401403 findprice.com.tw / referral    7998  10
## 3: ON300341508243501760    feebee.com.tw / referral    7980  10
##    ShippingFee Refund Amount
## 1:         100      0      6
## 2:           0      0      2
## 3:           0      0      2
```

- **Note:** After confirming that the former preprocessing step worked out just the way we wanted it to. Move on to the next step.

**Step4:** `data` Combine the new and the base dataset, and use the unique function to prevent accidently loading in data of the same day, resulting in misleading numbers.

**Step5:** Extract date from the TransactionCode column and add a new Date column to the data. The pattern is ON, followed by 5 random digits from 0 to 9, then the five digits after that is the recorded date in which that transaction occured. 


```r
# Step 4
data <- unique( rbind( data, newdata ) )
# Step 5
date <- gsub( "ON[0-9]{5}([0-9]{6}).*", "20\\1", data$TransactionCode )
data$Date <- as.Date( date, format = "%Y%m%d" )
```

**Step6:** The next section of the preprocessing is to categorize the MediaSource into user-defined category for aggregated source's exploratory data analysis. The category is pre-defined in the "sourcecategory.csv" file that is placed in the "code" file. If you're familiar with Excel, the way for doing it is sort of like the VLOOKUP function in excel.


```r
sourcecategory <- read.csv( "code/sourcecategory.csv", stringsAsFactors = FALSE )
head( sourcecategory, 3 )
```

```
##                     sources category aggregatecategory
## 1         (direct) / (none)   direct          all none
## 2   facebook.com / referral       fb            all fb
## 3 m.facebook.com / referral     m.fb            all fb
```

- **Note:** There're three columns for this "sourcecategory.csv" file. The "aggregatecategory" column is used here to categorize all the media source that Google Analytics kept tracked of. That way we can discover insights from these aggregated media source. The "category" column is another way of aggregating the souce, it will be used in the Write Out Files Section.


```r
# remove possible whitespace in the beginning and end of source before matching 
data$AggregatedSource <- sourcecategory$aggregatecategory[ match(data$MediaSource,
                                                           str_trim(sourcecategory$sources) ) ]
# all the ones that are not matched are categorized as all other
data$AggregatedSource[ is.na(data$AggregatedSource) ] <- "all other"
head( data, 3 )
```

```
##         TransactionCode                 MediaSource Revenue Tax
## 1: ON700381508185801258            google / organic    8926  10
## 2: ON400781508198401403 findprice.com.tw / referral    7998  10
## 3: ON100011508167701056 findprice.com.tw / referral    6190  10
##    ShippingFee Refund Amount       Date AggregatedSource
## 1:         400      0      3 2015-08-18      all organic
## 2:           0      0      2 2015-08-19    all findprice
## 3:           0      0      1 2015-08-16    all findprice
```

- **Note:** After all the necessary preprocessing are done, we now have the tidy version of the data to work with.

## III. Exploratory Data Analysis

This part generates four visualizations. All of them are put together into one single plot at the end of this section.

1. `transactionperday` Line graph : Count the number of transactions from each aggregated sources for each day.

2. `lastestrevenue` Bar plot : Total revenue that was generated from each aggregated sources of the most present date ( Suppose that data consists of between August 15 tp 24, then the most present date is August 24 ). 

3. `revenuedistribution` Histogram : Distribution of how much revenue does each transaction generate.

4. `transactionpercentage` Pie chart : Percentage of transactions from each aggregated sources of the most present date. 



```r
# Visualization 1
# use table so that days that have zero transcation will also show up 
countSource <- data.table( with( data, table( Date, AggregatedSource ) ) )
setnames( countSource, "N", "Count" )
# have to convert the Date column to date once again
countSource$Date <- as.Date( countSource$Date, format = "%Y-%m-%d" )

transactionperday <- ggplot( countSource, aes( Date, Count, color = AggregatedSource ) ) +
                     geom_line( size = 1 ) +
		             ggtitle( "Number of Transaction Per Day" )
```



```r
# Visualization 2
# latest date ( most present )
latestdate <- unique(data$Date)[ order( unique(data$Date), decreasing = TRUE ) ][1]

data$Revenue <- as.numeric(data$Revenue)
revenuedata  <- data[ data$Date == latestdate, .( TotalRevenue = sum(Revenue) ), by = list(AggregatedSource) ] %>%
                arrange( desc(TotalRevenue) )

lastestrevenue <- ggplot( revenuedata, aes( x = reorder( AggregatedSource, -TotalRevenue ),
                                            y = TotalRevenue, fill = AggregatedSource ) ) +
                  geom_bar( stat = "identity" ) + 
	              geom_text( aes( label = TotalRevenue ), vjust = -.2, size = 3, fontface = "bold" ) +
                  scale_fill_discrete( limits = revenuedata$AggregatedSource ) +
	              labs( title = sprintf( "Each Source's Total Revenue on %s", latestdate ), 
	                    x = "source" )
```



```r
# Visualization 3 
revenuedistribution <- ggplot( data, aes(Revenue) ) + 
                       geom_histogram( fill = "royalblue3" ) + 
					   ggtitle( "Revenue Distribution" )
```



```r
# Visualization 4 
sourcesdata <- data[ data$Date == latestdate, .( Source = .N ), by = list(AggregatedSource) ] %>%
		       arrange( desc(Source) )

breaks <- with( sourcesdata, cumsum(Source) - Source/2 )
labels <- round( with( sourcesdata, Source/sum(Source) ), 2 ) * 100

transactionpercentage <- ggplot( sourcesdata, aes( 1, Source, fill = AggregatedSource ) ) + 
						 geom_bar( stat = "identity", color = "black" ) +
						 guides( fill = guide_legend( override.aes = list( colour = NA ) ) ) + 
                         coord_polar("y") +
						 scale_y_continuous( breaks = breaks, labels = paste0( as.character(labels), "%" ) ) +
						 ggtitle( sprintf( "Each Source's Transaction Percentage on %s", latestdate ) ) +
                         scale_fill_discrete( limits = sourcesdata$AggregatedSource ) +
						 theme( axis.title  = element_blank(),
						        axis.ticks  = element_blank(), 
						        axis.text.y = element_blank(), 
						        axis.text.x = element_text( color = "black", face = "bold" ) ) 
```


```r
# print the plot 
DefineRegion <- function( row, col )
{
    viewport( layout.pos.row = row, layout.pos.col = col )
} 
# Open a new page on grid device
grid.newpage() 
# define the layout of the new grid
pushViewport( viewport( layout = grid.layout( 4, 5 ) ) )
# print each plot onto the grid with the specified row and col width
print( lastestrevenue       , vp = DefineRegion( 1:2, 1:3 ) )
print( transactionpercentage, vp = DefineRegion( 1:2, 4:5 ) )
print( transactionperday    , vp = DefineRegion( 3:4, 1:3 ) )
print( revenuedistribution  , vp = DefineRegion( 3:4, 4:5 ) )
```

![](Project1_files/figure-html/unnamed-chunk-12-1.png) 

**Findings from the Visualization**

- The top-left plot indicates how much revenue did each aggregated media source bring to the site on August 24, which is the latest ( most present ) date in the dataset. The plot in the top-right further tells us the number of transactions, expressed in percent, from each aggregated media source. Note that the number is rounded as percentage, so there are chances in which the percentages don't add up to 100.

- From the plot that depicts the number of transaction from each aggregated media source, we're able to identify that during this period of time, from August 15 to August 24, customers that came from feebee and findprice accounted for more transactions for this e-commerce websites.

- Histogram of the revenue distribution shows that most consumers spent amount 200 to 500 per transaction.

## IV. Write Out Files Section


```r
# write out the count the number of transaction from different sources in each day to excel file
# aggregated version
countSourceSpread <- spread( countSource, key = AggregatedSource, value = Count )
countSourceSpread$TotalTransaction <- rowSums( countSourceSpread[ , -1, with = FALSE ] )
write.csv( countSourceSpread, file = "data/aggregatedcategory.csv", row.names = FALSE )

#  non-aggregated version
data$Category <- sourcecategory$category[ match(data$MediaSource, str_trim(sourcecategory$sources) ) ]
# all the ones that are not matched are categorized as other
data$Category[ is.na(data$Category) ] <- "other"

countCategory <- data.table( with( data, table( Date, Category ) ) )
setnames( countCategory, "N", "Count" )

countCategorySpread <- spread( countCategory, key = Category, value = Count )
write.csv( countCategorySpread, file = "data/category.csv", row.names = FALSE )

# overide the base Analytics data, remember to exclude the added column
# Date, AggregatedSource, Category
write.csv( data %>% select( 1:7 ), file = "data/Analytics1.csv", row.names = FALSE )
```

## IV. Session Information


```r
sessionInfo()
```

```
## R version 3.2.2 (2015-08-14)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.5 (Yosemite)
## 
## locale:
## [1] C/C/C/C/C/en_US.UTF-8
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] data.table_1.9.6 stringr_1.0.0    dplyr_0.4.3      gridExtra_2.0.0 
## [5] ggplot2_1.0.1    tidyr_0.3.1     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.1      knitr_1.11       magrittr_1.5     MASS_7.3-43     
##  [5] munsell_0.4.2    colorspace_1.2-6 R6_2.1.1         plyr_1.8.3      
##  [9] tools_3.2.2      parallel_3.2.2   gtable_0.1.2     DBI_0.3.1       
## [13] htmltools_0.2.6  lazyeval_0.1.10  yaml_2.1.13      digest_0.6.8    
## [17] assertthat_0.1   reshape2_1.4.1   formatR_1.2.1    evaluate_0.8    
## [21] rmarkdown_0.8    labeling_0.3     stringi_0.5-5    scales_0.3.0    
## [25] chron_2.3-47     proto_0.3-10
```

