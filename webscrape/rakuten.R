# Task 
# webscrap the product's name, price, image and description image from the following webpage
# http://www.rakuten.com.tw/shop/electrolux/products/?h=3&p=16&l-id=tw_search_pagen_1
# the link above is only page 1, there's a total of 17 pages

library(plyr)
library(rvest)
library(stringr)
library(data.table)

# This is appended in front of the content url to access into the correct link for scraping images  
website <- "http://www.rakuten.com.tw"

# ------------------------------------------------------------------------
# function that does the webcrawling
webcrawl <- function( url )
{
    # webpage's product section
    product <- html(url) %>% 
               html_nodes( xpath = "//*[@class='b-container b-item-list-box-container']" )
    
    # 1. item's price
    price <- product %>% html_nodes( xpath = "//*[@class='b-text-prime']" ) %>%
                         html_text()
    # remove the rows that contains recommend.price 
    price <- price[ -grep( "recommend.price", price ) ]
    # remove commas in the number and convert to numeric
    price <- as.numeric( gsub( "\\,", "", price ) )
    
    # 2. item's name
    items <- product %>% html_nodes( xpath = "//*[@class='b-content b-fix-2lines']//b" ) %>% 
                         html_text()
    # remove \r and \n and whitespaces in the end from the item 
    items <- str_trim( gsub( "\r|\n", "", items ) )
    
    # 3. item's content, e.g. images that comes along with the item
    content <- product %>% html_nodes( xpath = "//*[@class='b-content b-fix-2lines']//a" ) %>%
                           html_attr( name = "href" )
    # only include the ones that contains the keyword shop
    content <- content[ grep( "shop", content ) ]
    # append the website url in front to access to the link
    content <- paste0( website, content )
    
    # item's image, i.e. the product's image
    imageurl <- lapply( content, function(x)
    {
        html(x) %>% 
        html_nodes( xpath = "//*[@class='b-main-image item-main-image qa-product-mainImage']//a" ) %>%
        html_attr( name = "href" )    
    })
    image <- as.character( do.call( rbind, imageurl ) )
    
    # item's description image, takes a bit longer to process, wait for it
    description_imageurl <- lapply( content, function(x)
    {
        link <- html(x) %>% 
                html_nodes( xpath = "//*[@class='b-container b-editable prd-description']//img" ) %>%
                html_attr( name = "src" ) 
        data.table( t(link) )
    })
    # rbind.fill from plyr allows combining data.frames with different columns
    # missing columns will be filled with NAs
    description_image <- do.call( rbind.fill, description_imageurl )
    
    data <- data.table( items = items, price = price, image = image )
    # extract at most four url from the product's description image link
    data <- cbind( data, description_image[ ,1:4 ] )
    return(data)
}    

# ------------------------------------------------------------------------
# crawl all the product 
number_of_webpage <- 17
productdatalist <- vector( mode = "list", length = number_of_webpage )

for( i in 1:number_of_webpage )
{
    url <- paste0( "http://www.rakuten.com.tw/shop/electrolux/products/?h=3&p=1&l-id=tw_search_pagen_", i )   
    productdatalist[[i]] <- webcrawl( url = url )
    
    # assign random sleeping time before crawling the next page, break if it's the last page
    if( i == number_of_webpage )
        break
    Sys.sleep( sample( 100, size = 1 ) )
}
productdata <- do.call( rbind, productdatalist )


# ------------------------------------------------------------------------
# putting it all together in a table and write out into csv file
setwd("C:/Users/ASUS/Desktop")
write.csv( productdata, "product.csv", row.names = FALSE )

# for downloading the image
# for( i in 1:nrow(image) )
# {
#     filename <- paste0( "photo/", i, ".jpg" )
#     download.file( image[i], destfile = filename, mode = "wb" )    
# }    


