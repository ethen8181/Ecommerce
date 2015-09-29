# Ecommerce

**GoogleAnalytics1:** Working with transaction data from Google Analytics. The data can be found under Conversions > Sales Performance and adding Source / Medium as the secondary dimension. Last updated: 2015.9.2

- `Project1.md` Report that states the whole process of working with the dataset, including the task required and it also serves as the codebook for further describing the dataset. There's also a html and a Rmd version of the report.
- `webflow_files` Graph that the report generated.
- `data` All the datasets that goes along with this project, please refer to the report for detailed description.
- `code` "Project1.R" contains only the r code for the report, and "sourcecategory.csv", please refer to the report for what is it used for.

**shinydashboard:** Uses the RGoogleAnalytics package, which is a R wrapper for the Google Analytics API to create dashboard that visualize daily sales performance and multiple web traffic measurements. In progress

Note that initial setup in terms of registering an app with the Google Analytics API is required, so if you’re not familiar with the required environment settings to use the package, please refer to the following [tutorial](http://www.tatvic.com/blog/google-analytics-data-extraction-in-r/). 

- `shinydashboard.R` To be able to run the dashboard, you still need to fill in the table.id or so called View ID for your Google Analytics in the fifth line of this code, you should see it under Admin > View Settings. All other R codes are functions that will be sourced in to “shinydashboard.R”.  

**webscrape:** Webscrape a e-commerce website to extract product info. Last updated: 2015.9.2

- `rakuten.R` Webscrape the product's name, price, image and description image. The targeted website is a Chinese website.