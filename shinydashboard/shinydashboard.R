# shiny dashboard with google analytics 

# environment setting
# change table.id and token for different websites  
table.id <- 

library(shiny)
library(scales)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(shinydashboard)
library(RGoogleAnalytics)

#setwd("/Users/ethen/Desktop/R/R code/google analytics")
#load( "./token_file" )
#ValidateToken(token)

source("/Users/ethen/Ecommerce/shinydashboard/Performance.R")
source("/Users/ethen/Ecommerce/shinydashboard/Dashboard.R")

# ---------------------------------------------------------------------------------------------

body <- dashboardBody(
   
    tabItems(
       
        tabItem( tabName = "DashBoard",

            fluidRow( 

                box( title = "Controls Panel", status = "warning", width = 3,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,

                    dateInput( inputId = "date",
                               label   = "Fill in a Date",
                               value   = Sys.Date() - 1 )
                )

            ),

            fluidRow(

                infoBoxOutput( outputId = "revenueBox" ),
                infoBoxOutput( outputId = "uniquePageViewBox" ),
                infoBoxOutput( outputId = "avgDurationBox" )
            )            
        ),

        # --------------------------------------------------------------------------------------
        # Uses a t-test to compare performances, measures : uniquePageView 
    	tabItem( tabName = "Performance",

    		fluidPage( theme = shinytheme("flatly"),

    			titlePanel("Compare Performances"),

                sidebarLayout(

                    sidebarPanel(

                        helpText( "Conduct a paired t-test on the same webpage",
                                  "to compare performances between two days." ),

                        tags$hr(),

                        textInput( inputId = "url",
                                   label   = "Website's Url" ),

                        # initialize the current date as yesterday
                        dateInput( inputId = "after",
                                   label   = "Current Date",
                                   value   = Sys.Date() - 1 ),

                        # date before placing ads etc.
                        # initialize the previoud date as a week before yesterday 
                        dateInput( inputId = "before",
                                   label   = "Previous Date",
                                   value   = Sys.Date() - 8 ),
                        br(),

                        actionButton( inputId = "compare",
                                      label   = "Compare!" )

                    ),

                    mainPanel(

                        tabsetPanel( type = "tabs",
     
                            tabPanel( "UU" , plotOutput( outputId = "uniqueView" ) )          
                            # tabPanel( "Table", tableOutput( outputId = "table") )
                        )
                    )
                )
    		)	
    	)
    )	
)



server <- function( input, output )
{
    # --------------------------------------------------------------------------------------
    # tabName = "DashBoard"
    daily_data <- reactive(
    {
        DailyData( as.character(input$date) )
    })

    output$revenueBox <- renderInfoBox(
    {
        infoBox( title = h4( strong("Total Revenue") ), 
                 icon  = icon("money"),
                 value = h4( comma( daily_data()$transactionRevenue ) )               
        )
    })

    output$uniquePageViewBox <- renderInfoBox(
    {
        infoBox( title = h4( strong("Total Unique Pageviews") ), 
                 icon  = icon("child"),
                 color = "light-blue",
                 value = h4( comma( daily_data()$uniquePageviews ) )               
        )
    })

    output$avgDurationBox <- renderInfoBox(
    {
        duration <- comma( round( daily_data()$avgSessionDuration, 1 ) )

        infoBox( title = h4( strong("Avg. Session Duration") ), 
                 icon  = icon("clock-o"),
                 color = "blue",
                 value = h4( paste0( duration, " sec." ) )               
        )
    })

    # --------------------------------------------------------------------------------------
    # tabName = "Performance" 
    output$uniqueView <- renderPlot(
    {
        # action button
        if( input$compare == 0 )
            return()

        uniqueview_data <- isolate(
        {
            after  <- PerformanceData( as.character(input$after ), "ga:uniquePageviews", input$url )
            before <- PerformanceData( as.character(input$before), "ga:uniquePageviews", input$url )
            data   <- rbind( after, before )                       
        })

        # perform the paired t-test on the performance between the two days
        test <- t.test( uniquePageviews ~ date, data = uniqueview_data, paired = TRUE )

        # if there in fact is data to compare, then the p.value will not be nan,
        # calculate the sum of uniquePageViews and set the title for the plot  
        if( !is.nan(test$p.value) )
        { 
            if( test$p.value < .05 )
            {
                if( test$estimate > 0 )
                {
                    string <- "Good job, showing improvements!!"
                }else
                    string <- "The Performance is decreasing ....." 
            }else
                string <- "Can Do Better ^^"    
            
            summary <- with( uniqueview_data, tapply( niquePageviews, date, sum ) )
            title   <- paste0( "Total UU = ", names(summary[1]), ":  ", summary[1], ",  ", 
                                              names(summary[2]), ":  ", summary[2] )
            
            chart_title <- substitute( atop( string, italic(title) ), 
                                       list( string = string,
                                              title  = title  ) )                  
        }
        
        plot <- ggplot( uniqueview_data, aes( hour, uniquePageviews, group = date, color = date ) ) +
                geom_line() + 
                geom_point( size = 3 ) +
                scale_color_economist() + 
                theme_economist()
        
        if( !is.nan(test$p.value) )
            plot <- plot + ggtitle( as.expression(chart_title) )

        return(plot)        
    })

}



ui <- dashboardPage(

	# header
    dashboardHeader( title = "Google Analytics" ),

	# sidebar
	dashboardSidebar(

		sidebarMenu(

			menuItem( "DashBoard"  , tabName = "DashBoard"  , icon = icon("dashboard" ) ),
			menuItem( "Performance", tabName = "Performance", icon = icon("line-chart") )
		)
	),

	body
)


shinyApp(ui, server)

