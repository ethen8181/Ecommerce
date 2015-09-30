# shiny dashboard with google analytics 

# environment setting
# change table.id and token for different websites  
table.id <- 

library(shiny)
library(scales)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(RGoogleAnalytics)

setwd("/Users/ethen/Desktop/R/R code/google analytics")
load( "./token_file" )
ValidateToken(token)

source("/Users/ethen/Ecommerce/shinydashboard/Performance.R")
source("/Users/ethen/Ecommerce/shinydashboard/Dashboard.R")

# ---------------------------------------------------------------------------------------------

body <- dashboardBody(
   
    tabItems(

        # --------------------------------------------------------------------------------------       
        # daily data 
        tabItem( tabName = "DashBoard",

            fluidRow( 

                box( title = "Control Panel", status = "warning", width = 3,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,

                     "Choose the date for the aggregated measurement below.",

                     dateInput( inputId = "date",
                                label   = "Choose the Date",
                                value   = Sys.Date() - 1,
                                max     = Sys.Date() )
                )
            ),

            fluidRow(

                infoBoxOutput( outputId = "revenueBox" ),
                infoBoxOutput( outputId = "uniquePageViewBox" ),
                infoBoxOutput( outputId = "avgDurationBox" )
            ),

            br(),

            fluidRow(

                box( title = "Control Panel", status = "warning", width = 3,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,

                     "Select the date range for all the plot down below.",
                     "Default as the current month up till yesterday.",
                         
                     dateRangeInput( inputId = "dateRange1",
                                     label   = "Select the Date Range:", 
                                     start   = floor_date( Sys.Date() - 1, "month" ),
                                     end     = Sys.Date() - 1,
                                     max     = Sys.Date() )
                )
            ),

            fluidRow(

                box( title = "Revenu", status = "info",
                     solidHeader = TRUE, collapsible = TRUE,

                     plotOutput( outputId = "revenuePlot" )
                ),

                box( title = "Unique Pageviews", status = "info",
                     solidHeader = TRUE, collapsible = TRUE,

                     plotOutput( outputId = "uniquePageViewPlot" )
                )
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
                                  "to compare performances between two days.",
                                  "As for webpage's url, please exclude the host name." ),

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

    # for the infoBoxes 
    daily_data <- reactive(
    {
        # remember to convert the date to character or else GA query won't work
        DailyData( as.character(input$date), table.id )
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
        average <- round( daily_data()$avgSessionDuration, 1 )

        infoBox( title = h4( strong("Avg. Session Duration") ), 
                 icon  = icon("clock-o"),
                 color = "blue",
                 value = h4( paste0( comma(average), " sec." ) )               
        )
    })

    # for the plot

    #output$revenuePlot <- renderPlot(
    #{
    #    return()
    #})

    #output$uniquePageViewPlot <- renderPlot(
    #{
    #    return()
    #})

    # --------------------------------------------------------------------------------------
    # tabName = "Performance" 
    output$uniqueView <- renderPlot(
    {
        # action button
        if( input$compare == 0 )
            return()

        uniqueview_data <- isolate(
        {
            after  <- PerformanceData( as.character(input$after ), "ga:uniquePageviews", input$url, table.id )
            before <- PerformanceData( as.character(input$before), "ga:uniquePageviews", input$url, table.id )
            data   <- rbind( after, before )                       
        })

        # perform the paired t-test on the performance between the two days
        test <- t.test( metric ~ date, data = uniqueview_data, paired = TRUE )

        # if there in fact is data to compare, then the p.value will not be nan,
        # calculate the sum of metric (uniquePageView) and set the title for the plot  
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
            
            summary <- with( uniqueview_data, tapply( metric, date, sum ) )
            title   <- paste0( "Total UU = ", names(summary[1]), ":  ", summary[1], ",  ", 
                                              names(summary[2]), ":  ", summary[2] )
            
            chart_title <- substitute( atop( string, italic(title) ), 
                                       list( string = string,
                                             title  = title ) )                  
        }
        
        plot <- ggplot( uniqueview_data, aes( hour, metric, group = date, color = date ) ) +
                geom_line( size = 1 ) + 
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

