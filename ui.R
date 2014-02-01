require(shiny)
require(recharts)

shinyUI(pageWithSidebar(
  
    # App title
    headerPanel('微博分析'),
    
    sidebarPanel(
  
    radioButtons("type", "Which graph do you want to adjust?",
                 list("Histogram" = "hist",
                      "Network graph" = "network"
                      )),
    conditionalPanel(
        condition = "input.type == 'hist'",
        selectInput('variable', 'Histogram of the time of your weibo:',
                list('months' = 'months',
                     'days' = 'days')),
      
        checkboxInput('more', 'Original vs Repost', FALSE)
      ),
    conditionalPanel(
      condition = "input.type == 'network'",
      helpText("This social network graph is consist of peoples you are following.
                Each point inside stands for one weibo user, with screen name labeled.
                A line between two points mean at least one of the users follows the other 
                one, maybe they follow each other.")
      )
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel('Histogram', plotOutput('time.hist')), # histogram
        tabPanel('Network graph',  includeHTML(recharts.shiny.init()),
                                   htmlOutput('network')),   # social network
        tabPanel('Map visualization', includeHTML(recharts.shiny.init()),
                                      htmlOutput('map')),
        tabPanel('Something more', verbatimTextOutput('guess'))
      )
    )
))