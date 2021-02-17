#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)

replies_summary = read.csv('original_posts_nytimes_replied_sentiment_merge_calc.csv')
summarySentiment <- read.csv('summarySentiment.csv')
tweets_2020 <- read.csv('nytimes.csv')
tweets_2020_sentiments <- read.csv('nytweets_2020_sentiment.csv')
tweets_summary = read.csv('nytimes_tweets_sentiment_summary_merge.csv')
mentions_sentiment = read.csv('mentions_sentiment_count.csv')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title ="New York Times dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Tweets analysis", tabName = "tweets", icon = icon("twitter-square")),
            menuItem("Replies analysis", tabName = "replies", icon = icon("comment-dots")),
            menuItem("Sentiment analysis 2020", tabName = "2020Sentimentanalysis", icon = icon("diagnoses")),
            menuItem("Conclusions", tabName = "conclusions", icon = icon("clipboard-check"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName="tweets", width = "100%",
                    fluidRow( width = "100%",
                        infoBox("Number of tweets", textOutput("total_number_tweets"),color = "light-blue", icon =icon("twitter"), width = 6)),
                    
                    h3("Tweets"),
                    fluidRow(
                        box(plotOutput("sentiment_tweets")),
                        box(plotOutput("over_time"))
                    )
                    
                    
            ),
            tabItem(tabName="replies", width = "100%",
                    fluidRow( width = "100%",
                              infoBox("Number of replies", textOutput("total_number_replies"), color = "light-blue", icon =icon("comment-dots"), width = 6)),
                    
                    h3("Replies"),
                    fluidRow(
                      box(plotOutput('sentiment_replies')),
                      box(plotOutput('sentiment_replies_over_time'))
                    )
                    
                    
            ),
            tabItem(tabName="conclusions", width = "100%",
                    h3("Conclusions"),
                    fluidRow(
                      h4('With only 901 friends and more than 49M followers, New York Times is a very popular account. 
                         It appears in many lists and is very active.'),
                      h4('From the topic analysis we can link 1, 2 and 3 which are about politics and the us investiture of Joe Biden. 
                          The 4rth topic is not insightfull as it is only notions of time
                          The 5th topic gives not any theme also.
                          The 6, 7 and 9 topics are about the pandemic.
                          The 10th topics tends to cover the scandals like the one of George Floyd - videos from his criminal arrestation are viral - or Harvey Weistein, accused of rape,with the word women is also a recent scandal, 
                          
                          
                          The more important topics are:
                          - Covid
                          - Politics in US in general
                          - Harvey Weinstein
                          - George Floyd
                          
                          
                          Possible improvements for the topic analysis:
                          - select only adjectives and nouns
                          - discover new techniques to get a more exact number of topics 
                          - get rid of MORE non insighfull words')
                    )


            ),
            tabItem(tabName="2020Sentimentanalysis", width = "100%",
                    fluidRow( width = "100%",
                              infoBox("Number of tweets Jan-June 2020", textOutput("total_number_tweets_2020"), color = "light-blue", icon =icon("comment-dots"), width = 6)),
                    h3("Sentiment analysis"),
                    fluidRow(
                      box(plotOutput('contribution_sentiment')),
                      box(plotOutput('tweets_2020_sentiments_over_time')),
                      h4('We can conclude more genarrly that news tend to be negative
It provides an overview by month where in febuary has rise due to the beggining of covid, the situation that starts to getting worse.
It was the time where the sentiment was the most negative. 
We can also observe a tendency in april, probably due to the rise of deaths all accross the worlds and especially in USA, the lockdowns, and the scandals about the president trump during the pandemic.
On the contrary the less negative month was June. 
The sentment has started to lower as of Febuary. ')
                    ),
                    h3("Mentions"),
                    fluidRow(
                      box(plotOutput("mentions_sentiment"))
                    )
                    
                    
            )
            
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    replies_summary = read.csv('original_posts_nytimes_replied_sentiment_merge_calc.csv')
    
    tweets_summary = read.csv('nytimes_tweets_sentiment_summary_merge.csv')
    summarySentiment <- read.csv('summarySentiment.csv')
    tweets_2020 <- read.csv('nytimes.csv')
    tweets_2020_sentiments <- read.csv('nytweets_2020_sentiment.csv')
    mentions_sentiment = read.csv('mentions_sentiment_count.csv')

    # KPI
    total_number_tweets <- tweets_summary %>% summarise(Number_of_tweets =n())
    total_number_tweets_sum <- sum(total_number_tweets$Number_of_tweets)
    
    
    total_number_replies <- replies_summary %>% summarise(Number_of_replies = n_distinct(status_id))
    total_number_replies <- sum(total_number_replies$Number_of_replies)
    
    total_number_replies <- replies_summary %>% summarise(Number_of_replies = n_distinct(status_id))
    total_number_replies <- sum(total_number_replies$Number_of_replies)
    
    total_number_tweets_2020 <- tweets_2020 %>% summarise(NoTweets = n_distinct(status_id))

    total_number_tweets_2020 <- sum(total_number_tweets_2020$NoTweets)
 
    # plots
    
    sentiment_count <- tweets_summary  %>% group_by(general_sentiment) %>% summarise(Number_of_tweets =n()) 
  
    
    
    count_sentiment_time <- tweets_summary  %>% 
        mutate(Day = substr(x = created_at, start = 9, stop = 10)) %>%
        group_by(general_sentiment, Day) %>% 
        summarise(Number_of_tweets =n()) 
    
    count_sentiment_time_positive <- count_sentiment_time %>% filter(general_sentiment == 'Positive') %>% mutate(Day = as.numeric(Day))
    count_sentiment_time_negative <- count_sentiment_time %>% filter(general_sentiment == 'Negative')%>% mutate(Day = as.numeric(Day))
    
    
    output$sentiment_tweets <- renderPlot({
       g <- ggplot(sentiment_count, aes( x =Number_of_tweets, y = general_sentiment, fill = general_sentiment, width=.75)) +
            ggtitle("Sentiment of NYTimes tweets") +
            theme(axis.title.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks.y=element_blank()) +
            theme_void()+
            labs(fill = "Sentiment") +
            scale_fill_manual(values=c("#01b8aa","#374649")) 
       g + geom_bar(stat = "identity")
    
    })
    output$over_time <- renderPlot({
        g <- ggplot() + 
            geom_line(data = count_sentiment_time_positive, aes(x = Day, y = Number_of_tweets,group = 1), color = "#374649") +
            geom_line(data = count_sentiment_time_negative, aes(x = Day, y = Number_of_tweets,group = 1), color = "#01b8aa") +
            scale_fill_manual(values=c("#01b8aa","#374649"))+
            scale_x_continuous( breaks = waiver()) +
            scale_y_continuous( name = 'No of tweets') +
            theme_minimal() 
            
        g + ggtitle('Sentiment over time')
    })

    
    output$total_number_tweets <- renderText({
        total_number_tweets_sum
    })
    
    output$total_number_replies <- renderText({
        total_number_replies
    })
    
    output$total_number_tweets_2020 <- renderText({
      total_number_tweets_2020
    })
    
    
    
    # negative vs positive bar chart for replies
    
    sentiment_count_replies <- replies_summary  %>% group_by(general_sentiment.y) %>% summarise(Number_of_tweets =n()) 
    
    output$sentiment_replies <- renderPlot({
       g <- ggplot(sentiment_count_replies, aes( x =Number_of_tweets, y = general_sentiment.y, fill = general_sentiment.y, width=.75)) +
          ggtitle("Sentiment of tweets NYTimes replied to") +
          theme(axis.title.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.ticks.y=element_blank()) +
          theme_void()+
         labs(fill = "Sentiment") +
          scale_fill_manual(values=c("#01b8aa","#374649"))
        g +  geom_bar(stat = "identity")
      
    })
    
    # replies sentiment over time
    
    count_sentiment_time <- replies_summary  %>% 
      mutate(Day = substr(x = created_at, start = 9, stop = 10)) %>%
      group_by(general_sentiment.y, Day) %>% 
      summarise(Number_of_tweets =n()) 
    
    count_sentiment_time_positive_replies <- count_sentiment_time %>% filter(general_sentiment.y == 'Positive') %>% mutate(Day = as.numeric(Day))
    count_sentiment_time_negative_replies <- count_sentiment_time %>% filter(general_sentiment.y == 'Negative')%>% mutate(Day = as.numeric(Day))
    
      output$sentiment_replies_over_time <- renderPlot({
        g <-ggplot() + 
          geom_line(data = count_sentiment_time_positive_replies, aes(x = Day, y = Number_of_tweets,group = 1), color = "#374649") +
          geom_line(data = count_sentiment_time_negative_replies, aes(x = Day, y = Number_of_tweets,group = 1), color = "#01b8aa") +
          scale_fill_manual(values=c("#01b8aa","#374649"))+
          scale_x_continuous( breaks = waiver()) +
          scale_y_continuous( name = 'No of tweets') +
          theme_minimal() 
        g+  ggtitle('Sentiment of the posts NYTimes replied to over time')
    
      })
      
 
      

      output$contribution_sentiment <- renderPlot({
        g <- summarySentiment %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment",
               x = NULL) +
          scale_fill_manual(values=c("#01b8aa","#374649"))
         g + coord_flip()


      })
      
      tweets_2020_sentiments <- tweets_2020_sentiments %>% arrange(created_at) %>%  
        mutate(Month = substr(x = created_at, start = 6, stop = 7)) 
      
      # analyze sentiment over time for tweets in 2020
      output$tweets_2020_sentiments_over_time <- renderPlot({
          g <- ggplot(tweets_2020_sentiments, aes(x=Month,y=mean)) +
            geom_col()
          g +labs(y = "Sentiment",title="Sentiment over time") 
      })
      
      output$mentions_sentiment <- renderPlot({
        g <- mentions_sentiment %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment",
               x = NULL) +
          scale_fill_manual(values=c("#01b8aa","#374649"))
          g  + coord_flip()
        
        
      })
 
    


}

# Run the application 
shinyApp(ui = ui, server = server)
