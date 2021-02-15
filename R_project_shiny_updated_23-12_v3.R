#Installing packages
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("plotly")
#install.packages("leaflet")
#install.packages("maps")
##install.packages("DT")
#install.packages("rmarkdown")
#install.packages("knitr")
#install.packages("tinytex")
#tinytex::install_tinytex()
#install.packages("shiny", repos = getOption("https://cloud.r-project.org"), contriburl = contrib.url(repos='https://cloud.r-project.org', type="source"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)
library(maps) 
library(DT)
library(rmarkdown)
library(knitr)

#install.packages("maps")

#read in the datamart file

#db <- read_csv("DatamartR.csv")
db <-get(load("DatamartR.RData"))
#chooseCRANmirror()
#options("repos")
colsCONVERT <- c('COUNTRY', 'LANGUAGE', 'GENDER')
db[colsCONVERT] <- lapply(db[colsCONVERT], factor)

ui <- dashboardPage(
  dashboardHeader(title='Dashboard Actual Internet Sports Gambling'),
  dashboardSidebar(
    #put all the controls in the Dashboard Sidebar
    sidebarMenu(
      #put slider for registration date 
      sliderInput('RegistrationDateSlider', 'Registration Date',
                  value = max(db$RegistrationDate),
                  min = min(db$RegistrationDate),
                  max = max(db$RegistrationDate),
                  step = 1, 
                  dragRange = TRUE
      ),
      #put slider for sum win
      sliderInput('sum_winslider', 'Total amount won', 
                  value=max(db$sum_win), 
                  min=min(db$sum_win), 
                  max=max(db$sum_win), 
                  step=200000
                  ),
      #put slider for amount stakes
      sliderInput('sum_stakeslider', 'Total amount of stakes', 
                  value=max(db$sum_stakes), 
                  min=min(db$sum_stakes), 
                  max=max(db$sum_stakes), 
                  step=200000
      ),
      #put slider for first game
      selectInput('firstgameslider', 'First game played', choices=db$Firstgame,
                  selected=c('Sport', 'Poker', 'Games', 'Casino', 'NA'), multiple = TRUE
                  ),
      #put slider for churn risk
      checkboxGroupInput('Churn_RISKSlider',
                   'Likely to churn?',
                   c('Yes' = 'YES', 'No' = 'NO'),
                   selected = 'NO', inline = TRUE
      ), 
      submitButton("Update View", icon("refresh")),
      downloadButton("report", "Generate report")
    )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel('Marketing statistics',
      fluidRow(
      column(width = 3,
             plotly::plotlyOutput('gender')),
      column(width = 3,
             plotly::plotlyOutput('age')),
      column(width = 3,
             plotly::plotlyOutput('FOBet')),
      column(width = 3,
             plotly::plotlyOutput('language'))
      ),
      fluidRow(
      column(width = 3,
             plotly::plotlyOutput('firstgame')),

      column(width = 3,
             plotly::plotlyOutput("FORecency")),
      column(width = 3,
             plotly::plotlyOutput("Application")),
      column(width = 3,
             plotly::plotlyOutput("Profitscountry"))
      ),
      fluidRow(
        column(width = 3,
               plotly::plotlyOutput('LARecency')),
        column(width = 3,
               plotly::plotlyOutput('LABet')), 
        column(width = 3,
               plotly::plotlyOutput('Profitgames')),
        column(width = 3,
               plotly::plotlyOutput('Profitgamessbets'))
    )),
      
      tabPanel('Country statistics',
               fluidRow(
                 
                 selectInput('country', 'Select Country', unique(db$COUNTRY)),
                 #Total customers at the moment per country
                 valueBoxOutput('nbrusers', width=2),
                 #Total stakes at the moment per country
                 valueBoxOutput('countryTOTALSTAKES', width=2),
                 # info about the total amount of stakes, in Euros, made by the above customers 
                 valueBoxOutput('countryTOTALWINS', width=2),
                 # value profits
                 valueBoxOutput('countryTOTALProfits', width=2),
                 # value total bets
                 valueBoxOutput('countryTOTALBets', width=2)
               
               ),
               fluidRow(
                 column(width = 6,
                        plotly::plotlyOutput('map')),  
                 
                 column(width = 3,
                        plotly::plotlyOutput('Profits_countryGames'))
              
               ),
               fluidRow(
                 column(width = 8,
                        DT::DTOutput('topcustomers'))               
                 
               )
      )
     )
    
  )
)
      
    
server <- function(input, output, session){  
        #Plot showing the gender distirbution
      output$gender <- plotly::renderPlotly({
        plotdb1 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          count(GENDER)
        ggplot(plotdb1) + 
          geom_col(aes(GENDER,n), fill='blue') + 
          
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title= "Gender Distribution", x="Gender", y= "nr of customers")
      })
      #Plot showing the top 10 language spoken
      output$language <- plotly::renderPlotly({
        plotdb2 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          filter(sum_win<=input$sum_winslider)%>%
          count(LANGUAGE, sort=TRUE) %>%
          top_n(10)
        ggplot(plotdb2, aes(x=reorder(LANGUAGE,n), y=n)) + 
          geom_bar(stat='identity', fill='orange') + 
          coord_flip() +
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="Top 10 Languages", x="Language", y="nr of customers")
      })
      
      #Plot showing age distribution
      output$age <- plotly::renderPlotly({
        plotdb3 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider)
        ggplot(plotdb3, aes(AGE)) + 
          geom_histogram(fill='seagreen4', bins = 20) + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="Age Distribution", x="Age", y="nr of customers")
      })
      
      #Plot showing first game distribution
      output$firstgame <- plotly::renderPlotly({
        plotdb4 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          count(Firstgame, sort=TRUE)
        ggplot(plotdb4, aes(Firstgame,n)) + 
          geom_col(fill='red',  position = "dodge", stat="identity") + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="First Game Distribution", x="Game Type", y="nr of customers")
      })
      #Plot showing FO Bets vs wins
      output$FOBet <- plotly::renderPlotly({
        plotdb5 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider)
        ggplot(plotdb5, aes(FOTotalBets,FOTotalWinnings, color= FOstatus)) + 
          geom_point() + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="FO Bet vs Win", x="Total Bets", y="Total Win")
      })
      #Plot showing FO Recency vs Active days
      output$FORecency <- plotly::renderPlotly({
        plotdb6 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider)
        ggplot(plotdb6, aes(RecencyFO,FOTotalDaysActive, color= FOstatus)) + 
          geom_point() + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="FO Recency vs Active Days", x="Recency", y="Active Days")
      })
      
      #Plot showing the top 10 Application
      output$Application <- plotly::renderPlotly({
        plotdb7 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          count(Application, sort = TRUE)%>%
          top_n(10)
        ggplot(plotdb7, aes(x=reorder(Application,n), y=n)) + 
          geom_bar(stat='identity', fill='red') + 
          coord_flip() +
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="Top 10 Applications", x="Applications", y="nr of customers")
      })
      #Plot showing the top 10 Profitable country
      output$Profitscountry <- plotly::renderPlotly({
        plotdb10 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          group_by(COUNTRY)%>%
          summarise(totalwin = sum(Sports_bookFO_Win, Sports_bookLA_Win, Casino_Chartwell_Win, 
                                   Games_VS_Win,Games_bwin_Win, Casino_BossMedia_Win, Supertoto_Win, na.rm=TRUE), 
                    totalstakes = sum(Sports_bookFO_Stakes, Sports_bookLA_Stakes, Casino_Chartwell_Stakes, 
                                      Games_VS_Stakes,Games_bwin_Stakes, Casino_BossMedia_Stakes, Supertoto_Stakes, na.rm=TRUE),
                    totalpokersell = sum(pokerchip_SELL, na.rm=TRUE),
                    totalpokerbuy = sum(pokerchip_BUY, na.rm = TRUE))%>%
          mutate(totalprofit = totalstakes - totalwin, totalpokerpro = totalpokersell - totalpokerbuy)%>%
          arrange(desc(totalprofit))%>%
          select(c("COUNTRY","totalprofit", "totalpokerpro"))%>%
          top_n(10)%>%
          pivot_longer(cols = -COUNTRY, names_to = "Profits", values_to = "Values")
        ggplot(plotdb10, aes(x=reorder(Profits,Values), y=Values/1000000, fill = COUNTRY)) + 
          geom_bar(position= position_dodge(), stat='identity') + 
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="Top 10 Profitable Country", x="Types", y="Profits in Millions Euros")
      })
      #Plot showing LARecency vs Active days
      output$LARecency <- plotly::renderPlotly({
        plotdb11 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider)
        ggplot(plotdb11, aes(RecencyLA,LATotalDaysActive, color= LAstatus)) + 
          geom_point() + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="LA Recency vs Active Days", x="Recency", y="Active Days")
      })
      #Plot showing bet vs wins
      output$LABet <- plotly::renderPlotly({
        plotdb12 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider)
        ggplot(plotdb12, aes(LATotalBets,LATotalWinnings, color= LAstatus)) + 
          geom_point() + 
          theme_minimal() + 
          theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
          labs(title="LA Bet vs Win", x="Total Bets", y="Total Win")
      })
      #Plot showing profits per games types
      output$Profitgames <- plotly::renderPlotly({
        plotdb13 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          summarise(Sports_bookFO = sum(Sports_bookFO_Stakes, na.rm=TRUE)-sum(Sports_bookFO_Win, na.rm=TRUE), Sports_bookLA = sum(Sports_bookLA_Stakes, na.rm=TRUE)-sum(Sports_bookLA_Win, na.rm=TRUE), Casino_Chartwell = sum(Casino_Chartwell_Stakes, na.rm=TRUE) - sum(Casino_Chartwell_Win, na.rm=TRUE),
                    Games_VS = sum(Games_VS_Stakes, na.rm=TRUE)-sum(Games_VS_Win, na.rm=TRUE), Gamebwin = sum(Games_bwin_Stakes, na.rm=TRUE)-sum(Games_bwin_Win, na.rm=TRUE), Casino = sum(Casino_BossMedia_Stakes, na.rm=TRUE)-sum(Casino_BossMedia_Win, na.rm=TRUE), Supertoto =sum(Supertoto_Stakes, na.rm=TRUE)-sum(Supertoto_Win, na.rm=TRUE),
                    PokerChips = sum(pokerchip_SELL, na.rm = TRUE)-sum(pokerchip_BUY, na.rm = TRUE) )%>%
          pivot_longer (everything() ,names_to = "Games",values_to = "Sum_Profits")%>%
          arrange(desc(Sum_Profits))
         
        ggplot(plotdb13, aes(x=reorder(Games,Sum_Profits), y=Sum_Profits/1000000)) + 
          geom_bar(stat='identity', fill = "brown") + 
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, hjust=1)) + 
          labs(title="TOtal Profits Per Games", x="Types", y="Total Amount in Millions Euros")
      })
      #Plot showing profits per bets and per games
      output$Profitgamessbets <- plotly::renderPlotly({
        plotdb14 <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          summarise(Sports_bookFO = (sum(Sports_bookFO_Stakes, na.rm=TRUE)-sum(Sports_bookFO_Win, na.rm=TRUE))/sum(Sports_bookFO_Bets, na.rm=TRUE), Sports_bookLA = (sum(Sports_bookLA_Stakes, na.rm=TRUE)-sum(Sports_bookLA_Win, na.rm=TRUE))/sum(Sports_bookLA_Bets, na.rm=TRUE), Casino_Chartwell = (sum(Casino_Chartwell_Stakes, na.rm=TRUE) - sum(Casino_Chartwell_Win, na.rm=TRUE))/sum(Casino_Chartwell_Bets, na.rm=TRUE),
                    Games_VS = (sum(Games_VS_Stakes, na.rm=TRUE)-sum(Games_VS_Win, na.rm=TRUE))/sum(Games_VS_Bets, na.rm=TRUE), Gamebwin = (sum(Games_bwin_Stakes, na.rm=TRUE)-sum(Games_bwin_Win, na.rm=TRUE))/sum(Games_bwin_Bets, na.rm=TRUE), Casino = (sum(Casino_BossMedia_Stakes, na.rm=TRUE)-sum(Casino_BossMedia_Win, na.rm=TRUE))/sum(Casino_BossMedia_Bets, na.rm=TRUE), supertoto =(sum(Supertoto_Stakes, na.rm=TRUE)-sum(Supertoto_Win, na.rm=TRUE))/sum(Supertoto_Bets, na.rm=TRUE) )%>%
          pivot_longer (everything() ,names_to = "Games",values_to = "Profits_Bets")%>%
          arrange(desc(Profits_Bets))
        
        ggplot(plotdb14, aes(x=reorder(Games,Profits_Bets), y=Profits_Bets)) + 
          geom_bar(stat='identity', fill = "seagreen4") + 
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, hjust=1)) + 
          labs(title="Profits per Bets and Per Games", x="Types", y="Amount in  Euros")
      })
      
      
      #Plot showing 
      filtered_data <- reactive({
        plotdb <- db %>%
          filter(RegistrationDate<=input$RegistrationDateSlider) %>%
          filter(Firstgame==input$firstgameslider)%>%
          filter(sum_stakes<=input$sum_stakeslider)%>%
          filter(sum_win<=input$sum_winslider)%>%
          filter(CHURN_risk==input$Churn_RISKSlider) %>%
          group_by(COUNTRY)
       
      })
      #number of customer for particular country
      output$nbrusers <- renderValueBox({
        users <- db %>%
          filter(COUNTRY==input$country) %>%
          nrow()
        valueBox(users, 
                 'Number of Customers',
                 icon = icon('users'),
                 color = 'blue'
        )
      })
    
      #Total stakes wagered in Euros per country
      output$countryTOTALSTAKES <- renderValueBox({
        country_stakes <- db %>%
          filter(COUNTRY==input$country)
      
        valueBox(paste0(round(sum(country_stakes$Sports_bookFO_Stakes , country_stakes$Sports_bookLA_Stakes , country_stakes$Casino_Chartwell_Stakes , country_stakes$Games_VS_Stakes , country_stakes$Casino_BossMedia_Stakes , country_stakes$Games_bwin_Stakes , country_stakes$Supertoto_Stakes , country_stakes$pokerchip_SELL, na.rm=TRUE)/1000,0),' ' ,'K'), 
                 'TOTAL Stakes (Turnover in Euros)',
                 icon = icon('euro'),
                 color = 'blue'
        )
      })
      
      #Total wins per country
      output$countryTOTALWINS <- renderValueBox({
        country_stakes <- db %>%
          filter(COUNTRY==input$country)
        
        valueBox(paste0(round(sum(country_stakes$Sports_bookFO_Win, country_stakes$Sports_bookLA_Win, country_stakes$Casino_Chartwell_Win, country_stakes$Games_VS_Win , country_stakes$Casino_Chartwell_Win , country_stakes$Games_bwin_Win , country_stakes$Supertoto_Win , country_stakes$pokerchip_BUY, na.rm=TRUE)/1000,0),' ' ,'K'), 
                 'TOTAL Winnings ',
                 icon = icon('euro'),
                 color = 'blue'
        )
      })
      
      #calculation for value box on profit in Euros
      output$countryTOTALProfits <- renderValueBox({
        country_profits <- db %>%
          filter(COUNTRY==input$country)%>%
          summarise(Sports_bookFO = sum(Sports_bookFO_Stakes, na.rm=TRUE)-sum(Sports_bookFO_Win, na.rm=TRUE), Sports_bookLA = sum(Sports_bookLA_Stakes, na.rm=TRUE)-sum(Sports_bookLA_Win, na.rm=TRUE), Casino_Chartwell = sum(Casino_Chartwell_Stakes, na.rm=TRUE) - sum(Casino_Chartwell_Win, na.rm=TRUE),
                    Games_VS = sum(Games_VS_Stakes, na.rm=TRUE)-sum(Games_VS_Win, na.rm=TRUE), Gamebwin = sum(Games_bwin_Stakes, na.rm=TRUE)-sum(Games_bwin_Win, na.rm=TRUE), Casino = sum(Casino_BossMedia_Stakes, na.rm=TRUE)-sum(Casino_BossMedia_Win, na.rm=TRUE), Supertoto =sum(Supertoto_Stakes, na.rm=TRUE)-sum(Supertoto_Win, na.rm=TRUE),
                    PokerChips = sum(pokerchip_SELL, na.rm = TRUE)-sum(pokerchip_BUY, na.rm = TRUE),
                    profits = sum(Sports_bookFO,Sports_bookLA, Casino_Chartwell, Games_VS, Gamebwin, Casino, PokerChips, Supertoto, na.rm = TRUE))
        
        valueBox(paste0(round((country_profits$profits)/1000,0),' ' ,'K'), 
                 'TOTAL Profits ',
                 icon = icon('euro'),
                 color = 'green'
        )
      })
      
      #calculation for value box on total bets
      output$countryTOTALBets <- renderValueBox({
        country_bets <- db %>%
          filter(COUNTRY==input$country)
        
        valueBox(paste0(round(sum(country_bets$Sports_bookFO_Bets, country_bets$Sports_bookLA_Bets, country_bets$Casino_Chartwell_Bets, country_bets$Games_VS_Bets , country_bets$Casino_Chartwell_Bets , country_bets$Games_bwin_Bets , country_bets$Supertoto_BetsWin , na.rm=TRUE),0)), 
                 'TOTAL BETS ',
                 icon = icon('users'),
                 color = 'blue'
        )
      })
      
      #Plot showing the profits per games for particular country
      output$Profits_countryGames <- plotly::renderPlotly({
        plotdb15 <- db %>%
          filter(COUNTRY==input$country)%>%
          summarise(Sports_bookFO = sum(Sports_bookFO_Stakes, na.rm=TRUE)-sum(Sports_bookFO_Win, na.rm=TRUE), Sports_bookLA = sum(Sports_bookLA_Stakes, na.rm=TRUE)-sum(Sports_bookLA_Win, na.rm=TRUE), Casino_Chartwell = sum(Casino_Chartwell_Stakes, na.rm=TRUE) - sum(Casino_Chartwell_Win, na.rm=TRUE),
                    Games_VS = sum(Games_VS_Stakes, na.rm=TRUE)-sum(Games_VS_Win, na.rm=TRUE), Gamebwin = sum(Games_bwin_Stakes, na.rm=TRUE)-sum(Games_bwin_Win, na.rm=TRUE), Casino = sum(Casino_BossMedia_Stakes, na.rm=TRUE)-sum(Casino_BossMedia_Win, na.rm=TRUE), Supertoto =sum(Supertoto_Stakes, na.rm=TRUE)-sum(Supertoto_Win, na.rm=TRUE),
                    PokerChips = sum(pokerchip_SELL, na.rm = TRUE)-sum(pokerchip_BUY, na.rm = TRUE) )%>%
          pivot_longer (everything() ,names_to = "Games",values_to = "Sum_Profits")%>%
          arrange(desc(Sum_Profits))
        
        ggplot(plotdb15, aes(x=reorder(Games,Sum_Profits), y=Sum_Profits/1000000)) + 
          geom_bar(stat='identity', fill = "red") + 
          theme_minimal()+ 
          theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, hjust=1)) + 
          labs(title="TOtal Profits Per Games", x="Types", y="Total Amount in Millions Euros")
      })
     
      #Plot map 
      output$map <- plotly::renderPlotly({
        world_map <- map_data("world", region = unique(filtered_data()$COUNTRY))
        plotd16 <- db %>%
          select(COUNTRY, FOProfit_Loss, FOTotalBets)%>%
          count(COUNTRY)
        plotd17 <- db %>%
          group_by(COUNTRY)%>%
          summarise(totalwin = sum(Sports_bookFO_Win, Sports_bookLA_Win, Casino_Chartwell_Win, 
                                   Games_VS_Win,Games_bwin_Win, Casino_BossMedia_Win, Supertoto_Win, na.rm=TRUE), 
                    totalstakes = sum(Sports_bookFO_Stakes, Sports_bookLA_Stakes, Casino_Chartwell_Stakes, 
                                      Games_VS_Stakes,Games_bwin_Stakes, Casino_BossMedia_Stakes, Supertoto_Stakes, na.rm=TRUE),
                    totalpokersell = sum(pokerchip_SELL, na.rm=TRUE),
                    totalpokerbuy = sum(pokerchip_BUY, na.rm = TRUE),
                    country_bets = sum(Sports_bookFO_Bets, Sports_bookLA_Bets, Casino_Chartwell_Bets, Games_VS_Bets, Games_bwin_Bets, Casino_BossMedia_Bets, Supertoto_Bets, na.rm=TRUE ),
                    country_profit = totalstakes - totalwin)
        
        
        
        merge_data1 <- inner_join(plotd17,plotd16, by=c("COUNTRY"))
        merge_data <- inner_join(merge_data1,world_map, by=c("COUNTRY"="region"))%>%
          group_by(COUNTRY)%>%
          mutate(mean_long = mean(long), mean_lat = mean(lat))
        
        my_theme <- function () { 
          theme_bw() + theme(axis.text = element_blank(),
                             axis.title = element_blank(),
                             strip.text = element_blank(),
                             panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(), 
                             legend.position = "right",
                             panel.border = element_blank(), 
                             strip.background = element_rect(fill = 'white', colour = 'white'))
        }
        ggplot() + 
          geom_polygon(data = merge_data, aes(x=long, y=lat, group=group, fill = n),
                       color="white", size=0.01 )+
          scale_fill_continuous(name="Number customers", 
                                low = "lightblue", high = "darkblue",limits = c(0,2500), 
                                breaks=c(200,400,600,800,1000,1200,1400, 1600, 1800, 2000), na.value = "grey50")+ 
          geom_point(data=merge_data, aes(x=mean_long, y=mean_lat, size = country_profit/1000), color = "red",  alpha = .5) + 
          scale_size(name="Profit/Loss", range=c(0,5))+
          my_theme()
      })
      
      #Create table with top 10 customer in term of profits 
      output$topcustomers <- DT::renderDT({
        db %>% 
          filter(COUNTRY == input$country) %>%
          group_by(UserID)%>%
          summarise(Sports_bookFO_Profits = sum(Sports_bookFO_Stakes, na.rm=TRUE)-sum(Sports_bookFO_Win, na.rm=TRUE), Sports_bookLA_Profits = sum(Sports_bookLA_Stakes, na.rm=TRUE)-sum(Sports_bookLA_Win, na.rm=TRUE), Casino_Chartwell_Profits = sum(Casino_Chartwell_Stakes, na.rm=TRUE) - sum(Casino_Chartwell_Win, na.rm=TRUE),
                    Games_VS_Profits = sum(Games_VS_Stakes, na.rm=TRUE)-sum(Games_VS_Win, na.rm=TRUE), Gamebwin_Profits = sum(Games_bwin_Stakes, na.rm=TRUE)-sum(Games_bwin_Win, na.rm=TRUE), Casino_Profits = sum(Casino_BossMedia_Stakes, na.rm=TRUE)-sum(Casino_BossMedia_Win, na.rm=TRUE), Supertoto_Profits =sum(Supertoto_Stakes, na.rm=TRUE)-sum(Supertoto_Win, na.rm=TRUE),
                    PokerChips = sum(pokerchip_SELL, na.rm = TRUE)-sum(pokerchip_BUY, na.rm = TRUE),
                    Total_Wins = sum(Sports_bookFO_Win,Sports_bookLA_Win,Casino_Chartwell_Win, Games_VS_Win, Games_bwin_Win, Casino_BossMedia_Win, Supertoto_Win, pokerchip_BUY, na.rm = TRUE),
                    Total_Stakes = sum(Sports_bookFO_Stakes,Sports_bookLA_Stakes,Casino_Chartwell_Stakes, Games_VS_Stakes, Games_bwin_Stakes, Casino_BossMedia_Stakes, Supertoto_Stakes, pokerchip_SELL, na.rm = TRUE),
                    Sports_bookFO_Bets = sum(Sports_bookFO_Bets, na.rm=TRUE), Sports_bookLA_Bets = sum(Sports_bookLA_Bets, na.rm=TRUE), Casino_Chartwell_Bets = sum(Casino_Chartwell_Bets, na.rm=TRUE),
                    Games_VS_Bets = sum(Games_VS_Bets, na.rm=TRUE), Gamebwin_Bets = sum(Games_bwin_Bets, na.rm=TRUE), Casino_Bets = sum(Casino_BossMedia_Bets, na.rm=TRUE), Supertoto_Bets =sum(Supertoto_Bets, na.rm=TRUE),
                    Total_Profits = sum(Sports_bookFO_Profits, Sports_bookLA_Profits, Casino_Chartwell_Profits, Games_VS_Profits, Gamebwin_Profits, Casino_Profits, Supertoto_Profits, PokerChips,  na.rm=TRUE ),
                    Total_Bets = sum(Sports_bookFO_Bets, Sports_bookLA_Bets, Casino_Chartwell_Bets, Games_VS_Bets, Games_bwin_Bets, Casino_BossMedia_Bets, Supertoto_Bets, na.rm=TRUE ),
                    Ratio_Profits_Bets = round(Total_Profits/Total_Bets,2))%>%
          select(UserID, Total_Wins, Total_Stakes, Total_Bets,  Total_Profits , Ratio_Profits_Bets)%>%
          arrange(desc(Total_Profits)) %>%
          top_n(10)
      })
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path("report.Rmd")
          #file.copy("report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(n = input$Churn_RISKSlider, m = input$RegistrationDateSlider, o = input$sum_winslider, p = input$sum_stakeslider, q = input$firstgameslider , s = input$country)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
           
          )
        }
      )
      
     
      }
      
shinyApp(ui = ui, server = server)

      