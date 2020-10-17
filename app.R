
library(shiny)
library(shinymanager)
library(shinythemes)
library(tidyverse)
library(data.table)
library(DT)
library(forcats)
library(plotly)
library(scales)
library(stringr)
library(jsonlite)
library(metathis)
library(fuzzyjoin)


# Keywords data from Damian 
# Updated 2020-10-07
keywords_melted <- fread("keywords_melted_Oct17.csv")
# keyword_state_timeseries_Oct16 <- fread("keyword_state_timeseries101620.tsv")
# keyword_state_timeseries_Oct16 <- keyword_state_timeseries_Oct16 %>%
#   select(-keyword_category) 
# keyword_state_timeseries_Oct16$state <- str_replace_all(keyword_state_timeseries_Oct16$state, "national", "National") 

# ****************************************
# Data that's preprocessed by Aleszu
# ****************************************

# Data from Leaderboards that has national domain leaderboards folded in  
# Updated 2020-10-07
state_domain_leaderboards_Oct7 <- fread("state_domain_leaderboards_Oct7.csv")

# Ingest df of top 10 URLs per state that have had titles scraped
# Updated 2020-10-07
state_url_leaderboards_lastmonth_top10_titles <- fread("state_and_national_url_months1thru9_top10_titles.csv")
state_url_leaderboards_lastmonth_top10_titles$first_date <- as.Date(state_url_leaderboards_lastmonth_top10_titles$first_date)

# TFIDF of top 10 URLs title scrape by state
# Updated 2020-10-07
# Should probably be done across more than 1 month ******** 
tfidf_top50_titles <- fread("tfidf_top50_titles_Oct7.csv")

# Ingest **pre-processed** monthly ranked leaderboards 
# Updated 2020-10-07
state_domain_leaderboards_monthy_all <- fread("state_domain_leaderboards_monthy_all_Oct7.csv")

# State abreviations and names
state_abbr_and_names <- fread("state_abbr_and_names.csv")

# Demos
demo_select <- tibble(demo = c("18-29", "30-49", "50-64", ">65"))

# Parties
party_select <- tibble(party = c("Democrat", "Independent", "Republican"))

# Shortened URLs
shortenedurls <- c("bit.ly", "ow.ly", "trib.al", "buff.ly", "dlvr.it", "flip.it")

# Colors
colors_demo <- c("18-29" = "lightblue", "30-49"= "#FFE523", "50-64"="#b1ab99", ">65"= "#635e4e")
colors_party <- c("Democrat" = "#59a2e9", "Independent" = "#228B22", "Republican" = "#863605")
colors_keywords <- c("#789a6f", "#800080")

# credentials <- data.frame(
#     user = c("masks", "reopen"), # mandatory
#     password = c("masks", "reopen"), # mandatory
#     start = c("2020-07-15"), # optional (all others)
#     expire = c(NA, "2021-12-31"),
#     admin = c(FALSE, TRUE),
#     comment = "Enter password",
#     stringsAsFactors = FALSE
# )

ui <- navbarPage("Covid-19 tweets", 
                 id = 'menu',
                 tabPanel("How to use this tool", 
                          shinyjs::useShinyjs(),
                          
                          fluidPage(
                            meta() %>%
                              meta_social(
                                title = "Covid-19 tweets",
                                description = "The Covid-19 tweets project from Northeastern University's Lazer Lab and Network Science Institute",
                                url = "https://storybench.shinyapps.io/covid-tweets",
                                image = "https://storybench.org/statelogos/covid-shiny-preview.jpg",
                                image_alt = "Covid-19 tweets project from Northeastern University",
                                twitter_creator = "@aleszubajak",
                                twitter_card_type = "summary"
                              ),
                              list(tags$script(HTML("var header = $('.navbar > .container-fluid');
        header.append('<div style=\"float:right\"><ahref=\"https://www.networkscienceinstitute.org\"><img src=\"https://uploads-ssl.webflow.com/5c9104426f6f88af009ef3ad/5d83de8fdb4091605831e95d_NU_NetworkScienceInstitute_RGB-01-p-800.png\" alt=\"netsci\" style=\"float:right;width:200px;height:46px;padding-top:0px;\"> </a></div>');
            console.log(header)")
                              ),
                              tags$style(".shiny-text-output {font-size:18px;
                                           color:black;
                                           padding-bottom: 10px;
                                           display:block; }"),
                              tags$style(HTML(".navbar-default .navbar-brand {color: #000000;}"))), 
                             # tags$div(class="tagline",
                            #           tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                             #                 align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                              br(),
                              hr(),
                            fluidRow(
                              column(2),
                              column(8, align="center",
                              tags$div(
                                "This dashboard created by researchers at the ",
                                tags$a(href="https://lazerlab.net/", "Lazer Lab", target="_blank"),
                                "allows you to explore the top links, domains and keywords extracted from 29 million tweets related to Covid-19 shared between January 1st and September 30th, 2020 by over half a million Americans for whom we have demographic information such as age, state of residence and political party registration. Here's how to use it:"
                                ,style = "float:left; text-align:left;")
                                    ),
                              column(2)
                            ),
                            fluidRow(
                              column(1),
                              column(8, align="center",
                                     uiOutput("how_to_covid_tweets")
                                     ),
                              column(2)
                          ),
                            fluidRow( br(),
                                      br(),
                                      br(),
                                      br()
                            )  
                          )
                 ),         
                        tabPanel("Top links", value = "toplinks",
                                     fluidPage(
                                       tags$div(class="tagline",
                                                tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                                                       align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                                       br(),
                                       hr(),
                              fluidRow(
                                  column(2,align="center",
                                         textOutput("selectedstate2"),
                                         uiOutput("statelogo2"),
                                         br(),
                                         selectInput("state",
                                                            "Choose a state",
                                                            choices = c(state_abbr_and_names$state),
                                                            selected = "National"),
                                         selectInput("month",
                                                     "Choose a month",
                                                     choices = c(state_url_leaderboards_lastmonth_top10_titles$month),
                                                     selected = "September")
                                         ),
                                  column(10, 
                                         h4(textOutput("input_state_title")), #"Top links shared in the last month", align = "center"),
                                         br(),
                                         DTOutput("output_top10_table","100%"), align="center")
                              ),
                              fluidRow(
                                column(2),
                                  column(10,
                                         h4(textOutput("input_state_title2")), #"Top links distinctive to this state", align = "center"),
                                         br(),
                                         DTOutput("output_top10_distinctive_table","100%"), align="center")
                                      ),
                              fluidRow( br(),
                                        br(),
                                        br(),
                                        br()
                              )       
                          )
                 ),
                 tabPanel("Top domains", 
                          fluidPage(
                              tags$div(class="tagline",
                                       tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                                              align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                              br(),
                              hr(),
                              fluidRow(
                                  column(2,
                                         align="center",
                                         textOutput("selectedstate3"),
                                         uiOutput("statelogo3")
                                         ),
                                  column(5, 
                                         h4("Top domains shared since September 1", align = "center"),
                                         br(),
                                         DTOutput("top_domains","100%"), align="center"),
                                  column(5, 
                                         h4("Popularity of top domains by month", align = "center"),
                                         br(),
                                         plotlyOutput("top_domains_rank","100%"), align="center")
                              ),
                              
                              hr(),
                              h4("Sharing demographics of top domains since January", align="center"),
                              br(),
                              fluidRow(
                                  column(2),
                                  column(2, align="center",
                                         selectInput("demo",
                                                     "Sort by age group",
                                                     choices = c(demo_select$demo), selected="50-64")
                                  ),
                                  column(4),
                                  column(2, align="center",
                                         selectInput("party",
                                                     "Sort by party",
                                                     choices = c(party_select$party),selected="Republican")
                                  ),
                                  column(2)
                              ),
                              fluidRow(
                                  column(6, 
                                         plotlyOutput("domains_demo","100%"), align="center"),
                                
                                  column(6, 
                                         plotlyOutput("domains_party","100%"), align="center")
                                      ),
                              br(),
                              br()
                       )
                          
                 ),
                 tabPanel("Top keywords",
                          fluidPage(
                              tags$div(class="tagline",
                                       tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information",
                                              align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                              br(),
                              hr(),
                              fluidRow(
                                  column(2,
                                         align="center",
                                         textOutput("selectedstate4"),
                                         uiOutput("statelogo4"),
                                         br(),
                                         textInput("keywords", "Compare specific words over time", #keyword2
                                                   value = "masks"),
                                         p("Separate multiple terms with comma")#masks
                                  ),
                                  column(10,
                                         plotlyOutput("mainbarchart", "90%", 500), align="center")
                                  
                              ),
                              br(),
                              br(),
                              fluidRow(
                                column(2,
                                       align="center",
                                       textInput("keyword3",
                                                 "Measure media coverage of keyword",
                                                 value = "masks"),
                                       div("This feature uses the open-source media analysis platform",
                                           tags$a(href="https://mediacloud.org/", "Media Cloud", target="_blank")
                                       ),
                                       br(),
                                       br(),
                                       div(
                                         tags$a(uiOutput("googletrends"))
                                       )
                                ), 
                                column(9,
                                       h4("Media coverage as measured by Media Cloud", align = "center"),
                                       plotlyOutput("mediacloud", "90%", 500), align="center"
                                       ),
                                column(1)
                              ),
                              br(),
                              br(),
                              fluidRow(column(2), 
                                            column(9, 
                                                 h4(textOutput("input_state_keywordtitle")),
                                                 id="topkeywords", 
                                                 DTOutput("topkeywords","100%"), align="center"),
                                         column(1)
                              ), 
                              fluidRow( br(),
                                        br(),
                                        br(),
                                        br()
                              )   
                          )
                 ),
                 tabPanel("About", 
                          fluidPage(
                              column(2),
                              column(9,
                                     h3("Authors"),
                                     br(),
                                     div("These findings are the product of a collaboration at the ",
                                         tags$a(href="https://lazerlab.net/", "Lazer Lab", target="_blank"),
                                         " at the Network Science Institute, Northeastern University, Boston."),
                                     br(),
                                     div("This app was built by ",
                                         tags$a(href="http://aleszu.com/", "Aleszu Bajak", target="_blank"), 
                                         " at Northeastern's School of Journalism, with help from Damian Ruck, Hong Qu, Sarah Shugars, Alexi Quintana and the Lazer Lab."),
                                     br(),
                                     h3("About the data"),
                                     br(),
                                     div("Between January 1st and September 30th, 2020, we collected COVID-19 related tweets from registered voters in America. We examined the content posted by a list of accounts matched to demographic information such as age, race, gender and political party affiliation (1). Our full panel contains 1.6 million accounts, of which 527,958 tweeted about COVID-19. The total number of COVID-19 tweets is 29,662,169. We then collected all the URLs shared by our panel and removed URLs from platforms, such as YouTube, Facebook, Instagram, etc, so our sample contains mainly news domains."),
                                     br(),
                                     div("We retained only COVID-19 tweets by filtering using a broad list of 974 multi-lingual keywords, phrases and hashtags related to COVID-19. The keyword list contains words directly related to COVID-19 (e.g. coronavirus, COVID-19) and also those related to phenomenon that occurred as a result of the virus (e.g. “reopening”) . A tweet was included in the sample if it contained at least one item from our list; it could be contained in the tweet text, quoted text, hashtag or any part of the URL string -- this does not not include the content from the linked web page. For more details on COVID-19 tweet selection, see Gallagher et al 2020. (2)."),
                                     br(),
                                     div("We then extracted all the shared URLs, domains and Covid-19 keywords from these tweets."),
                                     br(),
                                     h4("URLs"),
                                     div("For the top 10 URLs in each state and nationally, we identify the title for the news articles. We do this by scraping the HTML code from the URL link and extracting the contents of the “title” meta tags. We were able to do this for 93% of the top 10 URLS. To highlight the URLs that are distinctive to a certain state, but not to others, we calculated the TF-IDF scores for each URL relative to state using URLs shared since January. TF-IDF gives us a way of emphasizing the URLs that are uniquely popular in one state."),
                                     br(),
                                     h4("Domains"),
                                     div("We present the most shared domains in each state and nationally. We also breakdown the sample by age and political affiliation (1). Political affiliation is decided, either Democrat, Independent or Republican, using a political classifier from Targetsmart. Each person in our sample is assigned a score between 0 and 100, based on a number of factors that predict party affiliation, ranging from self-reported registered party, mass local voting for state and national elections, % votes in Repubican/Democractic primaries and many others."),
                                     br(),
                                     h4("Keywords"),
                                     div("We track the frequency of our COVID-19 search terms on a daily basis. We only include keywords that have been shared more than ten times between January 1st and September 30th, 2020."),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     div("1. ", 
                                     tags$a(href="https://targetearly.targetsmart.com/modeling.html", "Targetsmart", target="_blank")
                                     ),
                                     br(),
                                     div("2. Gallagher, R. J., Doroshenko, L., Shugars, S., Lazer, D., & Welles, B. F. (2020). Sustained Online Amplification of COVID-19 Elites in the United States. http://arxiv.org/abs/2009.07255"),
                                     br(),
                                     br()
                          ) 
                        )
                 )
                 
)

# UNCOMMENT TO PROMPT FOR PASSWORD
# ui <- secure_app(ui)

server <- function(input, output, session) {
    
    # UNCOMMENT TO PROMPT FOR PASSWORD
    # res_auth <- secure_server(
    #     check_credentials = check_credentials(credentials)
    # )
    # 
    # output$auth_output <- renderPrint({
    #     reactiveValuesToList(res_auth)
    # })
  
    observe(
        {
          updateSelectInput(session,
                                  "state",
                                  "Choose a state",
                                  selected = "National",
                                  choices = c(state_abbr_and_names$state))
          
          updateSelectInput(session,
                            "month",
                            "Choose a month",
                            selected = "September",
                            choices = c(state_url_leaderboards_lastmonth_top10_titles$month))
          
        }
    )
    
    
    
    shinyjs::addClass(id = "menus", class = "navbar-right")
    
    #addResourcePath(prefix = 'statessprites', directoryPath = 'states-sprites')
    # Can't get "addResourcePath()" to work to pull state sprites from local directory

    output$how_to_covid_tweets <- renderUI({
      tags$img(src = "https://storybench.org/statelogos/how-to-covid-tweets.gif")
    })
    
    
    c_url <- reactive({
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        paste0("https://storybench.org/statelogos/", input$state, ".svg") 
    })
    
    output$statelogo1 <- renderUI({
        tags$img(src = c_url())
    })
    
    output$selectedstate1 <- renderText({ 
        statename <- state_abbr_and_names %>%
            filter(state == input$state) %>%
            select(name) %>%
            as.character()
        statename
    })
    
    output$statelogo2 <- renderUI({
        tags$img(src = c_url())
    })
    
    output$selectedstate2 <- renderText({ 
        statename <- state_abbr_and_names %>%
            filter(state == input$state) %>%
            select(name) %>%
            as.character() 
        statename
    })
    
    output$statelogo3 <- renderUI({
        tags$img(src = c_url())
    })
    
    output$selectedstate3 <- renderText({ 
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      statename
    })
    
    output$statelogo4 <- renderUI({
      tags$img(src = c_url())
    })
    
    output$selectedstate4 <- renderText({ 
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      statename
    })
    
    output$input_state_title <- renderText({
        statename <- state_abbr_and_names %>%
          filter(state == input$state) %>%
          select(name) %>%
          as.character() 
        
        state_title <- paste("Top", statename, "links shared since ", input$month, "1") 
        state_title
    })
    
    output$input_state_title2 <- renderText({
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      state_title <- paste("Top 50 links distinctive to", statename, "since January 1") 
      state_title
    })
    
    output$googletrendstext <- renderText({
      
      paste0("Search Google Trends for interest in '",input$keyword3,"' by clicking ")
      
    })
    
    output$googletrends <- renderUI({
      
      tags$a(href = paste0("https://trends.google.com/trends/explore?q=%22", input$keyword3,"%22&geo=US"), "Search Google Trends for keyword interest")
    
    })
      
      
    
    output$input_state_keywordtitle <- renderText({
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      state_title <- paste("Most popular", statename, "keywords since January 1") 
      state_title
    })
    
    
    # TOP LINKS
    
    output$output_top10_table <- DT::renderDataTable({

        shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
      
    state_url_leaderboards_titles_input <- state_url_leaderboards_lastmonth_top10_titles %>%
        filter(state == input$state) %>%
        filter(month == input$month) %>%
        rename(first_shared = first_date)
    
    output_top10_table <- state_url_leaderboards_titles_input %>% 
        select(title, count, first_shared, url, domain) %>%
        mutate(title = paste0("<a href='", url,"' target='_blank'>", title,"</a>")) %>%
        select(title, count, first_shared, domain) %>%
        arrange(desc(count))
    output_top10_table
    }, 
    extensions = 'Buttons',
    options = list(dom = 'Bftp',
                   buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top10-urls'))
                   ),
    escape = FALSE)
    
      
    # TF-IDF table
    
    output$output_top10_distinctive_table <-  DT::renderDataTable({

      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )

        output_top50_distinctive_table <- tfidf_top50_titles %>%
            filter(state == input$state) %>%
            rename(first_shared = first_date) %>%
            arrange(desc(tf_idf)) %>%
            select(title, count, first_shared, url, domain, total_shares) %>%
            mutate(title = paste0("<a href='", url,"' target='_blank'>", title,"</a>")) %>%
            select(title, count, first_shared, domain, total_shares) 
        output_top50_distinctive_table


    },
    extensions = 'Buttons',
    options = list(dom = 'Blftp',
                   buttons = list('copy', list(extend = 'csv', filename='covid-tweets-top-distinctive-urls'))
                   ),
    escape=FALSE)
    
    
    # Top domains ranked month over month
    
    output$top_domains_rank <- renderPlotly({
        
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        
        top_domains_ranks <- state_domain_leaderboards_monthy_all %>%
            filter(!domain %in% shortenedurls) %>%
            select(state, domain, rank, month) %>%
            filter(state == input$state) %>%
            filter(month == "2020-09-01") %>% # CHANGE MONTH INPUT
            arrange(rank) %>%
            distinct()
    
        state_domain_leaderboards_monthy_all_input <- state_domain_leaderboards_monthy_all %>%
            filter(state == input$state)
        
        top_9 <- head(top_domains_ranks, n=9)
        top_9 <- top_9 %>% select(domain)
        
        top_9_domains_viz <- top_9 %>%
            inner_join(state_domain_leaderboards_monthy_all_input, by = "domain") %>%
            mutate(domain = fct_reorder(domain, rank))
        
        top_9_domains_viz$month <- as.Date(top_9_domains_viz$month)
        
        top_9_viz_final <- ggplot(top_9_domains_viz, aes(x=month, y=rank, color="firebrick")) +
            geom_line() +
            scale_y_reverse() +
            #scale_x_date(date_breaks = "2 months", date_labels = "%b") +
            xlab("") +
            theme_minimal() +
            facet_wrap(~domain, ncol=3) +
            theme(legend.position = "none")
        
        top_9_viz_plotly <- ggplotly(top_9_viz_final, tooltip=c("month", "rank"))
        top_9_viz_plotly 
        
    })
    
    # Top domains table
    
    output$top_domains <- DT::renderDataTable({
        
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        
        state_month <- state_domain_leaderboards_monthy_all %>%
            filter(state== input$state) %>%      # CHANGE STATE2 INPUT
            filter(!domain %in% shortenedurls)
        
        state_month_topdomains <- state_month %>%
            select(domain, count, unique_users, month) %>%
            filter(month == "2020-09-01" ) %>% # CHANGE MONTH INPUT 
            arrange(desc(count)) %>%
            slice_max(count, n=100)
        
       state_month_topdomains_tbl <- state_month_topdomains %>%
            select(domain, count, unique_users) %>%
            datatable(extensions = 'Buttons', options = list(dom = 'Btpl',
                                                             buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top-domains'))
                                                             ),
                colnames = c("rank", "domain", "total shares", "unique users")) %>%
            formatStyle(
                'count',
                background = styleColorBar(range(state_month_topdomains$count), 'lightblue')) %>%
            formatStyle(
                'unique_users',
                background = styleColorBar(range(state_month_topdomains$unique_users), '#FFE523'))
       state_month_topdomains_tbl
        
    })
    
    # Top domains by age
    
    output$domains_demo <- renderPlotly({
        
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        
        state_topdomains_demos <- state_domain_leaderboards_Oct7 %>%
            filter(state == input$state) %>% # CHANGE STATE INPUT
            filter(!domain %in% shortenedurls) %>%
            arrange(desc(count))
        
        state_topdomains_demos_pivot <- head(state_topdomains_demos, n=20) %>%
            select(domain, share, `18-29`, `30-49`, `50-64`, `>65`) %>% #`<18`, 
            gather(demo, value, -domain, -share) %>%
            filter(!is.na(value))
        
        state_topdomains_demos_pivot$demo <- factor(state_topdomains_demos_pivot$demo,levels = c("18-29", "30-49", "50-64", ">65"))
        
        state_topdomains_demos_pivot_stacked <- state_topdomains_demos_pivot %>%
            mutate(share = paste0(value*100, "%")) %>%
            filter(demo == input$demo) %>%
            mutate(age = demo) 
      
        stacked_demos <- ggplot(state_topdomains_demos_pivot_stacked, aes(fill=age, 
                                                                          y=value, label=share,
                                                                          x=fct_reorder2(domain, age==input$demo, value, .desc = F))) + 
            geom_bar(position="stack", stat="identity") +
            coord_flip()+
            scale_fill_manual(values=colors_demo) + 
            theme_minimal()+ 
            xlab("") +
            ylab("") +
            ggtitle("") +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
        
        demos_plotly <- ggplotly(stacked_demos, tooltip=c("age", "share")) 
        demos_plotly
    })
    
    # Top domains by party
    
    output$domains_party <- renderPlotly({
        
        
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        
        
        state_topdomains_demos <- state_domain_leaderboards_Oct7 %>%
            filter(state == input$state) %>% # CHANGE STATE INPUT
            filter(!domain %in% shortenedurls) %>%
            arrange(desc(count))
        
        state_topdomains_party_pivot <- head(state_topdomains_demos, n=20) %>%
            select(domain, share, Democrat:Republican) %>%
            gather(party, value, -domain, -share) %>%
            filter(!is.na(value))
    
        state_topdomains_party_pivot_stacked <- state_topdomains_party_pivot %>%
            mutate(share = paste0(value*100, "%")) %>%
            filter(party == input$party)
        
        stacked_party <- ggplot(state_topdomains_party_pivot_stacked, aes(fill=party, 
                                                                          y=value, label=share,
                                                                          x=fct_reorder2(domain, party==input$party, value, .desc = F))) + 
            geom_bar(position="stack", stat="identity") +
            coord_flip()+
            scale_fill_manual(values=colors_party) +
            theme_minimal()+ 
            xlab("") +
            ylab("") +
            ggtitle("") +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
        
        party_plotly <- ggplotly(stacked_party, tooltip=c("share", "party")) 
        party_plotly
        
    })
    
    
    # KEYWORD SEARCH
    
    output$mainbarchart <- renderPlotly({
      
      search_term <- strsplit(input$keywords, ",")[[1]] %>% str_trim()
      #search_term <- search_term
      #search_term <- c(inputkeywords)
      search_term_df <- as.data.frame(search_term)
      search_term_df$search_term <- as.character(search_term_df$search_term)

      inputkeywordsdf_covid <- keywords_melted %>%
        fuzzy_inner_join(search_term_df, by=c("keyword" = "search_term"), match_fun= str_detect) %>%
        filter(state == input$state) %>%
        group_by(search_term, date) %>%
        summarise(daily_total = sum(value), .groups = 'drop')

      inputkeywordsdf_covid$date <- as.Date(inputkeywordsdf_covid$date)
      
      p <- ggplot(inputkeywordsdf_covid, aes(date, daily_total, color=search_term)) + # or 'value'
        geom_line() + theme_minimal() + ylab("daily total") + xlab("") 
        #scale_color_manual(values=colors_keywords) 
        
      p_interactive <- ggplotly(p, dynamicTicks = TRUE) %>%
        rangeslider() %>%
        layout(hovermode = "x")
      p_interactive
      
    })
    
    # selectedData_search1 <- reactive({
    #     req(input$keyword1)   
    # 
    #     DT.m2 <- keyword_state_timeseries_Oct16[keyword %like% input$keyword1]   
    #     DT.m2 = melt(DT.m2,
    #                  id = c("keyword", "state"),
    #                  variable.name = "date",
    #                  value.name = "value")
    #     
    #     DT_search1 <- DT.m2[, search_term := input$keyword1]  
    #     set(DT_search1, j = "date", value = as.IDate(strptime(DT_search1[,date], "%Y-%m-%d")))
    #     DT_search1
    # })
    # 
    # selectedData_search2 <- reactive({
    #    req(input$keyword2)   
    #   
    #    #keyword2react <- reactive({ input$keyword2 })
    #      DT.m3 <- keyword_state_timeseries_Oct16[keyword %like% input$keyword2]  
    #      DT.m3 = melt(DT.m3,
    #                   id = c("keyword", "state"),
    #                   variable.name = "date",
    #                   value.name = "value")
    #      
    #      DT_search2 <- DT.m3[, search_term := input$keyword2]   
    #      set(DT_search2, j = "date", value = as.IDate(strptime(DT_search2[,date], "%Y-%m-%d")))
    #      DT_search2
    #    
    #    
    # })
    # 
    # output$mainbarchart  <- renderPlotly({
    # 
    #     df1<-as.data.frame(selectedData_search1())
    #     df2<-as.data.frame(selectedData_search2())
    # 
    #     bind_search_terms <- bind_rows(df1, df2)
    # 
    #     bind_search_terms_summarized <- bind_search_terms %>%
    #         filter(state == input$state) %>%     
    #         group_by(search_term, date) %>%
    #         summarise(daily_total = sum(value), .groups = 'drop')
    # 
    #     p <- ggplot(bind_search_terms_summarized, aes(date, daily_total, color=search_term)) + 
    #         geom_line() + theme_minimal() + ylab("daily total") + xlab("") +
    #         scale_color_manual(values=colors_keywords)
    # 
    #     p_interactive <- ggplotly(p, dynamicTicks = TRUE) %>%
    #         rangeslider() %>%
    #         layout(hovermode = "x")
    #     p_interactive
    # 
    # })
    
    output$mediacloud  <- renderPlotly({

    #req(input$keyword3)

    #query1 <- URLencode(paste(c(input$keyword3), collapse = "%20"))

    # Media Cloud credentials
    mc.key <- "2a54451396c08c15a024d81bffa18e6ce3bd5bfc9296363581e405ff4fcece80"
    mc.q1 <- "https://api.mediacloud.org/api/v2/stories_public/count?q="
    mc.q2 <- "&split=1&split_period=day&fq=publish_date:%5B2020-01-01T00:00:00.000Z+TO+2020-09-30T00:00:00.000Z%5D&key="

    mc.query1 <- jsonlite::fromJSON(paste0(mc.q1, input$keyword3, mc.q2, mc.key))$counts

    mc.query1$date <- as.Date(mc.query1$date, format ="%Y-%m-%d")
    mc.query1 <- mc.query1 %>% filter(date > "2020-01-01" & date < "2020-09-30")

    p_mediacloud <- ggplot(mc.query1, aes(date, count)) +
      geom_line(stat="identity") +
      theme_minimal() +
      ylab("Sentences per day") +
      xlab("") +
      #  ylim(0,10000) +
      scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b"))

    p_mediacloud_interactive <- ggplotly(p_mediacloud, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
    p_mediacloud_interactive

    })
    
    # Top keywords table 
    
    output$topkeywords <- DT::renderDataTable({
    
    topkeywords_tbl <- keywords_melted %>%
        filter(state == input$state) %>%
        group_by(keyword) %>%
        summarise(total = sum(value), .groups = 'drop') %>% 
        arrange(desc(total))  
    
    topkeywords_tbl_format <- topkeywords_tbl %>%
        datatable(extensions = 'Buttons', options = list(dom = 'Btpl',
                                                        buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top-keywords'))
        ),
                  colnames = c("rank", "keyword", "total")) %>%
        formatStyle(
            'total',
            background = styleColorBar(range(topkeywords_tbl$total), 'lightblue')) 
    
    topkeywords_tbl_format

    })
    
}


shinyApp(ui = ui, server = server)





