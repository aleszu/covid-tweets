# Covid-19 tweets

## Authors

These findings are the product of a collaboration at [Lazer Labs](https://lazerlab.net/) at the Network Science Institute, Northeastern University, Boston. 

This app was built by [Aleszu Bajak](http://aleszu.com/), at Northeastern's School of Journalism, with help from Damian Ruck, Hong Qu, Sarah Shugars, Alexi Quintana and the Lazer Lab. 

## Full written report

Demographics of COVID-19 fake news sharing on Twitter

## Visualization app

[Explore app here](https://storybench.shinyapps.io/covid-tweets/).

## How to use 

![gif](https://github.com/aleszu/covid-tweets/blob/main/how-to-covid-tweets.gif)

# About the data

Between January 1st and September 30th 2020, we collected COVID-19 related tweets from registered voters in America. We examined the content posted by a list of accounts matched to demographic information such as age, race, gender and political party affiliation. Our full panel contains 1.6 million accounts, of which 527,958 tweeted about COVID-19. The total number of COVID-19 tweets is 29,662,169. We then collected all the URLs shared by our panel and removed URLs from platforms, such as youtube, facebook, instagram etc, so our sample contains mainly news domains.  

We retained only COVID-19 tweets by filtering using a broad list of 974 multi-lingual keywords, phrases and hashtags related to COVID-19. The keyword list contains words directly related to COVID-19 (e.g. coronavirus, COVID-19) and also those related to phenomenon that occurred as a result of the virus (e.g. “reopening”) . A tweet was included in the sample if it contained at least one item from our list; it could be contained in the tweet text, quoted text, hashtag or any part of the URL string -- this does not not include the content from the linked web page. For more details on COVID-19 tweet selection, see Gallagher et al 2020.

We then extracted all the shared URLs, domains and COVID-19 search keywords from these tweets.

## URLs 

For the top 10 URLs in each state and nationally, we identify the title for the news articles. We do this by scraping the HTML code from the URL link and extracting the contents of the “title” meta tags. We were able to do this for 93% of the top 10 URLS. To highlight the URLs that are distinctive to a certain state, but not to others, we calculated the TF-IDF scores for each URL relative to state. TF-DIF gives us a way of emphasizing the URLs that are uniquely popular in one state.

## Domains

We present the most shared domains in each state and nationally. We also breakdown the sample by age and political affiliation. Political affiliation is decided, either Democrat, Independent or Republican, using a political classifier from Targetsmart. Each person in our sample is assigned a score between 0 and 100, based on a number of factors that predict party affiliation, ranging from self-reported registered party, mass local voting for state and national elections, % votes in Repubican/Democractic primaries and many others.

## Keywords 

We track the frequency of our COVID-19 search terms on a daily basis. We only include keywords that have been shared more than ten times between January 1st and September 30th 2020.  

The media attention feature searches [Media Cloud's](https://mediacloud.org) 'U.S. Top Newspapers 2018' collection of 50 media sources -- including The Washington Post, The New York Times, USA TODAY and The Wall Street Journal -- which is based on research from the Pew Research Center published in August 2019. A full list of sources can be found [here](https://sources.mediacloud.org/#/collections/186572435).

