library(twitteR)
library(ROAuth)
library(RCurl)


#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("RCurl")

#download sertifikat curl
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#Set Authorization
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
CUSTOMER_KEY <- "CFI8FFyLdoCLTj8RHAamLsiT5" #consumerAPIkey
CUSTOMER_SECRET <- "FA2nLamE7LJDPGC9VdyZPeGpJfPcj445416lJ3DYio8RtHbjxV" #APISecretkey
ACCESS_TOKEN <- "1276135430203641856-n3EFu5wr8RyRSQIrmRSmraX3w0MIYO" #Accesstoken
ACCESS_secret<-"FGcs0BLEdbDCxzMkkdfxeWJe4P0QxT8d15e3ot6CcIDnI" #Accesstokensecret

setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

#ambil data
search <- c("#flood", "#earthquake", "#volcano")
no.of.tweets <- 1000

#lihat data

for (i in 1:length(search)) {
  cat(sprintf("Proses: (%d / %d) %s\n", i, length(search), search[i]))
  bencana_tweets <- searchTwitter(search[i], n=no.of.tweets,lang="en",)
  df_bencana <- do.call("rbind", lapply(bencana_tweets, as.data.frame))
}
View(df_bencana)

#simpan
write.csv(df_bencana, file = 'data-raw/tweets_scraping.csv', row.names=F)
