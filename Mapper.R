#############################
#Shravan Kumar Chandrasekaran
#c.shravankumar@columbia.edu
# WHAT -> Map followers of Twitter user
# WHY -> Get insights on Who influences where
#############################




if (!require("twitteR")) {
  install.packages("twitteR", repos="http://cran.rstudio.com/") 
  library("twitteR")
}
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)


KachaniS <- getUser("KachaniS")
location(KachaniS)

KachaniS_follower_IDs <- KachaniS$getFollowers(retryOnRateLimit=180)

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}
KachaniS_followers_df = rbindlist(lapply(KachaniS_follower_IDs,as.data.frame))

KachaniS_followers_df <- subset(KachaniS_followers_df,location!="")

KachaniS_followers_df$location<-gsub("%", " ",KachaniS_followers_df$location)


#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")


geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="")
}

geocode_results<-sapply(KachaniS_followers_df$location, geocode_apply, simplify = F)


condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results<-geocode_results[condition_a]



condition_b <- lapply(geocode_results, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
geocode_results<-geocode_results[condition_b2]
length(geocode_results)

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")

results_b<-lapply(geocode_results, as.data.frame)

results_c<-lapply(results_b,function(x) subset(x, select=c("results.formatted_address",
                                                           "results.geometry.location")))



results_d<-lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))


results_e<-rbindlist(results_d)
results_f<-results_e[,Original_Location:=names(results_d)]

american_results<-subset(results_f,
                         grepl(", USA", results_f$Location)==TRUE)

head(american_results,5)


american_results$commas<-sapply(american_results$Location, function(x)
  length(as.numeric(gregexpr(",", as.character(x))[[1]])))
american_results<-subset(american_results, commas==2)
#Drop the "commas" column:
american_results<-subset(american_results, select=-commas)

nrow(american_results)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("maps", "mapproj")
ipak(packages)


#param=c(39,45),
albers_proj<-map("world", proj="globular",  
                 col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=FALSE, resolution=1)
#Add points to it:
points(mapproject(results_f$lng, results_f$lat), col=NA, bg="#00000030", pch=21, cex=1.0)
#Add a title:
mtext("The Geography of @KachaniS's Followers", side = 3, line = -3.5, outer = T, cex=1.5, font=3)




