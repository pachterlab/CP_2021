#Create dataframe used in analysis from voteview.com data
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("usedist","phangorn","readr","tidyverse","tidyr","hash","STAT","graphics","grDevices","ggpubr","viridis","stringi","pheatmap","gplots","scales")

#Set wd()
  setwd("../../R")
  source('scripts/distFuncs.R')

#Read in and generate data for analysis
  unzip(zipfile = "./data/voteData.zip", exdir = "./data")
  
  Sall_members <- as.tibble(read_csv("./data/Sall_members.csv") )
  
  Sall_votes <- as.tibble(read_csv("./data/Sall_votes.csv"))  
  
  #Get real names
  Sall_votes[c("name")] <- 
    lapply(Sall_votes[c("icpsr")], function(col) Sall_members$bioname[match(col, Sall_members$icpsr)])
  
  #Get party affiliation
  Sall_votes[c("party_code")] <- 
    lapply(Sall_votes[c("icpsr")], function(col) Sall_members$party_code[match(col, Sall_members$icpsr)])

  #Get NOMINATE scores
  Sall_votes[c("nominate_dim1")] <- 
    lapply(Sall_votes[c("icpsr")], function(col) Sall_members$nominate_dim1[match(col, Sall_members$icpsr)])

  #Get NOMINATE scores
  Sall_votes[c("nominate_dim2")] <- 
    lapply(Sall_votes[c("icpsr")], function(col) Sall_members$nominate_dim2[match(col, Sall_members$icpsr)])
  
  class(Sall_votes)
  
  #Sall_votes <- na.omit(Sall_votes)
  
  write.table(Sall_votes , file = "./data/Sall_votes_withPartyAndNames.csv",sep=",",row.names = FALSE)
