source('scripts/distFuncs.R')
Sall_votes = as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

people <- c("BOOKER, Cory Anthony","WARREN, Elizabeth",
            "KLOBUCHAR, Amy","SANDERS, Bernard","HARRIS, Kamala Devi")

Sall_votes_sub  <- Sall_votes %>% 
  filter(grepl(paste(people, collapse="|"), name))

firstCong <- min(Sall_votes_sub$congress)
lastCong <- max(Sall_votes_sub$congress)



# Make distance matrix and nexus output for 110-116th congresses
for (i in firstCong:lastCong){
  cong = i
  Sall_votes_sub_dist  <- Sall_votes %>% filter(congress == cong)
  
  fname <- paste("./data/dist_",as.character(cong),"th.nex",sep="")
  
  l1DistsAll <- makeDistMat(Sall_votes_sub_dist,fname)
}

# Use splitsTree to generate weigths matrices with NNET

# --------- Get distances from center for Dem primary candidates ---------
people <- c("BOOKER, Cory Anthony","WARREN, Elizabeth",
            "KLOBUCHAR, Amy","SANDERS, Bernard","HARRIS, Kamala Devi",
            "OBAMA, Barack","COLLINS, Susan Margaret")

Sall_votes_sub  <- Sall_votes %>% 
  filter(grepl(paste(people, collapse="|"), name))

data <- setNames(data.frame(matrix(ncol = length(people), nrow = lastCong-firstCong+1)), people)

# Calculate distances for each person
allDists <- list()

# Weights matrices saved in directory
for (cong in firstCong:lastCong){
  fname <- paste("./data/splitWeights_tab_",as.character(cong),"th.txt",sep="")
  splitMat <- read_delim(fname, "\t", escape_double = FALSE, trim_ws = TRUE)
  
  centerDists <- centerDist(splitMat) 
  allDists[[cong-firstCong+1]] <- centerDists
  
  Sall_votes_sub_cands  <- Sall_votes_sub %>% filter(congress == cong)
  cands <- unique(Sall_votes_sub_cands$name)
  
  for (c in cands){
    c_sub <- gsub(",.*", "", c)
    pos <- grep(c_sub,colnames(centerDists))
    
    if (!identical(pos, integer(0))) {
    
      dist <- centerDists[pos]
      
      df_pos <- grep(c,colnames(data))
      data[cong-firstCong+1,df_pos] <- dist
    }
    
  }
}



# https://stackoverflow.com/questions/44346233/overlay-lines-with-a-varying-number-of-points-from-a-list-using-ggplot2
# Collect relevant data for each person
times <- firstCong:lastCong
test <- list()
times_sub <- list()
for(i in 1:length(people)){

  p <- people[i]
  ind <- !is.na(data[p])
  test[[i]] <- data[p][ind]
  times_sub[[i]] <- times[ind]
  
}

# Plot senators distances from center over time

# Make dataframe with distances, senator names, and congress year
ids <- as.factor(people)             # curve ids as factors
mkdf <- function(x,y) data.frame(Distance=x,Senator=y)   # makes into dataframe
mkdf2 <- function(x,y) data.frame(Senator=y,Congress=x)

df <- test %>% map2(ids,mkdf) %>%  bind_rows()   
df2 <- times_sub %>% map2(ids,mkdf2) %>%  bind_rows() 
df2  <- subset(df2, select = -c(Senator) )
dfMerge <- cbind(df,df2)

# plot it
ggplot(dfMerge) + geom_line(aes(x=Congress,y=Distance,color=Senator)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(aes(x=Congress,y=Distance,color=Senator))

ggsave("./figures/candidateDistsTime.pdf",width=8,height=6)

#  ------- Plot distances from center over time as density plots of whole House --------
# Make distance matrix and nexus output for 100-116th congresses (~30 years)
firstCong = 101
for (i in firstCong:lastCong){
  cong = i
  Sall_votes_sub_dist  <- Sall_votes %>% filter(congress == cong)
  
  fname <- paste("./data/dist_",as.character(cong),"th.nex",sep="")
  
  l1DistsAll <- makeDistMat(Sall_votes_sub_dist,fname)
}

# Calculate distances for each person
allDists <- list()

# Weights matrices saved in directory
for (cong in firstCong:lastCong){
  fname <- paste("./data/splitWeights_tab_",as.character(cong),"th.txt",sep="")
  splitMat <- read_delim(fname, "\t", escape_double = FALSE, trim_ws = TRUE)
  
  centerDists <- centerDist(splitMat) 
  allDists[[cong-firstCong+1]] <- centerDists
  
}

noNames <- list()
demNames <- list()
repNames <- list()
for (i in 1:length(allDists)){
  
  noNames[[i]] <- as.vector(allDists[[i]])
  
  filt <- stri_detect_fixed(colnames(allDists[[i]]), "Rep")
  repNames[[i]] <- as.vector(allDists[[i]][filt])
  demNames[[i]] <- as.vector(allDists[[i]][!filt])

  
}

#colnames(df) <- as.factor(times) 
times <- firstCong:lastCong
ids <- as.factor(times) 
mkdfAllTime <- function(x,y) data.frame(Distance=x,Congress=y) 
dfAll <- noNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()   


dfDem <- demNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()  
dfDem$Party <- rep('Dem/Indep',length(dfDem$Distance))

dfRep <- repNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()  
dfRep$Party <- rep('Rep',length(dfRep$Distance))

dfParties <- rbind(dfDem,dfRep)
#df2 <- tidyr::gather(df)
#head(df2)


ggplot(dfAll, aes(x = Distance, fill = Congress)) + 
  geom_density(alpha=.6) 
  #labs(x=expression(d["HB"]), y="Frequency")  

ggplot(dfAll, aes(x=Congress, y=Distance, fill=Congress)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1) + 
  xlab('Senate Session') +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #theme_minimal(axis.line = element_line(colour = "black"))

ggsave("./figures/allMembersDistTime.pdf",width=8,height=5)

dodge <- position_dodge(width = 0.5)
ggplot(dfParties, aes(x=Congress, y=Distance, fill=Party)) +
  geom_violin(trim=FALSE,position = dodge) + 
  geom_boxplot(width=.2, position = dodge) +
  xlab('Senate Session') +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#0392cf","#ee4035"))

ggsave("./figures/centerDistByParty.pdf",width=12,height=5)
