source('scripts/distFuncs.R')
Sall_votes = as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#  ------- Plot distances from center over time as density plots of whole House --------
# Make distance matrix and nexus output for 100-116th congresses (~30 years)
  firstCong = 101
  lastCong = 116
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
  
  #Get positions of names for Dem and Rep senators
  noNames <- list()
  demNames <- list()
  repNames <- list()
  for (i in 1:length(allDists)){
    
    noNames[[i]] <- as.vector(allDists[[i]])
    
    filt <- stri_detect_fixed(colnames(allDists[[i]]), "Rep")
    repNames[[i]] <- as.vector(allDists[[i]][filt])
    demNames[[i]] <- as.vector(allDists[[i]][!filt])
  
    
  }

  #Make df for plotting
  times <- firstCong:lastCong
  ids <- as.factor(times) 
  mkdfAllTime <- function(x,y) data.frame(Distance=x,Congress=y) 
  dfAll <- noNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()   
  
  
  dfDem <- demNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()  
  dfDem$Party <- rep('Dem/Indep',length(dfDem$Distance))
  
  dfRep <- repNames %>% map2(ids,mkdfAllTime) %>%  bind_rows()  
  dfRep$Party <- rep('Rep',length(dfRep$Distance))
  
  dfParties <- rbind(dfDem,dfRep)


  #Plot
  ggplot(dfAll, aes(x=Congress, y=Distance, fill=Congress)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1) + 
    xlab('Senate Session') +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
    #theme_minimal(axis.line = element_line(colour = "black"))
  
  #ggsave("./figures/allMembersDistTime.pdf",width=8,height=5)
  
  dodge <- position_dodge(width = 0.5)
  ggplot(dfParties, aes(x=Congress, y=Distance, fill=Party)) +
    geom_violin(trim=FALSE,position = dodge) + 
    geom_boxplot(width=.2, position = dodge) +
    xlab('Senate Session') +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("#0392cf","#ee4035"))
  
  ggsave("./figures/centerDistByParty.pdf",width=12,height=5)
  
  
  
  
  
  #  ------------- Get distances of all Senators from 'center' at 116th congress -------------
  splitMat_116 <- read_delim("./data/splitWeights_tab_116th.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
  centerDists <- centerDist(splitMat_116) 
  
  plotCenterDist(centerDists)
