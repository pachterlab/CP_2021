source('scripts/distFuncs.R')
Sall_votes = as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#  ------- Plot distances from center over time as density plots of whole Senate --------
# Make distance matrix and nexus output for 100-116th congresses (~30 years)
  firstCong = 101
  lastCong = 116
  majs = c('Dem','Dem','Dem','Rep','Rep','Rep','Dem','Rep','Rep','Dem','Dem','Dem','Dem','Rep','Rep','Rep')
  
  for (i in firstCong:lastCong){
    cong = i
    Sall_votes_sub_dist  <- Sall_votes %>% filter(congress == cong)
    
    fname <- paste("./data/dist_",as.character(cong),"th.nex",sep="")
    
    l1DistsAll <- makeDistMat(Sall_votes_sub_dist,fname)
  }

  # Calculate distances for each person
  allDists <- list()
  
  # Weights matrices saved in directory,downloaded from SplitsTree program output (using NEXUS input)
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


  #Get range of distances for each sessions within part
  labs <- c('Dem/Indep', 'Rep')
  cong <- list()
  maj <- list()
  ranges <- list()
  party <- list()
  
  p <- 1
  for(i in 1:length(ids)){
    
    c <- ids[i]
    sub <- dfParties %>% filter(Congress == c)
    
    for(l in labs){
      
      r <- max(sub$Distance[sub$Party ==  l]) - min(sub$Distance[sub$Party ==  l])
      ranges[[p]] <- as.vector(r)
      cong[[p]] <- as.vector(c)
      maj[[p]] <- as.vector(majs[i])
      party[[p]] <- as.vector(l)
      
      p = p+1
      
    }

  }
  
  rangeByParty <- data.frame(Congress = unlist(cong), Majority = unlist(maj), Party = unlist(party), Range = unlist(ranges))

  
  #Plot ranges in center distances
  pdf(file = "./figures/rangeDists_majority.pdf",width=6,height=4)
  
  dodge <- position_dodge(width = 0.5)
  ggplot(rangeByParty, aes(x=Majority, y=Range, fill=Party)) +
  geom_violin(trim=FALSE,position = dodge) +
  geom_point(position = dodge) + 
  xlab('Majority Party') +
  ylab('Range of Center Distances') +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#0392cf","#ee4035")) +
  font("xlab", size = 10) +
  font("ylab", size = 10) +
  font("xy.text", size = 10,color='black' )
  
  dev.off()
  
  dem_rangeByParty <- rangeByParty %>% filter(Party == 'Dem/Indep')
  rep_rangeByParty <- rangeByParty %>% filter(Party == 'Rep')
  #Print results of two-sided Mann-Whitney U-test for range differences with different majority parties
  wilcox.test(Range ~ Majority, data= dem_rangeByParty, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
  wilcox.test(Range ~ Majority, data= rep_rangeByParty, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
  
  #Print Hedge's g for effect size of majority party effect on ranges
  cohen.d(Range ~ Majority, data=dem_rangeByParty,hedges.correction=TRUE)
  cohen.d(Range ~ Majority, data=rep_rangeByParty,hedges.correction=TRUE)
  
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
  Sall_votes_sub  <- Sall_votes %>% filter(congress == 116)
  
  metaMat <- makeMetaMat(Sall_votes_sub)
  
  df <- data.frame (dists  = centerDists[1,])
  names <- rownames(df)
  noms <- rep(0, length(names))
  party <- rep('',length(names))
  
  for(i in 1:length(names)){
    nom <- metaMat$nominate_dim1[metaMat$plotID == names[i]][1]
    noms[i] <- nom
    party[i] <- metaMat$party[metaMat$plotID == names[i]][1]
  }
  df$nom_dim1 <- noms
  df$party <- party
  
  df$party <- factor(df$party, levels=c("Dem","Rep","Ind"))
  
  df$dists[df$party == 'Dem'] <- -1*df$dists[df$party == 'Dem']
  df$dists[df$party == 'Ind'] <- -1*df$dists[df$party == 'Ind']
  
  #Plot center distances for 116th Senate
  plotCenterDist(centerDists)
  
  #Plot correlation to NOMINATE scores (Spearmanr correlation) 
  pdf(file = "./figures/nominateScore_corr.pdf",width=6,height=4)
  
  ggscatter(df, x = "dists", y = "nom_dim1",add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "spearman",alpha = 0 ,
            xlab = "Center Distances", ylab = "DW-NOMINATE Dim. 1",legend.title = 'Party') +
  geom_point(aes(colour = party),
             alpha = .6,size = 2) + scale_color_manual(values=c("#0392cf","#ee4035","#128B03")) +
  font("xlab", size = 10) +
  font("ylab", size = 10) +
  font("xy.text", size = 10) + theme(legend.position = c(0.8, 0.2))
  
  dev.off()
