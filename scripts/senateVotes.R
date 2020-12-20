source('scripts/distFuncs.R')
#Senate member voting records
Sall_votes <- as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#Information on each vote (rollcall number)
Sall_vote_dates <- as_tibble(read.csv("./data/Sall_rollcalls.csv", colClasses = c("numeric","character","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character","character","character","character","character")))


# ------------- Make distance matrix and nexus output for 116th congresses -------------
  cong <- 116
  Sall_votes_sub  <- Sall_votes %>% filter(congress == cong)
    
  fname <- paste("./data/dist_",as.character(cong),"th.nex",sep="")
    
  l1DistsAll <- makeDistMat(Sall_votes_sub,fname)




# ------------- Get split distances for 116th congress ------------- (From SplitsTree)

  splitMat_116 <- read_delim("./data/splitWeights_tab_116th.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
  
  splitDists_116 <- pairSplitDists(splitMat_116)
  
  # Plot pairwise Split distances
  par(mar=c(8,4,4,1)+.1,cex.main=0.5,cex.axis=0.1)
  pdf(file = "./figures/splitDists_116.pdf",width=10,height=10)
  pheatmap(as.matrix(splitDists_116),scale="none",
                col=viridis(max(splitDists_116),direction = -1),
                main = "Senator Pairwise Split Distances",
                fontsize_row = 4,fontsize_col = 5)
  
  dev.off()


# ------------- Compare dist changes from input to split-distance output -------------


    corr <- cor(as.vector(splitDists_116),as.vector(l1DistsAll))
    mylabel = bquote(italic(corr) == .(format(corr, digits = 3)))
    
    pdf(file = "./figures/l1vsplit_dists_corr.pdf",width=10,height=10)
    plot(as.matrix(splitDists_116),as.matrix(l1DistsAll),
         xlab='NeighborNet Distances',ylab='L1 Distances')
    text(x = 500, y = 300, labels = mylabel)
    dev.off()

    #Get pos of Rep names (of 98) and Dem names (of 98)
    
    #For each Rep row, set pos with Dem names --> set Purple (mix)

#  ------------- Get distances of all Senators from 'center' -------------
  centerDists <- centerDist(splitMat_116) 
  
  plotCenterDist(centerDists)

#  ------------- Within-party strucures and distances -------------

  # Distance between non-Republican members only --> Check if Dem Primary Group still exists
    cong <- 116
    Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
    Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)
    
    fname <- paste("./data/dist_dem_",as.character(cong),"th.nex",sep="")
    
    l1DistsDem <- makeDistMat(Sall_votes_dem,fname)


  # Runs Test for Dem Primary Candidates grouping (at least 6 of 7 ordered contiguously)
    numCands <- 7 #Including Gillibrand and Bennet
    numRestAllParties <- dim(as.matrix(l1DistsAll))[1] - numCands
    numRestDem <- dim(as.matrix(l1DistsDem))[1] - numCands
    
    pvalAll <- calcRunsTest(numCands,numRestAllParties,7,exactTest = FALSE) #Gillibrand not ordered sequentially with others
    print(pvalAll)
    pvalDem <- calcRunsTest(numCands,numRestDem,7,exactTest = FALSE) #Bennet not ordered sequentially with others
    print(pvalDem)

  
  # Distance between Republican members only 
    cong <- 116
    Sall_votes_rep <- Sall_votes %>% filter(congress == cong)
    Sall_votes_rep  <- Sall_votes_rep %>% filter(party_code == 200)
    
    fname <- paste("./data/dist_rep_",as.character(cong),"th.nex",sep="")
    
    l1DistsRep <- makeDistMat(Sall_votes_rep,fname)



# ------------- Ranking votes of Dem primary candidates by agreement with rest of party -------------

  cong <- 116
  Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
  Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)
  
  
  votesDem <- makeVoteMat(Sall_votes_dem)
  rows <- rownames(votesDem)
  
  toPlot <- calcDisagree(votesDem,c("SANDERS_B_Ind","WARREN_E_Dem",
                          "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem"))
  
  ggplot(toPlot, aes(x=Rollcall, y=Percent)) + 
    geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8)) +
    xlab('Vote Rollcall Number') + 
    ylab('Percent Agreement (Within Party Votes)') +
    labs(color="Vote Type")+
    scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"))
  
  ggsave("./figures/demVoteAgreementCandidates.pdf",width=5, height=3)

  #Determine which dates votes with all 5 candidates voting the same were on (with low rest of party-agreement)
    lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
  
  #Get dist matrix for SplitsTree with these particular votes removed (See their effect on ordering/visual)
    allRolls <- 1:length(unique(Sall_votes_sub$rollnumber))
    remaining <- allRolls[! allRolls %in% lowAgree]
    
    Sall_votes_allCong_removVotes <- Sall_votes_sub %>% filter(rollnumber %in% remaining)
    
    #Same with only within-Dem. party votes
    Sall_votes_dem_removVotes <- Sall_votes_dem %>% filter(rollnumber %in% remaining)
    
    fname <- paste("./data/dist_all_remov_",as.character(cong),"th.nex",sep="")
    removDistsAll <- makeDistMat(Sall_votes_allCong_removVotes,fname)
    
    fname <- paste("./data/dist_dem_remov_",as.character(cong),"th.nex",sep="")
    removDistsDem <- makeDistMat(Sall_votes_dem_removVotes,fname)

    removDem <- makeVoteMat(Sall_votes_dem_removVotes)
    ppl <- c("SANDERS_B_Ind","WARREN_E_Dem","BOOKER_C_Dem","HARRIS_K_Dem")
    toPlot <- calcDisagree(removDem,ppl)
    
    ggplot(toPlot, aes(x=Rollcall, y=Percent)) + 
      geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title=element_text(size=8)) +
      xlab('Vote Rollcall Number') + 
      ylab('Percent Agreement (Within Party Votes)') +
      labs(color="Vote Type")+
      scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"))
    
    

  # Test disagreement profiles for another clustered groups of Democrats 
    toPlot <- calcDisagree(votesDem,c("MANCHIN_J_Dem","SINEMA_K_Dem",
                                      "JONES_G_Dem"))
    
    ggplot(toPlot, aes(x=Rollcall, y=Percent)) + 
      geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title=element_text(size=8)) +
      xlab('Vote Rollcall Number') + 
      ylab('Percent Agreement (Within Party Votes)') +
      labs(color="Vote Type")+
      scale_color_manual(values=c("#8DD3C7","#BEBADA"))
    
    ggsave("./figures/demVoteAgreementCandidates_rand2.pdf",width=5, height=3)


# ------------- Ranking votes in splits of Dem Senators by p-value (contribution to split) -------------
  #Define split of interest 
  
  #Get rownames --> binary list (1 - in Primary Candidate split, 0 - else)
  ppl <- c("SANDERS_B_Ind","WARREN_E_Dem",
           "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem")
  names <- rownames(votesDem)
  
  #Binarize names for members on one side of split
  bin_names <- as.integer(names %in% ppl)
  
  pvals <- calcSplitVotPval(votesDem,bin_names)
  plot(rollcallNums,-log10(pvals))
  #Colored plot relating to temporal votes
  voteTypes <- as.numeric(votesDem[ppl[1],rollcallNums])
  
  pvalPlot <- data.frame(rollcallNums, -log10(pvals),voteTypes)
  colnames(pvalPlot) <- c('Rollcall','-log10(P-value)','Vote')
  
  ggplot(pvalPlot, aes(x=Rollcall, y=`-log10(P-value)`)) + 
    geom_point(aes(x=Rollcall, y=`-log10(P-value)`,color=as.factor(Vote)),alpha=0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8)) +
    xlab('Vote Rollcall Number') + 
    ylab('-log10(p-value)') +
    labs(color="Vote Type")+
    scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"))
  ggsave("./figures/pvalVotes_Candidates.pdf",width=5, height=3)
  
  #Get rownames --> binary list (Other cluster of democratic senators)
  ppl <- c("MANCHIN_J_Dem","SINEMA_K_Dem",
           "JONES_G_Dem")
  names <- rownames(votesDem)
  
  #Binarize names for members on one side of split
  bin_names <- as.integer(names %in% ppl)
  
  pvals <- calcSplitVotPval(votesDem,bin_names)
  plot(rollcallNums,-log10(pvals))
  #Colored plot relating to temporal votes
  voteTypes <- as.numeric(votesDem[ppl[1],rollcallNums])
  
  pvalPlot <- data.frame(rollcallNums, -log10(pvals),voteTypes)
  colnames(pvalPlot) <- c('Rollcall','-log10(P-value)','Vote')
  
  ggplot(pvalPlot, aes(x=Rollcall, y=`-log10(P-value)`)) + 
    geom_point(aes(x=Rollcall, y=`-log10(P-value)`,color=as.factor(Vote)),alpha=0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8)) +
    xlab('Vote Rollcall Number') + 
    ylab('-log10(p-value)') +
    labs(color="Vote Type")+
    scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"))
  ggsave("./figures/pvalVotes_rand2.pdf",width=5, height=3)
  
  

