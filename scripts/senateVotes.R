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
    
    pvalAll <- calcRunsTest(numCands,numRestAllParties,7) #Gillibrand not ordered sequentially with others
    print(pvalAll)
    pvalDem <- calcRunsTest(numCands,numRestDem,7) #Bennet not ordered sequentially with others
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
  
  ppl<- c("SANDERS_B_Ind","WARREN_E_Dem",
          "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem")
  toPlot <- calcDisagree(votesDem,ppl)
  
  ggplot(toPlot, aes(x=Rollcall, y=Percent)) + 
    geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8)) +
    xlab('Vote Rollcall Number') + 
    ylab('Percent Agreement (Within Party Votes)') +
    labs(color="Vote Type")+
    scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"),labels= c("Yea", "Abstain","Nay"))
  
  ggsave("./figures/demVoteAgreementCandidates.pdf",width=5, height=3)

  #Determine which dates votes with all 5 candidates voting the same were on (with low rest of party-agreement)
  lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
  
  #Get dist matrix for SplitsTree with these particular votes removed (See their effect on ordering/visual)
  allRolls <- unique(Sall_votes_sub$rollnumber)
  remaining <- allRolls[! allRolls %in% lowAgree]
   
  #With only within-Dem. party votes
  Sall_votes_dem_removVotes <- Sall_votes_dem %>% filter(rollnumber %in% remaining)

  fname <- paste("./data/dist_dem_remov_",as.character(cong),"th.nex",sep="")
  removDistsDem <- makeDistMat(Sall_votes_dem_removVotes,fname)

    # Next split 
      removDem <- makeVoteMat(Sall_votes_dem_removVotes)
      ppl <- c("SANDERS_B_Ind","WARREN_E_Dem","BOOKER_C_Dem","HARRIS_K_Dem")
      toPlot_remov <- calcDisagree(removDem,ppl)
      
      ggplot(toPlot_remov, aes(x=Rollcall, y=Percent)) + 
        geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              axis.title=element_text(size=8)) +
        xlab('Vote Rollcall Number') + 
        ylab('Percent Agreement (Within Party Votes)') +
        labs(color="Vote Type")+
        scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"),labels= c("Yea", "Abstain","Nay"))
      
      ggsave("./figures/demVoteAgreementCandidates_remov1.pdf",width=5, height=3)
      
      lowAgree <- toPlot_remov$Rollcall[toPlot_remov$Percent < 0.25 & toPlot_remov$Vote == 0.5]
      
      #Get dist matrix for SplitsTree with these particular votes removed (See their effect on ordering/visual)
      allRolls <- unique(Sall_votes_dem_removVotes$rollnumber)
      remaining <- allRolls[! allRolls %in% lowAgree]
      
      #With only within-Dem. party votes
      Sall_votes_dem_removVotes_02 <- Sall_votes_dem_removVotes %>% filter(rollnumber %in% remaining)
      
      fname <- paste("./data/dist_dem_remov02_",as.character(cong),"th.nex",sep="")
      removDistsDem <- makeDistMat(Sall_votes_dem_removVotes_02,fname)
      
      
        #Final Split
      
        votesDemRemov02 <- makeVoteMat(Sall_votes_dem_removVotes_02)
        
        toPlot <- calcDisagree(votesDemRemov02,c("SANDERS_B_Ind","WARREN_E_Dem",
                                          "BOOKER_C_Dem","HARRIS_K_Dem"))
        
        ggplot(toPlot, aes(x=Rollcall, y=Percent)) + 
          geom_point(aes(x=Rollcall, y=Percent,color=as.factor(Vote)),alpha=0.7) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                axis.title=element_text(size=8)) +
          xlab('Vote Rollcall Number') + 
          ylab('Percent Agreement (Within Party Votes)') +
          labs(color="Vote Type")+
          scale_color_manual(values=c("#8DD3C7","#FB8072","#BEBADA"),labels= c("Yea", "Abstain","Nay"))
    
    

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
      scale_color_manual(values=c("#8DD3C7","#BEBADA"),labels= c("Yea","Nay"))
    
    ggsave("./figures/demVoteAgreementCandidates_rand2.pdf",width=5, height=3)


# ------------- Ranking votes in splits of Dem Senators by p-value (contribution to split) -------------
  rollcallNums <- 1:length(votesDem)
  #Define split of interest 
      
    #Get rownames --> binary list (1 - in Primary Candidate split, 0 - else)
    ppl <- c("SANDERS_B_Ind","WARREN_E_Dem",
             "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem")
    names <- rownames(votesDem)
    toPlot <- calcDisagree(votesDem,ppl)
    lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
    
    #Binarize names for members on one side of split
    bin_names <- as.integer(names %in% ppl)
    
    pvals <- calcSplitVotPval(votesDem,bin_names)
    #Colored plot relating to temporal votes
    color <- ifelse(rollcallNums %in% lowAgree,'Abstains','Other')
    
    pvalPlot <- data.frame(rollcallNums, -log10(pvals),color)
    colnames(pvalPlot) <- c('Rollcall','-log10(P-value)','color')
    
    ggplot(pvalPlot, aes(x=Rollcall, y=`-log10(P-value)`)) + 
      geom_point(aes(x=Rollcall, y=`-log10(P-value)`,color=color),alpha=0.7) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title=element_text(size=8),plot.title = element_text(size = 8)) +
      xlab('Vote Rollcall Number') + 
      ylab('-log10(p-value)') +
      labs(color="Votes")+
      scale_color_manual(values=c("#FB8072","grey"))+
      ggtitle("Votes for {SANDERS,BOOKER,WARREN,HARRIS,KLOBUCHAR} Split")
    ggsave("./figures/pvalVotes_Candidates.pdf",width=5, height=3)
    
    #Look for time variance of p-value rankings
    loessMod <- loess(`-log10(P-value)` ~ Rollcall, data=pvalPlot, span=0.05) # 25% smoothing span
    smoothed <- predict(loessMod) 
    
    plot(pvalPlot$`-log10(P-value)`, x=pvalPlot$Rollcall, type="l", main="Loess Smoothing and Prediction", xlab="Rollcall", ylab="-log10(pval)")
    lines(smoothed, x=pvalPlot$Rollcall, col="red")
    
    res <- loessMod$residuals
    sse <- sum(res^2)  
    
# ----------------------------------------------------------------- 
    
  #Get rownames --> binary list (Other cluster of democratic senators)
  ppl <- c("MANCHIN_J_Dem","SINEMA_K_Dem",
           "JONES_G_Dem")
  names <- rownames(votesDem)
  
  #Binarize names for members on one side of split
  bin_names <- as.integer(names %in% ppl)
  
  pvals <- calcSplitVotPval(votesDem,bin_names)
  #plot(rollcallNums,-log10(pvals))
  
  #Colored plot relating to temporal votes
  sorted_pvals <- sort(-log10(pvals), decreasing = TRUE, index.return=TRUE)
  topInd <- sorted_pvals$ix[sorted_pvals$x>3]
  color <- rollcallNums
  inTop <- color %in% topInd
  color[inTop] <- 'Top Ranked'
  color[!inTop] <- 'Rest'
  
  pvalPlot <- data.frame(rollcallNums, -log10(pvals),color)
  colnames(pvalPlot) <- c('Rollcall','-log10(P-value)','color')
  
  ggplot(pvalPlot, aes(x=Rollcall, y=`-log10(P-value)`)) + 
    geom_point(aes(x=Rollcall, y=`-log10(P-value)`,color=color),alpha=0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8),plot.title = element_text(size = 10)) +
    xlab('Vote Rollcall Number') + 
    ylab('-log10(p-value)') +
    labs(color="Votes")+
    scale_color_manual(values=c("grey","black"))+
    ggtitle("Votes for {MANCHIN,SINEMA,JONES} Split")
  ggsave("./figures/pvalVotes_rand2.pdf",width=5, height=3)
  
  
  #Get vote descriptions for set of ranked votes
  
  Sall_vote_dates_sub  <- Sall_vote_dates %>% filter(congress == cong)
  vote_ques <- Sall_vote_dates_sub$vote_question[Sall_vote_dates_sub$rollnumber %in% topInd]
  
  vote_desc <- Sall_vote_dates_sub$vote_desc[Sall_vote_dates_sub$rollnumber %in% topInd]
  
  plot(as.factor(vote_ques))
  
  #descrPlot  <- data.frame(topInd, vote_ques)
  descrPlot  <- data.frame(1:length(vote_ques), vote_ques)
  colnames(descrPlot ) <- c('Rollcall','Vote Topic')
  
  ggplot(descrPlot) + geom_bar(aes(y = `Vote Topic`),fill = "black",alpha=0.8) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=8),axis.text.x = element_text( size=7),
          axis.text.y = element_text( size=7),plot.title = element_text(size = 10)) +
    ggtitle('Vote Topics for Top Votes')+
    ylab('')+
    xlab('Counts')
  ggsave("./figures/voteTopics.png",width=5, height=3)
