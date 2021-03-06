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
  #pdf(file = "./figures/splitDists_116.pdf",width=10,height=10)
  pheatmap(as.matrix(splitDists_116),scale="none",
           col=viridis(max(splitDists_116),direction = -1),
           main = "Senator Pairwise Split Distances",
           fontsize_row = 4,fontsize_col = 5)
  
  #dev.off()


# ------------- Compare dist changes from input to split-distance output -------------


  corr <- cor(as.vector(splitDists_116),as.vector(l1DistsAll))
  mylabel = bquote(italic(corr) == .(format(corr, digits = 3)))
  
  #Get pos of Rep names (of 98) and Dem (non - Rep) names (of 98)
  corrColors <-array(0L,dim(as.matrix(splitDists_116)))
  names <- rownames(as.matrix(splitDists_116))
  repInd <- stri_detect_fixed(names, "Rep")
  
  allInd <- 1:dim(corrColors)[1]
  repRows <- allInd[repInd]
  demRows <- allInd[!repInd]
  
  #For each Rep row, set pos with Dem names --> set Purple (mix) else Red
  for (i in repRows){
    corrColors[i,demRows] = 'mediumpurple1'
    corrColors[i,repRows] = "#ee4035"
  }
  
  #For each Dem row, set pos with Rep names --> Purple else Blue
  for (i in demRows){
    corrColors[i,demRows] = "#0392cf"
    corrColors[i,repRows] = 'mediumpurple1'
  }
  
  
  matSplitDists <- as.matrix(splitDists_116)
  matL1Dists <- as.matrix(l1DistsAll)
  
  #convex hull outlier points
  hpts <- chull(as.vector(matSplitDists),as.vector(matL1Dists))

  pdf(file = "./figures/l1vsplit_dists_corr.pdf",width=10,height=10)
  
  plot(matSplitDists,matL1Dists,col = alpha(corrColors,0.3),pch=19,cex=1.5,
       xlab='NeighborNet Distances',ylab='L1 Distances',cex.main=1.25, cex.lab=1.5, cex.axis=0.75)
  text(x = 500, y = 300, labels = mylabel,cex = 1.5)
  points(as.vector(matSplitDists)[hpts],as.vector(matL1Dists)[hpts],col='black',pch=1,cex=1.5,lwd=2)
  legend("bottomright", legend = c('Rep-Rep','Dem-Dem','Rep-Dem'), 
         col = c("#ee4035","#0392cf",'mediumpurple1'), pch = 19, bty = "n",cex = 1.5)
  
  dev.off()
  
  
  