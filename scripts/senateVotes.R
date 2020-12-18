source('scripts/distFuncs.R')
#Senate member voting records
Sall_votes = as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#Information on each vote (rollcall number)
Sall_vote_dates = as_tibble(read_csv("./data/Sall_rollcalls.csv"))


# ------------- Make distance matrix and nexus output for 116th congresses -------------
cong = 116
Sall_votes_sub  <- Sall_votes %>% filter(congress == cong)
  
fname <- paste("./data/disttest_",as.character(cong),"th.nex",sep="")
  
l1DistsAll <- makeDistMat(Sall_votes_sub,fname)




# ------------- Get split distances for 116th congress ------------- (From SplitsTree)

splitMat_116 <- read_delim("./data/splitWeights_tab_116th.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

splitDists_116 <- pairSplitDists(splitMat_116)

# Plot pariwise Split distances
par(mar=c(8,4,4,1)+.1,cex.main=0.5,cex.axis=0.1)
pdf(file = "./splitDists_116.pdf",width=10,height=10)
pheatmap(as.matrix(splitDists_116),scale="none",
              col=viridis(max(splitDists_116),direction = -1),
              main = "Senator Pairwise Split Distances",
              fontsize_row = 4,fontsize_col = 5)

dev.off()


# ------------- Compare dist changes from input to split-distance output -------------

percDiff <- distDiff(l1DistsAll, splitDists_116 )



# Plot percent diffs between L1 and NNet LS distances (Split distances)
par(mar=c(7,6,4,1)+.1)
pdf(file = "./percDiff_l1_fromNNet.pdf",width=10,height=10)
boxplot(percDiff,labels = rownames(percDiff),
        main="Percent Difference in Pairwise L1 Dist vs Split Distances",
        xaxt="none",
        ylab = 'Percent Change (from L1 to Split)',
        cex.lab = 0.8, cex.main = 0.8,cex.axis=0.8)
axis(1, at=c(1:length(rownames(percDiff))),labels=rownames(percDiff), las=3,cex.axis=0.35)
dev.off()


corr <- cor(as.vector(splitDists_116),as.vector(l1DistsAll))
mylabel = bquote(italic(corr) == .(format(corr, digits = 3)))

pdf(file = "./l1vsplit_dists_corr.pdf",width=10,height=10)
plot(as.matrix(splitDists_116 ),as.matrix(l1DistsAll ),
     xlab='NeighborNet Distances',ylab='L1 Distances')
text(x = 500, y = 300, labels = mylabel)
dev.off()


#  ------------- Get distances of all Senators from 'center' -------------
centerDists <- centerDist(splitMat_116) 

plotCenterDist(centerDists)

#  ------------- Within-party strucures and distances -------------

# Distance between non-Republican members only --> Check if Dem Primary Group still exists
cong = 116
Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)

fname <- paste("./data/dist_dem_",as.character(cong),"th.nex",sep="")

l1DistsDem <- makeDistMat(Sall_votes_dem,fname)


# Runs Test for Dem Primary Candidates grouping (at least 6 of 7 ordered contiguously)
numCands = 7 #Including Gillibrand and Bennet
numRestAllParties = dim(as.matrix(l1DistsAll))[1] - numCands
numRestDem = dim(as.matrix(l1DistsDem))[1] - numCands

pvalAll = calcRunsTest(numCands,numRestAllParties,5,exactTest = FALSE) #Gillibrand not ordered sequentially with others
print(pvalAll)
pvalDem = calcRunsTest(numCands,numRestDem,5,exactTest = FALSE) #Bennet not ordered sequentially with others
print(pvalDem)


# Distance between Republican members only 
cong = 116
Sall_votes_rep <- Sall_votes %>% filter(congress == cong)
Sall_votes_rep  <- Sall_votes_rep %>% filter(party_code == 200)

fname <- paste("./data/dist_rep_",as.character(cong),"th.nex",sep="")

l1DistsRep <- makeDistMat(Sall_votes_rep,fname)



# ------------- Ranking votes of Dem primary candidates by agreement with rest of party -------------

cong = 116
Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)


votesDem <- makeVoteMat(Sall_votes_dem)
rows <- rownames(votesDem)

toPlot <- calcDisagree(votesDem,c("SANDERS_B_Ind","WARREN_E_Dem",
                        "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem"))

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) + 
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=8)) +
  xlab('Vote Rollcall Number') + 
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates.pdf",width=5, height=3)

#Determine which dates votes with all 5 candidates voting the same were on (with low rest of party-agreement)
lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
dates <- Sall_vote_dates %>% filter(congress %in% cong)
dates <- dates %>% filter(rollnumber %in% lowAgree)  # rollcalls 182 --> 428 include low percentage votes

#Get dist matrix for SplitsTree with these particular votes removed (See their effect on ordering/visual)
# allRolls <- 1:length(unique(Sall_votes_sub$rollnumber))
# remaining <- allRolls[! allRolls %in% lowAgree]
#   
# Sall_votes_allCong_removVotes <- Sall_votes_sub %>% filter(rollnumber %in% remaining)
# 
# #Same with only within-Dem. party votes
# Sall_votes_dem_removVotes <- Sall_votes_dem %>% filter(rollnumber %in% remaining)

# fname <- paste("./data/dist_all_remov_",as.character(cong),"th.nex",sep="")
# removDistsAll <- makeDistMat(Sall_votes_allCong_removVotes,fname)
# 
# fname <- paste("./data/dist_dem_remov_",as.character(cong),"th.nex",sep="")
# removDistsDem <- makeDistMat(Sall_votes_dem_removVotes,fname)


# Within split, Sanders and Harris are closest --> asses disagreement for just these two individuals
toPlot <- calcDisagree(votesDem,c("SANDERS_B_Ind",
                                  "HARRIS_K_Dem"))

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) +
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=8)) +
  xlab('Vote Rollcall Number') +
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates_sub.pdf",width=5, height=3)

lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
dates_S_H <- Sall_vote_dates %>% filter(congress %in% cong)
dates_S_H <- dates_S_H %>% filter(rollnumber %in% lowAgree)


# Booker and Warren are next closest split
toPlot <- calcDisagree(votesDem,c("WARREN_E_Dem",
                                  "BOOKER_C_Dem"))

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) +
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=8)) +
  xlab('Vote Rollcall Number') +
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates_sub_02.pdf",width=5, height=3)
lowAgree <- toPlot$Rollcall[toPlot$Percent < 0.25]
dates_B_W <- Sall_vote_dates %>% filter(congress %in% cong)
dates_B_W <- dates_S_H %>% filter(rollnumber %in% lowAgree)



# Test disgreement profiles for another clustered groups of Democrats 
toPlot <- calcDisagree(votesDem,c("ROSEN_J_Dem","CARDIN_B_Dem",
                                  "CORTEZ_MASTO_C_Dem"))

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) + 
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=8)) +
  xlab('Vote Rollcall Number') + 
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates_rand.pdf",width=5, height=3)


toPlot <- calcDisagree(votesDem,c("MANCHIN_J_Dem","SINEMA_K_Dem",
                                  "JONES_G_Dem"))

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) + 
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=8)) +
  xlab('Vote Rollcall Number') + 
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates_rand2.pdf",width=5, height=3)



# --------------- Get distances for Senate members not present for full congress -----------------

# KELLY, Mark Edward, LOEFFLER, Kelly (and her preceding senator ISAKSON, ) having missing votes for the 116th congress 

votes_df_withna <- makeVoteMatwithNA(Sall_votes_sub)
missingData_names <- rownames(votes_df_withna [rowSums(is.na(votes_df_withna )) > 0,])

loeffler_votes <- Sall_votes_sub %>% filter(name == 'LOEFFLER, Kelly')
loeffler_roll <- loeffler_votes$rollnumber

loeffler_df <- Sall_votes_sub %>% filter(rollnumber %in% loeffler_roll)

#Make distance matrix for subset of rollnumbers
fname <- paste("./data/dist_subset_",as.character(cong),"th.nex",sep="")
sub_dists <- makeDistMat(loeffler_df,fname)

loeffler_rep_df <- Sall_votes_rep %>% filter(rollnumber %in% loeffler_roll)

#Make distance matrix for subset of rollnumbers
fname <- paste("./data/dist_subset_rep_",as.character(cong),"th.nex",sep="")
sub_rep_dists <- makeDistMat(loeffler_rep_df,fname)

# kelly_votes <- Sall_votes_sub %>% filter(name == 'KELLY, Mark Edward')
# kelly_roll <- kelly_votes$rollnumber





