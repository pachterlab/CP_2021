#Senate member voting records
Sall_votes <- as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#Information on each vote (rollcall number)
Sall_vote_dates <- as_tibble(read.csv("./data/Sall_rollcalls.csv", colClasses = c("numeric","character","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character","character","character","character","character")))


# ------------- Make distance matrix and nexus output for 116th congresses -------------
cong <- 116
Sall_votes_sub  <- Sall_votes %>% filter(congress == cong)

# --------------- Get distances for Senate members not present for full congress -----------------

# KELLY, Mark Edward, LOEFFLER, Kelly (and her preceding senator ISAKSON, ) having missing votes for the 116th congress 
#Get votes with NA (missing votes) (see all individuals with missing votes)
votes_df_withna <- makeVoteMatwithNA(Sall_votes_sub)
missingData_names <- rownames(votes_df_withna [rowSums(is.na(votes_df_withna )) > 0,])

#Get votes for Sen. Loeffler
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





