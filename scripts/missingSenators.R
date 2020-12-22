#Senate member voting records
Sall_votes <- as_tibble(read_csv("./data/Sall_votes_withPartyAndNames.csv") )

#Information on each vote (rollcall number)
Sall_vote_dates <- as_tibble(read.csv("./data/Sall_rollcalls.csv", colClasses = c("numeric","character","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character","character","character","character","character")))


# ------------- Make distance matrix and nexus output for 116th congresses -------------
cong <- 116
Sall_votes_sub  <- Sall_votes %>% filter(congress == cong)

Sall_votes_rep <- Sall_votes %>% filter(congress == cong)
Sall_votes_rep  <- Sall_votes_rep %>% filter(party_code == 200)

Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)

# --------------- Get distances for Senate members not present for full congress -----------------

# KELLY, Mark Edward, LOEFFLER, Kelly (and her preceding senator ISAKSON, ) having missing votes for the 116th congress 
#Get votes with NA (missing votes) (see all individuals with missing votes)
votes_df_withna <- makeVoteMatwithNA(Sall_votes_sub)
missingData_names <- rownames(votes_df_withna [rowSums(is.na(votes_df_withna )) > 0,])

    #Get votes for Sen. Loeffler
    loeffler_votes <- Sall_votes_sub %>% filter(name == 'LOEFFLER, Kelly')
    loeffler_roll <- loeffler_votes$rollnumber
    
    loeffler_rep_df <- Sall_votes_rep %>% filter(rollnumber %in% loeffler_roll)
    
    #Make distance matrix for subset of rollnumbers
    fname <- paste("./data/dist_subset_loeffler_",as.character(cong),"th.nex",sep="")
    sub_rep_dists <- makeDistMat(loeffler_rep_df,fname)
    
#   _______________________________________    
    #Get votes for Sen. Isakson
    loeffler_votes <- Sall_votes_sub %>% filter(name == 'ISAKSON, Johnny')
    loeffler_roll <- loeffler_votes$rollnumber
    
    loeffler_rep_df <- Sall_votes_rep %>% filter(rollnumber %in% loeffler_roll)
    
    #Make distance matrix for subset of rollnumbers
    fname <- paste("./data/dist_subset_isakson_",as.character(cong),"th.nex",sep="")
    sub_rep_dists <- makeDistMat(loeffler_rep_df,fname)
    
#   _______________________________________
    #Get votes for Sen. Kelly
    loeffler_votes <- Sall_votes_sub %>% filter(name == 'KELLY, Mark Edward')
    loeffler_roll <- loeffler_votes$rollnumber
    
    loeffler_dem_df <- Sall_votes_dem %>% filter(rollnumber %in% loeffler_roll)
    
    #Make distance matrix for subset of rollnumbers
    fname <- paste("./data/dist_subset_kelly_",as.character(cong),"th.nex",sep="")
    sub_dem_dists <- makeDistMat(loeffler_dem_df,fname)
    
#   _______________________________________
    #Get votes for Sen. McSally
    loeffler_votes <- Sall_votes_sub %>% filter(name == 'McSALLY, Martha')
    loeffler_roll <- loeffler_votes$rollnumber
    
    loeffler_rep_df <- Sall_votes_rep %>% filter(rollnumber %in% loeffler_roll)
    
    #Make distance matrix for subset of rollnumbers
    fname <- paste("./data/dist_subset_mcsally_",as.character(cong),"th.nex",sep="")
    sub_rep_dists <- makeDistMat(loeffler_rep_df,fname)

# Kelly has very few votes to plot





