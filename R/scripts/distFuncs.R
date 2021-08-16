# All Functions Used For Split Senate Analysis

# ---------- Make L1 distance matrix and nexus file for SplitsTree ----------

# Make vote matrix
makeVoteMat <- function(Sall_votes) {
	
	retParty <- function(code) {
		if (code == 100) {
			return("Dem")
		}
		else if (code == 200){
			return("Rep")
		}
		else{
			return("Ind")
		}
		
	}
	
	Sall_votes[c("party")] <- 
		lapply(Sall_votes[c("party_code")], function(col) map(col,retParty))
	
	
	# yes-->1, no --> 0, abstain --> 0.5
	map_vote <- c(1.0,1.0,1.0, 0.0,0.0,0.0, 0.5,0.5,0.5)
	
	#Get names in Nexus-legal format
	Sall_votes <- Sall_votes %>% mutate(cast_code = map_vote[as.integer(cast_code)])
	
	#Change names --> LASTNAME_FirstInitial_Party
	newName <- function(name){
		new = substr(name, start=1, stop=str_locate(name, ",")[1]+2)
		new = gsub("'", "",new)
		new = gsub("\\(", "",new)
		new = gsub(", ", "_",new)
		new = gsub(" ", "_",new)
		
	}
	
	Sall_votes[c("name")] <- 
		lapply(Sall_votes[c("name")], function(col) map(col,newName))
	
	Sall_votes[c("plotID")] = paste(Sall_votes$name, Sall_votes$party, sep="_")
	
	sub_votes <-  Sall_votes[c('plotID','cast_code','rollnumber')]
	
	votes_df <- pivot_wider(sub_votes, names_from = rollnumber, values_from = cast_code)
	
	votes_df <- na.omit(votes_df) #Remove members who were not present for full term
	
	
	#Make numeric df, without names column
	votes_df <- as.data.frame(votes_df)
	rownames(votes_df) <- votes_df$plotID
	votes_df <- subset(votes_df,select=-c(plotID))
	
	return(votes_df)
}

# Make metadata matrix
makeMetaMat <- function(Sall_votes) {
	
	retParty <- function(code) {
		if (code == 100) {
			return("Dem")
		}
		else if (code == 200){
			return("Rep")
		}
		else{
			return("Ind")
		}
		
	}
	
	Sall_votes[c("party")] <- 
		lapply(Sall_votes[c("party_code")], function(col) map(col,retParty))
	
	
	
	#Change names --> LASTNAME_FirstInitial_Party
	newName <- function(name){
		new = substr(name, start=1, stop=str_locate(name, ",")[1]+2)
		new = gsub("'", "",new)
		new = gsub("\\(", "",new)
		new = gsub(", ", "_",new)
		new = gsub(" ", "_",new)
		
	}
	
	Sall_votes[c("name")] <- 
		lapply(Sall_votes[c("name")], function(col) map(col,newName))
	
	Sall_votes[c("plotID")] = paste(Sall_votes$name, Sall_votes$party, sep="_")

	
	
	#Make new names the rownames
	votes_df <- Sall_votes
	#rownames(votes_df) <- votes_df$plotID
	
	return(votes_df)
}

#Generate NEXUS distance matrix from vote matrix
makeDistMat <- function(Sall_votes,outfile='dist.nex') {
	
	votes_df <- makeVoteMat(Sall_votes)
	
	
	#Get pairwise (L1) distances
	d <- dist(votes_df,method="manhattan",upper = FALSE,diag = TRUE)
	
	#Write distance matrix as nexus file
	write.nexus.dist(d,file=outfile, append = FALSE, upper = FALSE,
									 diag = TRUE, digits = getOption("digits"))
	
	return(d)
	
}

#Generate normal distance matrix from vote matrix (FOR PACKAGE)
makeDistMat2 <- function(Sall_votes) {
	
	votes_df <- makeVoteMat(Sall_votes)
	
	
	#Get pairwise (L1) distances
	d <- unname(as.matrix(dist(votes_df,method="manhattan",upper = TRUE,diag = TRUE)))
	
	#vals <- d[2:-1,2:-1]
	
	
	return(list('Dists'=d, 'Labs'=rownames(votes_df)))
	
}

# ---------- Convert circular split-system weights to pairwise distances ----------
pairSplitDists <- function(splitMat) {
	n <- dim(splitMat)[2]
	# Get split-binary matrix
	splitMat_sub <- splitMat[,3:n]
	splitMat_sub<- as.data.frame(splitMat_sub)
	mems <- dim(splitMat_sub)[2]
	# Get split weights
	w <- as.data.frame(splitMat[,2])
	
	# Sum of all splits with i != j at unique i,j pairs (regardless of order)
	posPairs <- combn(c(1:mems), 2)
	
	splitDists <- matrix(0,mems,mems)
	
	# Go through all pairs, and calculate split distance
	pairs <- dim(posPairs)[2]
	for (p in c(1:pairs)){
		i = posPairs[1,p]
		j = posPairs[2,p]
		# Get rows where i,j are on opp sides of split
		opp <- splitMat_sub[splitMat_sub[,i] != splitMat_sub[,j],]
		splits <- as.integer(rownames(opp))
		
		dist <- sum(w[splits,])
		
		splitDists[i,j] = dist
		splitDists[j,i] = dist

	}
	
	splitDists <- as.data.frame(splitDists)
	rownames(splitDists) <- colnames(splitMat_sub)
	colnames(splitDists) <- colnames(splitMat_sub)
	
	return(as.dist(splitDists, upper = FALSE,diag = TRUE))
}

# ---------- Difference between distance matrices (Split or L1) ----------
distDiff <- function(l1Dists, splitDists) {
	
	diff <- l1Dists - splitDists
	delta <- diff / l1Dists
	delta <- -1*delta*100
	return(as.matrix(delta))
	
}

# ---------- Find distance of senators to center of a congress ----------
centerDist <- function(splitMat) {

	n <- dim(splitMat)[2]
	# Get split-binary matrix
	splitMat_sub <- splitMat[,3:n]
	splitMat_sub<- as.data.frame(splitMat_sub)
	mems <- dim(splitMat_sub)[2]
	# Get split weights
	w <- as.data.frame(splitMat[,2])
	
	colNames <- colnames(splitMat_sub)
	memInd <- c(1:mems)
	
	centerDists <- matrix(0, 1,mems)
	
	for (m in memInd) {
		
		if (stri_detect_fixed(colNames[m], "Rep")) {
			filt <- !stri_detect_fixed(colNames, "Rep")
			others <- memInd[filt]
		}
		else{
			filt <- stri_detect_fixed(colNames, "Rep")
			others <- memInd[filt]
		}
	
		
		
		# Get rows where i,j are on opp sides of split
		# Rows where all members of other part of opposite to this member
		pos <- splitMat_sub[,m] != splitMat_sub[,others]
		collapse <- (rowSums(pos) == ncol(pos))
		
		opp <- splitMat_sub[collapse,]
		splits <- as.integer(rownames(opp))
		
		dist <- sum(w[splits,])
		
		centerDists[,m] <- dist
		
	}
	colnames(centerDists) <- colNames
	return(centerDists)
	
}

# ---------- Plot center dists for a congress ----------
plotCenterDist <- function(centerDists) {
	colNames <- colnames(centerDists)

	filt <- stri_detect_fixed(colNames, "Rep")
	repValues <- centerDists[,filt]
	demValues <- centerDists[,!filt]
	
	sortRep <- as.data.frame(sort(unlist(repValues),decreasing=FALSE))
	colnames(sortRep) <- "dists"
	sortDem <- as.data.frame(sort(unlist(demValues),decreasing=TRUE))
	colnames(sortDem) <- "dists"
	
	forPlot <- rbind(sortDem,sortRep)
	
	# Add colors
	blue <- rep("#0392cf",length(demValues))
	red <- rep("#ee4035",length(repValues))
	colors <- c(blue,red)
	
	pdf(file = "./figures/center_dists.pdf",width=7,height=5)
	x<- barplot(forPlot$dists,names=rownames(forPlot),col = colors,
							space = 0.3, width = .01, border=NA,xaxt="none",cex.axis=0.5)
	axis(1, at=x,labels=rownames(forPlot),
			 las=3,cex.axis=0.3)
	lines(x=rep((x[length(demValues)]+x[length(demValues)+1])/2,2),
				y=c(0,max(forPlot$dists)),
				lty = 2)
	dev.off()
	
}

# ---------- Runs test (Wald-Wolfowitz test) , probability of <= some numRuns of runs vs random (H_0) ----------
calcRunsTest <- function(numInGrp,numOutGrp,numRuns) {
	
	 # Z-test
		tot <- numInGrp+numOutGrp
		mu <- (2*numInGrp*numOutGrp)/(tot) + 1
		sigma <- sqrt((mu-1)*(mu-2)/(tot-1))
		c <- 0.5
		
		z <- (numRuns - mu + c)/(sigma) #for continuity correction

		# One-tailed
		return(pnorm(-abs(z)))


}

# -------------- Calculate Disagreement of Split Senate Members' Votes ---------------
calcDisagree <- function(votesDem, names) {
	votesDem_sub <- votesDem[names,] 
	
	#Indices of votes where all named members vote the same
	sameInds <- sapply(votesDem_sub, function(x) length(unique(x)) == 1 ) 
	# Keep rollcall numbers associated with votes were all vote the same
	rollcalls <- as.numeric(colnames(votesDem_sub)) #1:length(sameInds)
	rollcalls <- rollcalls[sameInds]
	
	#Find votes where all 5 candidates vote the same
	sameVotes <- votesDem[,sameInds]
	scores <- rep(0,length(sameVotes))
	for (i in 1:length(sameVotes)){
		# Just need to check if other member matches with 1 candidate's vote
		comp <- sameVotes[names[1],i] 
		v <- sameVotes[,i]
		same <- v == comp
		scores[i] <- sum(same)/length(v)
		
	}
	
	sortedScores <- sort(scores, index.return=TRUE)
	justScores <- sortedScores$x
	newRoll <- rollcalls[sortedScores$ix]
	
	voteTypes <- as.numeric(votesDem[names[1],as.character(newRoll)])
	
	#Set colors
	toPlot <- data.frame(justScores, newRoll,voteTypes)
	
	colnames(toPlot) <- c('Percent','Rollcall','Vote')
	
	return(toPlot)

}


# ---------------------- Rank votes by p-value for given split ------------------
calcSplitVotPval <- function(votesDem,bin_names) {
	rollcallNums <- 1:length(votesDem)
	pvals <- numeric(length(votesDem))
	#For each vote (Col of votesDem)
	for (i in rollcallNums){
		
		votes <- votesDem[[i]]
		#Remove all vote positions with 1/2 (abstain) (& name positions)
		
		#Construct contingency table for each vote relative to split of interest 
		zero_names <-  bin_names == 0
		one_names <-  bin_names == 1
		
		zeroFilt_votes <- votes[zero_names]
		oneFilt_votes <- votes[one_names]
		
		# 0-0, 0-0.5, 0-1
		zeroZero <- sum(zeroFilt_votes == 0)
		zeroHalf <- sum(zeroFilt_votes == 0.5)
		zeroOne <- sum(zeroFilt_votes == 1)
		
		# 1-0, 1-0.5, 1-1
		oneZero <- sum(oneFilt_votes == 0)
		oneHalf <- sum(oneFilt_votes == 0.5)
		oneOne <- sum(oneFilt_votes == 1)
		

		#Calculate p-value with Fishers exact test
		pvals[i] <- fisher.test(rbind(c(zeroZero,zeroHalf,zeroOne),c(oneZero,oneHalf,oneOne)), alternative="two.sided")$p.value
		
		
	}
	return(pvals)
}


# Make vote matrix (NEXUS compatible), keep senators with NAs (missing votes) (Turn into if statement)
makeVoteMatwithNA <- function(Sall_votes) {
	
	retParty <- function(code) {
		if (code == 100) {
			return("Dem")
		}
		else if (code == 200){
			return("Rep")
		}
		else{
			return("Ind")
		}
		
	}
	
	Sall_votes[c("party")] <- 
		lapply(Sall_votes[c("party_code")], function(col) map(col,retParty))
	
	
	# yes-->1, no --> 0, abstain --> 0.5
	map_vote <- c(1.0,1.0,1.0, 0.0,0.0,0.0, 0.5,0.5,0.5)
	
	#Get names in Nexus-legal format
	Sall_votes <- Sall_votes %>% mutate(cast_code = map_vote[as.integer(cast_code)])
	
	#Change names --> LASTNAME_FirstInitial_Party
	newName <- function(name){
		new = substr(name, start=1, stop=str_locate(name, ",")[1]+2)
		new = gsub("'", "",new)
		new = gsub("\\(", "",new)
		new = gsub(", ", "_",new)
		new = gsub(" ", "_",new)
		
	}
	
	Sall_votes[c("name")] <- 
		lapply(Sall_votes[c("name")], function(col) map(col,newName))
	
	Sall_votes[c("plotID")] = paste(Sall_votes$name, Sall_votes$party, sep="_")
	
	sub_votes <-  Sall_votes[c('plotID','cast_code','rollnumber')]
	
	votes_df <- pivot_wider(sub_votes, names_from = rollnumber, values_from = cast_code)
	
	#votes_df <- na.omit(votes_df) #Remove members who were not present for full term
	
	
	#Make numeric df, without names column
	votes_df <- as.data.frame(votes_df)
	rownames(votes_df) <- votes_df$plotID
	votes_df <- subset(votes_df,select=-c(plotID))
	
	return(votes_df)
}

