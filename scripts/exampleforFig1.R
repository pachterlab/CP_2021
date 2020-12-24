#Make numeric df, without names column
distEx <- cbind(c(0,21.5,15,5,22,11),c(21.5,0,15.5,23.5,4.5,12.5),c(15,15.5,0,12,13,16),
                c(5,23.5,12,0,23,14),c(22,4.5,13,23,0,15),c(11,12.5,16,14,15,0))

dist_df <- as.data.frame(distEx)
rownames(dist_df) <- 1:6


#Write distance matrix as nexus file
fname <- paste("./data/dist_ex.nex",sep="")
write.nexus.dist(dist_df,file=fname, append = FALSE, upper = FALSE,
                 diag = TRUE, digits = getOption("digits"))


