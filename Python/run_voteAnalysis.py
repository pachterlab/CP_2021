
#NEED TO PIP INSTALL FisherExact 

from zipfile import ZipFile
import pandas as pd
import numpy as np
import voteFuncs as vf

import splitspy.nnet.nnet_algo as nnet_algorithm


# Unzip all data
data_dir = 'voteData'
output_dir = 'voteData'
# Create a ZipFile Object and load sample.zip in it
with ZipFile(data_dir+'/voteData.zip', 'r') as zipObj:
   # Extract all the contents of zip file into same directory
   zipObj.extractall('voteData')


#Read in all voting data
Sall_votes = pd.read_csv(data_dir+'/Sall_votes_withPartyAndNames.csv')



#Distance matrix for 116th congress

cong = 116
Sall_votes_sub = Sall_votes[Sall_votes.congress == cong]


voteMat = vf.makeVoteMat(Sall_votes_sub)

labels, matrix = vf.makeDistMat(voteMat)
print('Pairwise Distances Calculated')

cycle, splits = nnet_algorithm.neighbor_net(labels, matrix)
print('Cycle and Splits Determined')

im116 = '116th_outline.pdf'
filename = output_dir+'/'+im116

nexus_file = output_dir+'/dist_116th.nex'

vf.makeVis(labels,cycle,splits,matrix,filename,nexus_file)



# Calculate distances from 'center' for all members
demDists, repDists = vf.centerDists(labels, splits)
#Plot (scatter func)
df = pd.DataFrame()
df['dist'] = list(demDists) + list(repDists)
df['party'] = list(np.repeat('Dem',len(demDists))) + list(np.repeat('Rep',len(repDists)))
df['names'] = [y for y in labels if '_Rep' not in y] + [y for y in labels if '_Rep' in y]
imDists = '116_dists.pdf'
filename = output_dir+'/'+imDists
vf.plotScatter(df, 'names', 'dist', c = 'party', colors = {'Dem': "#0392cf", 'Rep': "#ee4035"},outfile= filename, xaxis = "Senators", yaxis = "Center Distances", 
	title="Center Distances of Senators in 116th Session", rotation = 90, show =False)

# Calculate distances from 'center' for list of congresses
congs = range(111,117)
allDists = pd.DataFrame()
dists = []
party = []
session = []
for c in congs:

	Sall_votes_sub_2 = Sall_votes[Sall_votes.congress == c]
	voteMat = vf.makeVoteMat(Sall_votes_sub_2)

	labels, matrix = vf.makeDistMat(voteMat)
	cycle, splits = nnet_algorithm.neighbor_net(labels, matrix)

	demDists, repDists = vf.centerDists(labels, splits)
	dists += list(demDists) + list(repDists)
	party += list(np.repeat('Dem',len(demDists))) + list(np.repeat('Rep',len(repDists)))
	session += list(np.repeat(c,len(list(demDists) + list(repDists))))
	print('Cong '+str(c)+' Processed')

allDists['dist'] = dists
allDists['party'] = party
allDists['session'] = session

#PLOT
vf.plotViolin(allDists, 'session', 'dist', c = 'party', colors = {'Dem': "#0392cf", 'Rep': "#ee4035"},outfile= filename, xaxis = "Session", yaxis = "Center Distances", 
	title="Center Distances of Parties Across Senates", rotation = 90, fontL = 8,show =True)


#Within Dem. party analysis

dem_votes = Sall_votes_sub[Sall_votes_sub.party_code != 200]

voteMat = vf.makeVoteMat(dem_votes)

labels, matrix = vf.makeDistMat(voteMat)
cycle, splits = nnet_algorithm.neighbor_net(labels, matrix)

imDem = 'dem116_outline.pdf'
filename = output_dir+'/'+imDem

nexus_file = output_dir+'/dem_116th.nex'

vf.makeVis(labels,cycle,splits,matrix,filename,nexus_file,show=False)


members = ["SANDERS_B_Ind","WARREN_E_Dem","KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem"]
members2 = ["MANCHIN_J_Dem","SINEMA_K_Dem","JONES_G_Dem"]
disagree = vf.calcDisagree(voteMat, members)
#Plot aggreement with rest of party
filename = output_dir+'/'+'five_disagree.pdf'
vf.plotScatter(disagree, 'Rollcall', 'Frac', c = 'Vote', colors = {0.0: "#8DD3C7", 0.5: "#FB8072", 1.0:"#BEBADA"},
	outfile= filename, xaxis = "Rollcall Number", yaxis = "Fraction Agreement within Party", 
	title="Agreement Within Party for Sanders, Warren, Klobuchar, Booker, Harris Split",fontL = 8, show =True)

pvals = vf.calcSplitVotPval(voteMat, members)
filename = output_dir+'/'+'five_pvals.pdf'
#Plot p-values for ranking vote contribution to split
vf.plotScatter(pvals, 'Rollcall', 'pval',outfile= filename, xaxis = "Rollcall Number", yaxis = "-log10(p-value)", 
	title="P-values of Vote Contribution to Sanders, Warren, Klobuchar, Booker, Harris Split", fontL = 8, show =True)



disagree = vf.calcDisagree(voteMat, members2)
filename = output_dir+'/'+'three_disagree.pdf'
#Plot aggreement with rest of party
vf.plotScatter(disagree, 'Rollcall', 'Frac', c = 'Vote', colors = {0.0: "#8DD3C7", 0.5: "#FB8072", 1.0:"#BEBADA"},
	outfile= filename, xaxis = "Rollcall Number", yaxis = "Fraction Agreement within Party", 
	title="Agreement Within Party for Manchin, Sinema, Jones Split", fontL = 8, show =False)

pvals = vf.calcSplitVotPval(voteMat, members2)
filename = output_dir+'/'+'three_pvals.pdf'
#Plot p-values for ranking vote contribution to split
vf.plotScatter(pvals, 'Rollcall', 'pval',outfile= filename, xaxis = "Rollcall Number", yaxis = "-log10(p-value)", 
	title="P-values of Vote Contribution to Manchin, Sinema, Jones Split", fontL = 8 , show =False)



#Within Rep. party analysis
rep_votes = Sall_votes_sub[Sall_votes_sub.party_code == 200]

voteMat = vf.makeVoteMat(rep_votes)

labels, matrix = vf.makeDistMat(voteMat)
cycle, splits = nnet_algorithm.neighbor_net(labels, matrix)

imRep = 'rep116_outline.pdf'
filename = output_dir+'/'+imRep

nexus_file = output_dir+'/rep_116th.nex'

vf.makeVis(labels,cycle,splits,matrix,filename,nexus_file,show=False)



