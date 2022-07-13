#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
# Script Purpose: Determing if LYK5, CERK1 homologs are closest to At LYK5, CERK1 or another LYK
# Inputs Necessary: All-by-all blast data
# Outputs: Filtered receptor list for LYK5 and CERK1 to run downstream analyses
#-----------------------------------------------------------------------------------------------


######################################################################
# All-by-all blast used to filtered homologs  closer to other LYKs
#####################################################################

## load all-by-all blast output (Tsv file)
All_by_all_blast <- read.table(file = "./Protein_Trees/all_vs_all.tsv", sep = '\t', header = FALSE)
colnames(All_by_all_blast) <- c("Subject","Query","Similarity")


All_by_all_blast <- All_by_all_blast[All_by_all_blast$Subject != All_by_all_blast$Query,]


# filter for those homologs against arabidopsis
All_by_all_blast_arabidopsis <- All_by_all_blast[All_by_all_blast$Query %in% c("NP_566689.2|Arabidopsis_thaliana|CERK1",
                                                                               "NP_180916.1|Arabidopsis_thaliana|LYK5", 
                                                                               "AT2G23770.1|Arabodopsis_thaliana|LYK4",
                                                                               "AT1G51940.1|Arabodopsis_thaliana|LYK3",
                                                                               "AT3G01840.1|Arabodopsis_thaliana|LYK2"),]


All_by_all_blast_arabidopsis <- All_by_all_blast_arabidopsis[,1:3] %>% group_by(Subject) %>% filter(Similarity == max(Similarity))


# Those Homologs are closer to At LYK2, LYK3, and LYK4 are filtered out from respective fasta homolog files
to_remove <- All_by_all_blast_arabidopsis[All_by_all_blast_arabidopsis$Query %in% c("AT2G23770.1|Arabodopsis_thaliana|LYK4",
                                                                       "AT1G51940.1|Arabodopsis_thaliana|LYK3",
                                                                       "AT3G01840.1|Arabodopsis_thaliana|LYK2"),]

View(to_remove)

######################################################################
# filtering out hits
#####################################################################

#filter All_by_all blast by those removed from above list
All_by_all_blast <- All_by_all_blast[!All_by_all_blast$Subject %in% to_remove$Subject,]
All_by_all_blast <- All_by_all_blast[!All_by_all_blast$Query %in% to_remove$Query,]
All_by_all_blast_arabidopsis <- All_by_all_blast_arabidopsis[!All_by_all_blast_arabidopsis$Subject %in% to_remove$Subject,]
All_by_all_blast_arabidopsis <- All_by_all_blast_arabidopsis[!All_by_all_blast_arabidopsis$Query %in% to_remove$Query,]


#remove hits below 35 percent similarity and have weird issues trying to build tree
All_by_all_blast_arabidopsis <- All_by_all_blast_arabidopsis[!All_by_all_blast_arabidopsis$Subject %in% c("XP_030969896.1|Quercus_lobata",
                                                                                                          "XP_020084255.1|Ananas_comosus"),]

#"XP_011025973.1|Populus_euphratica", "XP_021912215.1|Carica_papaya", "XP_031406608.1|Punica_granatum"






