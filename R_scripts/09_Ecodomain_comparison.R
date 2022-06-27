#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 06/21/2022
# Script Purpose: Loading Libraries for Processing data
# Inputs Necessary: tree files from iqtree run
# Outputs: phylogenetic trees of receptors LYK5 and CERK1
#-----------------------------------------------------------------------------------------------


#########################################################
# import reference At LysM to compare against
#########################################################

At_lysM_domains <- Biostrings::readAAStringSet(filepath = "./Protein_Trees/02_LysM_Comparison/LysM_sequences/lysM_sequences.fasta", format = "fasta") 
At_lysM_domains <- dss2df(At_lysM_domains)


#########################################################
# load LYK5 LysM hit from blastp search of lysM database
#########################################################

# load lysM hits from blast search
LYK5_lysM_domains <- read.table(file = "./Protein_Trees/02_LysM_Comparison/blastp/LYK5_lysM_hits.tsv", sep = '\t', header = FALSE)
colnames(LYK5_lysM_domains) <- c("Subject","Query","Similarity")

# filter out cerk1 hits and pick best match per lysM domain
LYK5_lysM_domains <- LYK5_lysM_domains[!grepl("CERK1", LYK5_lysM_domains$Query),]
LYK5_lysM_domains <- LYK5_lysM_domains[,1:3] %>% group_by(Subject, Query) %>% filter(Similarity == max(Similarity))


LYK5_lysM_domains_reshaped <- reshape(as.data.frame(LYK5_lysM_domains), idvar = "Subject", timevar = "Query", direction = "wide")
for (i in 1:length(LYK5_lysM_domains_reshaped$Subject)){
  LYK5_lysM_domains_reshaped$Subject[[i]] <- strsplit(LYK5_lysM_domains_reshaped$Subject[[i]], "|", fixed = T )[[1]][1]
}

#LYK5_lysM_domains_reshaped <- melt(LYK5_lysM_domains_reshaped)
LYK5_lysM_domains_reshaped %>% arrange(factor(Subject, levels = LYK5_phylo$tip.label))
rownames(LYK5_lysM_domains_reshaped) <- LYK5_lysM_domains_reshaped$Subject
LYK5_lysM_domains_reshaped <- LYK5_lysM_domains_reshaped[,2:4]
LYK5_lysM_domains_reshaped <- LYK5_lysM_domains_reshaped[,c(3,1,2)]
colnames(LYK5_lysM_domains_reshaped) <- c("LysM1","LysM2","LysM3")



#########################################################
# load CERK1
#########################################################

# load lysM hits
CERK1_lysM_domains <- read.table(file = "./Protein_Trees/02_LysM_Comparison/blastp/CERK1_lysM_hits.tsv", sep = '\t', header = FALSE)
colnames(CERK1_lysM_domains) <- c("Subject","Query","Similarity")
CERK1_lysM_domains <- CERK1_lysM_domains[!grepl("LYK5", CERK1_lysM_domains$Query),]
CERK1_lysM_domains <- CERK1_lysM_domains[,1:3] %>% group_by(Subject, Query) %>% filter(Similarity == max(Similarity))


CERK1_lysM_domains_reshaped <- reshape(as.data.frame(CERK1_lysM_domains), idvar = "Subject", timevar = "Query", direction = "wide")
for (i in 1:length(CERK1_lysM_domains_reshaped$Subject)){
  CERK1_lysM_domains_reshaped$Subject[[i]] <- strsplit(CERK1_lysM_domains_reshaped$Subject[[i]], "|", fixed = T)[[1]][1]
}

#LYK5_lysM_domains_reshaped <- melt(LYK5_lysM_domains_reshaped)
CERK1_lysM_domains_reshaped %>% arrange(factor(Subject, levels = CERK1_phylo$tip.label))
rownames(CERK1_lysM_domains_reshaped) <- CERK1_lysM_domains_reshaped$Subject
CERK1_lysM_domains_reshaped <- CERK1_lysM_domains_reshaped[,2:4]
colnames(CERK1_lysM_domains_reshaped) <- c("LysM1","LysM2","LysM3")




######################################################################
# imoprt HMMER domain hits and process for lysM occurance on tree
######################################################################

domain_of_lysM_proteins <- read.table(file = "./Protein_Trees/02_LysM_Comparison/all_domains_lysM_receptors.txt", header = T)
colnames(domain_of_lysM_proteins) <- c("Target","Accession","Query Name","Query Accession","E-value","Score","Bias","Domain E-value",
                                       "Domain Score","Domain Bias","Expected","Occurance","Number of Multidomains","Overlap", "Envelops Definied",
                                       "Number of Domains Defined")

domain_of_lysM_proteins_filtered <- domain_of_lysM_proteins[,c(1,3,12)]
domain_of_lysM_proteins_filtered <- reshape(domain_of_lysM_proteins_filtered, idvar = "Target", timevar = "Query Name", direction = "wide")
domain_of_lysM_proteins_filtered[is.na(domain_of_lysM_proteins_filtered)] <- 0

for (i in 1:length(domain_of_lysM_proteins_filtered$Target)){
  domain_of_lysM_proteins_filtered$Target[[i]] <- strsplit(domain_of_lysM_proteins_filtered$Target[[i]], "|", fixed = T )[[1]][1]
}


LYK5_domain_count <- domain_of_lysM_proteins_filtered[domain_of_lysM_proteins_filtered$Target %in% rownames(LYK5_lysM_domains_reshaped),c(1,9)]
LYK5_domain_count <- LYK5_domain_count[match(rownames(LYK5_lysM_domains_reshaped), LYK5_domain_count$Target), ]
colnames(LYK5_domain_count) <- c("Target", "LysM Occurance")
LYK5_lysM_domains_reshaped <- cbind(LYK5_domain_count$`LysM Occurance`,LYK5_lysM_domains_reshaped)
colnames(LYK5_lysM_domains_reshaped) <- c("Occurance","LysM1","LysM2","LysM3")
LYK5_lysM_domains_reshaped$Occurance <- as.character(LYK5_lysM_domains_reshaped$Occurance)


#domain number count for cerk1
CERK1_domain_count <- domain_of_lysM_proteins_filtered[domain_of_lysM_proteins_filtered$Target %in% rownames(CERK1_lysM_domains_reshaped),c(1,9)]
CERK1_domain_count <- CERK1_domain_count[match(rownames(CERK1_lysM_domains_reshaped), CERK1_domain_count$Target), ]
colnames(CERK1_domain_count) <- c("Target", "LysM Occurance")
CERK1_lysM_domains_reshaped <- cbind(CERK1_domain_count$`LysM Occurance`,CERK1_lysM_domains_reshaped)
colnames(CERK1_lysM_domains_reshaped) <- c("Occurance","LysM1","LysM2","LysM3")
CERK1_lysM_domains_reshaped$Occurance <- as.character(CERK1_lysM_domains_reshaped$Occurance)









######################################################################
# Import Ecodomain blastp data - 
#####################################################################


# Import blastp hits of eco domain
EcoDomain_Blast_LYK5 <- xlsx::read.xlsx(file = "./Protein_Trees/03_Ectodomain_Comparison/LYK5_eco_all_hits-2.xlsx", sheetIndex = 1, header = TRUE)
EcoDomain_Blast_CERK1 <- xlsx::read.xlsx(file = "./Protein_Trees/03_Ectodomain_Comparison/CERK1_eco_all_hits-2.xlsx", sheetIndex = 1, header = TRUE)

# weird issue need to remove NAs
EcoDomain_Blast_LYK5 <- EcoDomain_Blast_LYK5[1:138,1:3]
EcoDomain_Blast_CERK1 <- EcoDomain_Blast_CERK1[1:155,1:3]


# arabidopsis against all eco domain hits
Blast_arabidopsis_eco <- rbind(EcoDomain_Blast_LYK5[1:102,], EcoDomain_Blast_CERK1[1:119,])
colnames(Blast_arabidopsis_eco) <- c('Query',"Hit","Identity")


# citrus against citris eco domain hits
Blast_citrus_eco <-  rbind(cbind(EcoDomain_Blast_LYK5[103:138,],
                                 "Gene" = rep("LYK5", nrow(EcoDomain_Blast_LYK5[103:138,]))
                                 ), 
                           cbind(EcoDomain_Blast_CERK1[120:155,],
                                 "Gene" = rep("CERK1",nrow(EcoDomain_Blast_CERK1[120:155,]))) 
)
colnames(Blast_citrus_eco) <- c('Query',"Hit","Identity","Gene")


######################################################################
# plot All_by_All blast Comparisons
#####################################################################

level_order <- c('LYK5', 'CERK1') #this vector might be useful for other plots/analyses


(ggplot(Blast_citrus_eco,aes(x = factor(Gene, level = level_order), y = Identity)) +   
  
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, outlier.color = NA, fill = "grey", center = TRUE) +
  geom_half_point(side = "l", size = 0.5, alpha = 0.9) +
  coord_flip() + my_ggplot_theme +
  ylim(20, 100) +
  xlab("") +
  ylab("Percent Identity") +
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 14))
  
)/
  
  ggplot(Blast_arabidopsis_eco) + 
  geom_half_boxplot(aes(x = Query, y = Identity), side = "r", errorbar.draw = FALSE,outlier.color = NA, fill = "grey", center = TRUE) +
  coord_flip() +
  xlab("") +
  ylab("Percent Identity") +
  ylim(20, 100) +
  scale_x_discrete(labels= c("At LYK5","At CERK1"))+
  my_ggplot_theme +
  geom_half_point(aes(x = Query, y = Identity), side = "l", size = 0.5, alpha = 0.9) +
    theme(axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 14))






############# old version ?
(ggplot(Blast_citrus_eco,aes(x = factor(Gene, level = level_order), y = Identity)) + 
    #geom_violin(fill = "grey", scale = "width")+
    geom_boxplot(color = "grey14", fill = "grey 80", outlier.shape = NA) + 
    geom_quasirandom(method = "pseudorandom", fill = "black", color = "black", size = 0.5, pch = 21, dodge.width = 0.8) +
    coord_flip() + my_ggplot_theme +
    ylim(20, 120) +
    xlab("") +
    ylab("") +
    theme(axis.text.y = element_text(color = "black", size = 14),
          axis.text.x = element_text(color = "black", size = 14))
)/
  
  #Against Arabidopsis LYK5 and CERK1 
  ggplot(Blast_arabidopsis_eco) + 
  geom_boxplot(aes(x = Query, y = Identity), inherit.aes = F, fill = "grey", outlier.shape = NA) +
  coord_flip() +
  xlab("") +
  ylab("Percent Identity") +
  ylim(20, 120) +
  scale_x_discrete(labels= c("At LYK5","At CERK1"))+
  my_ggplot_theme +
  geom_quasirandom(aes(x = Query, y = Identity),
                   method = "pseudorandom", fill = "black", color = "black", size = 0.5, pch = 21, dodge.width = 0.8) +
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 16))






################################################################### old code #####################################################################

#subset just citrus against citrus
subset_CERK1_citrus <- subset(load_CERK1_tipdata, load_CERK1_tipdata$Plant_family == "Rutaceae")
subset_CERK1_citrus <- subset_CERK1_citrus[,1:7]
subset_LYK5_citrus <- subset(load_LYK5_tipdata, load_LYK5_tipdata$Plant_family == "Rutaceae")
citrus_receptors <- rbind(subset_CERK1_citrus, subset_LYK5_citrus)

All_by_all_blast_citrus <- All_by_all_blast[grepl(paste(citrus_receptors$XP_Accession, collapse = "|"), All_by_all_blast$Subject),]
All_by_all_blast_citrus <- All_by_all_blast_citrus[grepl(paste(citrus_receptors$XP_Accession, collapse = "|"), All_by_all_blast_citrus$Query),]

# CERK1 citrus comparisons
All_by_all_blast_citrus_CERK1 <- All_by_all_blast_citrus[grepl(paste(subset_CERK1_citrus$XP_Accession, collapse = "|"), All_by_all_blast_citrus$Subject),]
All_by_all_blast_citrus_CERK1 <- All_by_all_blast_citrus_CERK1[grepl(paste(subset_CERK1_citrus$XP_Accession, collapse = "|"), All_by_all_blast_citrus_CERK1$Query),]
All_by_all_blast_citrus_CERK1 <- All_by_all_blast_citrus_CERK1[!grepl("LYK5", All_by_all_blast_citrus_CERK1$Subject),]
All_by_all_blast_citrus_CERK1 <- All_by_all_blast_citrus_CERK1[!grepl("LYK5", All_by_all_blast_citrus_CERK1$Query),]
All_by_all_blast_citrus_CERK1 <- All_by_all_blast_citrus_CERK1[!duplicated(apply(All_by_all_blast_citrus_CERK1,1,function(x) paste(sort(x),collapse=''))),1:3]
All_by_all_blast_citrus_CERK1 <- cbind(All_by_all_blast_citrus_CERK1, "Gene" = rep("CERK1",nrow(All_by_all_blast_citrus_CERK1)))



# CERK1 citrus comparisons
All_by_all_blast_citrus_LYK5 <- All_by_all_blast_citrus[grepl(paste(subset_LYK5_citrus$XP_Accession, collapse = "|"), All_by_all_blast_citrus$Subject),]
All_by_all_blast_citrus_LYK5 <- All_by_all_blast_citrus_LYK5[grepl(paste(subset_LYK5_citrus$XP_Accession, collapse = "|"), All_by_all_blast_citrus_LYK5$Query),]
All_by_all_blast_citrus_LYK5 <- All_by_all_blast_citrus_LYK5[!grepl("CERK1", All_by_all_blast_citrus_LYK5$Subject),]
All_by_all_blast_citrus_LYK5 <- All_by_all_blast_citrus_LYK5[!grepl("CERK1", All_by_all_blast_citrus_LYK5$Query),]
All_by_all_blast_citrus_LYK5 <- All_by_all_blast_citrus_LYK5[!duplicated(apply(All_by_all_blast_citrus_LYK5,1,function(x) paste(sort(x),collapse=''))),1:3]
All_by_all_blast_citrus_LYK5 <- cbind(All_by_all_blast_citrus_LYK5, "Gene" = rep("LYK5",nrow(All_by_all_blast_citrus_LYK5)))


All_by_all_blast_citrus_receptors <- rbind(All_by_all_blast_citrus_CERK1, All_by_all_blast_citrus_LYK5)







# list of LYK5 homologs
#LYK5_homologs_list_sequences <- Biostrings::readAAStringSet(filepath = "./Protein_Trees/LYK5/LYK5_homologs_cleaned_up.fasta", format = "fasta") 
#LYK5_homologs_list_sequences <- dss2df(LYK5_homologs_list_sequences)



#for (i in 1:nrow(LYK5_homologs_list_sequences)){
#  seq_global_M1 <- Biostrings::pairwiseAlignment(LYK5_homologs_list_sequences$seq[i], At_lysM_domains$LYK5_LysM1, type = 'local-global', substitutionMatrix = "BLOSUM62")
#  seq_global_M2 <- Biostrings::pairwiseAlignment(LYK5_homologs_list_sequences$seq[i], At_lysM_domains$LYK5_LysM2, type = 'local-global', substitutionMatrix = "BLOSUM62")
#  seq_global_M3 <- Biostrings::pairwiseAlignment(LYK5_homologs_list_sequences$seq[i], At_lysM_domains$LYK5_LysM3, type = 'local-global', substitutionMatrix = "BLOSUM62")
  
#  seq_global_M1 <- Biostrings::pid(seq_global_M1, "PID1")
#  seq_global_M2 <- Biostrings::pid(seq_global_M2, "PID1")
#  seq_global_M3 <- Biostrings::pid(seq_global_M3, "PID1")
  
  #if local-comparison is higher than blastp hit, replace value
#  if(LYK5_lysM_domains_reshaped$LysM1[i] < seq_global_M1){
#    LYK5_lysM_domains_reshaped$LysM1[i] <- seq_global_M1
#    print(LYK5_lysM_domains_reshaped[i,])
#  }
#  if(LYK5_lysM_domains_reshaped$LysM2[i] < seq_global_M2){
#    LYK5_lysM_domains_reshaped$LysM2[i] <- seq_global_M2
#    print(LYK5_lysM_domains_reshaped[i,])
#  }
#  if(LYK5_lysM_domains_reshaped$LysM3[i] < seq_global_M3){
#    LYK5_lysM_domains_reshaped$LysM3[i] <- seq_global_M3
#    print(LYK5_lysM_domains_reshaped[i,])
#  }
#  print(i)
  
#}



