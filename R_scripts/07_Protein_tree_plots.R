#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 03/26/2022
# Script Purpose: Loading Libraries for Processing data
# Inputs Necessary: tree files from iqtree run
# Outputs: phylogenetic trees of receptors LYK5 and CERK1
#-----------------------------------------------------------------------------------------------

######################################################################
#library packages need to load
######################################################################


# mafft --reorder --thread 12 --maxiterate 1000 --localpair LYK5_homologs.fasta > "LYK5_alignment2"
# iqtree -s LYK5_alignment2 -bb 1000 -T AUTO -st AA -v -m TEST -safe


##############################################
# LYK5 gene phylogeny
##############################################

load_LYK5_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 1)
load_LYK5_tipdata <- load_LYK5_tipdata[complete.cases(load_LYK5_tipdata),]

LYK5_phylo <- read.tree("./Protein_Trees/LYK5/LYK5_alignment.treefile")
LYK5_phylo <- phangorn::midpoint(LYK5_phylo, node.labels='label')

for (i in 1:length(LYK5_phylo$tip.label)){
  LYK5_phylo$tip.label[[i]] <- strsplit(LYK5_phylo$tip.label[[i]], "|", fixed = T )[[1]][1]
}

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
#
#)

#plot tree and add bootstrap values
p <-  ggtree(LYK5_phylo, layout="fan", ladderize = T, size = 0.25, linetype = 1, open.angle = 180)  %<+% load_LYK5_tipdata 

p + geom_tippoint(aes(color = Designation), size = 0.5) + 
  #scale_color_manual(values = getPalette(8)) +
  geom_tiplab(aes(label = Plant_Species), size = 1, color="black", offset = 0.05) +
  geom_treescale(x = -0.7, y = 0.2, width = 0.2, offset = 6, color='black', fontsize = 2) + 
  geom_nodelab(data = d, aes(label = label), size = 0.8, alpha = 0.6, vjust = 2, hjust = 1.4)


  
  


##############################################
# CERK1 gene phylogeny
##############################################

load_CERK1_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 2)
load_CERK1_tipdata <- load_CERK1_tipdata[complete.cases(load_CERK1_tipdata),]

CERK1_phylo <- read.tree("./Protein_Trees/CERK1/CERK1_alignment.treefile")
CERK1_phylo <- phangorn::midpoint(CERK1_phylo, node.labels='label')

for (i in 1:length(CERK1_phylo$tip.label)){
  CERK1_phylo$tip.label[[i]] <- strsplit(CERK1_phylo$tip.label[[i]], "|", fixed = T )[[1]][1]
}



#plot tree and add bootstrap values
p2 <-  ggtree(CERK1_phylo, layout="fan", ladderize = T, size = 0.25, linetype = 1, open.angle = 220) %<+% load_CERK1_tipdata 

p2 <- p2 + geom_tippoint(aes(color = Designation)) + 
  geom_tiplab(aes(label = Plant_Species), size=3, color="black", offset = 0.05) +
  geom_treescale(x=-0.7, y=0, width=0.2, offset = 12, color='black', fontsize = 4) + 
  geom_nodelab(data = d, aes(label = label),size = 2, alpha = 0.6, vjust = 2, hjust = 1.4)

rotate_tree(p2, 180)




  

  
