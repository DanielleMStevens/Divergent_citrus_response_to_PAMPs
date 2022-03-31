#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 03/26/2022
# Script Purpose: Loading Libraries for Processing data
# Inputs Necessary: tree files from iqtree run
# Outputs: phylogenetic trees of receptors LYK5 and CERK1
#-----------------------------------------------------------------------------------------------

######################################################################
# ran the following commands on the command line 
######################################################################


# mafft --reorder --thread 12 --maxiterate 1000 --localpair LYK5_homologs.fasta > "LYK5_alignment"
# iqtree -s LYK5_alignment -bb 1000 -T AUTO -st AA -v -m MFP -safe


# run similar command for CERK1 homologs


##############################################
# LYK5 gene phylogeny - Top Figure 5
##############################################

# load tip data
load_LYK5_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 1)
load_LYK5_tipdata <- load_LYK5_tipdata[complete.cases(load_LYK5_tipdata),]

# load 
LYK5_phylo <- read.tree("./Protein_Trees/LYK5/LYK5_alignment.treefile")
LYK5_phylo <- phangorn::midpoint(LYK5_phylo, node.labels = 'label')

for (i in 1:length(LYK5_phylo$tip.label)){
  LYK5_phylo$tip.label[[i]] <- strsplit(LYK5_phylo$tip.label[[i]], "|", fixed = T )[[1]][1]
}


#plot tree and add bootstrap values
p <-  ggtree(LYK5_phylo, layout = "fan", ladderize = T, size = 0.25, linetype = 1, open.angle = 180) %<+% load_LYK5_tipdata 


# need to make data table to map bootstrap values onto tree
d <- p$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)


p <- p +
  geom_tippoint(aes(color = Designation), size = 0.6, show.legend = FALSE) + 
  geom_tiplab(aes(label = Plant_Species), size = 1.35, color = "black", offset = 0.03) +
  geom_treescale(x = -0.7, y = 0.2, width = 0.2, offset = 6, color = 'black', fontsize = 2) +
  geom_nodelab(data = d, aes(label = label), size = 1, alpha = 0.6, vjust = 2, hjust = 1.4)




#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/LYK5_phylogenetic_tree.pdf
# save the following image size:width = 9, height = 11
# import pdf into vector based editing software such as inkscpae or adobe illustrator, adjust the bootstrap values
# to allign with the right internal node. Color citrus tips Red (#), and chnage Q. obata to a dotten line and 
# bring closer to other tip labels



##############################################
# CERK1 gene phylogeny - Bottom Figure 5
##############################################

load_CERK1_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 2)
#load_CERK1_tipdata <- load_CERK1_tipdata[complete.cases(load_CERK1_tipdata),]

CERK1_phylo <- read.tree("./Protein_Trees/CERK1/CERK1_alignment.treefile")
CERK1_phylo <- phangorn::midpoint(CERK1_phylo, node.labels='label')

for (i in 1:length(CERK1_phylo$tip.label)){
  CERK1_phylo$tip.label[[i]] <- strsplit(CERK1_phylo$tip.label[[i]], "|", fixed = T )[[1]][1]
}


#plot tree and add bootstrap values
p2 <-  ggtree(CERK1_phylo, layout="fan", ladderize = T, size = 0.25, linetype = 1, open.angle = 180) %<+% load_CERK1_tipdata 


# need to make data table to map bootstrap values onto tree
d <- p2$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)


p2 <- p2 + geom_tippoint(aes(color = Designation), size = 1) + 
  geom_tiplab(aes(label = Plant_Species), size = 1.2, color="black", offset = 0.03) +
  geom_treescale(x = -0.7, y = 0, width = 0.2, offset = 12, color='black', fontsize = 4) + 
  geom_nodelab(data = d, aes(label = label), size = 1.5, alpha = 0.6, vjust = 2, hjust = 1.4)

rotate_tree(p2, 180)



#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/LYK5_phylogenetic_tree.pdf
# save the following image size:width = 9, height = 11
# import pdf into vector based editing software such as inkscpae or adobe illustrator, adjust the bootstrap values
# to allign with the right internal node. Color citrus tips Red (#), and chnage Q. obata to a dotten line and 
# bring closer to other tip labels





  

  
