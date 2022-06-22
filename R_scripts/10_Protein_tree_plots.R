#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 03/26/2022
# Script Purpose: Loading Libraries for Processing data
# Inputs Necessary: tree files from iqtree run
# Outputs: phylogenetic trees of receptors LYK5 and CERK1
#-----------------------------------------------------------------------------------------------



# load tip data
load_LYK5_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 1)
load_LYK5_tipdata <- load_LYK5_tipdata[complete.cases(load_LYK5_tipdata),]

load_CERK1_tipdata <- xlsx::read.xlsx("./Protein_Trees/Info_for_tree.xlsx", header = T, stringsAsFactors = F, sheetIndex = 2)
load_CERK1_tipdata <- load_CERK1_tipdata[complete.cases(load_CERK1_tipdata),]



##############################################
# LYK5 gene phylogeny - Top Figure 5
##############################################


# load tree file
LYK5_phylo <- read.tree("./Protein_Trees/LYK5/LYK5_alignment_trimmed.treefile")
LYK5_phylo <- phangorn::midpoint(LYK5_phylo, node.labels = 'label')

# clean up tree tips to map data on to
for (i in 1:length(LYK5_phylo$tip.label)){
  LYK5_phylo$tip.label[[i]] <- strsplit(LYK5_phylo$tip.label[[i]], "|", fixed = T)[[1]][1]
}


#plot tree and add bootstrap values
p <-  ggtree(LYK5_phylo, layout = "rectangular", ladderize = T, size = 0.25, linetype = 1) %<+% load_LYK5_tipdata +
  geom_treescale(x = 1, y = -3, linesize = 1, family = "Arial", offset = 0.7) +
  xlim(0,2.6) 
  #geom_tiplab(size = 1) 
  #geom_tiplab(aes(label = Plant_Species), size = 1.8, color = "black", offset = 0.3, linesize = 0.1, align = T, family = "Arial", fontface = 'italic')

# need to make data table to map bootstrap values onto tree
d <- p$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d <- d[d$label > 90,]

p <- p +
  geom_nodepoint(data=d,aes(label=label), shape = 21, size = 1, color = "black", fill = "grey35") +
  geom_tiplab(aes(label = Plant_Species), size = 1.8, color = "black", offset = 0.3, linesize = 0.1, align = T, family = "Arial", fontface = 'italic') +
  geom_tippoint(aes(color = Designation), size = 1.2, show.legend = FALSE) + 
  scale_color_manual(values = c("#7570B3","#66A61E")) 
  #geom_treescale(x = -0.4, y = 1, width = 0.2, offset = 6, color = 'black', fontsize = 2) 

p <- p + new_scale_fill() 
p <- gheatmap(p, data.frame("Occurance" = LYK5_lysM_domains_reshaped[,1], row.names = rownames(LYK5_lysM_domains_reshaped)), 
              width = 0.05, colnames_angle = 90, offset = 0, font.size = 3,  colnames_offset_y = -6, 
              family = "Arial", color = "black", hjust = 0.5,
              legend_title = "LysM Domains predicted by Hmmer")  +
  scale_fill_manual(values = brewer.pal(n = 5, name = "Paired"))

p <- p + new_scale_fill() 
p <-gheatmap(p, LYK5_lysM_domains_reshaped[,2:4], width = 0.15, colnames_angle = 90, offset = 0.07, font.size = 3,
              family = "Arial", color = "black", hjust = 0.5, colnames_offset_y = -3.8, 
              legend_title = "Similarity to At LysM Domains") +
  scale_fill_gradient(low="white", high="red", na.value="grey42") + 
  theme(legend.position = "none") +
  ggtree::vexpand(.1, -1)

p



#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/LYK5_phylogenetic_tree.pdf
# save the following image size:width = 5.88, height = 6.4
# import pdf into vector based editing software such as inkscpae or adobe illustrator, adjust the bootstrap values
# to allign with the right internal node. Color citrus tips Red (#), and chnage Q. obata to a dotten line and 
# bring closer to other tip labels



##############################################
# CERK1 gene phylogeny - Bottom Figure 5
##############################################


CERK1_phylo <- read.tree("./Protein_Trees/CERK1/CERK1_alignment_trimmed.treefile")
CERK1_phylo <- phangorn::midpoint(CERK1_phylo, node.labels='label')

for (i in 1:length(CERK1_phylo$tip.label)){
  CERK1_phylo$tip.label[[i]] <- strsplit(CERK1_phylo$tip.label[[i]], "|", fixed = T )[[1]][1]
}


#plot tree and add bootstrap values
p2 <-  ggtree(CERK1_phylo, layout = "rectangular", ladderize = T, size = 0.25, linetype = 1, open.angle = 182) %<+% load_CERK1_tipdata +
  geom_treescale(x = 0.7, y = -5, linesize = 1, family = "Arial", offset = 0.9) +
  xlim(0,2.9) 
  #geom_tiplab(size =1) +
  #geom_tiplab(aes(label = Plant_Species), size = 1.2, color = "black", offset = 0.27, linesize = 0.1, align = T, family = "Arial", fontface = 'italic') 
  

p2 <- flip(p2, 180, 179)  %>% flip(216, 181)

# need to make data table to map bootstrap values onto tree
d <- p2$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d <- d[d$label > 90,]

p2 <- p2 +
  geom_nodepoint(data=d, aes(label=label), shape = 21, size = 1, color = "black", fill = "grey35") +
  geom_tippoint(aes(color = Designation), size = 1.2,  show.legend = FALSE) + 
  geom_tiplab(aes(label = Plant_Species), size = 1.2, color = "black", offset = 0.27, linesize = 0.1, align = T, family = "Arial", fontface = 'italic') +
  scale_color_manual(values = c("#7570B3","#66A61E")) 

p2 <- p2 + new_scale_fill() 
p2 <- gheatmap(p2, data.frame("Occurance" = CERK1_lysM_domains_reshaped[,1], row.names = rownames(CERK1_lysM_domains_reshaped)), 
              width = 0.05, colnames_angle = 90, offset = 0, font.size = 3,  colnames_offset_y = -6.4, 
              family = "Arial", color = "black", hjust = 0.5,
              legend_title = "LysM Domains predicted by Hmmer")  +
  scale_fill_manual(values = brewer.pal(n = 5, name = "Paired"))

p2 <- p2 + new_scale_fill() 
p2 <-gheatmap(p2, CERK1_lysM_domains_reshaped[,2:4], width = 0.15, colnames_angle = 90, offset = 0.07, font.size = 3,
             family = "Arial", color = "black", hjust = 0.5, colnames_offset_y = -4, 
             legend_title = "Similarity to At LysM Domains") +
  scale_fill_gradient(low="white", high="red", na.value="grey42") + 
  theme(legend.position = 'none') +
  ggtree::vexpand(.1, -1)

p2










#geom_text(aes(label=node))















#open.angle = 179
#plot tree and add bootstrap values
p2 <- ggtree(CERK1_phylo, layout = "rectangular", ladderize = T, size = 0.2, linetype = 1) %<+% load_CERK1_tipdata +
  geom_treescale(x = 1, y= -1) +
  xlim(0.2)

# need to make data table to map bootstrap values onto tree
d <- p2$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d <- d[d$label > 70,]

#p2 <- p2 + 
#  geom_tippoint(aes(color = Designation), size = 0.6, show.legend = FALSE) + 
#  geom_tiplab(aes(label = Plant_Species), size = 1.1, color="black", offset = 0.03) +
#  geom_treescale(x = -0.8, y = 0, width = 0.2, offset = 6, color = 'black', fontsize = 2) +
#  geom_nodelab(data = d, aes(label = label), size = 1, alpha = 0.6, vjust = 2, hjust = 1.4)

p2 <- p2 +
  geom_nodepoint(data=d,aes(label=label), size = 1, color = "grey43", alpha = 0.6) +
  geom_tippoint(aes(color = Designation), size = 0.8, show.legend = FALSE) + 
  geom_tiplab(aes(label = Plant_Species), size = 1.1, color="black", offset = 0.03) +
  #geom_tiplab(aes(label = Plant_Species), size = 1.2, color = "black", linesize = .03, offset = 0.24, align = TRUE) +
  geom_treescale(x = -0.8, y = 0, width = 0.2, offset = 6, color = 'black', fontsize = 2)
#geom_treescale(x = -1, y = 0, width = 0.2, offset = 6, color = 'black', fontsize = 2) 

p2 <- p2 + new_scale_fill() 
p2 <-gheatmap(p2, CERK1_lysM_domains_reshaped, width=.2,colnames_angle=90, colnames_offset_y = -2) +
  scale_fill_gradient(low="white", high="red", na.value="grey42", name =) +
  theme(legend.position = 'bottom') 


rotate_tree(p2, 180)
  


#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/LYK5_phylogenetic_tree.pdf
# save the following image size:width = 9, height = 11
# import pdf into vector based editing software such as inkscpae or adobe illustrator, adjust the bootstrap values
# to allign with the right internal node. Color citrus tips Red (#), and chnage Q. obata to a dotten line and 
# bring closer to other tip labels





  

  
