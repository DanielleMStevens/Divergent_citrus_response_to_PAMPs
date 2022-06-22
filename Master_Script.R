#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Master Script Controlling all Subscripts
# Inputs Necessary: n/a
# Outputs: n/a
#-----------------------------------------------------------------------------------------------



##############################################
# Load Processed Data and Colors
##############################################

# set path to location of where repository is located/downloaded
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#### Loading libraries, color codes, and processing data
# Load libraries to run scripts
source("./R_scripts/01_Libraries_to_load.R")

# load processed data
source("./R_scripts/02_Process_PAMP_responses.R")

# load figure colors
source("./R_scripts/03_Figure_colors.R")

# Load custom ggplot
source("./R_scripts/04_Theme_ggplot.R")


##############################################
# Plot data post processing
##############################################

# plot ROS to determine cutoffs for low, medium, and high 
source("./R_scripts/05_Catagotize_ROS_magnitude.R")


# run complex heatmap script figures
source("./R_scripts/06_Plot_Responses_ComplexHeatmap.R")


# run ggplot script for assessing responses in relationship with disease
source("./R_scripts/07_Assess_ROS_to_disease_assay.R")


##############################################
# Plot phylogenetic trees of receptors
##############################################

#  Before building the tree, we will run 4 independent tests/comparisons
#  1. all by all blast comparisons of homlogs to other lyks to cross verify down stream analysis are being computed exclusively on cerk1 and lyk5. Filter out hits and create new cleaned up fasta file in LYK5 and CERK1 directories.
#  2. Run lysM specific analysis. lysM is the critical domain feature which is found in all lyk family proteins. Previous work has xhown differences in number of domains predicted. We will assess if this incongurnace in prediction hold up in our dataset using multiple appraches (blast, hmm)
#  3. To showcase the divergence in the extracellular domain for lyk5 and cerk1 (yet the conserved binding sites), we will do a comparison to all by all showcasing all againast arabiopdis and all citrus against themselves

# Details: 1. will be used to further refine the dataset. 2. will be plotted on the tree 3. shows similar diversity as tree using an alternate approach

##############################################
# 1. all by all blast comparisons of homlogs to other lyks
##############################################

# to filter out any misannotations with another LYK, we will all-by-all blast with other At LYKs
# ran the following commands on the command line  

# concatinate all proteins tooegther in a single fasta file and add in the sequences for lyk2, lyk3, and lyk4 mannually, build a blast database 
# conda install -c bioconda blast
# makeblastdb -in All_proteins_combine.fasta -dbtype prot -out all_proteins_blast_db
# blastp -db ./All_proteins_best_blast_db/all_proteins_blast_db -query ./All_proteins_best_blast_db/All_proteins_combine.fasta -outfmt 6 -out all_vs_all.tsv -num_threads 4


# compare all possible LYK hits to all At LYKs to filter out - copy all orginal homologs to a new file and manually remove those which are closer to other lyks
source("./R_scripts/08_All_by_all_blast_comparison.R")

# see which lyks to (and were) remvoed, just run on the R console: View(to_remove)
# the exception is one citrus lyk5 which in cleminina was closer to lyk5 but in sinensis was closer to lyk4 (1 AA difference). After mannual inspection, they look closer to lyk5 from citrus and thus are kept.


##############################################
# 2. Run lysM specific analysis.
##############################################

# hits were closer to other lyks were mannually removed and remaining Lyks were coppied to a new file, 'cleaned up' homologs 
# to further assess these LYK type homoogs, I want to assess how many LysM domains they have (At LYK5 and CERK1 each have 3) and because
# standard domain prediction (interproscan) typically only finds 1/2 domains (even in At) making in not very accurate

# run on the command line 
# hmm model based search - run after installing via conda 
# conda install -c bioconda hmmer 
# hmmsearch --tblout all_domains_lysM_receptors.txt -E 1e-5 
# --cpu 6 ./../../../Documents/Mining_MAMPs/Mining_Known_MAMPs/Protein_alignments_and_trees
# /cold_shock_protein/Hmm_modles/Pfam-A.hmm All_proteins_best_blast_db/All_proteins_combine.fasta


# one arroach will be by blastp and one with be based on HMMER using LysM domain modwl
# blast apprach - run on command line
# makeblastdb -in lysM_sequences.fasta -dbtype prot -out lysM_blast_db
# blastp -db ./../LysM_sequences/lysM_blast_db -query ./cleaned_LYK5_homologs.fasta -outfmt 6 -out LYK5_lysM_hits.tsv
# blastp -db ./../LysM_sequences/lysM_blast_db -query ./cleaned_CERK1_homologs.fasta -outfmt 6 -out CERK1_lysM_hits.tsv

# files were moved to Ecodomain_Comparison/blastp

##############################################
# 3. Ecodomain comparison.
##############################################

# to extract the eco domain for analysis, we can use blastp online to quickly obtain just this sequence.
# 1. the eco domain both LYK5 and CERK1 can be found in Eco_domains.fasta in the Protein_Trees/Ecodomain_Comparison folder
# 2. cleaned up homolog files can be download. Aligned sequences can be dowloaded (be use to change max hit number to above 100) into txt file.
# 3. each txt file can be cleaned up using a text editor such as submine with the follow commands.
#        3a. search (or find)  ; [a-z]{3}\|[a-z]{5}_[0-9]{5}:   ; select Find all  ; select delete
#        3b. search (or find)  ; [0-9]{2}-[0-9]{3}   ; select Find all ; commands X  ; command right arrow key  ; space ; [] ; inside brakes command V
#         Each hit file was saved as: CERK1_eco_domain_hit.txt or LYK5_eco_domain_hit.txt

# run all by all domain comparisons online again, download tsv file, and copy and paste relavent comparions to new excel file (xlsx)

# parses and compare domain
source("./R_scripts/09_Ecodomain_comparison.R")


##############################################
# Building and plotting lyk5 and cerk1 phylogenetic trees
##############################################

# ran the following commands on the command line 
# mafft --reorder --thread 12 --maxiterate 1000 --localpair cleaned_LYK5_homologs.fasta  > "LYK5_alignment"
# trimal -in LYK5_alignment_cleaned_up -out LYK5_alignment_cleaned_up_trimmed -automated1
# iqtree -s LYK5_alignment -bb 1000 -T AUTO -st AA -v -m MFP -safe
# run similar command for CERK1 homologs



# parses and plots trees
source("./R_scripts/10_Protein_tree_plots.R")





####################################################
# NOTE: If you have issues loading packages, I reccomend restarting R. This fixed the issues most times in my experience
#####################################################

