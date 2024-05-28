##################################################
## Project: RNASeq
## Purpose: deseq2 and pathfindR script
## Date: May 2023
## Author: Berk Gürdamar
##################################################
library("DESeq2")

deseq_input <- read.csv2("count_table.csv", row.names = 1)

sample_names <- colnames(deseq_input)
count_info <- as.data.frame(cbind(sample_names, num_vec = c(rep(0,8),rep(1,16))))

count_info$num_vec <- as.factor(count_info$num_vec)

deseq_obj <- DESeq2::DESeqDataSetFromMatrix(countData = round(deseq_input), colData = count_info, design = ~ num_vec)


run_deseq <- DESeq2::DESeq(deseq_obj)
deseq_result <- as.data.frame(DESeq2::results(run_deseq,
                                              contrast = c("num_vec", "1", "0")))

deseq_result <- deseq_result[order(deseq_result$padj),]
deseq_result <-na.omit(deseq_result)

gene_Name_converter <- read.csv2("mart_export.txt", sep = ",")

deseq_result$ids <- rownames(deseq_result)

colnames(deseq_result)[7] <- colnames(gene_Name_converter)[1]


deseq_out <- deseq_result[,c(7,2,6)]

pathfindr_in <- dplyr::left_join(deseq_out, gene_Name_converter, by = "Gene.stable.ID")[, c(4,2,3)]


pathfindr_result <- pathfindR::run_pathfindR(pathfindr_in,
                                             plot_enrichment_chart = T,
                                             n_processes = 10,
                                             p_val_threshold = 0.05,
                                             iterations = 10)
