last_data = read.csv2("C:/Users/sagla/Downloads/macro.csv", sep = "\t", row.names = 1)
install.packages("ISLR")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")


library("dplyr")

last_data

sample_names <- colnames(last_data)
count_info <- as.data.frame(cbind(sample_names, num_vec = c(rep(1,4),rep(0,4))))
count_info$num_vec <- as.factor(count_info$num_vec)
deseq_obj <- DESeq2::DESeqDataSetFromMatrix(countData = round(last_data), colData = count_info, design = ~ num_vec)
run_deseq <- DESeq2::DESeq(deseq_obj)
deseq_result <- as.data.frame(DESeq2::results(run_deseq,
                                              contrast = c("num_vec", "1", "0")))
deseq_result <- deseq_result[order(deseq_result$padj),]
deseq_result <-na.omit(deseq_result)

gene_Name_converter <- read.csv2("C:/Users/sagla/Downloads/macro.csv", sep = ",")
deseq_result$ids <- rownames(deseq_result)

colnames(deseq_result)[7] <- colnames(gene_Name_converter)[1]
deseq_out <- deseq_result[,c(7,2,5)]
pathfindr_in <- dplyr::left_join(deseq_out, gene_Name_converter, by = "Gene.stable.ID")[, c(4,2,3)]
neoadj_pathfindr_result <- pathfindR::run_pathfindR(pathfindr_in,
                                                    plot_enrichment_chart = T,
                                                    visualize_enriched_terms = T,
                                                    n_processes = 8,
                                                    p_val_threshold = 0.01,
                                                    iterations = 25,
                                                    output_dir = "D:/BerkG/SezermanLab/2021_emed_presentation2/Enes_Dr")

