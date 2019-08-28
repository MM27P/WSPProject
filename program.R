setwd("D:\IO SHEET\WSP\WSPProject")
BiocManager::install("affy")
BiocManager::install("limma")
BiocManager::install("openxlsx")
BiocManager::install("GSA")
BiocManager::install("gplots")
BiocManager::install("Biobase")
BiocManager::install("affycoretools")
BiocManager::install("msigdbr")
BiocManager::install("pheatmap")
BiocManager::install("genefilter")
library(pheatmap)
library(affy)
library(GSA)
library(limma)
library(openxlsx)
library(Biobase)
library(gplots)
library(msigdbr)
library(affycoretools)
BiocManager::install("hgu95av2.db")
library(hgu95av2.db)
library(genefilter)

cel_files <- list.files("./","*.CEL")
description <- read.table("datasetA_scans.txt", header = TRUE, sep = "\t")
sampleNames(opis) = paste(sampleNames(opis), ".CEL", sep="")
opis = read.AnnotatedDataFrame("datasetA_scans.txt", sep="\t", header=TRUE, row.names=4, stringsAsFactors = F)
data_Affy<-ReadAffy(filenames=sampleNames(opis), verbose=TRUE)
RMA=rma(data_Affy)
RMA <- readRDS("RMA.RDS")
eSet <- annotateEset(RMA,hgu95av2.db)


eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
phenoData(eSet)@data <- opis@data

features <- featureData(eSet)@data
adeno <- expr[,which(opis@data$CLASS=="ADENO")]

expr <- exprs(eSet)
squamous <- expr[,which(opis@data$CLASS=="SQUAMOUS")]
ph <- phenoData(eSet)@data

tstat <- sapply(1:nrow(adeno),function(x){t.test(adeno[x,],squamous[x,])$statistic})
pval <- sapply(1:nrow(adeno),function(x){t.test(adeno[x,],squamous[x,])$p.val})
pval_fdr <- p.adjust(pval, method="BH")

wyniki <- features
wyniki$t_stat <- tstat
wyniki$pval <- pval
wyniki$pval_adjusted <- pval_fdr

p_threshold <- 0.05
diff_genes <- wyniki[which(wyniki$pval_adjusted<p_threshold),]
  
test<-importGeneSets(c("MIR","CP"),gene_identifier = "SYMBOL")
source("importGenesets.R")

source("geneEnrichment.R")
test_gst <- geneEnrichment(tabletest[[2]],test)

add_link <- function(x){
  ncbi_link <- paste("https://ncbi.nlm.nih.gov/gene/",x, sep = "", collapse = NULL)
}
  return(ncbi_link)

diff_genes_links <- sapply(as.character(diff_genes$ENTREZID),add_link)
names(diff_genes_links) <- as.character(diff_genes$ENTREZID)
class(diff_genes_links)<- "hyperlink"
wb<-createWorkbook()

addWorksheet(wb,"workshit")
writeData(wb,sheet=1,diff_genes)
writeData(wb,sheet=1,diff_genes_links,startCol = 2,startRow = 2)
saveWorkbook(wb,"bla.xlsx",overwrite = T)
eSet1 <- usuniecie_sond(eSet)


tabletest<-summary_table(eSet1, klasy=c("NORMAL","SQUAMOUS"),method='holm', sort_criterion="p_val",number = 20)

source("geneset.heatmap.R")

cieplamapa1<-geneset.heatmap(eSet,test,geneset=as.character(tabletest[[1]]$SYMBOL),classes = c("NORMAL","SQUAMOUS"))
cieplamapa <- geneset.heatmap(eSet,test,"WNT_SIGNALING",classes = c("ADENO","SQUAMOUS"))


