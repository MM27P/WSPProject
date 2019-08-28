
BiocManager::install("affy")
BiocManager::install("limma")
BiocManager::install("GSA")
BiocManager::install("openxlsx")
BiocManager::install("gplots")
BiocManager::install("Biobase")
BiocManager::install("msigdbr")
BiocManager::install("affycoretools")
BiocManager::install("pheatmap")
BiocManager::install("genefilter")
library(pheatmap)
library(affy)
library(limma)
library(GSA)
library(openxlsx)
library(gplots)
library(Biobase)
library(msigdbr)
library(affycoretools)
library(genefilter)
BiocManager::install("hgu95av2.db")
library(hgu95av2.db)

load_ExpSet=function(fileName){
  
  RMA <- readRDS("RMA.RDS")
  eSet <- annotateEset(RMA,hgu95av2.db)
  eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
  return(eSet)
}  




eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
phenoData(eSet)@data <- opis@data

features <- featureData(eSet)@data
expr <- exprs(eSet)
adeno <- expr[,which(opis@data$CLASS=="ADENO")]
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


add_link <- function(x){
  ncbi_link <- paste("https://ncbi.nlm.nih.gov/gene/",x, sep = "", collapse = NULL)
  return(ncbi_link)
}

SaveExcel=function(diff_genes, fileName)
{
  diff_genes_links <- sapply(as.character(diff_genes$ENTREZID),add_link)
  names(diff_genes_links) <- as.character(diff_genes$ENTREZID)
  class(diff_genes_links)<- "hyperlink"
  
  wb<-createWorkbook()
  addWorksheet(wb,"workshit")
  writeData(wb,sheet=1,diff_genes)
  writeData(wb,sheet=1,diff_genes_links,startCol = 2,startRow = 2)
  saveWorkbook(wb,"fileName",overwrite = T)
  
}

RunGen=function(eSet, clases, method, sort_criterion, number)
{
  eSet1 <- usuniecie_sond(eSet)
  
  tabletest<-summary_table(eSet1, clases,method=method, sort_criterion=sort_criterion,number = number)
}


source("geneset.heatmap.R")
cieplamapa <- geneset.heatmap(eSet,test,"WNT_SIGNALING",classes = c("ADENO","SQUAMOUS"))

cieplamapa1<-geneset.heatmap(eSet,test,geneset=as.character(tabletest[[1]]$SYMBOL),classes = c("NORMAL","SQUAMOUS"))


