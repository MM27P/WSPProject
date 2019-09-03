
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
library(hgu95av2.db)
source("importGeneSets.R")

eSetAnnotation <- function(eSet_file,description_file){
  #zakładam że description file  i eSet file to ścieżki do esetu i opisu i że opisy są wg tego samego schematu zawsze
  eSet <- readRDS(eSet_file)
  eset_annotation <- eSet@annotation
  sample_description <- read.AnnotatedDataFrame(description_file, sep="\t", header=TRUE, row.names=4, stringsAsFactors = F)
  sampleNames(sample_description) = paste(sampleNames(sample_description), ".CEL", sep="")
  phenoData(eSet)@data <- sample_description@data
  annotation_package <- paste(eset_annotation,"db",sep=".")
  require(annotation_package,character.only = T)
  eSet <- annotateEset(eSet,get(annotation_package))
  eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
  return(eSet)
  
}
#unique(eset@CLASS)
description<- function(fileName){
opis = read.AnnotatedDataFrame(fileName, sep="\t", header=TRUE, row.names=4, stringsAsFactors = F)
}

usuniecie_sond = function(ExprSet){
  
  dane_sort = sort(rowMeans(exprs(ExprSet)),index.return=T)
  #iloœæ zestawów po reannotacji
  sondy_zestawy= dim(ExprSet)[1]
  #sondy do usuniêcia
  sondy_usun =round(dim(ExprSet)[1]*0.025)
  indeksy_usun=dane_sort$ix[c(1:sondy_usun,(sondy_zestawy-sondy_usun):sondy_zestawy)]
  nowy_ExprSet=ExprSet[-indeksy_usun,] 
}

#sort criterion fold change, p value pavalue ater correction
summary_table=function(ExprSet,klasy, method, sort_criterion, threshold = NULL, number =NULL){
  # sort_criterion  - nazwa kolumny po której sortujemy (FoldChange, p_val, p_val_adjusted)
  klasa1 = which(pData(ExprSet)$CLASS==klasy[1])
  klasa2 = which(pData(ExprSet)$CLASS==klasy[2])
  expr=exprs(ExprSet)
  sr1=rowMeans(expr[,klasa1])
  sr2=rowMeans(expr[,klasa2])
  FC=sr1-sr2
  statistic=apply(expr,1,function(x) t.test(x[klasa1],x[klasa2])$statistic) 
  p_wartosc=apply(expr,1,function(x) t.test(x[klasa1],x[klasa2])$p.val)
  p_val_adjusted=p.adjust(p_wartosc, method = method)
  TAB<-featureData(ExprSet)@data
  TAB$FoldChange<-FC
  TAB$mean_class1<-sr1
  TAB$mean_class2<-sr2
  TAB$t_statistic<-statistic
  TAB$p_val<-p_wartosc
  TAB$p_val_adjusted<-p_val_adjusted
  #zwrócić tu tabelę
  TAB_ALL <- TAB
  
  if (!is.null(number)){
    TAB <- TAB[order(abs(TAB[,sort_criterion])),]
    TAB <-TAB[1:number,]}
  if (!is.null(threshold)){ 
    ind_sort=which(abs(TAB[,sort_criterion])<threshold)
    TAB=TAB[ind_sort,]}
  expr_wybrane=expr[as.character(row.names(TAB)),]
  return(list(TAB,TAB_ALL,expr_wybrane))
  
}

geneset.heatmap <- function(eSet, genesets, geneset_name = NULL,geneset = NULL,classes){
  
  if(is.null(geneset)){
    if(!(geneset_name %in% names(genesets))){
      stop("Please provide valid geneset name")
    }
    else
      geneset <- genesets[[geneset_name]]
  }
  else
    geneset_name <- "Wybrane geny"
  if(all(is.na(as.numeric(geneset))))
    gene_identifier <- "SYMBOL"
  else
    gene_identifier <- "ENTREZID"
  expr <- exprs(eSet)
  features <- featureData(eSet)@data
  ph <- phenoData(eSet)@data
  array_ids <- which(ph$CLASS %in% classes)
  genes <- row.names(features[which(features[,gene_identifier] %in% geneset),])
  heatmap_expr <- expr[genes,array_ids]
  row.names(heatmap_expr)<-features[genes,]$SYMBOL
  colnames(heatmap_expr)<-ph$Sample[array_ids]
  hmcol = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255))
  annotation_heatmap <- data.frame(class = ph$CLASS[array_ids])
  row.names(annotation_heatmap) <- ph$Sample[array_ids]
  gst_heatmap<-pheatmap(heatmap_expr,col = (hmcol), main=geneset_name, annotation_col = annotation_heatmap,cluster_cols = F,scale='row')
  print(gst_heatmap)
  
  
  heatmap_expr
}

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
  saveWorkbook(wb,fileName,overwrite = T)
  
}

RunGen=function(eSet, clases, method, sort_criterion, number)
{
  eSet1 <- usuniecie_sond(eSet)
  
  tabletest<-summary_table(eSet1, clases,method=method, sort_criterion=sort_criterion,number = number)
}

Transform_Exp2DataFrame = function(es) {
  return (as(es,"data.frame"))
  emat = t(exprs(es))
  rownames(emat) = sampleNames(es)
  dd = data.frame(emat)
  return (dd);
}

GenerateHitMap = function(eSet)
{
  test<-importGeneSets(c("MIR","CP"),gene_identifier = "SYMBOL")
  cieplamapa <- geneset.heatmap(eSet,test,"WNT_SIGNALING",classes = c("ADENO","SQUAMOUS"))
  return(cieplamapa)
  #cieplamapa1<-geneset.heatmap(eSet,test,geneset=as.character(tabletest[[1]]$SYMBOL),classes = c("NORMAL","SQUAMOUS"))
}

#TO JEST ZBĘDNE
Rest<-function(eSet, clas1, clas2)
{
  opis = read.AnnotatedDataFrame("datasetA_scans.txt", sep="\t", header=TRUE, row.names=4, stringsAsFactors = F)
  eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
  phenoData(eSet)@data <- opis@data
  
  features <- featureData(eSet)@data
  adeno <- expr[,which(opis@data$CLASS==clas1)]
  
  expr <- exprs(eSet)
  squamous <- expr[,which(opis@data$CLASS==clas2)]
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
  return (diff_genes)
}

#eset2<- eSetAnnotation('D:\\IO SHEET\\WSP\\WSPProject\\RMA.RDS','D:\\IO SHEET\\WSP\\WSPProject\\datasetA_scans.txt')
#Rest(eset2,'ADENO','SQUAMOUS')
