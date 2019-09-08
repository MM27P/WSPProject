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
  if(sort_criterion=="FoldChange"){
    if (!is.null(number)){
      TAB <- TAB[order(-abs(TAB[,sort_criterion])),]
      TAB <-TAB[1:number,]}
    if (!is.null(threshold)){ 
      ind_sort=which(abs(TAB[,sort_criterion])>threshold)
      TAB=TAB[ind_sort,]}
  }
  else{
    if (!is.null(number)){
      TAB <- TAB[order(abs(TAB[,sort_criterion])),]
      TAB <-TAB[1:number,]}
    if (!is.null(threshold)){ 
      ind_sort=which(abs(TAB[,sort_criterion])<threshold)
      TAB=TAB[ind_sort,]}
  }
  return(list(TAB,TAB_ALL))
  
}

geneset.heatmap <- function(eSet, genesets=NULL, geneset_name = NULL,geneset = NULL,classes){
  
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
  features <- distinct(features, SYMBOL, .keep_all= TRUE)
  expr <- expr[as.character(features$PROBEID),]
  ph <- phenoData(eSet)@data
  array_ids <- which(ph$CLASS %in% classes)
  genes <- row.names(features[which(features[,gene_identifier] %in% geneset),])
  heatmap_expr <- expr[as.numeric(genes),array_ids]
  nazwy <- as.character(features[genes,]$SYMBOL)
  row.names(heatmap_expr)<- nazwy
  colnames(heatmap_expr)<-ph$Sample[array_ids]
  # hmcol = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255))
  # annotation_heatmap <- data.frame(class = ph$CLASS[array_ids])
  # row.names(annotation_heatmap) <- ph$Sample[array_ids]
  # gst_heatmap<-iheatmap(heatmap_expr,colors = hmcol, name="z-score", cluster_cols="none",scale='row',
  #                       cluster_rows = "hclust",col_annotation=annotation_heatmap) %>% 
  #   add_col_labels(ticktext = as.character(colnames(heatmap_expr))) %>%
  #   add_row_labels(ticktext = nazwy)
  # 
  # gst_heatmap
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


#TO JEST ZB?DNE
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

geneEnrichment <- function(selection_results, genesets, method = "geneSetTest", FDR_adjustment = "BH"){
  if(!(method %in% c("geneSetTest","CAMERA"))){
    stop("Please provide valid testing method")
  }
  else{
    if(FDR_adjustment %in% p.adjust.methods){
      if(all(is.na(as.numeric(genesets[[1]]))))
        gene_identifier <- "SYMBOL"
      else
        gene_identifier <- "ENTREZID"
      no_duplicates <- findLargest(as.character(selection_results$PROBEID),selection_results$t_statistic,"hgu95av2")
      selection_results <- selection_results[no_duplicates,]
      geneset_ids<-lapply(1:length(genesets),function(x){which(selection_results[,gene_identifier] %in% genesets[[x]])})
      names(geneset_ids) <- names(genesets)
      if(method=="geneSetTest"){
        gst <- sapply(1:length(geneset_ids),function(x){geneSetTest(geneset_ids[[x]],selection_results$t_statistic)})
        names(gst)<-names(genesets)
        gst <- as.data.frame(gst)
        colnames(gst)<-"pval"
        gst$pval_adjusted <- p.adjust(gst$pval,method=FDR_adjustment)
        
      }
      else{
        cam <- cameraPR(selection_results$t_statistic,geneset_ids)
        gst <- as.data.frame(cam$PValue)
        rownames(gst)<-names(genesets)
        colnames(gst)<-"pval"
        gst$pval_adjusted <- cam$FDR
      }
      gst <- gst[order(gst$pval_adjusted),]
    }
    else
      stop("Please provide valid FDR adjustment method")
  }
  gst
}


importGeneSets <- function(geneset_categories,gene_identifier = "ENTREZID"){
  if(!(gene_identifier %in% c("ENTREZID","SYMBOL"))){
    stop("Please provide valid gene identifier")
  }
  else{
    if(all(geneset_categories %in% c("H","C1","C2","CGP","CP","CP:BIOCARTA","CP:KEGG","CP:REACTOME",
                                     "C3","MIR","TFT","C4","CGN","CM","C5","BP","CC","MF","c6","C7"))){
      get_geneset <- function(geneset_cat){
        if(geneset_cat %in% c("H","C1","C2","C3","C4","C5","c6","C7")){
          geneset <- msigdbr(species="Homo sapiens",category = geneset_cat)
        }
        else if(geneset_cat %in% c("CGP","CP","CP:BIOCARTA","CP:KEGG","CP:REACTOME")){
          geneset <- msigdbr(species="Homo sapiens",category = "C2", subcategory = geneset_cat)
        }
        else if(geneset_cat %in% c("MIR","TFT")){
          geneset <- msigdbr(species="Homo sapiens",category = "C3", subcategory = geneset_cat)
        }
        else if(geneset_cat %in% c("CGN","CM")){
          geneset <- msigdbr(species="Homo sapiens",category = "C4", subcategory = geneset_cat)
        }
        else if(geneset_cat %in% c("BP","CC","MF")){
          geneset <- msigdbr(species="Homo sapiens",category = "C5", subcategory = geneset_cat)
        }
        geneset
      }
      genesets1 <- lapply(geneset_categories,get_geneset)
      geneset_no <- length(genesets1)
      if(geneset_no>1){
        genesets<-rbind(genesets1[[1]],genesets1[[2]])
        if(geneset_no>2){
          for(i in 3:geneset_no){
            genesets <- rbind(genesets,genesets1[[i]])
          }
        }
      }
      else
        genesets <- genesets1[[1]]
      geneset_names <- unique(genesets$gs_name)
      make_genesets <- function(msigdbr_list,geneset_name,gene_id){
        if(gene_id == "ENTREZID"){
          geneset <- unique(msigdbr_list[which(msigdbr_list$gs_name == geneset_name),]$entrez_gene)
        }
        else if(gene_id == "SYMBOL"){
          geneset <- unique(msigdbr_list[which(msigdbr_list$gs_name == geneset_name),]$gene_symbol)
        }
        geneset
      }
      genesets_list <- lapply(geneset_names,make_genesets,msigdbr_list = genesets, gene_id = gene_identifier)
      names(genesets_list)<-geneset_names
    }
    else
      stop("Please provide valid categories")
  }
  genesets_list 
}


#eset2<- eSetAnnotation('D:\\IO SHEET\\WSP\\WSPProject\\RMA.RDS','D:\\IO SHEET\\WSP\\WSPProject\\datasetA_scans.txt')
#Rest(eset2,'ADENO','SQUAMOUS')
