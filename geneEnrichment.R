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
