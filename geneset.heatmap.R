geneset.heatmap <- function(eSet, genesets, geneset_name){
  if(!(geneset_name %in% names(genesets))){
    stop("Please provide valid geneset name")
  }
  else
  {
    if(all(is.na(as.numeric(genesets[[1]]))))
      gene_identifier <- "SYMBOL"
    else
      gene_identifier <- "ENTREZID"
    expr <- exprs(eSet)
    features <- featureData(eSet)@data
    ph <- phenoData(eSet)@data
    genes <- row.names(features[which(features[,gene_identifier] %in% genesets[[geneset_name]]),])
    heatmap_expr <- expr[genes,]
    row.names(heatmap_expr)<-features[genes,]$SYMBOL
    colnames(heatmap_expr)<-ph$Sample
    hmcol = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255))
    annotation_heatmap <- data.frame(class = ph$CLASS)
    row.names(annotation_heatmap) <- ph$Sample
    gst_heatmap<-pheatmap(heatmap_expr,col = (hmcol), main=geneset_name, annotation_col = annotation_heatmap,cluster_cols = F,scale='row')
    print(gst_heatmap)
  }
  gst_heatmap
}