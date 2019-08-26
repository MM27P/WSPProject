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