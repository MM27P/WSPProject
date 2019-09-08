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
    hmcol = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255))
    annotation_heatmap <- data.frame(class = ph$CLASS[array_ids])
    row.names(annotation_heatmap) <- ph$Sample[array_ids]
    gst_heatmap<-iheatmap(heatmap_expr,colors = hmcol, name="z-score", cluster_cols="none",scale='row',
                          cluster_rows = "hclust",col_annotation=annotation_heatmap) %>% 
      add_col_labels(ticktext = as.character(colnames(heatmap_expr))) %>%
      add_row_labels(ticktext = nazwy)
    
gst_heatmap
}
