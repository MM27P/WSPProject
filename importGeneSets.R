# Function importing genesets from GSEA MsigDB



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
  