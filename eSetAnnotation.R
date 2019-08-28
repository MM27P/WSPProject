eSetAnnotation <- function(eSet_file,description_file){
  #zakładam że description file  i eSet file to ścieżki do esetu i opisu i że opisy są wg tego samego schematu zawsze
  eSet <- readRDS(eSet_file)
  eset_annotation <- eSet@annotation
  sample_description <- read.AnnotatedDataFrame(description_file, sep="\t", header=TRUE, row.names=4, stringsAsFactors = F)
  sampleNames(sample_description) = paste(sampleNames(sample_description), ".CEL", sep="")
  phenoData(eSet)@data <- sample_description@data
  annotation_package <- paste(eset_annotation,"db",sep=".")
  BiocManager::install(annotation_package, ask= F)
  require(annotation_package,character.only = T)
  eSet <- annotateEset(eSet,get(annotation_package))
  eSet <- eSet[rowSums(is.na(featureData(eSet)@data))==0,]
  eSet
  
}

eset<- eSetAnnotation('D:\\IO SHEET\\WSP\\WSPProject\\RMA.RDS','D:\\IO SHEET\\WSP\\WSPProject\\datasetA_scans.txt')