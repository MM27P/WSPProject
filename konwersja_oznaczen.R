##cześć I


source("http://bioconductor.org/biocLite.R")

setwd('C:/Users/superstudent/Desktop/dziwny przedmiot aka bs/wsp')
library(Biobase)
library('affy')
biocLite('gahgu95av2.db')
library(gahgu95av2.db)
biocLite("gplots")
library(gplots)
biocLite('limma')
library('limma')

exampleFile = system.file("extdata", "pData.txt", package="Biobase")
data = read.table("datasetA_scans.txt", header = TRUE, sep = "\t")
opis = read.AnnotatedDataFrame("datasetA_scans.txt", sep="\t", header=TRUE, row.names=4, stringsAsFactors = F) 
sampleNames(opis) = paste(sampleNames(opis), ".CEL", sep="")
data_Affy=ReadAffy(filenames=sampleNames(opis), verbose=TRUE)#wczytanie plików danych z rozpoznaniem typu macierzy
data_Affy@cdfName=paste("ga",data_Affy@cdfName,sep="")#Zamiana annotacji mikromacierzy na annotacje Ferrariego
data_Affy@annotation=paste("ga",data_Affy@annotation,sep="")
RMA=rma(data_Affy)
dataRMA=exprs(RMA)
entrezid = mget(rownames(dataRMA),as.environment(as.list(gahgu95av2ENTREZID)))
genename = mget(rownames(dataRMA),as.environment(as.list(gahgu95av2GENENAME)))

macierz = data.frame(unlist(entrezid), unlist(genename))
names(macierz)[1]<-"entrez_id"
names(macierz)[2]<-"gene_name"

experiment = new("MIAME", name = "Dane mikromacierzowe",lab = "IO",title = "dane tesowe", 
                 abstract = "Przyklad",url = "http://www.bioconductor.org", other = list(notes = "inne"))
ExprSet= new("ExpressionSet", expr=dataRMA, phenoData = opis, 
            experimentData=experiment,annotation="gahgu95av2.db", featureData=AnnotatedDataFrame(macierz))


