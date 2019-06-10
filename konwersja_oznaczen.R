##czêœæ I
#fenodata
#projekt musi mieæ czêœæ zwi¹zan¹ z ekspresj¹ genów
#tabela opisu genów powinna zaw. symbol genów, entres ID, opis(mniej istotne)
#stworzyæ expression set 
#program musi funkcjonowaæ na zasadzie pliku expr set
#wybieraæ z niego dane grupy do analizy
#wynik to tabela genów róznicuj¹cych 
#wybór genów za pomoc¹ lime albo test t

## czêœæ II
#dla genów ró¿nicuj¹cych wybranych w czêsci 1
#do analizy œcie¿ek sygna³owych wykorzystujemy metodê gsa
#same sygnatury œcie¿ek powinny byæ wczytane ze strony brow institute - w katalogu wytyczne jest link
#wystarczy siê na stronie zarejestrowaæ
#to s¹ pliki tekstowe z sygnatur¹ i symbole genów, które w œciezkê ekspresji wchodz¹
#mo¿na plik edytowac i stworzyæ po swojemu sygnatury œciezek
#do poprawnej analizy oprócz listy genów ró¿nicuj¹cych potrzebna jest lista wszystkich genów!!!
#zalezne od typu macierzy
#w trakcie podane zostan¹ przydatne funkcje
#program dostêpny ze strony www
#wykorzystany do tego pakiet shiny - dzia³a na zasadzie klient-serwer
#wykorzystane wykresy budowane na pakiecie plotly, korzysta z gg plota
#stworzyæ projekt w gicie
#kazdy pisze osobno, zcalane w jeden interfejs

source("http://bioconductor.org/biocLite.R")

#nie oznaczaæ affymetrix, tylko ferrariego albo jakaœ inna - do nazwy dodane ga
#funckja emget

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
#data=data[c(1:5,244:248),]# do testówlepiej pracowaæna ma³ej liczbie mikromacierzy
opis = read.AnnotatedDataFrame("datasetA_scans.txt", sep="\t", header=TRUE, row.names=4, stringsAsFactors = F) 
#opis=opis[c(1:5,244:248)]
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


