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

saveRDS(ExprSet, file = "Exprset.rds")


#posk³adany expression set 
#mget - pierwszy arg to lista oznaczeñ, a drugi to zmienna environment
#jak wyrzucamy tabelê Excelowsk¹ to za pomoc¹ openxlsx
#wygodniejsze w wykorzystaniu

expr_sort=sort(rowMeans(exprs(ExprSet)),index.return=T)
feat_num=dim(ExprSet)[1]# sprawdzamy ile jest zestawów sond na macierzy po reannotacji
cutoff=round(dim(ExprSet)[1]*0.025) #wyznaczamy iloœæ sond do usuniêcia po obu stronach
ind_clear=expr_sort$ix[c(1:cutoff,(feat_num-cutoff):feat_num)]
ExprSet=ExprSet[-ind_clear,]

## tu siê zaczyna dalsza czêœæ przyk³adowego programu
PCA_model=prcomp(t(exprs(ExprSet)))
summary(PCA_model)
PCA_model$x
adeno=which(pData(ExprSet)$CLASS=='ADENO')#wyszukujemy próby ADENO
squamous=which(pData(ExprSet)$CLASS=='SQUAMOUS')# analogicznie do SQUAMOUS
colors=ifelse(pData(ExprSet)$CLASS=='ADENO', 'red', 'blue')#dobieramy kolory
plot(PCA_model$x[,1:2], col=colors, main='PCA')
barplot(PCA_model$sdev[1:5]/sum(PCA_model$sde),main='PCA')   

summary_table=function(ExprSet, method, sort_criterion, col_nr, sep){
  adeno=which(pData(ExprSet)$CLASS=='ADENO')#wyszukujemy próbyADENO
  squamous=which(pData(ExprSet)$CLASS=='SQUAMOUS')
  expr=exprs(ExprSet)
  sr_adeno=rowMeans(expr[,adeno])
  sr_squamous=rowMeans(expr[,squamous])
  FC=sr_adeno/sr_squamous
  statistic=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$statistic) 
  pval=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$p.val)
  p_val_skorygowane=p.adjust(pval, method = method)
  symbol=unlist(mget(featureNames(ExprSet),env=gahgu95av2SYMBOL)) 
  genNames=unlist(mget(featureNames(ExprSet),env=gahgu95av2GENENAME))
  TAB=array(dim=c(dim(expr)[1],9))
  colnames(TAB)=c("FerrariID","Symbol","description","fold change","œrednia w gr.ADENO","œrednia w gr.NORMAL","t-statistics",
                  "p-value","corrected p-value")
  TAB[,1]=featureNames(ExprSet)
  TAB[,2]=symbol
  TAB[,3]=genNames
  TAB[,4]=FC
  TAB[,5]=sr_adeno
  TAB[,6]=sr_squamous
  TAB[,7]=statistic
  TAB[,8]=pval
  TAB[,9]=p_val_skorygowane
  head(TAB) 
  if (sort_criterion>1){
    ind_sort=sort(p_val_skorygowane,index=TRUE)$ix#interesuj¹mnie indeksy
    TAB=TAB[ind_sort[1:sort_criterion],]}
  if (sort_criterion<1){ 
    ind_sort=which(p_val_skorygowane<sort_criterion)
    TAB=TAB[ind_sort,]}
  if (col_nr!=0){
    ind_sort=sort(TAB[col_nr,],index=TRUE)$ix
    TAB=TAB[ind_sort,]}
  write.table(TAB, file="data_table.txt",sep=sep,row.names = FALSE)
  ekspr_wybrane=expr[ind_sort,]
  png("heatmap.png")
  heatmap.2(ekspr_wybrane)
  dev.off()
  return(TAB)
}

summary_table(ExprSet, method='holm', sort_criterion=15, col_nr=5, sep=',')
