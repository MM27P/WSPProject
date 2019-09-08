#część II
usuniecie_sond = function(ExprSet){
  
  dane_sort = sort(rowMeans(exprs(ExprSet)),index.return=T)
  #iloœæ zestawów po reannotacji
  sondy_zestawy= dim(ExprSet)[1]
  #sondy do usuniêcia
  sondy_usun =round(dim(ExprSet)[1]*0.025)
  indeksy_usun=dane_sort$ix[c(1:sondy_usun,(sondy_zestawy-sondy_usun):sondy_zestawy)]
  nowy_ExprSet=ExprSet[-indeksy_usun,] 
}
  #return (nowy_ExprSet)
  
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
  
eSet1 <- usuniecie_sond(eSet)

tabletest<-summary_table(exprSet, klasy=c("ADENO","SQUAMOUS"),method='holm', sort_criterion="FoldChange",number=20 )

