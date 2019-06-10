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
  
  summary_table=function(nowy_ExprSet, method, sort_criterion, col_nr, sep){
    adeno=which(pData(ExprSet)$CLASS=='ADENO')
    squamous=which(pData(ExprSet)$CLASS=='SQUAMOUS')
    expr=exprs(nowy_ExprSet)
    sr_A=rowMeans(expr[,adeno])
    sr_S=rowMeans(expr[,squamous])
    FC=sr_A/sr_S
    statistic=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$statistic) 
    p_wartosc=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$p.val)
    p_wartosc_skorygowane=p.adjust(p_wartosc, method = method)
    symbol=unlist(mget(featureNames(ExprSet),env=gahgu95av2SYMBOL)) 
    genNames=unlist(mget(featureNames(ExprSet),env=gahgu95av2GENENAME))
    TAB=array(dim=c(dim(expr)[1],9))
    colnames(TAB)=c("FerrariID","Symbol","opis","fold_change","srednia_w_gr_ADENO","srednia_w_gr_NORMAL","Wartosc_statystyki_t",
                    "p-wartosc","skorygowana_p-wartosc")
    TAB[,1]=featureNames(ExprSet)
    TAB[,2]=symbol
    TAB[,3]=genNames
    TAB[,4]=FC
    TAB[,5]=sr_A
    TAB[,6]=sr_S
    TAB[,7]=statistic
    TAB[,8]=p_wartosc
    TAB[,9]=p_wartosc_skorygowane
    #zwrócić tu tabelę
    TAB_ALL=TAB
    head(TAB) 
    if (sort_criterion>1){
      ind_sort=sort(p_wartosc_skorygowane,index=TRUE)$ix
      TAB=TAB[ind_sort[1:sort_criterion],]}
    if (sort_criterion<1){ 
      ind_sort=which(p_wartosc_skorygowane<sort_criterion)
      TAB=TAB[ind_sort,]}
    if (col_nr!=0){
      ind_sort=sort(TAB[col_nr,],index=TRUE)$ix
      TAB=TAB[ind_sort,]}
    write.table(TAB, file="data_table.txt",sep=sep,row.names = FALSE)
    ekspr_wybrane=expr[ind_sort,]
    png("heatmap.png")
    heatmap.2(ekspr_wybrane)
    dev.off()
    return(list(TAB,TAB_ALL))
    
  }
  


summary_table(ExprSet, method='holm', sort_criterion=15, col_nr=5, sep=',')

