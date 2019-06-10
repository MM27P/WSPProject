## Usuniecie 5% sond o min i max œredniej ekspresji
usuniecie_sond = function(ExprSet){
 
  dane_sort = sort(rowMeans(exprs(ExprSet)),index.return=T)
  #iloœæ zestawów po reannotacji
  sondy_zestawy= dim(ExprSet)[1]
  #sondy do usuniêcia
  sondy_usun =round(dim(ExprSet)[1]*0.025)
  indeksy_usun=dane_sort$ix[c(1:sondy_usun,(sondy_zestawy-sondy_usun):sondy_zestawy)]
  nowy_ExprSet=ExprSet[-indeksy_usun,] 
  
  #return (nowy_ExprSet)
  
  summary_table=function(nowy_ExprSet, method, sort_criterion, col_nr, sep){
    adeno=which(pData(ExprSet)$CLASS=='ADENO')
    squamous=which(pData(ExprSet)$CLASS=='SQUAMOUS')
    expr=exprs(nowy_ExprSet)
    sr_A=rowMeans(expr[,adeno])
    sr_S=rowMeans(expr[,squamous])
    FC=sr_adeno/sr_squamous
    statistic=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$statistic) 
    p_wartosc=apply(expr,1,function(x) t.test(x[adeno],x[squamous])$p.val)
    p_wartosc_skorygowane=p.adjust(pval, method = method)
    symbol=unlist(mget(featureNames(ExprSet),env=gahgu95av2SYMBOL)) 
    genNames=unlist(mget(featureNames(ExprSet),env=gahgu95av2GENENAME))
    TAB=array(dim=c(dim(expr)[1],9))
    colnames(TAB)=c("FerrariID","Symbol","description","fold change","orednia w gr.ADENO","orednia w gr.NORMAL","t-statistics",
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
      ind_sort=sort(p_val_skorygowane,index=TRUE)$ix
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
  
 }