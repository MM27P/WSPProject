## Usuniecie 5% sond o min i max œredniej ekspresji
usuniecie_sond = function(ExprSet){
 
  dane_sort = sort(rowMeans(exprs(ExprSet)),index.return=T)
  #iloœæ zestawów po reannotacji
  sondy_zestawy= dim(ExprSet)[1]
  #sondy do usuniêcia
  sondy_usun =round(dim(ExprSet)[1]*0.025)
  indeksy_usun=dane_sort$ix[c(1:cutoff,(sondy_zestawy-sondy_usun):sondy_zestawy)]
  nowy_ExprSet=ExprSet[-indeksy_usun,] 
  
  return (nowy_ExprSet)
}