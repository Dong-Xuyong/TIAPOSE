#FUNCAO PARA O CUSTO DO ARMAZEM
custo_armazem <- function(quantidade,sera_fds,sera_dia_normal){
  result <- (ceiling((quantidade/72))*( (15*sera_fds) + (10*sera_dia_normal) ));
  return(result);
}
#FUNCAO PARA O CUSTO_DISTRIBUICAO
custo_distribuicao <- function(v1,v2,v3,sera_fds,quantidade){
  result <- ((40+sera_fds*5)*((quantidade-(v2*90)-(v3*120))/60) 
             + (50+sera_fds*5)*((quantidade-(v1*60)-(v2*120))/90) 
             + (53+sera_fds*5)*((quantidade-(v1*60)-(v2*90))/120));
  return(result);
}