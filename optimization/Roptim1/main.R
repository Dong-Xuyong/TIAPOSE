source("custos.R");
source("./Roptim1/blind.R");
source("./Roptim1/montecarlo.R");
#CUSTO TOTAL
custo <- function(quantidade,sera_fds,sera_dia_normal,v1,v2,v3){
    ca <- custo_armazem(quantidade,sera_fds,sera_dia_normal);
    cd <- custo_distribuicao(v1,v2,v3,sera_fds,quantidade);
    result <- ca + cd;
    return(result);
}
#VALOR DE VENDA
valor_venda <- function(valor_venda,quantidade){
    result <- valor_venda*quantidade;
    return(result);
}
#LUCRO TOTAL
lucro <- function(quantidade,sera_fds,sera_dia_normal,v1,v2,v3,valor_venda){
    c <- custo(quantidade,sera_fds,sera_dia_normal,v1,v2,v3);
    vv <- valor_venda(valor_venda,quantidade);
    result <- vv-c;
    return(result);
}
#START OF THE FUNCTION
main <- function(quantidade,sera_fds,sera_dia_normal,valor_venda){
#EVALUATION FUNCTION
evaluation_function <- function(unsigned_vector){
    result <- custo_distribuicao(unsigned_vector[1],unsigned_vector[2],unsigned_vector[3],sera_fds,quantidade);
    return(result);
}
#ASSIGN THE RESULT OF THE MONTECARMO AND GO
result <- mcsearch(evaluation_function,c(0,0,0),c(4,4,4),20,type="min");
return(result);
}
