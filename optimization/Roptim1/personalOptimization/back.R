source("custos.R");
# FUNCAO PARA CALCULAR V1,V2,V3 RETALHISTA E QUANTIDADES DANDO OS OTIMOS
back <- function(
quantidade_bud_v1,
quantidade_bud_v2,
quantidade_bud_v3,
quantidade_stella_v1,
quantidade_stella_v2,
quantidade_stella_v3    
){
quantidade_lote_retalhista <- 72;
lote_maximo_v1 <- 60;
lote_maximo_v2 <- 90;
lote_maximo_v3 <- 120;
#QUANTO SERA V1
v1 <- quantidade_transporte_calculator(quantidade_bud_v1,quantidade_stella_v1,lote_maximo_v1);
#QUANTO SERA V2
v2 <- quantidade_transporte_calculator(quantidade_bud_v2,quantidade_stella_v2,lote_maximo_v2);
#QUANTO SERA V3
v3 <- quantidade_transporte_calculator(quantidade_bud_v3,quantidade_stella_v3,lote_maximo_v3);
#NUMERO DE RETALHISTAS
retalhista <- quantidade_retalhista_calculator(
quantidade_bud_v1,
quantidade_bud_v2,
quantidade_bud_v3,
quantidade_stella_v1,
quantidade_stella_v2,
quantidade_stella_v3,
quantidade_lote_retalhista
);
#QUANTIDADES
quantidade_bud <- quantidade_bud_v1 + quantidade_bud_v2 + quantidade_bud_v3;
quantidade_stella <- quantidade_stella_v1 + quantidade_stella_v2 + quantidade_stella_v3;
return(list(v1=v1,v2=v2,v3=v3,retalhista=retalhista,quantidade_bud=quantidade_bud,quantidade_stella=quantidade_stella));
}
# INFERIR PELO OTIMO
inferir_pelo_otimo <- function(vetor,procura){
list_v1 <- c();
list_v2 <- c();
list_v3 <- c();
list_retalhista <- c();
list_quantidade_bud <- c();
list_quantidade_stella <- c();
for(lote in 1:(length(vetor)/6)){
index <- ((6*(lote-1))+1);
result <- back(
vetor[index],
vetor[index+1],
vetor[index+2],
vetor[index+3],
vetor[index+4],
vetor[index+5]    
);
list_v1[length(list_v1)+1]<-result$v1;
list_v2[length(list_v2)+1]<-result$v2;
list_v3[length(list_v3)+1]<-result$v3;
list_retalhista[length(list_retalhista)+1]<-result$retalhista;
list_quantidade_bud[length(list_quantidade_bud)+1]<-result$quantidade_bud;
list_quantidade_stella[length(list_quantidade_stella)+1]<-result$quantidade_stella;
}
list_sobras_bud <- c(0);
list_sobras_stella <- c(0);
if((length(vetor)/6)>1){
 for(lote in 1:((length(vetor)/6)-1)){
    index <- ((6*(lote-1))+1)
    index_procura <- ((2*(lote-1))+1)
list_sobras_bud[length(list_sobras_bud)+1] <- sobras_loja_calculator(
    vetor[index],
    vetor[index+1],
    vetor[index+2],
    list_sobras_bud[[lote]],
    procura[index_procura]
    );
list_sobras_stella[length(list_sobras_stella)+1] <- sobras_loja_calculator(
    vetor[index+3],
    vetor[index+4],
    vetor[index+5],
    list_quantidade_stella[[lote]],
    procura[index_procura+1]
);
}   
}

result <- list(
    list_v1=list_v1,
    list_v2=list_v2,
    list_v3=list_v3,
    list_retalhista=list_retalhista,
    list_quantidade_bud=list_quantidade_bud,
    list_quantidade_stella=list_quantidade_stella,
    list_sobras_bud=list_sobras_bud,
    list_sobras_stella=list_sobras_stella
    );
return(result);
}