# @param quantidade_v1, quantidade de um dado tipo de cerveja usada no recurso v1,
# @param quantidade_v2, quantidade de um dado tipo de cerveja usada no recurso v2,
# @param quantidade_v3, quantidade de um dado tipo de cerveja usada no recurso v3,
# @param procura, procura por uma dada cerveja expectada na previsao
# @returns sobras totais de um dado produto de um dado dia
sobras_loja_calculator <- function(
    quantidade_v1,
    quantidade_v2,
    quantidade_v3,
    procura
){
    value <- procura - (quantidade_v1+quantidade_v2+quantidade_v3)
    return(value);
}

# @param quantidade_bud_v1, quantidade de bud para v1,
# @param quantidade_bud_v2, quantidade de bud para v2,
# @param quantidade_bud_v3, quantidade de bud para v3,
# @param quantidade_stella_v1, quantidade de stella para v1,
# @param quantidade_stella_v2, quantidade de stella para v2,
# @param quantidade_stella_v3, quantidade de stella para v3,
# @param quantidade_lote_retalhista, quantidade necessaria para utilizar um recurso de retalho
# @returns quantidade de recursos tipo retalhista vou precisar
quantidade_retalhista_calculator <- function(
    quantidade_bud_v1,
    quantidade_bud_v2,
    quantidade_bud_v3,
    quantidade_stella_v1,
    quantidade_stella_v2,
    quantidade_stella_v3,
    quantidade_lote_retalhista){
    # SOMAR TODAS AS QUANTIDADES INDIVIDUAIS E DIVIDIR POR 72 PARA OBTER ARMAZEM DE RETALHISTA TOTAL
    value <- ((
               quantidade_bud_v1+quantidade_bud_v2+quantidade_bud_v3+
               quantidade_stella_v1+quantidade_stella_v2+quantidade_stella_v3
              )/quantidade_lote_retalhista);
    return(value);
    }

# @param quantidade_bud, quantidade de bud utilizada num dado tipo de transporte
# @param quantidade_stella, quantidade de stella utilizada num dado tipo de transporte
# @param custo, custo do próprio transporte, este vai dizer que tipo de transporte é (v1=60,v2=90,v3=120)
# @returns quantidade de transporte afeta a um dado tipo de custo
quantidade_transporte_calculator <- function(quantidade_bud,quantidade_stella,lote_maximo){
# NUMERO DE TRANSPORTES AFETOS A UM DADO TIPO DE TRANSPORTE
quantidade <- ((quantidade_bud + quantidade_stella)/lote_maximo);
return(quantidade);
}
#CUSTO REFERENTE AO PRÓPRIO DIA
# @param v1, opcao de transporte v1,
# @param v2, opcao de transporte v2,
# @param v3, opcao de transporte v3
# @returns custo normal afeto aquele dia
custo_dia_calculator <- function(
    v1,
    v2,
    v3,
    quantidade_retalhista,
    sobras_bud,
    sobras_stella,
    custo_v1,
    custo_v2,
    custo_v3,
    custo_retalhista_fds,
    custo_retalhista_normal,
    sera_fds,
    sera_normal,
    custo_sobra
    ){
#CUSTO REFERENTE AO TRANSPORTE
custo_distribuicao <- v1*custo_v1 + v2*custo_v2 + v3*custo_v3;
#CUSTO REFERENTE AO RETALHISTA
custo_retalhista <- quantidade_retalhista * 
(custo_retalhista_fds * sera_fds + custo_retalhista_normal * sera_normal);
#CUSTO REFERENTE ÁS SOBRAS DE BUD
custo_sobras_bud <- sobras_bud * custo_sobra;
#CUSTO REFERENTE ÁS SOBRAS DE STELLA
custo_sobras_stella <- sobras_stella * custo_sobra;
#RESULT
result <- custo_distribuicao + custo_retalhista + custo_sobras_bud + custo_sobras_stella;
return(result);
}
#CUSTO REFERENTE AO DIA TOTAL
# @param procura_bud, procura de bud
# @param procura_stella, procura de stella
# @param sobras_bud_dia_anterior, sobras do dia anterior
# @param sobras_stella_dia_anterior, sobras da stella no dia anterior
# @param quantidade_bud_v1, quantidade de bud utilizada em v1
# @param quantidade_bud_v2, quantidade de bud utilizada em v2
# @param quantidade_bud_v3, quantidade de bud utilizada em v3
# @param quantidade_stella_v1, quantidade de stella utilizada em v1
# @param quantidade_stella_v2, quantidade de stella utilizada em v2
# @param quantidade_stella_v3, quantidade de stella utilizada em v3
# @param v1, quantidade em v1
# @param v2, quantidade em v2
# @param v3, quantidade em v3
# @param custo_v1, custo em v1
# @param custo_v2, custo em v2
# @param custo_v3, custo em v3
# @param quantidade_lote_retalhista, quantidade maxima por recurso retalhista
# @param sera_fds, se é fds
# @param sera_normal, se é dia_normal
# @param custo_retalhista_fds, custo do retalhista no fds
# @param custo_retalhista_normal, custo do retalhista num dia normal
# @param custo_stock_em_loja, custo do stock em loja
# @returns custo total do próprio dia
custo_dia_total_calculator <- function(
    procura_bud,
    procura_stella,
    sobras_bud_dia_anterior,
    sobras_stella_dia_anterior,
    quantidade_bud_v1,
    quantidade_bud_v2,
    quantidade_bud_v3,
    quantidade_stella_v1,
    quantidade_stella_v2,
    quantidade_stella_v3,
    custo_v1,
    custo_v2,
    custo_v3,
    quantidade_lote_retalhista,
    sera_fds,
    sera_normal,
    custo_retalhista_fds,
    custo_retalhista_normal,
    custo_stock_em_loja,
    lote_maximo_v1,
    lote_maximo_v2,
    lote_maximo_v3
){
# PROCURA NECESSARIA PARA BUD
procura_real_bud <- procura_bud - sobras_bud_dia_anterior;
print(procura_real_bud);
# PROCURA NECESSARIA PARA STELLA
procura_real_stella <- procura_stella - sobras_stella_dia_anterior;
print(procura_real_stella);
# SOBRAS DE BUD
sobras_bud <- sobras_loja_calculator(
              quantidade_bud_v1,
              quantidade_bud_v2,
              quantidade_bud_v3,
              procura_real_bud
              );
              print(sobras_bud);
#SOBRAS DE STELLA
sobras_stella <- sobras_loja_calculator(
                 quantidade_stella_v1,
                 quantidade_stella_v2,
                 quantidade_stella_v3,
                 procura_real_stella
                 );
                 print(sobras_stella);
#QUANTO SERA V1
v1 <- quantidade_transporte_calculator(quantidade_bud_v1,quantidade_stella_v1,lote_maximo_v1);
print(v1);
#QUANTO SERA V2
v2 <- quantidade_transporte_calculator(quantidade_bud_v2,quantidade_stella_v2,lote_maximo_v2);
print(v2);
#QUANTO SERA V3
v3 <- quantidade_transporte_calculator(quantidade_bud_v3,quantidade_stella_v3,lote_maximo_v3);
print(v3);
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
print(retalhista);
#CUSTO TOTAL
custo_total <- custo_dia_calculator(
v1,
v2,
v3,
retalhista,
sobras_bud,
sobras_stella,
custo_v1,
custo_v2,
custo_v2,
custo_retalhista_fds,
custo_retalhista_normal,
sera_fds,
sera_normal,
custo_stock_em_loja
);
print(custo_total);
#RETORNAR CUSTO TOTAL E RESPETIVAS SOBRAS PARA ITERAR NOVAMENTE
result <- list(custo=custo_total,sobras_bud=sobras_bud,sobras_stella=sobras_stella);
return(result);
}