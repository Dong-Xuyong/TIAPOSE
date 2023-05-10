source("custos.R");
source("receitas.R");
source("../Roptim1/blind.R");
source("../Roptim1/montecarlo.R");
# FUNCAO MAIN, RESPONSAVEL PELO RUN GLOBAL
main <- function(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes){
#FUNCAO DE AVALIACAO
eval <- function(vetor){
#INIT VARIAVEIS PRINCIPAIS
custo_total <- 0;
receita_total <- 0;
sobras_bud_dia_anterior <- 0;
sobras_stella_dia_anterior <- 0;
sera_fds <- 0;
sera_normal <- 0;
#CUSTOS FIXOS
custo_v1 <- 60;
custo_v2 <- 90;
custo_v3 <- 120;
lote_maximo_v1 <- 200;
lote_maximo_v2 <- 200;
lote_maximo_v3 <- 200;
quantidade_lote_retalhista <- 60;
custo_retalhista_fds <- 2;
custo_retalhista_normal <- 3;
custo_stock_em_loja <- 1;
preco_bud <- 4.4;
preco_stella <- 5;
#CONSTITUTICAO DE VETORES
#1- QUANTIDADE_BUD_V1
#2- QUANTIDADE_BUD_V2
#3- QUANTIDADE_BUD_V3
#4- QUANTIDADE_STELLA_V1
#5- QUANTIDADE_STELLA_V2
#6- QUANTIDADE_STELLA_V3
#EX: dia 2 V1 corresponde a vetorProcura[(6*(dia-1))+1]
for(dia in 1: (length(vetorProcura)/2)){
# VERIFICAR SE É FDS OU NAO
if(vetorSeraFds[dia]== 0){
    sera_fds <- 0;
    sera_normal <- 1;
}else{
    sera_fds <- 1;
    sera_normal <- 0;
}
# Index atual
indexInicial<-((6*(dia-1))+1);
# CUSTO DO DIA
custo_dia <- custo_dia_total_calculator(
vetorProcura[dia],
vetorProcura[dia+1],
sobras_bud_dia_anterior,
sobras_stella_dia_anterior,
vetor[indexInicial],
vetor[indexInicial+1],
vetor[indexInicial+2],
vetor[indexInicial+3],
vetor[indexInicial+4],
vetor[indexInicial+5],
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
);
# ADICIONAR CUSTO DO DIA AO CUSTO TOTAL
custo_total <- custo_total + custo_dia$custo;
# REALOCAR SOBRAS
sobras_bud_dia_anterior <- custo_dia$sobras_bud;
sobras_stella_dia_anterior <- custo_dia$sobras_stella;
# RECEITA DO DIA
venda <- venda_total_calculator(
vetor[indexInicial],
vetor[indexInicial+1],
vetor[indexInicial+2],
vetor[indexInicial+3],
vetor[indexInicial+4],
vetor[indexInicial+5], 
preco_bud,
preco_stella
);
# ADICIONAR RECEITA Á RECEITA TOTAL
receita_total <- receita_total + venda;
}
lucro <- receita_total - custo_total;
return(lucro);
}
result <- mcsearch(eval,lower,upper,numero_iteracoes,type="max");
return(result);
}