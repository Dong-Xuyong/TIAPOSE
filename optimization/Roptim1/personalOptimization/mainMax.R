source("custos.R");
source("receitas.R");
source("../Roptim1/blind.R");
source("../Roptim1/montecarlo.R");
source("../Roptim1/grid.R");
source("../Roptim1/hill.R");
library(tabuSearch)

# MAIN FOR THE GRID
# PUT AT THE MAX 2 ITERATIONS
main_grid <- function(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes){
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
custo_v1 <- 40;
custo_v2 <- 50;
custo_v3 <- 53;
lote_maximo_v1 <- 60;
lote_maximo_v2 <- 90;
lote_maximo_v3 <- 120;
quantidade_lote_retalhista <- 72;
custo_retalhista_fds <- 15;
custo_retalhista_normal <- 10;
custo_stock_em_loja <- 1;
preco_bud <- 5.7;
preco_stella <- 4.4;
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
# Index procura
indexProcura <- ((2*(dia-1))+1);
# CUSTO DO DIA
custo_dia <- custo_dia_total_calculator(
vetorProcura[indexProcura],
vetorProcura[indexProcura+1],
sobras_bud_dia_anterior,
sobras_stella_dia_anterior,
ceiling(vetor[indexInicial]),
ceiling(vetor[indexInicial+1]),
ceiling(vetor[indexInicial+2]),
ceiling(vetor[indexInicial+3]),
ceiling(vetor[indexInicial+4]),
ceiling(vetor[indexInicial+5]),
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
custo_dia$vendas_bud,
custo_dia$vendas_stella, 
preco_bud,
preco_stella
);
# ADICIONAR RECEITA Á RECEITA TOTAL
receita_total <- receita_total + venda;
}
lucro <- receita_total - custo_total;
return(lucro);
}
# 1st example: dimension D=2
D=length(lower)
# grid search 10x10 search: length(seq(-10.4,10.4,by=2.1))== 10:
range=upper[1]-lower[1]
nsearches=numero_iteracoes # searches per dimension
jump=range/(nsearches-1) # 10 searches per dimension
step=rep(jump,D) # 10 searches per dimension
iter=nsearches^D;
result=gsearch(fn=eval,lower=lower,upper=upper,step=step,type="max")
return(result);
}

# FUNCAO MAIN, RESPONSAVEL PELO RUN GLOBAL
main_hill <- function(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes){
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
custo_v1 <- 40;
custo_v2 <- 50;
custo_v3 <- 53;
lote_maximo_v1 <- 60;
lote_maximo_v2 <- 90;
lote_maximo_v3 <- 120;
quantidade_lote_retalhista <- 72;
custo_retalhista_fds <- 15;
custo_retalhista_normal <- 10;
custo_stock_em_loja <- 1;
preco_bud <- 5.7;
preco_stella <- 4.4;
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
# Index procura
indexProcura <- ((2*(dia-1))+1);
# CUSTO DO DIA
custo_dia <- custo_dia_total_calculator(
vetorProcura[indexProcura],
vetorProcura[indexProcura+1],
sobras_bud_dia_anterior,
sobras_stella_dia_anterior,
ceiling(vetor[indexInicial]),
ceiling(vetor[indexInicial+1]),
ceiling(vetor[indexInicial+2]),
ceiling(vetor[indexInicial+3]),
ceiling(vetor[indexInicial+4]),
ceiling(vetor[indexInicial+5]),
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
custo_dia$vendas_bud,
custo_dia$vendas_stella, 
preco_bud,
preco_stella
);
# ADICIONAR RECEITA Á RECEITA TOTAL
receita_total <- receita_total + venda;
}
lucro <- receita_total - custo_total;
return(lucro);
}

# hill climbing search
N=numero_iteracoes
REPORT=N/20 # report results


#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=20,round=FALSE) }

# initial solution: 
s0=rep(0,length(lower)) # one extreme point, could be a random point

result=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
             control=list(maxit=N,REPORT=REPORT,digits=1))
return(result);
}

# FUNCAO MAIN, RESPONSAVEL PELO RUN GLOBAL
main_mc <- function(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes){
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
custo_v1 <- 40;
custo_v2 <- 50;
custo_v3 <- 53;
lote_maximo_v1 <- 60;
lote_maximo_v2 <- 90;
lote_maximo_v3 <- 120;
quantidade_lote_retalhista <- 72;
custo_retalhista_fds <- 15;
custo_retalhista_normal <- 10;
custo_stock_em_loja <- 1;
preco_bud <- 5.7;
preco_stella <- 4.4;
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
# Index procura
indexProcura <- ((2*(dia-1))+1);
# CUSTO DO DIA
custo_dia <- custo_dia_total_calculator(
vetorProcura[indexProcura],
vetorProcura[indexProcura+1],
sobras_bud_dia_anterior,
sobras_stella_dia_anterior,
ceiling(vetor[indexInicial]),
ceiling(vetor[indexInicial+1]),
ceiling(vetor[indexInicial+2]),
ceiling(vetor[indexInicial+3]),
ceiling(vetor[indexInicial+4]),
ceiling(vetor[indexInicial+5]),
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
custo_dia$vendas_bud,
custo_dia$vendas_stella, 
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

# FUNCAO MAIN, RESPONSAVEL PELO RUN GLOBAL
main_sann <- function(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes){
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
custo_v1 <- 40;
custo_v2 <- 50;
custo_v3 <- 53;
lote_maximo_v1 <- 60;
lote_maximo_v2 <- 90;
lote_maximo_v3 <- 120;
quantidade_lote_retalhista <- 72;
custo_retalhista_fds <- 15;
custo_retalhista_normal <- 10;
custo_stock_em_loja <- 1;
preco_bud <- 5.7;
preco_stella <- 4.4;
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
# Index procura
indexProcura <- ((2*(dia-1))+1);
# CUSTO DO DIA
custo_dia <- custo_dia_total_calculator(
vetorProcura[indexProcura],
vetorProcura[indexProcura+1],
sobras_bud_dia_anterior,
sobras_stella_dia_anterior,
ceiling(vetor[indexInicial]),
ceiling(vetor[indexInicial+1]),
ceiling(vetor[indexInicial+2]),
ceiling(vetor[indexInicial+3]),
ceiling(vetor[indexInicial+4]),
ceiling(vetor[indexInicial+5]),
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
custo_dia$vendas_bud,
custo_dia$vendas_stella, 
preco_bud,
preco_stella
);
# ADICIONAR RECEITA Á RECEITA TOTAL
receita_total <- receita_total + venda;
}
lucro <- receita_total - custo_total;
return(lucro);
}
# dimension
D=length(lower)
# hill climbing search
N=numero_iteracoes;
# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=20,round=FALSE) }
CSANN=list(maxit=N,temp=D,trace=TRUE)
result=optim(par=lower,fn=eval,method="SANN",gr=rchange2,control=CSANN)
return(result);
}