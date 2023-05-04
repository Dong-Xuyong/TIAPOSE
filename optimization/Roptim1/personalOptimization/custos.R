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
quantidade_transporte_calculator <- function(quantidade_bud,quantidade_stella,custo){
# NUMERO DE TRANSPORTES AFETOS A UM DADO TIPO DE TRANSPORTE
quantidade <- ((quantidade_bud + quantidade_stella)/custo);
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
#CUSTO REFERENTE ÁS SOBRAS DE BUG
custo_sobras_bud <- sobras_bud * custo_sobra;
#CUSTO REFERENTE ÁS SOBRAS DE STELLA
custo_sobras_stella <- sobras_stella * custo_sobra;
#RESULT
result <- custo_distribuicao + custo_retalhista + custo_sobras_bud + custo_sobras_stella;
return(result);
}
# DIA 1
# @param dia_um_sobras_bud,
# @param dia_um_sobras_stella,
# @param dia_um_v1,
# @param dia_um_v2,
# @param dia_um_v3,
# @param dia_um_quantidade_bud_v1,
# @param dia_um_quantidade_bud_v2,
# @param dia_um_quantidade_bud_v3,
# @param dia_um_quantidade_stella_v1,
# @param dia_um_quantidade_stella_v2,
# @param dia_um_quantidade_stella_v3,
# @param dia_um_sera_fds,
# @param dia_um_sera_normal,
# @param dia_um_procura_bud,
# @param dia_um_procura_stella,
# @param dia_um_procura_real_bud,
# @param dia_um_procura_real_stella
# DIA 2
# @param dia_dois_sobras_bud,
# @param dia_dois_sobras_stella,
# @param dia_dois_v1,
# @param dia_dois_v2,
# @param dia_dois_v3,
# @param dia_dois_quantidade_bud_v1,
# @param dia_dois_quantidade_bud_v2,
# @param dia_dois_quantidade_bud_v3,
# @param dia_dois_quantidade_stella_v1,
# @param dia_dois_quantidade_stella_v2,
# @param dia_dois_quantidade_stella_v3
# @param dia_dois_sera_fds,
# @param dia_dois_sera_normal,
# @param dia_dois_procura_bud,
# @param dia_dois_procura_stella,
# @param dia_dois_procura_real_bud,
# @param dia_dois_procura_real_stella
# DIA 3
# @param dia_tres_sobras_bud,
# @param dia_tres_sobras_stella,
# @param dia_tres_v1,
# @param dia_tres_v2,
# @param dia_tres_v3,
# @param dia_tres_quantidade_bud_v1,
# @param dia_tres_quantidade_bud_v2,
# @param dia_tres_quantidade_bud_v3,
# @param dia_tres_quantidade_stella_v1,
# @param dia_tres_quantidade_stella_v2,
# @param dia_tres_quantidade_stella_v3,
# @param dia_tres_sera_fds,
# @param dia_tres_sera_normal,
# @param dia_tres_procura_bud,
# @param dia_tres_procura_stella,
# @param dia_tres_procura_real_bud,
# @param dia_tres_procura_real_stella
# DIA 4
# @param dia_quatro_sobras_bud,
# @param dia_quatro_sobras_stella,
# @param dia_quatro_v1,
# @param dia_quatro_v2,
# @param dia_quatro_v3,
# @param dia_quatro_quantidade_bud_v1,
# @param dia_quatro_quantidade_bud_v2,
# @param dia_quatro_quantidade_bud_v3,
# @param dia_quatro_quantidade_stella_v1,
# @param dia_quatro_quantidade_stella_v2,
# @param dia_quatro_quantidade_stella_v3,
# @param dia_quatro_sera_fds,
# @param dia_quatro_sera_normal,
# @param dia_quatro_procura_bud,
# @param dia_quatro_procura_stella,
# @param dia_quatro_procura_real_bud,
# @param dia_quatro_procura_real_stella
# DIA 5
# @param dia_cinco_sobras_bud,
# @param dia_cinco_sobras_stella,
# @param dia_cinco_v1,
# @param dia_cinco_v2,
# @param dia_cinco_v3,
# @param dia_cinco_quantidade_bud_v1,
# @param dia_cinco_quantidade_bud_v2,
# @param dia_cinco_quantidade_bud_v3,
# @param dia_cinco_quantidade_stella_v1,
# @param dia_cinco_quantidade_stella_v2,
# @param dia_cinco_quantidade_stella_v3,
# @param dia_cinco_sera_fds,
# @param dia_cinco_sera_normal,
# @param dia_cinco_procura_bud,
# @param dia_cinco_procura_stella,
# @param dia_cinco_procura_real_bud,
# @param dia_cinco_procura_real_stella
# DIA 6
# @param dia_seis_sobras_bud,
# @param dia_seis_sobras_stella,
# @param dia_seis_v1,
# @param dia_seis_v2,
# @param dia_seis_v3,
# @param dia_seis_quantidade_bud_v1,
# @param dia_seis_quantidade_bud_v2,
# @param dia_seis_quantidade_bud_v3,
# @param dia_seis_quantidade_stella_v1,
# @param dia_seis_quantidade_stella_v2,
# @param dia_seis_quantidade_stella_v3,
# @param dia_seis_sera_fds,
# @param dia_seis_sera_normal,
# @param dia_seis_procura_bud,
# @param dia_seis_procura_stella,
# @param dia_seis_procura_real_bud,
# @param dia_seis_procura_real_stella
# DIA 7
# @param dia_sete_sobras_bud,
# @param dia_sete_sobras_stella,
# @param dia_sete_v1,
# @param dia_sete_v2,
# @param dia_sete_v3,
# @param dia_sete_quantidade_bud_v1,
# @param dia_sete_quantidade_bud_v2,
# @param dia_sete_quantidade_bud_v3,
# @param dia_sete_quantidade_stella_v1,
# @param dia_sete_quantidade_stella_v2,
# @param dia_sete_quantidade_stella_v3,
# @param dia_sete_sera_fds,
# @param dia_sete_sera_normal,
# @param dia_sete_procura_bud,
# @param dia_sete_procura_stella,
# @param dia_sete_procura_real_bud,
# @param dia_sete_procura_real_stella
# @returns custos totais
custo_total_calculator <- function(
 dia_um_sobras_bud,
 dia_um_sobras_stella,
 dia_um_v1,
 dia_um_v2,
 dia_um_v3,
 dia_um_quantidade_bud_v1,
 dia_um_quantidade_bud_v2,
 dia_um_quantidade_bud_v3,
 dia_um_quantidade_stella_v1,
 dia_um_quantidade_stella_v2,
 dia_um_quantidade_stella_v3,
 dia_um_sera_fds,
 dia_um_sera_normal,
 dia_um_procura_bud,
 dia_um_procura_stella,
 dia_um_procura_real_bud,
 dia_um_procura_real_stella,
 dia_dois_sobras_bud,
 dia_dois_sobras_stella,
 dia_dois_v1,
 dia_dois_v2,
 dia_dois_v3,
 dia_dois_quantidade_bud_v1,
 dia_dois_quantidade_bud_v2,
 dia_dois_quantidade_bud_v3,
 dia_dois_quantidade_stella_v1,
 dia_dois_quantidade_stella_v2,
 dia_dois_quantidade_stella_v3,
 dia_dois_sera_fds,
 dia_dois_sera_normal,
 dia_dois_procura_bud,
 dia_dois_procura_stella,
 dia_dois_procura_real_bud,
 dia_dois_procura_real_stella,
 dia_tres_sobras_bud,
 dia_tres_sobras_stella,
 dia_tres_v1,
 dia_tres_v2,
 dia_tres_v3,
 dia_tres_quantidade_bud_v1,
 dia_tres_quantidade_bud_v2,
 dia_tres_quantidade_bud_v3,
 dia_tres_quantidade_stella_v1,
 dia_tres_quantidade_stella_v2,
 dia_tres_quantidade_stella_v3,
 dia_tres_sera_fds,
 dia_tres_sera_normal,
 dia_tres_procura_bud,
 dia_tres_procura_stella,
 dia_tres_procura_real_bud,
 dia_tres_procura_real_stella,
 dia_quatro_sobras_bud,
 dia_quatro_sobras_stella,
 dia_quatro_v1,
 dia_quatro_v2,
 dia_quatro_v3,
 dia_quatro_quantidade_bud_v1,
 dia_quatro_quantidade_bud_v2,
 dia_quatro_quantidade_bud_v3,
 dia_quatro_quantidade_stella_v1,
 dia_quatro_quantidade_stella_v2,
 dia_quatro_quantidade_stella_v3,
 dia_quatro_sera_fds,
 dia_quatro_sera_normal,
 dia_quatro_procura_bud,
 dia_quatro_procura_stella,
 dia_quatro_procura_real_bud,
 dia_quatro_procura_real_stella,
 dia_cinco_sobras_bud,
 dia_cinco_sobras_stella,
 dia_cinco_v1,
 dia_cinco_v2,
 dia_cinco_v3,
 dia_cinco_quantidade_bud_v1,
 dia_cinco_quantidade_bud_v2,
 dia_cinco_quantidade_bud_v3,
 dia_cinco_quantidade_stella_v1,
 dia_cinco_quantidade_stella_v2,
 dia_cinco_quantidade_stella_v3,
 dia_cinco_sera_fds,
 dia_cinco_sera_normal,
 dia_cinco_procura_bud,
 dia_cinco_procura_stella,
 dia_cinco_procura_real_bud,
 dia_cinco_procura_real_stella,
 dia_seis_sobras_bud,
 dia_seis_sobras_stella,
 dia_seis_v1,
 dia_seis_v2,
 dia_seis_v3,
 dia_seis_quantidade_bud_v1,
 dia_seis_quantidade_bud_v2,
 dia_seis_quantidade_bud_v3,
 dia_seis_quantidade_stella_v1,
 dia_seis_quantidade_stella_v2,
 dia_seis_quantidade_stella_v3,
 dia_seis_sera_fds,
 dia_seis_sera_normal,
 dia_seis_procura_bud,
 dia_seis_procura_stella,
 dia_seis_procura_real_bud,
 dia_seis_procura_real_stella,
 dia_sete_sobras_bud,
 dia_sete_sobras_stella,
 dia_sete_v1,
 dia_sete_v2,
 dia_sete_v3,
 dia_sete_quantidade_bud_v1,
 dia_sete_quantidade_bud_v2,
 dia_sete_quantidade_bud_v3,
 dia_sete_quantidade_stella_v1,
 dia_sete_quantidade_stella_v2,
 dia_sete_quantidade_stella_v3,
 dia_sete_sera_fds,
 dia_sete_sera_normal,
 dia_sete_procura_bud,
 dia_sete_procura_stella,
 dia_sete_procura_real_bud,
 dia_sete_procura_real_stella
){
    #CUSTOS FIXOS
    custo_v1 <- 60;
    custo_v2 <- 90;
    custo_v3 <- 120;
    quantidade_lote_retalhista <- 72;
    custo_stock_em_loja <- 1;
    custo_retalhista_normal <- 10;
    custo_retalhista_fds <- 15;
    #DIA 1
    dia_um_sobras_bud <- 0;
    dia_um_sobras_stella <- 0;
    dia_um_procura_real_bud <- dia_um_procura_bud - dia_um_sobras_bud;
    dia_um_procura_real_stella <- dia_um_procura_stella - dia_um_sobras_stella;
    dia_um_v1 <- quantidade_transporte_calculator(dia_um_quantidade_bud_v1,dia_um_quantidade_stella_v1,custo_v1);
    dia_um_v2 <- quantidade_transporte_calculator(dia_um_quantidade_bud_v2,dia_um_quantidade_stella_v2,custo_v2);
    dia_um_v3 <- quantidade_transporte_calculator(dia_um_quantidade_bud_v3,dia_um_quantidade_stella_v3,custo_v3);
    dia_um_retalhista <- quantidade_retalhista_calculator(
        dia_um_quantidade_bud_v1,dia_um_quantidade_bud_v2,dia_um_quantidade_bud_v3,
        dia_um_quantidade_stella_v1,dia_um_quantidade_stella_v2,dia_um_quantidade_stella_v3,
        quantidade_lote_retalhista
    );
    #DIA 2
    dia_dois_procura_real_bud <- dia_dois_procura_bud - dia_um_sobras_bud;
    dia_dois_procura_real_stella <- dia_dois_procura_stella - dia_um_sobras_stella;
    dia_dois_sobras_bud <- sobras_loja_calculator(
        dia_dois_quantidade_bud_v1,
        dia_dois_quantidade_bud_v2,
        dia_dois_quantidade_bud_v3,
        dia_dois_procura_real_bud
        );
    dia_dois_sobras_stella <- sobras_loja_calculator(
        dia_dois_quantidade_stella_v1,
        dia_dois_quantidade_stella_v2,
        dia_dois_quantidade_stella_v3,
        dia_dois_procura_real_stella
    );
    dia_dois_v1 <- quantidade_transporte_calculator(dia_dois_quantidade_bud_v1,dia_dois_quantidade_stella_v1,custo_v1);
    dia_dois_v2 <- quantidade_transporte_calculator(dia_dois_quantidade_bud_v2,dia_dois_quantidade_stella_v2,custo_v2);
    dia_dois_v3 <- quantidade_transporte_calculator(dia_dois_quantidade_bud_v3,dia_dois_quantidade_stella_v3,custo_v3);
    dia_dois_retalhista <- quantidade_retalhista_calculator(
    dia_dois_quantidade_bud_v1,dia_dois_quantidade_bud_v2,dia_dois_quantidade_bud_v3,
    dia_dois_quantidade_stella_v1,dia_dois_quantidade_stella_v2,dia_dois_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #DIA 3
    dia_tres_procura_real_bud <- dia_tres_procura_bud - dia_dois_sobras_bud;
    dia_tres_procura_real_stella <- dia_tres_procura_stella - dia_dois_sobras_stella;
    dia_tres_sobras_bud <- sobras_loja_calculator(
        dia_tres_quantidade_bud_v1,
        dia_tres_quantidade_bud_v2,
        dia_tres_quantidade_bud_v3,
        dia_tres_procura_real_bud
        );
    dia_tres_sobras_stella <- sobras_loja_calculator(
        dia_tres_quantidade_stella_v1,
        dia_tres_quantidade_stella_v2,
        dia_tres_quantidade_stella_v3,
        dia_tres_procura_real_stella
    );
    dia_tres_v1 <- quantidade_transporte_calculator(dia_tres_quantidade_bud_v1,dia_tres_quantidade_stella_v1,custo_v1);
    dia_tres_v2 <- quantidade_transporte_calculator(dia_tres_quantidade_bud_v2,dia_tres_quantidade_stella_v2,custo_v2);
    dia_tres_v3 <- quantidade_transporte_calculator(dia_tres_quantidade_bud_v3,dia_tres_quantidade_stella_v3,custo_v3);
    dia_tres_retalhista <- quantidade_retalhista_calculator(
    dia_tres_quantidade_bud_v1,dia_tres_quantidade_bud_v2,dia_tres_quantidade_bud_v3,
    dia_tres_quantidade_stella_v1,dia_tres_quantidade_stella_v2,dia_tres_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #DIA 4
    dia_quatro_procura_real_bud <- dia_quatro_procura_bud - dia_tres_sobras_bud;
    dia_quatro_procura_real_stella <- dia_quatro_procura_stella - dia_tres_sobras_stella;
    dia_quatro_sobras_bud <- sobras_loja_calculator(
        dia_quatro_quantidade_bud_v1,
        dia_quatro_quantidade_bud_v2,
        dia_quatro_quantidade_bud_v3,
        dia_quatro_procura_real_bud
        );
    dia_quatro_sobras_stella <- sobras_loja_calculator(
        dia_quatro_quantidade_stella_v1,
        dia_quatro_quantidade_stella_v2,
        dia_quatro_quantidade_stella_v3,
        dia_quatro_procura_real_stella
    );
    dia_quatro_v1 <- quantidade_transporte_calculator(dia_quatro_quantidade_bud_v1,dia_quatro_quantidade_stella_v1,custo_v1);
    dia_quatro_v2 <- quantidade_transporte_calculator(dia_quatro_quantidade_bud_v2,dia_quatro_quantidade_stella_v2,custo_v2);
    dia_quatro_v3 <- quantidade_transporte_calculator(dia_quatro_quantidade_bud_v3,dia_quatro_quantidade_stella_v3,custo_v3);
    dia_quatro_retalhista <- quantidade_retalhista_calculator(
    dia_quatro_quantidade_bud_v1,dia_quatro_quantidade_bud_v2,dia_quatro_quantidade_bud_v3,
    dia_quatro_quantidade_stella_v1,dia_quatro_quantidade_stella_v2,dia_quatro_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #DIA 5
    dia_cinco_procura_real_bud <- dia_cinco_procura_bud - dia_quatro_sobras_bud;
    dia_cinco_procura_real_stella <- dia_cinco_procura_stella - dia_quatro_sobras_stella;
    dia_cinco_sobras_bud <- sobras_loja_calculator(
        dia_cinco_quantidade_bud_v1,
        dia_cinco_quantidade_bud_v2,
        dia_cinco_quantidade_bud_v3,
        dia_cinco_procura_real_bud
        );
    dia_cinco_sobras_stella <- sobras_loja_calculator(
        dia_cinco_quantidade_stella_v1,
        dia_cinco_quantidade_stella_v2,
        dia_cinco_quantidade_stella_v3,
        dia_cinco_procura_real_stella
    );
    dia_cinco_v1 <- quantidade_transporte_calculator(dia_cinco_quantidade_bud_v1,dia_cinco_quantidade_stella_v1,custo_v1);
    dia_cinco_v2 <- quantidade_transporte_calculator(dia_cinco_quantidade_bud_v2,dia_cinco_quantidade_stella_v2,custo_v2);
    dia_cinco_v3 <- quantidade_transporte_calculator(dia_cinco_quantidade_bud_v3,dia_cinco_quantidade_stella_v3,custo_v3);
    dia_cinco_retalhista <- quantidade_retalhista_calculator(
    dia_cinco_quantidade_bud_v1,dia_cinco_quantidade_bud_v2,dia_cinco_quantidade_bud_v3,
    dia_cinco_quantidade_stella_v1,dia_cinco_quantidade_stella_v2,dia_cinco_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #DIA 6
    dia_seis_procura_real_bud <- dia_seis_procura_bud - dia_cinco_sobras_bud;
    dia_seis_procura_real_stella <- dia_seis_procura_stella - dia_cinco_sobras_stella;
    dia_seis_sobras_bud <- sobras_loja_calculator(
        dia_seis_quantidade_bud_v1,
        dia_seis_quantidade_bud_v2,
        dia_seis_quantidade_bud_v3,
        dia_seis_procura_real_bud
        );
    dia_seis_sobras_stella <- sobras_loja_calculator(
        dia_seis_quantidade_stella_v1,
        dia_seis_quantidade_stella_v2,
        dia_seis_quantidade_stella_v3,
        dia_seis_procura_real_stella
    );
    dia_seis_v1 <- quantidade_transporte_calculator(dia_seis_quantidade_bud_v1,dia_seis_quantidade_stella_v1,custo_v1);
    dia_seis_v2 <- quantidade_transporte_calculator(dia_seis_quantidade_bud_v2,dia_seis_quantidade_stella_v2,custo_v2);
    dia_seis_v3 <- quantidade_transporte_calculator(dia_seis_quantidade_bud_v3,dia_seis_quantidade_stella_v3,custo_v3);
    dia_seis_retalhista <- quantidade_retalhista_calculator(
    dia_seis_quantidade_bud_v1,dia_seis_quantidade_bud_v2,dia_seis_quantidade_bud_v3,
    dia_seis_quantidade_stella_v1,dia_seis_quantidade_stella_v2,dia_seis_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #DIA 7
    dia_sete_procura_real_bud <- dia_sete_procura_bud - dia_seis_sobras_bud;
    dia_sete_procura_real_stella <- dia_sete_procura_stella - dia_seis_sobras_stella;
    dia_sete_sobras_bud <- sobras_loja_calculator(
        dia_sete_quantidade_bud_v1,
        dia_sete_quantidade_bud_v2,
        dia_sete_quantidade_bud_v3,
        dia_sete_procura_real_bud
        );
    dia_sete_sobras_stella <- sobras_loja_calculator(
        dia_sete_quantidade_stella_v1,
        dia_sete_quantidade_stella_v2,
        dia_sete_quantidade_stella_v3,
        dia_sete_procura_real_stella
    );
    dia_sete_v1 <- quantidade_transporte_calculator(dia_sete_quantidade_bud_v1,dia_sete_quantidade_stella_v1,custo_v1);
    dia_sete_v2 <- quantidade_transporte_calculator(dia_sete_quantidade_bud_v2,dia_sete_quantidade_stella_v2,custo_v2);
    dia_sete_v3 <- quantidade_transporte_calculator(dia_sete_quantidade_bud_v3,dia_sete_quantidade_stella_v3,custo_v3);
    dia_sete_retalhista <- quantidade_retalhista_calculator(
    dia_sete_quantidade_bud_v1,dia_sete_quantidade_bud_v2,dia_sete_quantidade_bud_v3,
    dia_sete_quantidade_stella_v1,dia_sete_quantidade_stella_v2,dia_sete_quantidade_stella_v3,
    quantidade_lote_retalhista
    );
    #CALCULO DOS CUSTOS TOTAIS
    #DIA 1
    dia_um_custo_total <- custo_dia_calculator(
    dia_um_v1,
    dia_um_v2,
    dia_um_v3,
    dia_um_retalhista,
    dia_um_sobras_bud,
    dia_um_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_um_sera_fds,
    dia_um_sera_normal,
    custo_stock_em_loja
    );
    #DIA 2
    dia_dois_custo_total <- custo_dia_calculator(
    dia_dois_v1,
    dia_dois_v2,
    dia_dois_v3,
    dia_dois_retalhista,
    dia_dois_sobras_bud,
    dia_dois_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_dois_sera_fds,
    dia_dois_sera_normal,
    custo_stock_em_loja
    );
    #DIA 3
    dia_tres_custo_total <- custo_dia_calculator(
    dia_tres_v1,
    dia_tres_v2,
    dia_tres_v3,
    dia_tres_retalhista,
    dia_tres_sobras_bud,
    dia_tres_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_tres_sera_fds,
    dia_tres_sera_normal,
    custo_stock_em_loja
    );
    #DIA 4
    dia_quatro_custo_total <- custo_dia_calculator(
    dia_quatro_v1,
    dia_quatro_v2,
    dia_quatro_v3,
    dia_quatro_retalhista,
    dia_quatro_sobras_bud,
    dia_quatro_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_quatro_sera_fds,
    dia_quatro_sera_normal,
    custo_stock_em_loja
    );
    #DIA 5
    dia_cinco_custo_total <- custo_dia_calculator(
    dia_cinco_v1,
    dia_cinco_v2,
    dia_cinco_v3,
    dia_cinco_retalhista,
    dia_cinco_sobras_bud,
    dia_cinco_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_cinco_sera_fds,
    dia_cinco_sera_normal,
    custo_stock_em_loja
    );
    #DIA 6
    dia_seis_custo_total <- custo_dia_calculator(
    dia_seis_v1,
    dia_seis_v2,
    dia_seis_v3,
    dia_seis_retalhista,
    dia_seis_sobras_bud,
    dia_seis_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_seis_sera_fds,
    dia_seis_sera_normal,
    custo_stock_em_loja
    );
    #DIA 7
    dia_sete_custo_total <- custo_dia_calculator(
    dia_sete_v1,
    dia_sete_v2,
    dia_sete_v3,
    dia_sete_retalhista,
    dia_sete_sobras_bud,
    dia_sete_sobras_stella,
    custo_v1,
    custo_v2,
    custo_v2,
    custo_retalhista_fds,
    custo_retalhista_normal,
    dia_sete_sera_fds,
    dia_sete_sera_normal,
    custo_stock_em_loja
    );
    #SOMA DOS CUSTOS
    soma <- dia_um_custo_total+dia_dois_custo_total+dia_tres_custo_total+
    dia_quatro_custo_total+dia_cinco_custo_total+dia_seis_custo_total+
    dia_sete_custo_total;
    return(soma);
}