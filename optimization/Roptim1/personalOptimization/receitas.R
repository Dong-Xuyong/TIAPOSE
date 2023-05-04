#VENDA UNICA
# @param quantidade, quantidade de garrafas
# @param preco, the price associated to that quantity
venda_unica_calculator <- function(quantidade,preco){
    result <- quantidade * preco;
    return(result);
}
# VENDA TOTAL
# @param quantidade_bud_v1, quantidade de bud v1 para um certo dia
# @param quantidade_bud_v2, quantidade de bud v2 para um certo dia
# @param quantidade_bud_v3, quantidade de bud v3 para um certo dia
# @param quantidade_stella_v1, quantidade de stella v1 para um certo dia
# @param quantidade_stella_v2, quantidade de stella v2 para um certo dia
# @param quantidade_stella_v3, quantidade de stella v3 para um certo dia
# @param preco_bud, preco da cerveja bud
# @param preco_stella, preco da cerveja stella
venda_total_calculator <- function(
quantidade_bud_v1,
quantidade_bud_v2,
quantidade_bud_v3,
quantidade_stella_v1,
quantidade_stella_v2,
quantidade_stella_v3,
preco_bud,
preco_stella
){
#RESULT
result <- venda_unica_calculator(quantidade_bud_v1,preco_bud)+
venda_unica_calculator(quantidade_bud_v2,preco_bud)+
venda_unica_calculator(quantidade_bud_v3,preco_bud)+
venda_unica_calculator(quantidade_stella_v1,preco_stella)+
venda_unica_calculator(quantidade_stella_v2,preco_stella)+
venda_unica_calculator(quantidade_stella_v3,preco_stella);
return(result);
}
#VENDA GLOBAL
# @param dia_um_quantidade_bud_v1,
# @param dia_um_quantidade_bud_v2,
# @param dia_um_quantidade_bud_v3,
# @param dia_um_quantidade_stella_v1,
# @param dia_um_quantidade_stella_v2,
# @param dia_um_quantidade_stella_v3,
# @param dia_dois_quantidade_bud_v1,
# @param dia_dois_quantidade_bud_v2,
# @param dia_dois_quantidade_bud_v3,
# @param dia_dois_quantidade_stella_v1,
# @param dia_dois_quantidade_stella_v2,
# @param dia_dois_quantidade_stella_v3,
# @param dia_tres_quantidade_bud_v1,
# @param dia_tres_quantidade_bud_v2,
# @param dia_tres_quantidade_bud_v3,
# @param dia_tres_quantidade_stella_v1,
# @param dia_tres_quantidade_stella_v2,
# @param dia_tres_quantidade_stella_v3,
# @param dia_quatro_quantidade_bud_v1,
# @param dia_quatro_quantidade_bud_v2,
# @param dia_quatro_quantidade_bud_v3,
# @param dia_quatro_quantidade_stella_v1,
# @param dia_quatro_quantidade_stella_v2,
# @param dia_quatro_quantidade_stella_v3,
# @param dia_cinco_quantidade_bud_v1,
# @param dia_cinco_quantidade_bud_v2,
# @param dia_cinco_quantidade_bud_v3,
# @param dia_cinco_quantidade_stella_v1,
# @param dia_cinco_quantidade_stella_v2,
# @param dia_cinco_quantidade_stella_v3,
# @param dia_seis_quantidade_bud_v1,
# @param dia_seis_quantidade_bud_v2,
# @param dia_seis_quantidade_bud_v3,
# @param dia_seis_quantidade_stella_v1,
# @param dia_seis_quantidade_stella_v2,
# @param dia_seis_quantidade_stella_v3,
# @param dia_sete_quantidade_bud_v1,
# @param dia_sete_quantidade_bud_v2,
# @param dia_sete_quantidade_bud_v3,
# @param dia_sete_quantidade_stella_v1,
# @param dia_sete_quantidade_stella_v2,
# @param dia_sete_quantidade_stella_v3,
# @returns Vendas de um dia inteiro
venda_global_calculator <- function(
dia_um_quantidade_bud_v1,
dia_um_quantidade_bud_v2,
dia_um_quantidade_bud_v3,
dia_um_quantidade_stella_v1,
dia_um_quantidade_stella_v2,
dia_um_quantidade_stella_v3,
dia_dois_quantidade_bud_v1,
dia_dois_quantidade_bud_v2,
dia_dois_quantidade_bud_v3,
dia_dois_quantidade_stella_v1,
dia_dois_quantidade_stella_v2,
dia_dois_quantidade_stella_v3,
dia_tres_quantidade_bud_v1,
dia_tres_quantidade_bud_v2,
dia_tres_quantidade_bud_v3,
dia_tres_quantidade_stella_v1,
dia_tres_quantidade_stella_v2,
dia_tres_quantidade_stella_v3,
dia_quatro_quantidade_bud_v1,
dia_quatro_quantidade_bud_v2,
dia_quatro_quantidade_bud_v3,
dia_quatro_quantidade_stella_v1,
dia_quatro_quantidade_stella_v2,
dia_quatro_quantidade_stella_v3,
dia_cinco_quantidade_bud_v1,
dia_cinco_quantidade_bud_v2,
dia_cinco_quantidade_bud_v3,
dia_cinco_quantidade_stella_v1,
dia_cinco_quantidade_stella_v2,
dia_cinco_quantidade_stella_v3,
dia_seis_quantidade_bud_v1,
dia_seis_quantidade_bud_v2,
dia_seis_quantidade_bud_v3,
dia_seis_quantidade_stella_v1,
dia_seis_quantidade_stella_v2,
dia_seis_quantidade_stella_v3,
dia_sete_quantidade_bud_v1,
dia_sete_quantidade_bud_v2,
dia_sete_quantidade_bud_v3,
dia_sete_quantidade_stella_v1,
dia_sete_quantidade_stella_v2,
dia_sete_quantidade_stella_v3
){
#CUSTOS FIXOS
preco_bud <- 4.4;
preco_stella <- 5.7;
#RESULTADO
result <- venda_total_calculator(dia_um_quantidade_bud_v1,dia_um_quantidade_bud_v2,dia_um_quantidade_bud_v3,dia_um_quantidade_stella_v1,dia_um_quantidade_stella_v2,dia_um_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_dois_quantidade_bud_v1,dia_dois_quantidade_bud_v2,dia_dois_quantidade_bud_v3,dia_dois_quantidade_stella_v1,dia_dois_quantidade_stella_v2,dia_dois_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_tres_quantidade_bud_v1,dia_tres_quantidade_bud_v2,dia_tres_quantidade_bud_v3,dia_tres_quantidade_stella_v1,dia_tres_quantidade_stella_v2,dia_tres_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_quatro_quantidade_bud_v1,dia_quatro_quantidade_bud_v2,dia_quatro_quantidade_bud_v3,dia_quatro_quantidade_stella_v1,dia_quatro_quantidade_stella_v2,dia_quatro_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_cinco_quantidade_bud_v1,dia_cinco_quantidade_bud_v2,dia_cinco_quantidade_bud_v3,dia_cinco_quantidade_stella_v1,dia_cinco_quantidade_stella_v2,dia_cinco_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_seis_quantidade_bud_v1,dia_seis_quantidade_bud_v2,dia_seis_quantidade_bud_v3,dia_seis_quantidade_stella_v1,dia_seis_quantidade_stella_v2,dia_seis_quantidade_stella_v3,preco_bud,preco_stella)+
venda_total_calculator(dia_sete_quantidade_bud_v1,dia_sete_quantidade_bud_v2,dia_sete_quantidade_bud_v3,dia_sete_quantidade_stella_v1,dia_sete_quantidade_stella_v2,dia_sete_quantidade_stella_v3,preco_bud,preco_stella);
return(result);
}