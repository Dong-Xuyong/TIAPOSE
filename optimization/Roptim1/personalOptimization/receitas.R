#VENDA UNICA
# @param quantidade, quantidade de garrafas
# @param preco, the price associated to that quantity
venda_unica_calculator <- function(quantidade,preco){
    result <- quantidade * preco;
    return(result);
}
# VENDA TOTAL
# @param vendas_bud, vendas de bud
# @param vendas_stella, vendas de stella
# @param preco_bud, preco da cerveja bud
# @param preco_stella, preco da cerveja stella
venda_total_calculator <- function(
vendas_bud,
vendas_stella,
preco_bud,
preco_stella
){
#RESULT
result <- venda_unica_calculator(vendas_bud,preco_bud)+venda_unica_calculator(vendas_stella,preco_stella);
return(result);
}