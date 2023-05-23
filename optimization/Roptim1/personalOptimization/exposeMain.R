source("mainMax.R");
source("mainMin.R");
source("back.R");
source("../../../Code/Split.R");
#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}
#* @get /opt-sann
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_sann <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_sann(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-hill
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_hill <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_hill(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-mc
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_mc <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_mc(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-grid
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_grid <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_grid(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-sann-min
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_sann_min <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_sann_min(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-hill-min
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_hill_min <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_hill_min(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-mc-min
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_mc_min <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_mc_min(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /opt-grid-min
#* @param vetorProcura
#* @param vetorSeraFds
#* @param lower
#* @param upper
#* @param numero_iteracoes
opt_grid_min <- function(
vetorProcura,
vetorSeraFds,
lower,
upper,
numero_iteracoes
){
    vetorProcura <- as.numeric(strsplit(vetorProcura, ",")[[1]]);
    vetorSeraFds <- as.numeric(strsplit(vetorSeraFds,",")[[1]]);
    lower <- as.numeric(lower);
    upper <- as.numeric(upper);
    lower <- rep(lower,length(vetorSeraFds)* 6);
    upper <- rep(upper,length(vetorSeraFds)* 6);
    numero_iteracoes <- as.numeric(numero_iteracoes);
    result <- main_grid_min(vetorProcura,vetorSeraFds,lower,upper,numero_iteracoes);
    return(result);
}

#* @get /split
#* @param week
#* @param bud_model
#* @param stella_model
split <- function(
    week,
    bud_model,
    stella_model
){
    print(week);
    print(bud_model);
    print(stella_model);
    week <- as.numeric(week);
    result <- model(week = week, bud_model=bud_model, stella_model=stella_model);
    return(result);
}

#* @get /inferir
#* @param vetor
#* @param procura
split <- function(
vetor,
procura
){
    vetor <- as.numeric(strsplit(vetor, ",")[[1]]);
    procura <- as.numeric(strsplit(procura, ",")[[1]]);
    result <- inferir_pelo_otimo(vetor,procura);
    return(result);
}

