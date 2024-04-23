func_sinasc_univ <- function(df_sinasc_filt, df_ufs, df_mun_reg_saude,
                             var1, var1_name, labels_var1,
                             list_inputs){

  # browser()

  ## Organizando inputs recebidos
  input_year <- list_inputs[[1]]
  input_abrang <- list_inputs[[2]]
  input_uf <- list_inputs[[3]]
  input_reg <- list_inputs[[4]]
  input_mrs <- list_inputs[[5]]
  input_rs <- list_inputs[[6]]
  input_mun <- list_inputs[[7]]


  ## Adicionando dados das UFs
  if(input_abrang == "Brasil" || input_abrang == "Região"){
    ## Variável usada para o agrupamento das contagens
    var_group_1 <- "uf_nome"
    var_group_2 <- "uf_reg"
    ## Filtro inicial do ano selecionado
    df_sinasc_ufs_var <- df_sinasc |>
      dplyr::filter(`_ANONASC` == input_year) |>
      # dplyr::filter(RACACOR != "0" && RACACOR != "") |>
      dplyr::group_by(`_UF`, `_ANONASC`, !!as.name(var1)) |>
      dplyr::summarise(count = n()) |>
      dplyr::select(uf = `_UF`, ano_nasc = `_ANONASC`, !!as.name(var1), count) |>
      dplyr::ungroup() |>
      dplyr::collect()


    df_sinasc_var <- dplyr::left_join(df_sinasc_ufs_var, df_ufs, by = c("uf" = "uf_sigla")) |>
        dplyr::select(-uf_cod)
  }
  if(input_abrang == "Município" ||
     input_abrang == "Macroregião de saúde" || input_abrang == "Região de saúde"){
    browser()
    ## Variável usada para o agrupamento das contagens
    var_group_1 <- "mun_nome"
    var_group_2 <- "uf_nome"

    df_sinasc_filt$`_CODMUNRES` <- as.integer(df_sinasc_filt$`_CODMUNRES`)
    ## Unindo com dados de região de saúde para filtrar
    df_sinasc_reg_saud <- dplyr::inner_join(df_sinasc_filt, df_mun_reg_saude, by = c("_CODMUNRES" = "cod_ibge"))

    df_sinasc_rs_var <- df_sinasc_reg_saud |>
      dplyr::group_by(`_CODMUNRES`, `_ANONASC`, !!as.name(var1)) |>
      dplyr::summarise(count = n()) |>
      dplyr::select(cod_mun = `_CODMUNRES`, ano_nasc = `_ANONASC`, !!as.name(var1), count) |>
      dplyr::ungroup() |>
      dplyr::collect()

    df_sinasc_var <- dplyr::left_join(df_sinasc_rs_var, df_mun_reg_saude, by = c("cod_mun" = "cod_ibge")) |>
      dplyr::select(mun_nome = nome_mun, uf_nome = nome_uf, ano_nasc, !!as.name(var1), count)
  }


  df_sinasc_var <- dplyr::left_join(df_sinasc_var, labels_var1) |>
    dplyr::select(-!!as.name(var1)) |>
    dplyr::rename(!!var1 := label)


  ### Tratando inválidos ----
  df_sinasc_ufs_var_inv <- df_sinasc_var |>
    dplyr::mutate(categoria = ifelse(is.na(!!as.name(var1)), "Inválido ou nulo", !!as.name(var1))) |>
    dplyr::group_by(!!var_group_1, categoria) |>
    dplyr::mutate(count = sum(count)) |>
    dplyr::ungroup() |>
    dplyr::distinct(!!var_group_1, categoria, .keep_all = TRUE) |>
    dplyr::select(-!!as.name(var1)) |>
    dplyr::select(!!as.name(var_group_1), !!var_group_2, categoria, count)

  ## Tornando a tabela Wide
  df_sinasc_ufs_var_wide <- df_sinasc_ufs_var_inv |>
    tidyr::pivot_wider(names_from = categoria, values_from = count)

  ## Ordenando
  new_order <- c(var_group_1, var_group_2, unique(labels_var1$label))
  new_order_w_inv <- new_order[new_order != "Inválido ou nulo"]
  ## Adicionando coluna de total
  df_sinasc_ufs_var_wide <- df_sinasc_ufs_var_wide |>
    dplyr::mutate(Total = rowSums(across(where(is.numeric))) - `Inválido ou nulo`)

  ## Ordenando (Precisa ter todas as colunas para a ordenação funcionar)
  if(ncol(df_sinasc_ufs_var_wide) > 4){
    new_order <- new_order[new_order %in% colnames(df_sinasc_ufs_var_wide)]

    tabela_univariada <- df_sinasc_ufs_var_wide |>
      dplyr::select(all_of(new_order_w_inv), Total)

    df_invalid_col <- df_sinasc_ufs_var_wide |>
      dplyr::select(uf_nome, `Inválido ou nulo`)

    ## Calculando percentuais ----
    tabela_univariada_perc <- tabela_univariada |>
      dplyr::mutate(across(-all_of(c("uf_nome", "uf_reg")), ~ round(.x / Total, 4), .names = "{.col}_%"))

    ## Reordenando vetor, com novas colunas de percentual
    cols_with_perc <- colnames(tabela_univariada_perc)
    new_order_perc <- new_order[1:2]
    final_col_names <- c("UF", "Região")
    cols_jump <- length(new_order) - length(new_order_perc)

    ## Criando vetor de ordenação e nome das colunas com percentual
    for(i in 3:length(new_order)){
      # i <- 3
      new_order_perc <- c(new_order_perc, paste0(cols_with_perc[i]),
                          cols_with_perc[i+cols_jump])
      final_col_names <- c(final_col_names, paste0(cols_with_perc[i], "_N"),
                           cols_with_perc[i+cols_jump])
    }

    ## Ordenando colunas
    tabela_univariada_perc <- tabela_univariada_perc |>
      dplyr::select(!!new_order_perc)

    tab_biv_final <- tabela_univariada_perc

    colnames(tab_biv_final) <- final_col_names

    ## Ordenando linhas
    if(input_abrang == "Brasil"){
      tab_biv_final <- tab_biv_final |>
        dplyr::arrange(`Região`, UF)
    }
    tab_valid <- T
  }else{
    ## Senão tiver todas as colunas, devolver vazio e um aviso para dados faltantes na seleção
    tab_biv_final <- NULL
    tab_valid <- F
  }

  return(list(tab_biv_final, tab_valid))
}
