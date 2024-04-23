func_sinasc_order_perc_inv <- function(df_sinasc_biv_wider,
                                       labels_var1, labels_var2, var1, var2,
                                       var1_name, var2_name,
                                       input_tabela_c_invalido){
  # browser()
  ## Recebendo numa nova tabela os dados no formato wide
  tabela_bivariada <- df_sinasc_biv_wider

  ## Ordenando colunas da segunda variável
  new_order <- c(var1, labels_var2$label)
  new_order <- c(new_order[new_order != "Inválido ou nulo"], "Total")

  ## Removendo colunas que não aparecem (por não terem dados)
  # browser()
  # if(length(new_order) != ncol(tabela_bivariada)){
  #   new_oder <- new_order[new_order %in% colnames(tabela_bivariada)]
  # }

  ## Reordenando e organizando a ordem
  tabela_bivariada <- tabela_bivariada |>
    dplyr::select(!!new_order) |>
    dplyr::arrange(factor(!!as.name(var1), levels = labels_var1$label))

  #### Calculando percentuais ----
  tabela_bivariada_perc <- tabela_bivariada |>
    mutate(across(-CONSULTAS, ~ round(.x / Total, 4), .names = "{.col}_%"))

  ## Reordenando vetor, com novas colunas de percentual
  cols_with_perc <- colnames(tabela_bivariada_perc)
  new_order_perc <- new_order[1]
  final_col_names <- var1_name

  ## Criando vetor de ordenação e nome das colunas com percentual
  for(i in 2:length(new_order)){
    new_order_perc <- c(new_order_perc, paste0(cols_with_perc[i]),
                        cols_with_perc[i+length(new_order)-1])
    final_col_names <- c(final_col_names, paste0(cols_with_perc[i], "_N"),
                         cols_with_perc[i+length(new_order)-1])
  }

  tabela_bivariada_perc <- tabela_bivariada_perc |>
    dplyr::select(!!new_order_perc)

  tab_biv_final <- tabela_bivariada_perc
  colnames(tab_biv_final) <- final_col_names

  ## Adicionando coluna de inválido, no caso de tabela com inválido
  if(input_tabela_c_invalido){
    tab_biv_final <- bind_cols(tab_biv_final, df_sinasc_biv_col_invalid)
  }

  tab_biv_final
}
