func_sinasc_biv <- function(df_sinasc_filt,
                            labels_var1, labels_var2, var1, var2,
                            var1_name, var2_name,
                            input_tabela_c_invalido){
  ## Definindo variáveis selecionadas


  ## Separando dados por filtros selecionadas
  # df_sinasc_biv <- df_sinasc_biv()

  # browser()

  ## Selecionando variáveis e criando dados de contagem por variáveis selecionadas
  df_sinasc_biv_vars_filt <- df_sinasc_filt |>
    dplyr::select(!as.name(var1), !as.name(var2)) |>
    dplyr::group_by(!!as.name(var1), !!as.name(var2)) |>
    dplyr::summarise(count = n()) |>
    dplyr::ungroup() |>
    dplyr::collect()

  ## Adicionando labels
  df_sinasc_biv_vars_filt_lab <- df_sinasc_biv_vars_filt |>
    dplyr::left_join(labels_var1) |>
    dplyr::select(-!!as.name(var1), label) |>
    # dplyr::mutate(CONSULTAS = ifelse(is.na(CONSULTAS), "99", CONSULTAS))
    dplyr::mutate(label = ifelse(is.na(label), "Inválido ou nulo", label)) |>
    dplyr::rename(!!var1 := label) |>
    dplyr::left_join(labels_var2) |>
    dplyr::mutate(label = ifelse(is.na(label), "Inválido ou nulo", label)) |>
    dplyr::select(-!!var2, var2 = label) |>
    dplyr::select(!!as.name(var1), var2, count) |>
    dplyr::group_by(!!as.name(var1), var2) |>
    dplyr::summarise(count = sum(count)) |>
    dplyr::ungroup()

  ### Com inválido na tabela e soma de totais (com inválido) ----
  if(input_tabela_c_invalido){
    ## Não utilizar mais a linha de total
    # df_sinasc_biv_vars_filt_lab_total <- df_sinasc_biv_vars_filt_lab |>
    #   dplyr::group_by(var2) |>
    #   dplyr::summarise(count = sum(count)) |>
    #   dplyr::ungroup() |>
    #   dplyr::mutate(!!as.name(var1) := "Total") |>
    #   dplyr::select(!!as.name(var1), var2, count) |>
    #   dplyr::bind_rows(df_sinasc_biv_vars_filt_lab)

    ## Removendo apenas a linha de inválido ou nulo
    df_sinasc_biv_vars_filt_lab <- df_sinasc_biv_vars_filt_lab |>
      dplyr::filter(!!as.name(var1) != "Inválido ou nulo")

    df_sinasc_biv_vars_filt_lab_wider <- df_sinasc_biv_vars_filt_lab |>
      tidyr::pivot_wider(names_from = var2, values_from = count, names_sort = FALSE)

    df_sinasc_biv_col_invalid <- df_sinasc_biv_vars_filt_lab_wider |>
      dplyr::select(`Inválido ou nulo`)

    # sum(df_sinasc_biv_vars_filt_lab_wider[1, 2:7], na.rm = TRUE)
    # sum(df_sinasc_biv_vars_filt_lab_wider_Total, na.rm = TRUE)
    # sum_total <- df_sinasc_biv_vars_filt_lab_wider[-1, 1]
    # sum_total <- df_sinasc_biv_vars_filt_lab_wider[1, 2:7]

    df_sinasc_biv_vars_filt_lab_wider <- df_sinasc_biv_vars_filt_lab_wider |>
      dplyr::select(-`Inválido ou nulo`) |>
      dplyr::mutate(Total = rowSums(across(-!!as.name(var1))))

  }
  if(!input_tabela_c_invalido){
    ### Sem inválido na tabela e soma de totais (com inválido)
    df_sinasc_biv_vars_filt_lab_val <- df_sinasc_biv_vars_filt_lab |>
      dplyr::filter(!!as.name(var1) != "Inválido ou nulo",
                    var2 != "Inválido ou nulo")

    ## Não vou mais usar a linha de total, só a coluna
    # df_sinasc_biv_vars_filt_lab_val_total <- df_sinasc_biv_vars_filt_lab_val |>
    #   dplyr::group_by(var2) |>
    #   dplyr::summarise(count = sum(count)) |>
    #   dplyr::ungroup() |>
    #   dplyr::mutate(!!as.name(var1) := "Total") |>
    #   dplyr::select(!!as.name(var1), var2, count) |>
    #   dplyr::bind_rows(df_sinasc_biv_vars_filt_lab_val)


    df_sinasc_biv_vars_filt_lab_wider <- df_sinasc_biv_vars_filt_lab_val |>
      tidyr::pivot_wider(names_from = var2, values_from = count, names_sort = FALSE) |>
      dplyr::mutate(Total = rowSums(across(-!!as.name(var1))))
  }
  df_sinasc_biv_vars_filt_lab_wider
}
