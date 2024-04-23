func_applyFilters <- function(df_sinasc, input_year, input_abrang, input_uf, input_mun,
                              nome_var_db){
  # browser()
  if(input_year != "TODOS"){
    df_sinasc_filt <- df_sinasc |>
      dplyr::filter(`_ANONASC` == input_year)
  }

  # browser()
  #### Brasil ----
  if(input_abrang == "Brasil"){
    df_sinasc_filt <- df_sinasc_filt
  }
  #### UF ----
  if(input_abrang == "Unidade da federação"){
    if(input_uf == "TODOS"){
      df_sinasc_filt <- df_sinasc_filt
    }else{
      df_sinasc_filt <- df_sinasc_filt |>
        dplyr::filter(`_UF` == input_uf)
    }
  }
  #### Município ----
  if(input_abrang == "Município"){
    if(input_mun == "Selecione a UF"){
      df_sinasc_filt <- df_sinasc_filt
    }else{
      df_sinasc_filt <- df_sinasc_filt |>
        dplyr::filter(cod_mun == input_mun)
    }
  }
  df_sinasc_filt |>
    dplyr::select(all_of(nome_var_db))
}
