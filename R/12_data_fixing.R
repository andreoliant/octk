# fix per funzioni di setup dati

# fix localizzazioni----
get_fixmap_cod_regione_na_by_programma <- function() {
  tibble(
    OC_CODICE_PROGRAMMA = c(
      "2007SI001FA011",
      "2014IT05SFOP004",
      "2014IT05SFOP014",
      "2014IT16RFOP007",
      "2014IT16RFOP016",
      "2017POCSICILIA1",
      "ACCOESBASILICATA",
      "ACCOESCALABRIA",
      "ACCOESCAMPANIA",
      "ACCOESEMROMAGN",
      "ACCOESPIEMONTE",
      "ACCOESPUGLIA",
      "2014IT16RFOP002",
      "2014TC16I5CB008",
      "2014TC16M4TN002",
      "2017POCINFRASTR",
      "2017POCLEGAL01",
      "FSCCRESCISUD",
      "2016POCCAMPAN01",
      "PSCCAMPANIA"
    ),
    COD_REGIONE_FIX = c(
      "019", # Sicilia
      "006", # Friuli-Venezia Giulia
      "019", # Sicilia
      "015", # Campania
      "019", # Sicilia
      "019", # Sicilia
      "017", # Basilicata
      "018", # Calabria
      "015", # Campania
      "008", # Emilia-Romagna
      "001", # Piemonte
      "016", # Puglia
      "000", # nazionale / transnazionale
      "000",
      "000",
      "000",
      "000",
      "000",
      "015",
      "015"
    )
  )
}

chk_cod_regione_na_by_programma <- function(df, export=FALSE) {
  fun_name <- "chk_cod_regione_na_by_programma"
  fix_map <- get_fixmap_cod_regione_na_by_programma()
  
  out <- df %>%
    dplyr::filter(is.na(COD_REGIONE)) %>%
    dplyr::count(OC_CODICE_PROGRAMMA, x_programma, DEN_REGIONE, name = "n") %>%
    dplyr::left_join(fix_map, by = "OC_CODICE_PROGRAMMA") %>%
    dplyr::mutate(
      coperto_da_fix = !is.na(COD_REGIONE_FIX)
    ) %>%
    dplyr::arrange(dplyr::desc(coperto_da_fix), OC_CODICE_PROGRAMMA, DEN_REGIONE)
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }

  return(out)
}

fix_cod_regione_na_by_programma <- function(df) {
  fix_map <- get_fixmap_cod_regione_na_by_programma()
  
  df %>%
    left_join(fix_map, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(
      COD_REGIONE = if_else(
        is.na(COD_REGIONE) & !is.na(COD_REGIONE_FIX),
        COD_REGIONE_FIX,
        COD_REGIONE
      )
    ) %>%
    select(-COD_REGIONE_FIX)
}

# fix umbria-marche----
chk_riparto_cn_mz_programmi_marche_umbria <- function(df, progetti, export=FALSE) {
  fun_name <- "chk_riparto_cn_mz_programmi_marche_umbria"
  
  programmi_fix <- c(
    "2021IT16RFPR011",
    "2021IT16RFPR018",
    "2021IT05SFPR009",
    "2021IT05SFPR016"
  )
  
  out <- df %>%
    dplyr::left_join(
      progetti %>%
        dplyr::select(
          cod_locale_progetto = COD_LOCALE_PROGETTO,
          DEN_REGIONE
        ),
      by = "cod_locale_progetto"
    ) %>%
    dplyr::filter(oc_cod_programma %in% programmi_fix) %>%
    dplyr::group_by(oc_cod_programma, oc_descrizione_programma, DEN_REGIONE) %>%
    dplyr::summarise(
      costo_ammesso_MZ = sum(costo_ammesso_MZ, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = oc_cod_programma %in% programmi_fix
    ) %>%
    dplyr::filter(costo_ammesso_MZ != 0) %>%
    dplyr::arrange(
      dplyr::desc(coperto_da_fix),
      oc_cod_programma,
      dplyr::desc(abs(costo_ammesso_MZ)),
      DEN_REGIONE
    )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}

fix_riparto_cn_mz_programmi_marche_umbria <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      costo_ammesso_CN = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ costo_ammesso_MZ,
        oc_cod_programma == "2021IT16RFPR018" ~ costo_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR009" ~ costo_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR016" ~ costo_ammesso_MZ,
        TRUE ~ costo_ammesso_CN
      ),
      costo_ammesso_MZ = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ 0,
        oc_cod_programma == "2021IT16RFPR018" ~ 0,
        oc_cod_programma == "2021IT05SFPR009" ~ 0,
        oc_cod_programma == "2021IT05SFPR016" ~ 0,
        TRUE ~ costo_ammesso_MZ
      ),
      imp_ammesso_CN = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ imp_ammesso_MZ,
        oc_cod_programma == "2021IT16RFPR018" ~ imp_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR009" ~ imp_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR016" ~ imp_ammesso_MZ,
        TRUE ~ imp_ammesso_CN
      ),
      imp_ammesso_MZ = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ 0,
        oc_cod_programma == "2021IT16RFPR018" ~ 0,
        oc_cod_programma == "2021IT05SFPR009" ~ 0,
        oc_cod_programma == "2021IT05SFPR016" ~ 0,
        TRUE ~ imp_ammesso_MZ
      ),
      imp_trasf_ammesso_CN = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ imp_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT16RFPR018" ~ imp_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR009" ~ imp_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR016" ~ imp_trasf_ammesso_MZ,
        TRUE ~ imp_trasf_ammesso_CN
      ),
      imp_trasf_ammesso_MZ = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ 0,
        oc_cod_programma == "2021IT16RFPR018" ~ 0,
        oc_cod_programma == "2021IT05SFPR009" ~ 0,
        oc_cod_programma == "2021IT05SFPR016" ~ 0,
        TRUE ~ imp_trasf_ammesso_MZ
      ),
      pag_ammesso_CN = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ pag_ammesso_MZ,
        oc_cod_programma == "2021IT16RFPR018" ~ pag_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR009" ~ pag_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR016" ~ pag_ammesso_MZ,
        TRUE ~ pag_ammesso_CN
      ),
      pag_ammesso_MZ = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ 0,
        oc_cod_programma == "2021IT16RFPR018" ~ 0,
        oc_cod_programma == "2021IT05SFPR009" ~ 0,
        oc_cod_programma == "2021IT05SFPR016" ~ 0,
        TRUE ~ pag_ammesso_MZ
      ),
      pag_trasf_ammesso_CN = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ pag_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT16RFPR018" ~ pag_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR009" ~ pag_trasf_ammesso_MZ,
        oc_cod_programma == "2021IT05SFPR016" ~ pag_trasf_ammesso_MZ,
        TRUE ~ pag_trasf_ammesso_CN
      ),
      pag_trasf_ammesso_MZ = dplyr::case_when(
        oc_cod_programma == "2021IT16RFPR011" ~ 0,
        oc_cod_programma == "2021IT16RFPR018" ~ 0,
        oc_cod_programma == "2021IT05SFPR009" ~ 0,
        oc_cod_programma == "2021IT05SFPR016" ~ 0,
        TRUE ~ pag_trasf_ammesso_MZ
      )
    )
  
  return(out)
}

# fix rt su naz----
chk_riparto_cn_mz_programmi_nazionali_marche_umbria <- function(df, progetti, po, export=FALSE) {
  
  fun_name <- "chk_riparto_cn_mz_programmi_nazionali_marche_umbria"
  
  den_regione_fix <- c(
    # "UMBRIA:::MARCHE:::ABRUZZO", #DEV: questa non si può risolvere
    "MARCHE",
    "UMBRIA",
    "AMBITO NAZIONALE:::MARCHE",
    "AMBITO NAZIONALE:::UMBRIA"
  )
  
  out <- df %>%
    left_join(
      progetti %>% select(cod_locale_progetto = COD_LOCALE_PROGETTO, DEN_REGIONE),
      by = "cod_locale_progetto"
    ) %>%
    left_join(
      po %>% select(oc_cod_programma = OC_CODICE_PROGRAMMA, x_REGNAZ),
      by = "oc_cod_programma"
    ) %>%
    filter(
      x_REGNAZ == "NAZ",
      ue_categ_regione == "T",
      stringr::str_detect(stringr::str_to_upper(coalesce(DEN_REGIONE, "")), "MARCHE|UMBRIA")
    ) %>%
    group_by(oc_cod_programma, oc_descrizione_programma, DEN_REGIONE) %>%
    summarise(
      costo_ammesso_MZ = sum(costo_ammesso_MZ, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      coperto_da_fix = DEN_REGIONE %in% den_regione_fix
    ) %>%
    filter(costo_ammesso_MZ != 0) %>%
    arrange(coperto_da_fix, oc_cod_programma, desc(abs(costo_ammesso_MZ)), DEN_REGIONE)
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  out
}

fix_riparto_cn_mz_programmi_nazionali_marche_umbria <- function(df, progetti, po) {
  
  out <- df %>%
    dplyr::left_join(
      progetti %>%
        dplyr::select(
          cod_locale_progetto = COD_LOCALE_PROGETTO,
          DEN_REGIONE
        ),
      by = "cod_locale_progetto"
    ) %>%
    dplyr::left_join(
      po %>%
        dplyr::select(
          oc_cod_programma = OC_CODICE_PROGRAMMA,
          x_REGNAZ
        ),
      by = "oc_cod_programma"
    ) %>%
    dplyr::mutate(
      costo_ammesso_CN = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ costo_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ costo_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ costo_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ costo_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ costo_ammesso_CN
      ),
      costo_ammesso_MZ = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ costo_ammesso_MZ
      ),
      imp_ammesso_CN = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ imp_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ imp_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ imp_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ imp_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ imp_ammesso_CN
      ),
      imp_ammesso_MZ = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ imp_ammesso_MZ
      ),
      imp_trasf_ammesso_CN = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ imp_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ imp_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ imp_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ imp_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ imp_trasf_ammesso_CN
      ),
      imp_trasf_ammesso_MZ = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ imp_trasf_ammesso_MZ
      ),
      pag_ammesso_CN = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ pag_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ pag_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ pag_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ pag_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ pag_ammesso_CN
      ),
      pag_ammesso_MZ = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ pag_ammesso_MZ
      ),
      pag_trasf_ammesso_CN = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ pag_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ pag_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ pag_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ pag_trasf_ammesso_MZ,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ pag_trasf_ammesso_CN
      ),
      pag_trasf_ammesso_MZ = dplyr::case_when(
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::MARCHE" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "AMBITO NAZIONALE:::UMBRIA" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" & DEN_REGIONE == "UMBRIA:::MARCHE:::ABRUZZO" ~ 0,
        x_REGNAZ == "NAZ" & ue_categ_regione == "T" ~ 0,
        TRUE ~ pag_trasf_ammesso_MZ
      )
    ) %>%
    dplyr::select(-x_REGNAZ, -DEN_REGIONE)
  
  return(out)
}

# fix sezioni accordi----

# OLD:
# chk_sezione_accordi <- function(df, export=FALSE) {
#   fun_name <- "chk_accordi_sezione_extra"
#   
#   chk_fsc_fdr <- df %>%
#     dplyr::filter(oc_ambito %in% c("FSC", "FDR")) %>%
#     dplyr::count(oc_ambito, oc_cod_fonte, psc_sezione, name = "n") %>%
#     dplyr::arrange(oc_ambito, oc_cod_fonte, psc_sezione)
#   
#   chk_comp_fsc <- df %>%
#     dplyr::filter(
#       oc_ambito == "FSC",
#       as.character(psc_sezione) == "Comp"
#     ) %>%
#     dplyr::count(
#       oc_ambito,
#       oc_cod_fonte,
#       oc_cod_programma,
#       cod_locale_progetto,
#       name = "n"
#     ) %>%
#     dplyr::mutate(
#       coperto_da_fix = case_when(
#         oc_cod_programma == "ACCOESPUGLIA" & oc_cod_fonte == "FSC2127" ~ TRUE,
#         oc_cod_programma == "ACCOESMARCHE" & oc_cod_fonte == "FSC2127" ~ TRUE,
#         TRUE ~ FALSE)
#     ) %>%
#     dplyr::arrange(
#       coperto_da_fix,
#       oc_cod_programma,
#       cod_locale_progetto
#     )
#   
#   chk_zero_fsc_fdr <- df %>%
#     dplyr::filter(
#       oc_ambito %in% c("FSC", "FDR"),
#       as.character(psc_sezione) == "0"
#     ) %>%
#     dplyr::count(
#       oc_ambito,
#       oc_cod_fonte,
#       oc_cod_programma,
#       name = "n"
#     ) %>%
#     dplyr::mutate(
#       coperto_da_fix = FALSE
#     ) %>%
#     dplyr::arrange(
#       coperto_da_fix,
#       oc_ambito,
#       oc_cod_fonte,
#       oc_cod_programma
#     )
#   
#   out <- list(
#     chk_fsc_fdr = chk_fsc_fdr,
#     chk_comp_fsc = chk_comp_fsc,
#     chk_zero_fsc_fdr = chk_zero_fsc_fdr
#   )
#   
#   if (export == TRUE) {
#     openxlsx::write.xlsx(
#       x = out,
#       file = file.path(TEMP, paste0(fun_name, ".xlsx")),
#       overwrite = TRUE
#     )
#   }
#   
#   return(out)
# }
# 
# fix_sezione_accordi <- function(df) {
#   
#   out <- df %>%
#     dplyr::mutate(
#       oc_ambito = dplyr::case_when(
#         oc_ambito == "FSC" &
#           as.character(psc_sezione) == "Comp" &
#           oc_cod_programma == "ACCOESPUGLIA" ~ "FDR",
#         oc_ambito == "FSC" &
#           as.character(psc_sezione) == "Comp" &
#           oc_cod_programma == "ACCOESMARCHE" ~ "FDR",
#         TRUE ~ oc_ambito
#       ),
#       oc_cod_fonte = dplyr::case_when(
#         oc_cod_fonte == "FSC2127" &
#           as.character(psc_sezione) == "Comp" &
#           oc_cod_programma == "ACCOESPUGLIA" ~ "FDR2127",
#         oc_cod_fonte == "FSC2127" &
#           as.character(psc_sezione) == "Comp" &
#           oc_cod_programma == "ACCOESMARCHE" ~ "FDR2127",
#         TRUE ~ oc_cod_fonte
#       )
#     )
#   
#   return(out)
# }

# NEW:
chk_sezione_accordi <- function(df, export = FALSE) {
  fun_name <- "chk_accordi_sezione_extra"
  
  chk_fsc_fdr <- df %>%
    dplyr::filter(oc_ambito %in% c("FSC", "FDR")) %>%
    dplyr::count(oc_ambito, oc_cod_fonte, psc_sezione, name = "n") %>%
    dplyr::arrange(oc_ambito, oc_cod_fonte, psc_sezione)
  
  chk_comp_fsc <- df %>%
    dplyr::filter(
      oc_ambito == "FSC",
      as.character(psc_sezione) == "Comp"
    ) %>%
    dplyr::count(
      oc_ambito,
      oc_cod_fonte,
      oc_cod_programma,
      cod_locale_progetto,
      name = "n"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = dplyr::case_when(
        oc_cod_programma == "ACCOESPUGLIA" &
          oc_cod_fonte == "FSC2127" ~ TRUE,
        oc_cod_programma == "ACCOESMARCHE" &
          oc_cod_fonte == "FSC2127" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_programma,
      cod_locale_progetto
    )
  
  chk_zero_fsc_fdr <- df %>%
    dplyr::filter(
      oc_ambito %in% c("FSC", "FDR"),
      as.character(psc_sezione) == "0"
    ) %>%
    dplyr::count(
      oc_ambito,
      oc_cod_fonte,
      CODICE_TIPOLOGIA_PROGRAMMA,
      oc_cod_programma,
      name = "n"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = dplyr::case_when(
        oc_cod_fonte == "FSC1420" &
          CODICE_TIPOLOGIA_PROGRAMMA == "PSC" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_ambito,
      oc_cod_fonte,
      CODICE_TIPOLOGIA_PROGRAMMA,
      oc_cod_programma
    )
  
  chk_zero_fsc1420_psc <- df %>%
    dplyr::filter(
      oc_cod_fonte == "FSC1420",
      CODICE_TIPOLOGIA_PROGRAMMA == "PSC",
      as.character(psc_sezione) == "0"
    ) %>%
    dplyr::count(
      oc_ambito,
      oc_cod_fonte,
      CODICE_TIPOLOGIA_PROGRAMMA,
      oc_cod_programma,
      psc_sezione,
      name = "n"
    ) %>%
    dplyr::mutate(
      psc_sezione_fix = "SS_2",
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(
      oc_ambito,
      oc_cod_fonte,
      CODICE_TIPOLOGIA_PROGRAMMA,
      oc_cod_programma
    )
  
  out <- list(
    chk_fsc_fdr = chk_fsc_fdr,
    chk_comp_fsc = chk_comp_fsc,
    chk_zero_fsc_fdr = chk_zero_fsc_fdr,
    chk_zero_fsc1420_psc = chk_zero_fsc1420_psc
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}

fix_sezione_accordi <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      psc_sezione = dplyr::case_when(
        oc_cod_fonte == "FSC1420" &
          CODICE_TIPOLOGIA_PROGRAMMA == "PSC" &
          as.character(psc_sezione) == "0" ~ "SS_2",
        TRUE ~ as.character(psc_sezione)
      ),
      oc_ambito = dplyr::case_when(
        oc_ambito == "FSC" &
          as.character(psc_sezione) == "Comp" &
          oc_cod_programma == "ACCOESPUGLIA" ~ "FDR",
        oc_ambito == "FSC" &
          as.character(psc_sezione) == "Comp" &
          oc_cod_programma == "ACCOESMARCHE" ~ "FDR",
        TRUE ~ oc_ambito
      ),
      oc_cod_fonte = dplyr::case_when(
        oc_cod_fonte == "FSC2127" &
          as.character(psc_sezione) == "Comp" &
          oc_cod_programma == "ACCOESPUGLIA" ~ "FDR2127",
        oc_cod_fonte == "FSC2127" &
          as.character(psc_sezione) == "Comp" &
          oc_cod_programma == "ACCOESMARCHE" ~ "FDR2127",
        TRUE ~ oc_cod_fonte
      )
    )
  
  return(out)
}


# fix sezioni psc----
chk_sezione_psc <- function(df, export=FALSE) {
  fun_name <- "chk_sezione_psc_1420"
  
  chk_psc <- df %>%
    dplyr::filter(grepl("PSC", oc_cod_programma)) %>%
    dplyr::count(oc_ambito, oc_cod_fonte, psc_sezione, name = "n") %>%
    dplyr::arrange(oc_ambito, oc_cod_fonte, psc_sezione)
  
  chk_casi_fix <- df %>%
    dplyr::filter(grepl("PSC", oc_cod_programma)) %>%
    dplyr::count(
      oc_ambito,
      oc_cod_fonte,
      oc_cod_programma,
      psc_sezione,
      name = "n"
    ) %>%
    dplyr::mutate(
      coperto_da_fix =
        (oc_cod_programma == "PSCPUGLIA" & as.character(psc_sezione) == "0") |
        (as.character(psc_sezione) == "SO:::SS_1") |
        (oc_cod_fonte == "FSC2127")
    ) %>%
    dplyr::filter(
      as.character(psc_sezione) == "0" |
        as.character(psc_sezione) == "SO:::SS_1" |
        oc_cod_fonte == "FSC2127"
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_programma,
      oc_cod_fonte,
      psc_sezione
    )
  
  chk_pscpuglia <- df %>%
    dplyr::filter(oc_cod_programma == "PSCPUGLIA") %>%
    dplyr::count(psc_sezione, name = "n") %>%
    dplyr::mutate(
      coperto_da_fix =
        as.character(psc_sezione) == "0" |
        as.character(psc_sezione) == "SO:::SS_1"
    ) %>%
    dplyr::arrange(coperto_da_fix, psc_sezione)
  
  out <- list(
    chk_psc = chk_psc,
    chk_casi_fix = chk_casi_fix,
    chk_pscpuglia = chk_pscpuglia
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}

fix_sezione_psc <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      psc_sezione = dplyr::case_when(
        oc_cod_programma == "PSCPUGLIA" & as.character(psc_sezione) == "0" ~ "SO",
        as.character(psc_sezione) == "SO:::SS_1" ~ "SO",
        oc_cod_fonte == "FSC2127" & grepl("PSC", oc_cod_programma) ~ "ANT",
        TRUE ~ as.character(psc_sezione)
      )
    )
  
  return(out)
}

# fix duplicati----
get_fixmap_clp_duplicati_po_extra <- function() {
  
  # out <- tibble::tribble(
  #   ~cod_locale_progetto,                                                     ~oc_ambito, ~note,
  #   "D11C24000480009---2021IT16FFPR001",                                      "FSE",      "molise",
  #   "D14F24001690009---2021IT16FFPR001",                                      "FSE",      "molise",
  #   "D34F24001600009---2021IT16FFPR001",                                      "FSE",      "molise",
  #   "I89B25000120006---PRGADG_181229MLPSADGInvitalia_deprivazione_materiale", "FESR",     "articolazione",
  #   "J81C23001140007---MDS_AT_FORMEZ_J81C23001140007",                        "FESR",     "articolazione",
  #   "J84F24001720006---PRGAT1228GiustiziaAssistenza_Tecnica",                 "FESR",     "articolazione",
  #   "J89I25000350007---da generare",                                          "FESR",     "articolazione",
  #   "J89I25000360007---da generare",                                          "FESR",     "articolazione",
  #   "J89I25000520007---da generare",                                          "FESR",     "articolazione",
  #   "J89I25000530007---da generare",                                          "FESR",     "articolazione",
  #   "J89I23002160006---MISSIONE LGD J89I23002160006",                         "FESR",     "articolazione"
  # )
  
  out <- tibble::tibble(
    cod_locale_progetto = character(),
    oc_ambito = character(),
    note = character()
  )
  
  return(out)
}

chk_clp_duplicati_po_extra <- function(df, export=FALSE) {
  fun_name <- "chk_clp_duplicati_po_extra"
  
  fix_map <- get_fixmap_clp_duplicati_po_extra()
  
  dup_keys <- df %>%
    dplyr::count(cod_locale_progetto, oc_cod_programma, name = "n_dup") %>%
    dplyr::filter(n_dup > 1)
  
  dup_cover <- df %>%
    dplyr::semi_join(
      dup_keys,
      by = c("cod_locale_progetto", "oc_cod_programma")
    ) %>%
    dplyr::inner_join(
      fix_map %>% dplyr::select(cod_locale_progetto, oc_ambito),
      by = c("cod_locale_progetto", "oc_ambito")
    ) %>%
    dplyr::distinct(cod_locale_progetto, oc_cod_programma) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  chk_dup_summary <- dup_keys %>%
    dplyr::left_join(
      df %>%
        dplyr::distinct(
          cod_locale_progetto,
          oc_cod_programma,
          oc_descrizione_programma
        ),
      by = c("cod_locale_progetto", "oc_cod_programma")
    ) %>%
    dplyr::left_join(
      df %>%
        dplyr::semi_join(
          dup_keys,
          by = c("cod_locale_progetto", "oc_cod_programma")
        ) %>%
        dplyr::group_by(cod_locale_progetto, oc_cod_programma) %>%
        dplyr::summarise(
          oc_ambito = paste(sort(unique(oc_ambito)), collapse = " | "),
          ue_asse_prioritario = paste(sort(unique(as.character(ue_asse_prioritario))), collapse = " | "),
          .groups = "drop"
        ),
      by = c("cod_locale_progetto", "oc_cod_programma")
    ) %>%
    dplyr::left_join(
      dup_cover,
      by = c("cod_locale_progetto", "oc_cod_programma")
    ) %>%
    dplyr::mutate(
      coperto_da_fix = dplyr::coalesce(coperto_da_fix, FALSE)
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      dplyr::desc(n_dup),
      oc_cod_programma,
      cod_locale_progetto
    )
  
  chk_dup_detail <- df %>%
    dplyr::semi_join(
      dup_keys,
      by = c("cod_locale_progetto", "oc_cod_programma")
    ) %>%
    dplyr::left_join(
      fix_map,
      by = c("cod_locale_progetto", "oc_ambito")
    ) %>%
    dplyr::mutate(
      coperto_da_fix = !is.na(note)
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_programma,
      cod_locale_progetto,
      oc_ambito
    )
  
  out <- list(
    chk_dup_summary = chk_dup_summary,
    chk_dup_detail = chk_dup_detail
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}

fix_clp_duplicati_po_extra <- function(df) {
  
  fix_map <- get_fixmap_clp_duplicati_po_extra()
  
  fix_keys <- fix_map %>%
    dplyr::select(cod_locale_progetto, oc_ambito) %>%
    dplyr::distinct()
  
  out <- df %>%
    dplyr::anti_join(
      fix_keys,
      by = c("cod_locale_progetto", "oc_ambito")
    )
  
  return(out)
}


# fix ambiti multipli ----

get_fixmap_ambito_anomalo_extra <- function() {
  
  out <- tibble::tibble(
    oc_ambito = c("FESR:::FSE"),
    oc_ambito_fix = c("FSE"),
    note = c("forzo fesr")
  )
  
  return(out)
}


chk_ambito_anomalo_extra <- function(df, progetti, export=FALSE) {
  
  fun_name <- "chk_ambito_anomalo_extra"
  
  fix_map <- get_fixmap_ambito_anomalo_extra()
  
  chk_summary <- df %>%
    dplyr::filter(grepl(":::", oc_ambito)) %>%
    dplyr::count(oc_cod_fonte, oc_ambito, oc_cod_programma, ue_fondo, name = "n") %>%
    dplyr::left_join(
      fix_map %>% dplyr::select(oc_ambito, oc_ambito_fix),
      by = "oc_ambito"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = !is.na(oc_ambito_fix)
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_programma,
      oc_cod_fonte,
      oc_ambito
    )
  
  chk_detail <- df %>%
    dplyr::filter(grepl(":::", oc_ambito)) %>%
    dplyr::select(
      cod_locale_progetto,
      oc_cod_programma,
      oc_descrizione_programma,
      oc_ambito
    ) %>%
    dplyr::left_join(
      progetti %>%
        dplyr::select(
          cod_locale_progetto = COD_LOCALE_PROGETTO,
          CUP_COD_NATURA,
          OC_TITOLO_PROGETTO
        ),
      by = "cod_locale_progetto"
    ) %>%
    dplyr::left_join(
      fix_map,
      by = "oc_ambito"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = !is.na(oc_ambito_fix)
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_programma,
      cod_locale_progetto
    )
  
  out <- list(
    chk_summary = chk_summary,
    chk_detail = chk_detail
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}


fix_ambito_anomalo_extra <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      oc_ambito = dplyr::case_when(
        oc_ambito == "FESR:::FSE" ~ "FSE",
        TRUE ~ oc_ambito
      )
    )
  
  return(out)
}


# fix fonte----
get_fixmap_fonte_anomala_extra <- function() {
  
  out <- tibble::tibble(
    oc_cod_fonte = c("FDR"),
    oc_cod_fonte_fix = c("FDR2127"),
    note = c("riallineo fonte 21-27")
  )
  
  return(out)
}


chk_fonte_anomala_extra <- function(df, export=FALSE) {
  
  fun_name <- "chk_fonte_anomala_extra"
  
  fix_map <- get_fixmap_fonte_anomala_extra()
  
  chk_summary <- df %>%
    dplyr::count(oc_cod_fonte, oc_ambito, name = "n") %>%
    dplyr::left_join(
      fix_map %>% dplyr::select(oc_cod_fonte, oc_cod_fonte_fix),
      by = "oc_cod_fonte"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = !is.na(oc_cod_fonte_fix)
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      oc_cod_fonte,
      oc_ambito
    )
  
  chk_detail <- df %>%
    dplyr::filter(oc_cod_fonte == "FDR") %>%
    dplyr::count(oc_cod_programma, oc_ambito, psc_sezione, name = "n") %>%
    dplyr::mutate(
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(
      oc_cod_programma,
      oc_ambito,
      psc_sezione
    )
  
  out <- list(
    chk_summary = chk_summary,
    chk_detail = chk_detail
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}


fix_fonte_anomala_extra <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      oc_cod_fonte = dplyr::case_when(
        oc_cod_fonte == "FDR" ~ "FDR2127",
        TRUE ~ oc_cod_fonte
      )
    )
  
  return(out)
}


# fix ciclo sezioni speciali psc----
chk_ciclo_sezioni_speciali_psc_1420 <- function(df, export=FALSE) {
  
  fun_name <- "chk_ciclo_sezioni_speciali_psc_1420"
  
  chk_summary <- df %>%
    dplyr::filter(
      grepl("PSC", oc_cod_programma),
      psc_sezione %in% c("SS_1", "SS_2"),
      OC_COD_CICLO != 2
    ) %>%
    dplyr::count(oc_cod_programma, OC_COD_CICLO, name = "n") %>%
    dplyr::mutate(
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(
      oc_cod_programma,
      OC_COD_CICLO
    )
  
  chk_detail <- df %>%
    dplyr::filter(
      grepl("PSC", oc_cod_programma),
      psc_sezione %in% c("SS_1", "SS_2"),
      OC_COD_CICLO != 2
    ) %>%
    dplyr::count(
      oc_cod_programma,
      psc_sezione,
      OC_COD_CICLO,
      name = "n"
    ) %>%
    dplyr::mutate(
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(
      oc_cod_programma,
      psc_sezione,
      OC_COD_CICLO
    )
  
  out <- list(
    chk_summary = chk_summary,
    chk_detail = chk_detail
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}


fix_ciclo_sezioni_speciali_psc_1420 <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      OC_COD_CICLO = dplyr::case_when(
        grepl("PSC", oc_cod_programma) & psc_sezione %in% c("SS_1", "SS_2") ~ 2,
        TRUE ~ OC_COD_CICLO
      )
    )
  
  return(out)
}


# fix impegni e pagamenti negativi ----
chk_variabili_finanziarie_anomale_713 <- function(df, export=FALSE) {
  
  fun_name <- "chk_variabili_finanziarie_anomale_713"
  
  na_costo <- df %>%
    dplyr::filter(is.na(oc_costo_coesione)) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  neg_costo <- df %>%
    dplyr::filter(oc_costo_coesione < 0) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  na_impegni <- df %>%
    dplyr::filter(is.na(oc_impegni_coesione)) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  neg_impegni <- df %>%
    dplyr::filter(oc_impegni_coesione < 0) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  na_pagamenti <- df %>%
    dplyr::filter(is.na(oc_tot_pagamenti_coesione)) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  neg_pagamenti <- df %>%
    dplyr::filter(oc_tot_pagamenti_coesione < 0) %>%
    dplyr::mutate(coperto_da_fix = TRUE)
  
  summary <- tibble::tibble(
    check = c(
      "na_costo",
      "neg_costo",
      "na_impegni",
      "neg_impegni",
      "na_pagamenti",
      "neg_pagamenti"
    ),
    n = c(
      nrow(na_costo),
      nrow(neg_costo),
      nrow(na_impegni),
      nrow(neg_impegni),
      nrow(na_pagamenti),
      nrow(neg_pagamenti)
    ),
    coperto_da_fix = TRUE
  )
  
  out <- list(
    summary = summary,
    na_costo = na_costo,
    neg_costo = neg_costo,
    na_impegni = na_impegni,
    neg_impegni = neg_impegni,
    na_pagamenti = na_pagamenti,
    neg_pagamenti = neg_pagamenti
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  return(out)
}


fix_variabili_finanziarie_anomale_713 <- function(df) {
  
  out <- df %>%
    dplyr::mutate(
      oc_costo_coesione = dplyr::case_when(
        is.na(oc_costo_coesione) ~ 0,
        oc_costo_coesione < 0 ~ 0,
        TRUE ~ oc_costo_coesione
      ),
      oc_impegni_coesione = dplyr::case_when(
        is.na(oc_impegni_coesione) ~ 0,
        oc_impegni_coesione < 0 ~ 0,
        TRUE ~ oc_impegni_coesione
      ),
      oc_tot_pagamenti_coesione = dplyr::case_when(
        is.na(oc_tot_pagamenti_coesione) ~ 0,
        oc_tot_pagamenti_coesione < 0 ~ 0,
        TRUE ~ oc_tot_pagamenti_coesione
      )
    )
  
  return(out)
}


# fix su livello_1 per sie [VALUTARE SE TENERE, ORA NON LI USO]----

is_target_ambito_ue_ob_specifico <- function(x) {
  x %in% c("FESR", "FSE", "JTF")
}

is_vuoto_ue_ob_specifico <- function(x) {
  is.na(x) | x == "0"
}

add_fix_columns_from_progetti_to_operazioni_regex_sottosettore <- function(df, progetti) {
  df %>%
    dplyr::left_join(
      progetti %>%
        dplyr::select(
          cod_locale_progetto = COD_LOCALE_PROGETTO,
          CUP_DESCR_SOTTOSETTORE
        ) %>%
        dplyr::distinct(),
      by = "cod_locale_progetto"
    )
}

drop_fix_columns_from_progetti_to_operazioni_regex_sottosettore <- function(df) {
  df %>%
    dplyr::select(-dplyr::any_of("CUP_DESCR_SOTTOSETTORE"))
}

get_fixmap_ue_ob_specifico_vuoto_clp_puntuale <- function() {
  tibble::tribble(
    ~cod_locale_progetto,                           ~oc_cod_programma,  ~ue_ob_specifico_fix,
    "B51C23000730005---ESFAT_180_0003",             NA_character_,      "TA2",               #https://opencoesione.gov.it/it/dati/progetti/b51c23000730005-esfat_180_0003/
    # "F28H24001500002---FSR.26101.24XX.0.0002.SSM",  "2021IT16RFPR019", "RSO2.10",          #https://opencoesione.gov.it/it/dati/progetti/f28h24001500002-fsr2610124xx00002ssm/
    # "F58H24001410002---FSR.26101.24XX.0.0001.SSM",  "2021IT16RFPR019", "RSO2.10"           #https://opencoesione.gov.it/it/dati/progetti/f58h24001410002-fsr2610124xx00001ssm/   
  )
}
# MEMO: qui vanno solo casi che non sarebbero mai fixabili con altre regole o ai

get_fixmap_ue_ob_specifico_vuoto_programma <- function() {
  tibble::tribble(
    ~oc_cod_programma,  ~oc_ambito,     ~ue_asse_prioritario, ~ue_ob_specifico_fix,
    "2021IT16FFTA001",  "FESR",         "1",                  "TA1",
    "2021IT05FFPR003",  NA_character_,  "3",                  "ESO4.13",
    "2021IT16FFPR005",  "FSE",          "0",                  "TA2",
    "2021IT16RFPR017",  NA_character_,  "6",                  "RSO1.6",
    "2021IT16FFPR003",  NA_character_,  "2STEP",              "RSO2.9",
    "2021IT16FFPR003",  NA_character_,  "1STEP",              "RSO1.6",
    "2021IT16RFPR010",  NA_character_,  "6:::7",              "RSO1.6",
    "2021IT16RFPR015",  NA_character_,  "8",                  "RSO1.6",
    "2021IT16RFPR015",  NA_character_,  "9",                  "RSO2.9",
    "2021IT16RFPR018",  NA_character_,  "8",                  "RSO2.9",
    "2021IT16RFPR018",  NA_character_,  "7",                  "RSO1.6",
    "2021IT16RFPR019",  NA_character_,  "2",                  "RSO1.5" #,
    # "2021IT16FFPR002",  NA_character_,  NA_character_,        "RSO1.3"
  )
}
# MEMO: qui vanno casi che 


get_fixmap_ue_ob_specifico_vuoto_regex_sottosettore <- function() {
  tibble::tribble(
    ~pattern,               ~ignore_case, ~oc_cod_programma,  ~oc_ambito,     ~ue_asse_prioritario, ~ue_ob_specifico_fix,
    "assistenza\\s*tecnic", TRUE,         "2021IT16FFTA001", "FSE",          NA_character_,         "TA1",
    "assistenza\\s*tecnic", TRUE,         "2021IT16FFTA001", "FESR",         "1",                   "TA1",
    "assistenza\\s*tecnic", TRUE,         NA_character_,     NA_character_,  NA_character_,         "TA2"
  )
}

get_fixmap_ue_ob_specifico_vuoto_regex_articolazione <- function() {
  tibble::tribble(
    ~pattern,               ~ignore_case, ~oc_cod_programma, ~ue_ob_specifico_fix,
    "assistenza\\s*tecnic", TRUE,         NA_character_,     "TA2"
  )
}

get_fixmap_ue_ob_specifico_vuoto_regex_procedura <- function() {
  tibble::tribble(
    ~pattern,               ~ignore_case, ~oc_cod_programma, ~ue_ob_specifico_fix,
    "assistenza\\s*tecnic", TRUE,         NA_character_,     "TA2"
  )
}

match_ue_ob_specifico_vuoto_clp_puntuale <- function(df) {
  fix_map <- get_fixmap_ue_ob_specifico_vuoto_clp_puntuale()
  
  out <- df %>%
    dplyr::mutate(ue_ob_specifico_fix = NA_character_)
  
  if (nrow(fix_map) == 0) {
    return(out)
  }
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    
    cond <- out$cod_locale_progetto == rule$cod_locale_progetto
    
    if (!is.na(rule$oc_cod_programma)) {
      cond <- cond & out$oc_cod_programma == rule$oc_cod_programma
    }
    
    out$ue_ob_specifico_fix[is.na(out$ue_ob_specifico_fix) & cond] <- rule$ue_ob_specifico_fix
  }
  
  out
}

match_ue_ob_specifico_vuoto_programma <- function(df) {
  fix_map <- get_fixmap_ue_ob_specifico_vuoto_programma()
  
  out <- df %>%
    dplyr::mutate(ue_ob_specifico_fix = NA_character_)
  
  if (nrow(fix_map) == 0) {
    return(out)
  }
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    
    cond <- out$oc_cod_programma == rule$oc_cod_programma
    
    if (!is.na(rule$oc_ambito)) {
      cond <- cond & out$oc_ambito == rule$oc_ambito
    }
    
    if (!is.na(rule$ue_asse_prioritario)) {
      cond <- cond & out$ue_asse_prioritario == rule$ue_asse_prioritario
    }
    
    out$ue_ob_specifico_fix[is.na(out$ue_ob_specifico_fix) & cond] <- rule$ue_ob_specifico_fix
  }
  
  out
}

match_ue_ob_specifico_vuoto_regex_sottosettore <- function(df, progetti) {
  fix_map <- get_fixmap_ue_ob_specifico_vuoto_regex_sottosettore()
  
  out <- df %>%
    add_fix_columns_from_progetti_to_operazioni_regex_sottosettore(progetti = progetti) %>%
    dplyr::mutate(ue_ob_specifico_fix = NA_character_)
  
  if (nrow(fix_map) == 0) {
    return(out)
  }
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    
    cond <- stringr::str_detect(
      dplyr::coalesce(out$CUP_DESCR_SOTTOSETTORE, ""),
      stringr::regex(rule$pattern, ignore_case = rule$ignore_case)
    )
    
    if (!is.na(rule$oc_cod_programma)) {
      cond <- cond & out$oc_cod_programma == rule$oc_cod_programma
    }
    
    if (!is.na(rule$oc_ambito)) {
      cond <- cond & out$oc_ambito == rule$oc_ambito
    }
    
    if (!is.na(rule$ue_asse_prioritario)) {
      cond <- cond & out$ue_asse_prioritario == rule$ue_asse_prioritario
    }
    
    out$ue_ob_specifico_fix[is.na(out$ue_ob_specifico_fix) & cond] <- rule$ue_ob_specifico_fix
  }
  
  out
}

match_ue_ob_specifico_vuoto_regex_articolazione <- function(df) {
  fix_map <- get_fixmap_ue_ob_specifico_vuoto_regex_articolazione()
  
  out <- df %>%
    dplyr::mutate(ue_ob_specifico_fix = NA_character_)
  
  if (nrow(fix_map) == 0) {
    return(out)
  }
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    
    cond <- stringr::str_detect(
      dplyr::coalesce(out$ue_descr_asse_prioritario, ""),
      stringr::regex(rule$pattern, ignore_case = rule$ignore_case)
    )
    
    if (!is.na(rule$oc_cod_programma)) {
      cond <- cond & out$oc_cod_programma == rule$oc_cod_programma
    }
    
    out$ue_ob_specifico_fix[is.na(out$ue_ob_specifico_fix) & cond] <- rule$ue_ob_specifico_fix
  }
  
  out
}

match_ue_ob_specifico_vuoto_regex_procedura <- function(df) {
  fix_map <- get_fixmap_ue_ob_specifico_vuoto_regex_procedura()
  
  out <- df %>%
    dplyr::mutate(ue_ob_specifico_fix = NA_character_)
  
  if (nrow(fix_map) == 0) {
    return(out)
  }
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    
    cond <- stringr::str_detect(
      dplyr::coalesce(out$descr_proced_attivazione, ""),
      stringr::regex(rule$pattern, ignore_case = rule$ignore_case)
    )
    
    if (!is.na(rule$oc_cod_programma)) {
      cond <- cond & out$oc_cod_programma == rule$oc_cod_programma
    }
    
    out$ue_ob_specifico_fix[is.na(out$ue_ob_specifico_fix) & cond] <- rule$ue_ob_specifico_fix
  }
  
  out
}

chk_ue_ob_specifico_vuoto <- function(df, progetti, export = FALSE) {
  fun_name <- "chk_ue_ob_specifico_vuoto"
  
  base <- df %>%
    dplyr::filter(
      is_target_ambito_ue_ob_specifico(oc_ambito),
      is_vuoto_ue_ob_specifico(ue_ob_specifico)
    ) %>%
    dplyr::select(
      cod_locale_progetto,
      oc_cod_programma,
      oc_ambito,
      ue_asse_prioritario,
      ue_ob_specifico,
      ue_descr_asse_prioritario,
      descr_proced_attivazione
    )
  
  m_clp <- match_ue_ob_specifico_vuoto_clp_puntuale(base) %>%
    dplyr::transmute(
      cod_locale_progetto,
      oc_cod_programma,
      ue_fix_clp = ue_ob_specifico_fix
    )
  
  m_prog <- match_ue_ob_specifico_vuoto_programma(base) %>%
    dplyr::transmute(
      cod_locale_progetto,
      oc_cod_programma,
      ue_fix_prog = ue_ob_specifico_fix
    )
  
  m_sotto <- match_ue_ob_specifico_vuoto_regex_sottosettore(base, progetti) %>%
    dplyr::transmute(
      cod_locale_progetto,
      oc_cod_programma,
      CUP_DESCR_SOTTOSETTORE,
      ue_fix_sotto = ue_ob_specifico_fix
    )
  
  m_art <- match_ue_ob_specifico_vuoto_regex_articolazione(base) %>%
    dplyr::transmute(
      cod_locale_progetto,
      oc_cod_programma,
      ue_fix_art = ue_ob_specifico_fix
    )
  
  m_proc <- match_ue_ob_specifico_vuoto_regex_procedura(base) %>%
    dplyr::transmute(
      cod_locale_progetto,
      oc_cod_programma,
      ue_fix_proc = ue_ob_specifico_fix
    )
  
  out_detail <- base %>%
    dplyr::left_join(m_clp,  by = c("cod_locale_progetto", "oc_cod_programma")) %>%
    dplyr::left_join(m_prog, by = c("cod_locale_progetto", "oc_cod_programma")) %>%
    dplyr::left_join(m_sotto, by = c("cod_locale_progetto", "oc_cod_programma")) %>%
    dplyr::left_join(m_art,  by = c("cod_locale_progetto", "oc_cod_programma")) %>%
    dplyr::left_join(m_proc, by = c("cod_locale_progetto", "oc_cod_programma")) %>%
    dplyr::mutate(
      tipo_fix_intercettato = dplyr::case_when(
        !is.na(ue_fix_clp)   ~ "riempimento_clp_puntuale",
        !is.na(ue_fix_prog)  ~ "riempimento_programma",
        !is.na(ue_fix_sotto) ~ "riempimento_regex_sottosettore",
        !is.na(ue_fix_art)   ~ "riempimento_regex_articolazione",
        !is.na(ue_fix_proc)  ~ "riempimento_regex_procedura",
        TRUE                 ~ "riempimento_residuo_nd"
      ),
      ue_ob_specifico_fix = dplyr::case_when(
        !is.na(ue_fix_clp)   ~ ue_fix_clp,
        !is.na(ue_fix_prog)  ~ ue_fix_prog,
        !is.na(ue_fix_sotto) ~ ue_fix_sotto,
        !is.na(ue_fix_art)   ~ ue_fix_art,
        !is.na(ue_fix_proc)  ~ ue_fix_proc,
        TRUE                 ~ "ND"
      ),
      coperto_da_fix = tipo_fix_intercettato != "riempimento_residuo_nd"
    ) %>%
    dplyr::select(
      cod_locale_progetto,
      oc_cod_programma,
      oc_ambito,
      ue_asse_prioritario,
      ue_ob_specifico,
      ue_descr_asse_prioritario,
      descr_proced_attivazione,
      CUP_DESCR_SOTTOSETTORE,
      tipo_fix_intercettato,
      ue_ob_specifico_fix,
      coperto_da_fix
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      tipo_fix_intercettato,
      oc_cod_programma,
      cod_locale_progetto
    )
  
  out_summary <- out_detail %>%
    dplyr::count(
      tipo_fix_intercettato,
      coperto_da_fix,
      oc_cod_programma,
      oc_ambito,
      name = "n"
    ) %>%
    dplyr::arrange(
      coperto_da_fix,
      tipo_fix_intercettato,
      oc_cod_programma,
      oc_ambito
    )
  
  out_coperti_da_fix <- out_detail %>%
    dplyr::filter(coperto_da_fix)
  
  out_residuo_nd <- out_detail %>%
    dplyr::filter(!coperto_da_fix)
  
  out <- list(
    summary = out_summary,
    detail = out_detail,
    coperti_da_fix = out_coperti_da_fix,
    residuo_nd = out_residuo_nd
  )
  
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  
  out
}

fix_ue_ob_specifico_vuoto_clp_puntuale <- function(df) {
  matched <- match_ue_ob_specifico_vuoto_clp_puntuale(df)
  
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico_fix_tmp = matched$ue_ob_specifico_fix,
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) &
          !is.na(ue_ob_specifico_fix_tmp) ~ ue_ob_specifico_fix_tmp,
        TRUE ~ ue_ob_specifico
      )
    ) %>%
    dplyr::select(-ue_ob_specifico_fix_tmp)
  
  out
}

fix_ue_ob_specifico_vuoto_programma <- function(df) {
  matched <- match_ue_ob_specifico_vuoto_programma(df)
  
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico_fix_tmp = matched$ue_ob_specifico_fix,
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) &
          !is.na(ue_ob_specifico_fix_tmp) ~ ue_ob_specifico_fix_tmp,
        TRUE ~ ue_ob_specifico
      )
    ) %>%
    dplyr::select(-ue_ob_specifico_fix_tmp)
  
  out
}

fix_ue_ob_specifico_vuoto_regex_sottosettore <- function(df, progetti) {
  matched <- match_ue_ob_specifico_vuoto_regex_sottosettore(df, progetti)
  
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico_fix_tmp = matched$ue_ob_specifico_fix,
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) &
          !is.na(ue_ob_specifico_fix_tmp) ~ ue_ob_specifico_fix_tmp,
        TRUE ~ ue_ob_specifico
      )
    ) %>%
    dplyr::select(-ue_ob_specifico_fix_tmp)
  
  out
}

fix_ue_ob_specifico_vuoto_regex_articolazione <- function(df) {
  matched <- match_ue_ob_specifico_vuoto_regex_articolazione(df)
  
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico_fix_tmp = matched$ue_ob_specifico_fix,
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) &
          !is.na(ue_ob_specifico_fix_tmp) ~ ue_ob_specifico_fix_tmp,
        TRUE ~ ue_ob_specifico
      )
    ) %>%
    dplyr::select(-ue_ob_specifico_fix_tmp)
  
  out
}

fix_ue_ob_specifico_vuoto_regex_procedura <- function(df) {
  matched <- match_ue_ob_specifico_vuoto_regex_procedura(df)
  
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico_fix_tmp = matched$ue_ob_specifico_fix,
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) &
          !is.na(ue_ob_specifico_fix_tmp) ~ ue_ob_specifico_fix_tmp,
        TRUE ~ ue_ob_specifico
      )
    ) %>%
    dplyr::select(-ue_ob_specifico_fix_tmp)
  
  out
}

fix_ue_ob_specifico_vuoto_residuo_nd <- function(df) {
  df %>%
    dplyr::mutate(
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_ob_specifico(oc_ambito) &
          is_vuoto_ue_ob_specifico(ue_ob_specifico) ~ "ND",
        TRUE ~ ue_ob_specifico
      )
    )
}

#fix valori multipli su asse e os----
#helper condivisi

is_target_ambito_ue_levels <- function(x) {
  x %in% c("FESR", "FSE", "JTF")
}

is_multivalue_code <- function(x) {
  !is.na(x) & stringr::str_detect(x, ":::")
}

normalize_multivalue_code <- function(x) {
  dplyr::case_when(
    is.na(x) ~ x,
    stringr::str_detect(x, "^0:::") ~ stringr::str_split_fixed(x, ":::", n = 2)[, 2],
    stringr::str_detect(x, ":::") ~ stringr::str_split_fixed(x, ":::", n = 2)[, 1],
    TRUE ~ x
  )
}

export_chk_out <- function(out, fun_name, export = FALSE) {
  if (export == TRUE) {
    openxlsx::write.xlsx(
      x = out,
      file = file.path(TEMP, paste0(fun_name, ".xlsx")),
      overwrite = TRUE
    )
  }
  invisible(out)
}

# os
chk_ue_ob_specifico_multivalore <- function(df, export = FALSE) {
  fun_name <- "chk_ue_ob_specifico_multivalore"
  
  out_detail <- df %>%
    dplyr::filter(
      is_target_ambito_ue_levels(oc_ambito),
      is_multivalue_code(ue_ob_specifico)
    ) %>%
    dplyr::mutate(
      ue_ob_specifico_fix = normalize_multivalue_code(ue_ob_specifico),
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(oc_cod_programma, cod_locale_progetto)
  
  out_summary <- out_detail %>%
    dplyr::count(
      oc_cod_programma,
      oc_ambito,
      ue_ob_specifico,
      ue_ob_specifico_fix,
      name = "n"
    ) %>%
    dplyr::arrange(oc_cod_programma, ue_ob_specifico)
  
  out <- list(
    summary = out_summary,
    detail = out_detail
  )
  
  export_chk_out(out, fun_name, export)
  out
}

fix_ue_ob_specifico_multivalore <- function(df) {
  out <- df %>%
    dplyr::mutate(
      ue_ob_specifico = dplyr::case_when(
        is_target_ambito_ue_levels(oc_ambito) &
          is_multivalue_code(ue_ob_specifico) ~ normalize_multivalue_code(ue_ob_specifico),
        TRUE ~ ue_ob_specifico
      )
    )
  
  out
}


#asse

chk_ue_asse_prioritario_multivalore <- function(df, export = FALSE) {
  fun_name <- "chk_ue_asse_prioritario_multivalore"
  
  out_detail <- df %>%
    dplyr::filter(
      is_target_ambito_ue_levels(oc_ambito),
      is_multivalue_code(ue_asse_prioritario)
    ) %>%
    dplyr::mutate(
      ue_asse_prioritario_fix = normalize_multivalue_code(ue_asse_prioritario),
      coperto_da_fix = TRUE
    ) %>%
    dplyr::arrange(oc_cod_programma, cod_locale_progetto)
  
  out_summary <- out_detail %>%
    dplyr::count(
      oc_cod_programma,
      oc_ambito,
      ue_asse_prioritario,
      ue_asse_prioritario_fix,
      name = "n"
    ) %>%
    dplyr::arrange(oc_cod_programma, ue_asse_prioritario)
  
  out <- list(
    summary = out_summary,
    detail = out_detail
  )
  
  export_chk_out(out, fun_name, export)
  out
}

fix_ue_asse_prioritario_multivalore <- function(df) {
  out <- df %>%
    dplyr::mutate(
      ue_asse_prioritario = dplyr::case_when(
        is_target_ambito_ue_levels(oc_ambito) &
          is_multivalue_code(ue_asse_prioritario) ~ normalize_multivalue_code(ue_asse_prioritario),
        TRUE ~ ue_asse_prioritario
      )
    )
  
  out
}


# padding su asse e os----
get_fixmap_ue_asse_prioritario_formato_programma <- function() {
  tibble::tribble(
    ~oc_cod_programma,  ~tipo_formato, ~param,
    "2021IT16RFPR016",  "pad_left",    "4",
    "2021IT05FFPR001",  "pad_left",    "2",
    "2021IT16RFPR012",  "pad_left",    "2",
    "2021IT05SFPR014",  "prefix",      "P"
  )
}

compute_ue_asse_prioritario_formato_programma <- function(df) {
  fix_map <- get_fixmap_ue_asse_prioritario_formato_programma()
  
  fix_preview <- rep(NA_character_, nrow(df))
  
  for (i in seq_len(nrow(fix_map))) {
    rule <- fix_map[i, ]
    cond <- df$oc_cod_programma == rule$oc_cod_programma
    
    if (rule$tipo_formato[[1]] == "pad_left") {
      val <- stringr::str_pad(df$ue_asse_prioritario, width = as.integer(rule$param[[1]]), pad = "0")
    } else if (rule$tipo_formato[[1]] == "prefix") {
      val <- dplyr::if_else(
        stringr::str_starts(dplyr::coalesce(df$ue_asse_prioritario, ""), rule$param[[1]]),
        df$ue_asse_prioritario,
        paste0(rule$param[[1]], df$ue_asse_prioritario)
      )
    } else {
      val <- df$ue_asse_prioritario
    }
    
    fix_preview[cond] <- val[cond]
  }
  
  fix_preview
}

chk_ue_asse_prioritario_formato_programma <- function(df, export = FALSE) {
  fun_name <- "chk_ue_asse_prioritario_formato_programma"
  
  out_detail <- df %>%
    dplyr::filter(is_target_ambito_ue_levels(oc_ambito)) %>%
    dplyr::mutate(
      ue_asse_prioritario_fix = compute_ue_asse_prioritario_formato_programma(.),
      coperto_da_fix = !is.na(ue_asse_prioritario_fix)
    ) %>%
    dplyr::filter(coperto_da_fix, ue_asse_prioritario != ue_asse_prioritario_fix) %>%
    dplyr::arrange(oc_cod_programma, cod_locale_progetto)
  
  out_summary <- out_detail %>%
    dplyr::count(
      oc_cod_programma,
      ue_asse_prioritario,
      ue_asse_prioritario_fix,
      name = "n"
    ) %>%
    dplyr::arrange(oc_cod_programma, ue_asse_prioritario)
  
  out <- list(
    summary = out_summary,
    detail = out_detail
  )
  
  export_chk_out(out, fun_name, export)
  out
}

fix_ue_asse_prioritario_formato_programma <- function(df) {
  out <- df %>%
    dplyr::mutate(
      ue_asse_prioritario_fix_tmp = compute_ue_asse_prioritario_formato_programma(.),
      ue_asse_prioritario = dplyr::case_when(
        is_target_ambito_ue_levels(oc_ambito) &
          !is.na(ue_asse_prioritario_fix_tmp) ~ ue_asse_prioritario_fix_tmp,
        TRUE ~ ue_asse_prioritario
      )
    ) %>%
    dplyr::select(-ue_asse_prioritario_fix_tmp)
  
  out
}