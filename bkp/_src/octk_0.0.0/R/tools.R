

# ----------------------------------------------------------------------------------- #




# ----------------------------------------------------------------------------------- #
# verifica match
chk_match <- function(df_left, df_right, id) {
  # costruita in "match_patti.R"
  # DEBUG:
  # id <- "CUP"
  # df_left <- export
  # df_right <- appo
  # temp <- chk_multi(df_left = export, df_right = appo, id = "CUP")
  
  chk_nums <- function(df, id) {
    # require("glue")
    # DEBUG:
    # df <- df_left
    # df <- df_right
    out <- list(obs = df %>% 
                  nrow(), # numero di osservazioni totale
                
                obs_na = df %>% 
                  select(id) %>% 
                  filter(is.na(.)) %>% 
                  nrow(), # numero di obs con id missing (compreso in "obs" e complemento di "obs_n")
                
                obs_n = df %>% 
                  select(id) %>% 
                  filter(!(is.na(.))) %>% 
                  nrow(), # numero di obs con id non missing (compreso in "obs")
                
                id_n = df %>% 
                  select(id) %>% 
                  distinct() %>% 
                  filter(!(is.na(.))) %>% 
                  nrow, # numero di id univoci e non missing (da confrontare con "obs_n")
                
                obs_m = df %>% 
                  # filter(!(is.na(id))) %>% # QUESTO NON FUNZIONA PER NON STANDARD EVALUATON! >>> vignette("programming")
                  # filter_(paste0("!is.na(", id, ")")) %>%
                  filter_(glue::glue("!is.na({id})")) %>%
                  group_by_(id) %>% 
                  summarise(N = n()) %>%
                  filter(N > 1) %>%
                  summarise(S = sum(N)) %>%
                  .$S, # numero di obs per id molteplici (compreso in "obs_n")
                
                id_m = df %>% 
                  filter_(glue::glue("!is.na({id})")) %>%
                  group_by_(id) %>% 
                  summarise(N = n()) %>%
                  filter(N > 1) %>%
                  nrow() # numero di id multeplici (compreso in "id_n")
                
    ) 
    return(out)
  }
  
  chk <- list()
  chk[["left"]] <- chk_nums(df = df_left, id = id)
  chk[["right"]] <- chk_nums(df = df_right, id = id)
  
  chk[["inner"]] <- chk_nums(df = df_left %>% 
                               inner_join(df_right, by = id), 
                             id = id)
  
  chk[["semi_left"]] <- chk_nums(df = df_left %>% 
                                   semi_join(df_right, by = id), 
                                 id = id)
  
  chk[["anti_left"]] <- chk_nums(df = df_left %>% 
                                   anti_join(df_right, by = id), 
                                 id = id)
  
  chk[["semi_right"]] <- chk_nums(df = df_right %>% 
                                    semi_join(df_left, by = id), 
                                  id = id)
  
  chk[["anti_right"]] <- chk_nums(df = df_right %>% 
                                    anti_join(df_left, by = id), 
                                  id = id)
  
  out <- bind_rows(chk, .id = "area") # converte in dataframe
  return(out)
  
}

# come faccio gli stessi chk con sum su altra variabile (es. FTP)? e per due variabili diverse in left e right? 
# come faccio gli stessi chk con count su altra variabile?



