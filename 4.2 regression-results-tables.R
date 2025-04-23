#### ---- Set up ---- ####

# * load libraries ----
library( dplyr )
library( purrr )
library( tidyr )
library( stringr )
library( forcats )
library( openxlsx )

# * set file paths ----
path_rds <- here::here( "outputs/R-regression-results//" )
path_out <- here::here( "outputs/tables" )

# source funtions
source( "0.0-functions.R" )

# * load results ----
fileNames_all <- list.files( path_rds )
fileNames_read <- fileNames_all[ grep( "MAIN|EMM|SA", fileNames_all ) ]
fileNames_read <- fileNames_read[ !grepl( "SA_heterogeneity", fileNames_read ) ] # remove heterogeneity sensitivity analyses

resList <- lapply( paste0( path_rds, fileNames_read ), readRDS )
names( resList ) <- sub( "_\\d{4}-\\d{2}-\\d{2}\\.rds$", "", sub( "regression-model-results_?", "", fileNames_read ) )

extractRes <- function( lst, parent_names = NULL ) {
  if ( "regModInfo" %in% names( lst ) ) {
    tibble(
      source = paste( parent_names, collapse = " > " ),
      regModInfo = list( lst$regModInfo ),
      regModCoeffs = list( lst$regModCoeffs ),
      regModPRs = list( lst$regModPRs ),
      predRes = list( lst$predRes ),
      predDiff = list( lst$predDiff ),
    ) %>%
      separate( source, into = c( "source", "stigmaVar", "outcomeVar" ), sep = " > ", 
                fill = "right", remove = TRUE ) %>%
      mutate( resultsType = case_when( grepl( "MAIN", source ) ~ "MAIN",
                                       grepl( "EMM", source ) ~ "EMM",
                                       grepl( "SA", source ) ~ "SA",
                                       TRUE ~ "Other" ),
              emmVar = ifelse( grepl( "EMM_", source ),   # Extract EMM variable name
                               str_extract( source, "(?<=EMM_inter-)[^ ]+" ),
                               NA_character_ ) ) %>%
      relocate( resultsType, .after = "source" ) %>%
      relocate( emmVar, .after = "outcomeVar" )
    
  } else {
    lst %>%
      imap_dfr( ~ extractRes( .x, c( parent_names, .y ) ), .id = NULL )
  }
}

# Apply to resList
resDf <- extractRes( resList )

# print( resDf )

#### ---- Tables ---- ####

# Main
mainPRs <- resDf %>% 
  select( resultsType, stigmaVar, outcomeVar, regModPRs ) %>% 
  unnest( regModPRs ) %>% 
  relocate( analysis, .before = "Estimate" ) %>% 
  data.frame() %>% 
  mutate( stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
          outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ),
          resultsType = factor( resultsType, levels = c( "MAIN", "EMM", "SA" ) ),
          Estimate_pr10 = exp( log( Estimate ) * 0.1 ),
          lci_pr10 = exp( log( lci ) * 0.1 ),
          uci_pr10 = exp( log( uci ) * 0.1 ) ) %>% 
  arrange( stigmaVar, outcomeVar, resultsType )

# Main
# mainRDs <- 
  # resDf %>%
  # select( resultsType, stigmaVar, outcomeVar, predRes ) %>%
  # unnest( predRes ) %>%
#   relocate( analysis, .before = "Estimate" ) %>% 
#   data.frame() %>% 
#   mutate( stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
#           outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ),
#           resultsType = factor( resultsType, levels = c( "MAIN", "EMM", "SA" ) ),
#           Estimate_pr10 = exp( log( Estimate ) * 0.1 ),
#           lci_pr10 = exp( log( lci ) * 0.1 ),
#           uci_pr10 = exp( log( uci ) * 0.1 ) ) %>% 
#   arrange( stigmaVar, outcomeVar, resultsType )

  

# EMM
emmRes <- resDf %>%
  filter( grepl( "MAIN|EMM", resultsType ) ) %>%
  mutate( regModCoeffs = map2( regModCoeffs, emmVar,
                              ~ filter( .x, grepl( "exposure", variable ) | grepl( .y, variable ) ) ),
          regModPRs = map2( regModPRs, emmVar,
                               ~ filter( .x, grepl( "exposure", variable ) | grepl( .y, variable ) ) ) )
emmCoefs <- emmRes %>%
  select( resultsType, stigmaVar, outcomeVar, emmVar, regModCoeffs ) %>%
  unnest( regModCoeffs ) %>%
  relocate( analysis, .before = "Estimate" ) %>%
  data.frame( ) %>%
  mutate( stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
          outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ),
          resultsType = factor( resultsType, levels = c( "MAIN", "EMM", "SA" ) ),
          emmVar = factor( emmVar, levels = c( "reg_alt", "sex", "age_dichot", "restype", "wealth_dichot", "edu_dichot" ) ) ) %>%
  arrange( stigmaVar, outcomeVar, resultsType, emmVar )

emmPRs <- emmRes %>%
  select( resultsType, stigmaVar, outcomeVar, emmVar, regModPRs ) %>%
  unnest( regModPRs ) %>%
  data.frame( ) %>%
  mutate( stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
          outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ),
          resultsType = factor( resultsType, levels = c( "MAIN", "EMM", "SA" ) ),
          emmVar = factor( emmVar, levels = c( "reg_alt", "sex", "age_dichot", "restype", "wealth_dichot", "edu_dichot" ) ) ) %>%
  arrange( stigmaVar, outcomeVar, resultsType, emmVar )

emmModInfo <- emmRes %>%
  select( resultsType, stigmaVar, outcomeVar, emmVar, regModInfo ) %>%
  unnest( regModInfo ) %>%
  data.frame( ) %>%
  group_by( stigmaVar, outcomeVar, analysis ) %>%
  mutate( qlr = ifelse( resultsType == "MAIN", NA, 2 * ( quasiLik - quasiLik[ resultsType == "MAIN" ] ) ), # quasi-likelihood ratio test
          qlr_dof = ifelse( resultsType == "MAIN", NA, nCoeffs - nCoeffs[ resultsType == "MAIN" ] ) ) %>% 
  ungroup( ) %>%
  mutate( qlr_pValue = ifelse( resultsType == "MAIN", NA, 1 - pchisq( qlr, qlr_dof ) ), # compute p-value using X2
          stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
          outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ),
          resultsType = factor( resultsType, levels = c( "MAIN", "EMM", "SA" ) ),
          emmVar = factor( emmVar, levels = c( "reg_alt", "sex", "age_dichot", "restype", "wealth_dichot", "edu_dichot" ) ),
          across( c( QIC, quasiLik ), ~ round( .x,  0 ) ),
          qlr_pValue = sprintf( "%.3f", qlr_pValue ) ) %>%
  arrange( stigmaVar, outcomeVar, desc( analysis ), resultsType, emmVar ) %>%
  relocate( c( QIC, quasiLik, qlr_pValue, qlr, qlr_dof ), .after = "analysis" )

# emmModInfo %>% filter( analysis == "adjusted", qlr_pValue < 0.2 )

# SA ---- HERE ----
saPRs <- resDf %>%
  filter( grepl( "MAIN|SA", resultsType ) ) %>%
  mutate( regModPRs = map2( regModPRs, emmVar,
                            ~ filter( .x, grepl( "exposure", variable ) | grepl( .y, variable ) ) ) ) %>%
  select( source, resultsType, stigmaVar, outcomeVar, regModPRs ) %>%
  unnest( regModPRs ) %>%
  data.frame( ) %>%
  mutate( stigmaVar = factor( stigmaVar, levels = c( "stig_avg", "stig_perc_avg", "social_judge_avg", "ant_exp_stig" ) ),
          outcomeVar = factor( outcomeVar, levels = c( "test12m", "arv", "vlsup" ) ) ) %>%
  arrange( source, stigmaVar, outcomeVar )

# saModInfo <- resDf %>%
#   filter( grepl( "MAIN|SA", resultsType ) ) %>%
#   select( source, resultsType, stigmaVar, outcomeVar, emmVar, regModInfo ) %>%
#   unnest( regModInfo ) %>%
#   data.frame( )


#### ---- Save results files ----

# save_resLists_to_excel( resLists = list(
#                                          emmCoefs = emmCoefs,
#                                          emmPRs = emmPRs,
#                                          emmModInfo = emmModInfo,
#                                          saPRs = saPRs ),
#                         save_file_path = path_out )
