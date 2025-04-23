# ----------------------------------------------- #
# ---- 1. Describe demographics per exposure ----
# ----------------------------------------------- #

fn_demographics <- function( df, varlist, explist ) { 
  
  # select variables needed in the fn & manipulate for analysis
  dat <- df %>% 
    select( all_of( c( explist, varlist ) ), hiv_self, psu_u, surveyid, std_indweight, std_hivweight, stratum ) %>% 
    mutate( across( all_of( c( explist, varlist ) ), ~ as.factor( .x ) ), # ensure all are factor variables
            across( where( is.factor ), ~ forcats::fct_explicit_na( .x, na_level = "Missing" ) ) )
  
  # for each variable in varlist, remove:
  # people with missing exposure
  # surveys that did not measure the demographic variable (e.g. wealth in sabsssm)
  # people with missing weights (including HIV weights depending on exp) or stratum
  # people who did not self-report living with HIV (for "med_stig" and "stig_anticip" only)
  
  resList <- lapply( explist, function ( x ) {
    
    lapply( varlist, function( y ) {
      
      conditionHIV <- ifelse( x %in% c( "med_stig", "stig_anticip" ), " & hiv_self == 1 & ! is.na ( std_hivweight )",
                              ifelse( y == "hivstatus", " & ! is.na ( std_hivweight )", "" ) ) # also need hiv weights for measuring HIV status, regardless of exposure
      
      condition <- paste0( "! .data[[ x ]] == 'Missing' & ! all( .data[[ y ]] == 'Missing' ) & ! is.na ( std_indweight ) & ! is.na ( stratum )", conditionHIV )
      
      dat_ <- dat %>%
        group_by( surveyid ) %>%
        filter( !! parse_expr( condition ) ) %>%
        ungroup( ) %>%
        select( - setdiff( explist, x ),
                - setdiff( varlist, y ) ) %>%
        mutate( exposure = x,
                variable = y,
                across( where( is.factor ), droplevels ) ) %>%
        rename( exposureLevel = !!x,
                variableLevel = !!y ) %>%
        relocate( exposure, .before = everything() ) %>%
        relocate( variable, .before = variableLevel )
      
      wgt <- ifelse( x %in% c( "med_stig", "stig_anticip" ) | y == "hivstatus", "std_hivweight", "std_indweight" )
      
      # set the survey design
      des <- svydesign( ids = ~ psu_u , 
                        data = dat_, 
                        strata = ~ stratum, 
                        weights = as.formula( paste( "~", wgt ) ), 
                        nest = TRUE )
      
      # get weighted counts and proportions
      formula <- as.formula( "~ variableLevel + exposureLevel" )
      
      res <- as.data.frame( xtabs( formula = formula, data = dat_, addNA = TRUE ) ) %>%
        rename( n = Freq )
      
      res <- as.data.frame( prop.table( svytable( formula, design = des ), margin = 2 ) ) %>%
        rename( prop = Freq ) %>%
        full_join( res, by = c( "variableLevel", "exposureLevel" ) ) %>%
        pivot_wider( names_from = exposureLevel,
                     values_from = c( n, prop ),
                     names_prefix = "exposure_" )
      
      total_row <- res %>%
        summarise( across( starts_with( "n_" ), ~ sum( .x, na.rm = TRUE ) ) ) %>%
        mutate( variableLevel = "Total" )
      
      res <- full_join( res, total_row, by = c( "n_exposure_0", "n_exposure_1", "variableLevel" ) ) %>%
        mutate( across( starts_with( "n_" ), ~ scales::comma( .x ) ),
                across( starts_with( "prop_" ), ~ scales::percent( .x, accuracy = 0.1 ) ) ) %>%
        rename_with( ~ gsub( "^prop_", "perc_", .x ), starts_with( "prop_" ) ) %>%
        mutate( exposure = x,
                variable = y,
                n_surveys = length( unique( dat_$surveyid ) ) ) %>%
        relocate( c( exposure, variable, n_surveys ), .before = everything() )
      
    } ) %>%
      bind_rows()
    
  } ) %>%
    set_names( explist )
  
  return( resList )
  
}


# --------------------------------------------------- #
# -------- 2. Calculate prevalence of stigma  -------
# - ( overall, by survey ID, region, arv, and vls ) - #
# --------------------------------------------------- #

fn_expPrevalence <- function( df, explist ) { 
  
  # specify strata for analyses
  resStrata <- c( "overall", "surveyid", "reg_alt", "arv", "vlsup" )
  
  # select variables needed in the fn & manipulate for analysis
  dat <- df %>%
    select( all_of( explist ), hivstatus, psu_u, resStrata[ ! resStrata %in% "overall" ], 
            std_indweight, std_hivweight, stratum ) %>%
    mutate( across( all_of( c( explist, resStrata[ ! resStrata %in% "overall" ] ) ), ~ as.factor( .x ) ), # ensure all are factor variables
            across( where( is.factor ), ~ forcats::fct_explicit_na( .x, na_level = "Missing" ) ) )
  
  # for each variable in explist, remove:
  # surveys that did not measure the variable
  # individuals with missing weights (including HIV weights depending on exp) or stratum
  resList <- lapply( explist, function( x ) {
    
    conditionHIV <- ifelse( x %in% c( "med_stig", "stig_anticip", "ant_exp_stig" ), " & hivstatus == 1 & ! is.na( std_hivweight )", " & ! is.na( std_indweight )" )
    condition <- paste0( "! all( .data[[ x ]] == 'Missing' ) & ! is.na( stratum )", conditionHIV )
    
    dat_ <- dat %>%
      group_by( surveyid ) %>%
      filter( !! parse_expr( condition ) ) %>%
      ungroup( ) %>%
      select( - setdiff( explist, x ) ) %>%
      mutate( across( where( is.factor ), droplevels ) ) %>%
      rename( exposureLevel = !!x )
    
    # set the correct set of weights to include
    wgt <- ifelse( x %in% c( "med_stig", "stig_anticip", "ant_exp_stig" ), "std_hivweight", "std_indweight" )
    
    # set the survey design
    des <- svydesign( ids = ~ psu_u,
                      data = dat_,
                      strata = ~ stratum,
                      weights = as.formula( paste( "~", wgt ) ),
                      nest = TRUE )
    
    # get weighted counts and proportions overall and for each specified strata
    if( ! x %in% c( "med_stig", "stig_anticip", "ant_exp_stig" ) ) { 
      
      resStrata_ <- resStrata[ ! resStrata %in% c( "arv", "vlsup" ) ] # only include ART outcomes for stigma measures among PLHIV
      
    } else { 
      resStrata_ <- resStrata 
      
    }
    
    res <- lapply( resStrata_, function( y ) { 
      
      if( y == "overall" ) {
        
        formula <- as.formula( "~ exposureLevel" )
        margins <- NULL
        res_prop <- as.data.frame( prop.table( svytable( formula, design = des ) ) )
        
      } else {
        
        dat_ <- dat_ %>%
          group_by( surveyid ) %>% 
          filter( ! all( .data[[ y ]] == 'Missing' ) ) %>% # ensure survey measured the strata variable
          ungroup() %>%
          mutate( across( where( is.factor ), droplevels ) ) 
        
        formula <- as.formula( paste( "~ exposureLevel +", y ) )
        res_prop <- as.data.frame( prop.table( svytable( formula, design = des ), margin = 2 ) )
        
      }
      
      res <- as.data.frame( xtabs( formula = formula, data = dat_, addNA = TRUE ) ) %>%
        rename( n = Freq )
      
      res <- res_prop %>%
        rename( prop = Freq ) %>% 
        full_join( res, by = intersect( names( res_prop ), names( res ) ) ) %>%
        pivot_wider( names_from = exposureLevel,
                     values_from = c( n, prop ),
                     names_prefix = "exposure_" ) %>%
        mutate( strataType = y )
      
      if( y == "overall" ){
        
        res <- res %>%
          mutate( strataLevel = NA_character_ )
        
      } else {
        
        res <- res %>%
          rename( strataLevel = !!y )
        
      }
    } ) %>% 
      bind_rows() %>%
      mutate( exposure = x, 
              across( starts_with( "n_" ), ~ scales::comma( .x ) ),
              across( starts_with( "prop_" ), ~ scales::percent( .x, accuracy = 0.1 ) ) ) %>%
      rename_with( ~ gsub( "^prop_", "perc_", .x ), starts_with( "prop_" ) ) %>%
      relocate( c( exposure, strataType, strataLevel ), .before = everything() )
    
  } ) %>%
    set_names( explist )
  
  return( resList )
  
}


# --------------------------------------------------------------------------- #
# ------- 3. Calculate summary statistics for community-level stigma  -------
# --------- ( overall, by survey ID, region, testing, arv, and vls ) -------- #
# --------------------------------------------------------------------------- #

fn_commExpSummary <- function( df, explist ) {
  
  # set option for svymean variance estimation when only one stratum
  options( survey.lonely.psu = "adjust" ) 
  
  # specify strata for analyses
  resStrata <- c( "overall", "surveyid", "reg_alt", "test12m", "arv", "vlsup" ) 
  
  # select variables needed in the fn & manipulate for analysis
  dat <- df %>%
    select( all_of( explist ), hivstatus, hiv_self, psu_u, resStrata[ ! resStrata %in% "overall" ], 
            std_indweight, std_hivweight, stratum ) %>%
    mutate( across( all_of( explist ), ~ as.numeric( .x ) ), # ensure all exposures are numeric variables
            across( all_of( resStrata[ ! resStrata %in% "overall" ] ), ~ as.factor( .x ) ), # ensure all strata/outcomes are factor variables
            across( where( is.factor ), ~ forcats::fct_explicit_na( .x, na_level = "Missing" ) ) )
  
  # for each variable in explist, remove:
  # surveys that did not measure the variable
  # individuals with missing weights (including HIV weights depending on exp) or stratum
  resList <- lapply( explist, function( x ) {
    
    lapply( resStrata, function( y ) {
      
      conditionHIV <- ifelse( y %in% c( "arv", "vlsup" ), " & hivstatus == 1 & ! is.na( std_hivweight )", " & ! is.na( std_indweight )" ) # hiv_self == 1 &
      condition <- paste0( "! all( is.na( .data[[ x ]] ) ) & ! is.na( stratum )", conditionHIV )
      
      dat_ <- dat %>%
        group_by( surveyid ) %>%
        filter( !! parse_expr( condition ) ) %>%
        ungroup( ) %>%
        select( - setdiff( explist, x ) ) %>%
        mutate( across( where( is.factor ), droplevels ) ) %>%
        rename( exposureLevel = !!x )
      
      # set the correct set of weights to include
      wgt <- ifelse( y %in% c( "arv", "vlsup" ), "std_hivweight", "std_indweight" )
      
      # set the survey design
      des <- svydesign( ids = ~ psu_u ,
                        data = dat_,
                        strata = ~ stratum,
                        weights = as.formula( paste( "~", wgt ) ),
                        nest = TRUE )
      
      # get unweighted and weighted counts and proportions overall and for each specified strata
      if( y == "overall" ) {
        
        res_noWgt <- dat_ %>%
          summarize( n = n(),
                     mean_noWgt = mean( exposureLevel, na.rm = T ),
                     sd_mean_noWgt = sqrt( var( exposureLevel, na.rm = T ) ),
                     q1_noWgt = quantile( exposureLevel, probs = 0.25, na.rm = T ),
                     median_noWgt = median( exposureLevel, na.rm = T ),
                     q3_noWgt = quantile( exposureLevel, probs = 0.75, na.rm = T ),
                     min_noWgt = min( exposureLevel, na.rm = T ),
                     max_noWgt = max( exposureLevel, na.rm = T ) )
        
        resMean_wgt <- svymean( ~ exposureLevel, design = des, na.rm = TRUE, keep.var = TRUE )
        resMedian_wgt <- svyquantile( ~ exposureLevel, design = des, quantiles = c( 0.25, 0.5, 0.75 ), na.rm = TRUE, ci = TRUE )
        
        res_wgt <- data.frame( mean_wgt = unname( coef( resMean_wgt ) ),
                               se_mean_wgt = unname( SE( resMean_wgt ) ),
                               q1_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.25" ] ),
                               median_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.5" ] ),
                               q3_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.75" ] ),
                               se_q1_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.25" ] ),
                               se_median_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.5" ] ),
                               se_q3_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.75" ] ) ) %>%
          mutate( strataLevel = NA_character_ )
        
      } else {
        
        dat_ <- dat_ %>%
          group_by( surveyid ) %>% 
          filter( ! all( .data[[ y ]] == "Missing" ) ) %>% # ensure survey measured the strata variable
          ungroup() %>%
          mutate( across( where( is.factor ), droplevels ) ) 
        
        res_noWgt <- dat_ %>% 
          group_by( .data[[ y ]] ) %>% 
          summarize( n = n(),
                     mean_noWgt = mean( exposureLevel, na.rm = T ),
                     sd_mean_noWgt = sqrt( var( exposureLevel, na.rm = T ) ),
                     q1_noWgt = quantile( exposureLevel, probs = 0.25, na.rm = T ),
                     median_noWgt = median( exposureLevel, na.rm = T ),
                     q3_noWgt = quantile( exposureLevel, probs = 0.75, na.rm = T ),
                     min_noWgt = min( exposureLevel, na.rm = T ),
                     max_noWgt = max( exposureLevel, na.rm = T ) ) %>%
          ungroup( ) %>%
          rename( strataLevel = !!y )
        
        resMean_wgt <- svyby( ~ exposureLevel, by = as.formula( paste( "~", y ) ), design = des, FUN = svymean, na.rm = TRUE, keep.var = TRUE ) %>%
          rename( strataLevel = !!y ) %>%
          rename( mean_wgt = exposureLevel,
                  se_mean_wgt = se )
        
        resMedian_wgt <- svyby( ~ exposureLevel, by = as.formula( paste( "~", y ) ), design = des, FUN = svyquantile, quantiles = c( 0.25, 0.5, 0.75 ), na.rm = TRUE, ci = TRUE ) %>%
          rename( strataLevel = !!y ) %>%
          rename_with( ~ str_replace_all( ., c( "\\.0.25" = "q1", "\\.0.5"  = "median", "\\.0.75" = "q3", "\\." = "_" ) ) ) %>%
          rename_with( ~ str_replace( ., "exposureLevel", "" ) ) %>%
          rename_with( ~ paste0( ., "_wgt" ), - strataLevel )
        
        res_wgt <- full_join( resMean_wgt, resMedian_wgt, by = intersect( names( resMean_wgt ), names( resMedian_wgt ) )  )
        
      }
      
      res <- full_join( res_noWgt, res_wgt, by = intersect( names( res_noWgt ), names( res_wgt ) ) ) %>%
        mutate( strataType = y )
      
    } ) %>% 
      bind_rows() %>%
      mutate( exposure = x, 
              across( starts_with( "n" ), ~ scales::comma( .x ) ),
              across( where( is.numeric ) & ! starts_with( "n" ), ~ scales::percent( .x, accuracy = 0.1 ) ) ) %>%
      relocate( c( exposure, strataType, strataLevel, n ), .before = everything() )
    
  } ) %>%
    set_names( explist )
  
  return( resList )
  
}


# --------------------------------------------------- #
# ---- 4. Calculate prevalence of each outcome by stigma  ----
# --------------------------------------------------- #

fn_outPrev <- function( df, outlist ) { 
  
  # specify strata for analyses
  resStrata <- c( "stig_avg", "stig_perc_avg", "social_judge_avg", "stig_anticip", "med_stig" )
  
  # select variables needed in the fn & manipulate for analysis
  dat <- df %>%
    select( all_of( outlist ), surveyid, hivstatus, incidence, psu_u, resStrata[ ! resStrata %in% "overall" ], 
            std_indweight, std_hivweight, stratum ) 
  
  # for each variable in outlist, remove:
  # surveys that did not measure the variable
  # individuals with missing weights (including HIV weights depending on exp) or stratum
  resList <- lapply( outlist, function( x ) {
    
    conditionHIV <- ifelse( x %in% c( "arv", "vlsup" ), " & hivstatus == 1 & ! is.na( std_hivweight ) & incidence == 0 | ! all( .data[[ x ]] == 'Missing' ) & ! is.na( stratum ) & hivstatus == 1 & ! is.na( std_hivweight ) & is.na(incidence)", " & ! is.na( std_indweight )" )
    condition <- paste0( "! all( .data[[ x ]] == 'Missing' ) & ! is.na( stratum )", conditionHIV )
    
    dat_ <- dat %>%
      group_by( surveyid ) %>%
      filter( !! parse_expr( condition ) ) %>%
      ungroup( ) %>%
      mutate(
        # Calculate medians of community exposures
        stig_avg_median = median( stig_avg, na.rm = TRUE ),
        stig_perc_avg_median = median( stig_perc_avg, na.rm = TRUE ),
        social_judge_avg_median = median( social_judge_avg, na.rm = TRUE ),
        
        # Change community stigma variable to binary to represent if participant lived in psu above/below the median
        stig_avg = case_when(
          stig_avg >= stig_avg_median ~ paste("≥", round( stig_avg_median, 2 ) ),
          stig_avg < stig_avg_median ~ paste("<", round( stig_avg_median, 2 ) ),
          is.na(stig_avg) ~ NA_character_
        ),
        stig_perc_avg = case_when(
          stig_perc_avg >= stig_perc_avg_median ~ paste("≥", round( stig_perc_avg_median, 2 ) ),
          stig_perc_avg < stig_perc_avg_median ~ paste("<", round( stig_perc_avg_median, 2 ) ),
          is.na(stig_perc_avg) ~ NA_character_
        ),
        social_judge_avg = case_when(
          social_judge_avg >= social_judge_avg_median ~ paste("≥", round( social_judge_avg_median, 2 ) ),
          social_judge_avg < social_judge_avg_median ~ paste("<", round( social_judge_avg_median, 2 ) ),
          is.na(social_judge_avg) ~ NA_character_
        )
        
      ) %>% 
      mutate( across( all_of( c( outlist, resStrata[ ! resStrata %in% "overall" ] ) ), ~ as.factor( .x ) ), # ensure all are factor variables
              across( where( is.factor ), ~ forcats::fct_explicit_na( .x, na_level = "Missing" ) ) ) %>% 
      select( - setdiff( outlist, x ) ) %>%
      mutate( across( where( is.factor ), droplevels ) ) %>%
      rename( outcomeLevel = !!x )
    
    # set the correct set of weights to include
    wgt <- ifelse( x %in% c( "arv", "vlsup" ), "std_hivweight", "std_indweight" )
    
    # set the survey design
    des <- svydesign( ids = ~ psu_u,
                      data = dat_,
                      strata = ~ stratum,
                      weights = as.formula( paste( "~", wgt ) ),
                      nest = TRUE )
    
    # get weighted counts and proportions overall and for each specified strata
    
    resStrata_ <- resStrata
    
    res <- lapply(resStrata_, function( y ) { 
      
      dat_y <- dat_ %>%
        group_by( surveyid ) %>% 
        filter( ! all( .data[[ y ]] == 'Missing' ) ) %>% # keep only surveys with valid data for this stratum
        ungroup() %>%
        mutate( across( where( is.factor ), droplevels ) )
      
      # Recalculate design for subset if needed
      des_y <- svydesign(
        ids = ~ psu_u,
        data = dat_y,
        strata = ~ stratum,
        weights = as.formula( paste( "~", wgt ) ),
        nest = TRUE
      )
      
      # Total number of surveys that measured this variable
      nSurveys_y <- n_distinct(dat_y$surveyid)
      
      # Create table of proportions
      formula <- as.formula( paste( "~ outcomeLevel +", y ) )
      res_prop <- as.data.frame( prop.table( svytable( formula, design = des_y ), margin = 2 ) )
      
      # Create table of counts
      res_counts <- as.data.frame( xtabs( formula = formula, data = dat_y, addNA = TRUE ) ) %>%
        rename( n = Freq )
      
      # Join prop and count tables, and pivot
      res <- res_prop %>%
        rename( prop = Freq ) %>%
        full_join( res_counts, by = intersect(names(res_prop), names(res_counts)) ) %>%
        pivot_wider(
          names_from = outcomeLevel,
          values_from = c(n, prop),
          names_prefix = "outcome_"
        )
      
      # Get number of PSUs per strata level
      strata_psu_counts <- dat_y %>%
        group_by(strataLevel = .data[[y]]) %>%
        summarise(nPSU = n_distinct(psu_u), .groups = "drop")
      
      # Add metadata columns
      res <- res %>%
        mutate(
          exposure = y,
          strataLevel = .data[[y]],
          n_outcome_Total = n_outcome_0 + n_outcome_1 + n_outcome_Missing,
          nSurveys = nSurveys_y
        ) %>%
        left_join(strata_psu_counts, by = "strataLevel") %>%
        relocate( c( exposure, strataLevel, n_outcome_Total, nSurveys, nPSU ), .before = everything( ) )
      
      return(res)
    } ) %>% 
      bind_rows() %>%
      mutate( across( starts_with( "n_" ), ~ scales::comma( .x ) ),
              across( starts_with( "prop_" ), ~ scales::percent( .x, accuracy = 0.1 ) ) ) %>%
      rename_with( ~ gsub( "^prop_", "perc_", .x ), starts_with( "prop_" ) )
    
  } ) %>%
    set_names( outlist )
  
  return( resList )
  
}

# --------------------------------------------------- #
# ---- 5. Calculate summary statistics for community-level exposures by outcome  ----
# --------------------------------------------------- #

fn_outSummary <- function( df, outlist ) { 
  
  # specify strata for analyses
  resStrata <- c( "stig_avg", "stig_perc_avg", "social_judge_avg" )
  
  # select variables needed in the fn & manipulate for analysis
  dat <- df %>%
    select( all_of( outlist ), surveyid, hivstatus, incidence, psu_u, resStrata[ ! resStrata %in% "overall" ], 
            std_indweight, std_hivweight, stratum ) 
  
  # for each variable in outlist, remove:
  # surveys that did not measure the variable
  # individuals with missing weights (including HIV weights depending on exp) or stratum
  resList <- lapply( outlist, function( x ) {
    
    conditionHIV <- ifelse( x %in% c( "arv", "vlsup" ), " & hivstatus == 1 & ! is.na( std_hivweight ) & incidence == 0 | ! all( .data[[ x ]] == 'Missing' ) & ! is.na( stratum ) & hivstatus == 1 & ! is.na( std_hivweight ) & is.na(incidence)", " & ! is.na( std_indweight )" )
    condition <- paste0( "! all( .data[[ x ]] == 'Missing' ) & ! is.na( stratum )", conditionHIV )
    
    dat_ <- dat %>%
      group_by( surveyid ) %>%
      filter( !! parse_expr( condition ) ) %>%
      ungroup( ) %>% 
      mutate( across( all_of( c( outlist ) ), ~ as.factor( .x ) ), # ensure all are factor variables
              across( where( is.factor ), ~ forcats::fct_explicit_na( .x, na_level = "Missing" ) ) ) %>% 
      select( - setdiff( outlist, x ) ) %>%
      mutate( across( where( is.factor ), droplevels ) ) %>%
      rename( outcomeLevel = !!x )
    
    # set the correct set of weights to include
    wgt <- ifelse( x %in% c( "arv", "vlsup" ), "std_hivweight", "std_indweight" )
    
    # set the survey design
    options(survey.lonely.psu = "remove") # remove strata with only a single PSU
    
    des <- svydesign( ids = ~ psu_u,
                      data = dat_,
                      strata = ~ stratum,
                      weights = as.formula( paste( "~", wgt ) ),
                      nest = TRUE )
    
    # get weighted counts and proportions overall and for each specified strata
    # get unweighted and weighted counts and proportions overall and for each specified strata
    
    resSummary <- lapply( resStrata, function( y ) { 
      
      dat_ <- dat_ %>%
        group_by( surveyid ) %>% 
        filter( ! all( outcomeLevel == "Missing" | is.na( .data[[ y ]] ) ) ) %>% # ensure survey measured the strata variable
        ungroup() %>%
        mutate( across( where( is.factor ), droplevels ) ) %>% 
        rename( exposureLevel = !!y )
      
      res_noWgt <- dat_ %>% 
        summarize( n = n(),
                   mean_noWgt = mean( exposureLevel, na.rm = T ),
                   sd_mean_noWgt = sqrt( var( exposureLevel, na.rm = T ) ),
                   q1_noWgt = quantile( exposureLevel, probs = 0.25, na.rm = T ),
                   median_noWgt = median( exposureLevel, na.rm = T ),
                   q3_noWgt = quantile( exposureLevel, probs = 0.75, na.rm = T ),
                   min_noWgt = min( exposureLevel, na.rm = T ),
                   max_noWgt = max( exposureLevel, na.rm = T ) ) %>%
        ungroup( )
      
      resMean_wgt <- svymean( as.formula(paste("~", y ) ), design = des, na.rm = TRUE, keep.var = TRUE )
      resMedian_wgt <- svyquantile( as.formula(paste("~", y ) ), design = des, quantiles = c( 0.25, 0.5, 0.75 ), na.rm = TRUE, ci = TRUE )
      
      res_wgt <- data.frame( mean_wgt = unname( coef( resMean_wgt ) ),
                             se_mean_wgt = unname( SE( resMean_wgt ) ),
                             q1_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.25" ] ),
                             median_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.5" ] ),
                             q3_wgt = unname( coef( unname( resMedian_wgt ) )[ "0.75" ] ),
                             se_q1_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.25" ] ),
                             se_median_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.5" ] ),
                             se_q3_wgt = unname( SE( unname( resMedian_wgt ) )[ "0.75" ] ) )
      
      
      res_summary <- full_join( res_noWgt, res_wgt, by = intersect( names( res_noWgt ), names( res_wgt ) ) ) %>%
        mutate( strataType = y,
                outcomeType = x)
    } ) %>% 
      bind_rows() %>%
      mutate( across( starts_with( "n" ), ~ scales::comma( .x ) ),
              across( starts_with( c( "mean_", "median_", "q1_", "q3_", "min_", "max_" ) ), ~ scales::percent( .x, accuracy = 0.1 ) ) ) %>%
      relocate( c( strataType, outcomeType, n ), .before = everything() )
    
  } ) %>%
    set_names( outlist )
  
  return( resList )
  
}

# --------------------------------------------------- #
# ---- 6. Save lists of results in excel tables  ----
# --------------------------------------------------- #
save_resLists_to_excel <- function( resLists, save_file_path ) {
  
  lapply( names( resLists ), function( x ) {
    
    wb <- createWorkbook()
    # Check if resLists[[x]] is a list or a data frame
    if ( is.list( resLists[[ x ]] ) && all( sapply( resLists[[ x ]], is.data.frame ) ) ) {
      
      # Add sheets for each inner list
      lapply( names( resLists[[ x ]] ), function( y ) {
        addWorksheet( wb, y )
        writeData( wb, y, resLists[[ x ]][[ y ]], keepNA = TRUE, na.string = "NA" ) 
      } )
      
    } else if( is.data.frame( resLists[[ x ]] ) ) {
      
      addWorksheet( wb, x )
      writeData( wb, x, resLists[[ x ]], keepNA = TRUE, na.string = "NA" )
      
    }
    
    # Save workbook
    saveWorkbook( wb, paste0( save_file_path, "/", x, "_", Sys.Date(), ".xlsx" ), overwrite = TRUE )
    
  } )
  
}


# --------------------------------- #
# ---- 7. Regression analyses  ----
# --------------------------------- #

runGEE <- function( df, explist, outlist, covarlist, intervar = NULL, intervar_covar = NULL,
                    segReg = TRUE, predictions = FALSE, nSamples = NULL, intervarLevel1 = NULL, 
                    intervarLevel0 = NULL, nCores = NULL, save, save_file_path = NULL ) {
  
  expAll <- unlist( unname( explist ) )
  covarAll <- unlist( unname( covarlist ) )
  
  # select only variables to use in analyses and manipulate to correct variable types
  dat <- df %>% 
    select( all_of( c( expAll, outlist, covarAll, intervar, intervar_covar ) ), psu_u, surveyid, hivstatus, incidence ) %>%
    mutate( across( all_of( c( explist$stig_ind ) ), ~ as.factor( .x ) ) ) %>% # ensure all are factor variables
    arrange( psu_u ) # ensure the data is ordered by psu (to run the gee correctly)
  
  # iterate over each exposure
  resList <- lapply( expAll, function( exp ) {
    
    print( exp )
    
    # set correct level for the exposure
    expType <- ifelse( grepl( "avg", exp ), "comm", "ind" )
    
    # remove testing from outcomes if exposure is among PLHIV
    if( expType == "ind" ) {
      
      outcomes <- outlist[ ! outlist == "test12m" ]
      
    } else { outcomes <- outlist }
    
    # set correct list of confounders
    covars <- covarlist[[ exp ]]
    
    # iterate over each outcome
    resListExp <- lapply( outcomes, function( out ) {
      
      print( out )
      
      # subset the data based on the outcome, add region to interaction variables for 
      # HIV testing (except for stigma measures among PLHIV, since all surveys are from ES)
      if( ! out == "test12m" ) {
        
        condition <- ifelse( grepl( "adh", out ), "hivstatus == 1 & arv == 1", 
                             ifelse( out == "incidence", "hivstatus == 0 | ( hivstatus == 1 & incidence == 1 )",
                                     "( hivstatus == 1 & ! incidence == 1 ) | ( hivstatus == 1 & is.na( incidence ) )" ) )
        
        dat_ <- dat %>%
          filter( !! parse_expr( condition ) )
        
      } else { dat_ <- dat }
      
      # remove unused variables from the dataframe
      dat_removeVars <- setdiff( c( expAll, outlist, covarAll, intervar, intervar_covar ), c( exp, out, covars, intervar, intervar_covar ) )
      
      if( ! out == "incidence" ) { dat_removeVars <- c( dat_removeVars, "incidence" ) }
      if( out == "test12m" ) { dat_removeVars <- c( dat_removeVars, "hivstatus" ) }
      
      dat_ <- dat_ %>%
        select( - all_of( dat_removeVars ) ) %>%
        rename( exposure = !!exp,
                outcome = !!out )
      
      dat_analysis <- dat_ %>%
        na.omit %>% # remove individuals with any missing data
        mutate( across( where( is.factor ), droplevels ) )
      
      dat_crude <- dat_ %>%
        select( exposure, outcome, intervar, intervar_covar, psu_u, surveyid ) %>%
        na.omit %>% # remove individuals with any missing data
        mutate( across( where( is.factor ), droplevels ) )
      
      # create the model formulas
      formula_crude <- "outcome ~ exposure"
      formula_analysis <- paste( formula_crude, "+", paste( covars, collapse = " + " ) )
      
      if ( expType == "comm" & segReg == TRUE ) { # & ! out == "incidence"
        
        # create exposure variable for segmented regression around the median (crude analysis) and add it to the model formula
        medExp_crude <- median( dat_crude$exposure, na.rm = TRUE )
        
        dat_crude <- dat_crude %>% mutate( exposure_segReg = ifelse( exposure < medExp_crude, 0, exposure - medExp_crude ) )
        dat_analysis <- dat_analysis %>% mutate( exposure_segReg = ifelse( exposure < medExp_crude, 0, exposure - medExp_crude ) )
        
        formula_crude <- gsub( "exposure", "exposure + exposure_segReg", formula_crude )
        formula_analysis <- gsub( "exposure", "exposure + exposure_segReg", formula_analysis )
        
      } else { medExp_crude <- NA }
      
      # add interaction terms to formula if specified
      if( ! is.null( intervar ) ) {
        
        if( ( intervar == "wealth_dichot" & intervar_covar == "wealth" ) | ( intervar == "edu_dichot" & intervar_covar == "edu"  ) ) {
          
          stringI <- ifelse( intervar_covar == "wealth", "as.numeric( wealth ) >= 3", "edu %in% c( 'Secondary', 'Higher' )" )
          formula_intx <- paste( paste0( "I( as.numeric( ", 
                                         grep( "exposure", strsplit( formula_crude, "\\s*\\+ |~\\s*" )[[1]], value = TRUE ), 
                                         " ) * ( ", stringI, " ) )" ), collapse = " + " )
          
        } else { formula_intx <- paste( paste0( grep( "exposure", strsplit( formula_crude, "\\s*\\+ |~\\s*" )[[1]], value = TRUE ), ":", intervar ), collapse = " + " ) }
        
        formula_crude <- paste( formula_crude, "+", intervar_covar, "+", formula_intx )
        formula_analysis <- ifelse( intervar_covar %in% covars, 
                                    paste( formula_analysis, "+", formula_intx ), 
                                    paste( formula_analysis, "+", intervar_covar, "+", formula_intx )  )
        
      }
      
      # If including an interaction by region, need to avoid collinearity issue 
      # with surveyid in the adjusted regression models.
      if( ! is.null( intervar ) && intervar == "reg_alt" ) {
        
        # Change surveyid for smallest countries in each region as a workaround.
        ids_to_swap <- dat_analysis %>%
          filter( ! surveyid == "NG2018NAIIS" ) %>% # "NG2018NAIIS" only includes PLHIV
          mutate( across( where( is.factor ), droplevels ) ) %>%
          group_by( surveyid, reg_alt ) %>%
          summarise( n = n(), .groups = "drop" ) %>%
          group_by( reg_alt ) %>%
          filter( n == min( n ) ) %>%
          ungroup() %>%
          select( surveyid ) %>%
          pull( surveyid ) %>% 
          as.character()
        
        dat_analysis <- dat_analysis %>%
          mutate( surveyid = ifelse( as.character( surveyid ) == ids_to_swap[ 2 ], ids_to_swap[ 1 ], as.character( surveyid ) ),
                  surveyid = as.factor( surveyid ) )
      }
      
      regFamily <- if( out == "incidence" ) { binomial( link = "identity" ) } else { poisson() }
      
      # run the poisson regression models
      regMod_crude <- as.call( list( quote( geepack::geeglm ),
                                     data = dat_crude,
                                     formula = as.formula( formula_crude ),
                                     id = substitute( psu_u ),
                                     corstr = "exchangeable",
                                     family = substitute( regFamily ),
                                     scale.fix = TRUE ) )|> eval()
      
      regMod_adj <- as.call( list( quote( geepack::geeglm ),
                                   data = dat_analysis,
                                   formula = as.formula( formula_analysis ),
                                   id = substitute( psu_u ),
                                   corstr = "exchangeable",
                                   family = substitute( regFamily ),
                                   scale.fix = TRUE ) )|> eval()
      
      # set up list of results
      res <- list()
      res$regModInfo <- data.frame( analysis = c( "crude", "adjusted" ),
                                    nObs = c( nrow( dat_crude ), nrow( dat_analysis ) ),
                                    nSurveys = c( length( unique( dat_crude$surveyid ) ), length( unique( dat_analysis$surveyid ) ) ),
                                    nPSUs = c( length( unique( dat_crude$psu_u ) ), length( unique( dat_analysis$psu_u ) ) ),
                                    nCoeffs = c( length( regMod_crude$coefficients ), length( regMod_adj$coefficients ) ),
                                    QIC = c( as.numeric( MuMIn::QIC( regMod_crude ) ), as.numeric( MuMIn::QIC( regMod_adj ) ) ),
                                    quasiLik = c( as.numeric( MuMIn::quasiLik( regMod_crude ) ), as.numeric( MuMIn::quasiLik( regMod_adj ) ) ),
                                    medExp = medExp_crude ) %>%
        mutate( residualDf = nObs - nCoeffs )
      
      # set coefficient names
      coeffNames_crude <- rownames( vcov( regMod_crude ) )
      coeffNames_crude <- gsub( "exposure1", "exposure", coeffNames_crude ) # remove the 1 when exposure is a factor (0/1) variable
      coeffNames_crude <- ifelse( grepl( "I\\(", coeffNames_crude ), paste0( sub( ".*as.numeric\\(([^)]+)\\).*\\*.*", "\\1", coeffNames_crude ), ":", intervar ), coeffNames_crude ) # change interaction names when indicator used in formula
      
      coeffNames_adj <- rownames( vcov( regMod_adj ) )
      coeffNames_adj <- gsub( "exposure1", "exposure", coeffNames_adj ) 
      coeffNames_adj <- ifelse( grepl( "I\\(", coeffNames_adj ), paste0( sub( ".*as.numeric\\(([^)]+)\\).*\\*.*", "\\1", coeffNames_adj ), ":", intervar ), coeffNames_adj )
      
      coeffs_crude <- coefficients( summary( regMod_crude ) ) %>%
        mutate( lci = Estimate - qnorm( 0.975 ) * Std.err,
                uci = Estimate + qnorm( 0.975 ) * Std.err ) %>%
        mutate( variable = coeffNames_crude,
                analysis = "crude" ) %>%
        relocate( c( variable, analysis ), .before = everything( ) )
      rownames( coeffs_crude ) <- NULL
      
      coeffs_adj <- coefficients( summary( regMod_adj ) ) %>%
        mutate( lci = Estimate - qnorm( 0.975 ) * Std.err,
                uci = Estimate + qnorm( 0.975 ) * Std.err ) %>%
        mutate( variable = coeffNames_adj,
                analysis = "adjusted" ) %>%
        relocate( c( variable, analysis ), .before = everything( ) )
      rownames( coeffs_adj ) <- NULL
      
      res$regModCoeffs <- rbind( coeffs_crude, coeffs_adj )
      
      # convert the variance-covariance matrices to dataframes
      res$vCov_crude <- vcov( regMod_crude ) %>% 
        as.data.frame() %>%
        mutate( variable = coeffNames_crude ) %>%
        relocate( variable, .before = everything( ) )
      rownames( res$vCov_crude ) <- NULL
      colnames( res$vCov_crude ) <- c( "variable", coeffNames_crude )
      
      res$vCov_adj <- vcov( regMod_adj ) %>% 
        as.data.frame() %>%
        mutate( variable = coeffNames_adj ) %>%
        relocate( variable, .before = everything( ) )
      rownames( res$vCov_adj ) <- NULL
      colnames( res$vCov_adj ) <- c( "variable", coeffNames_adj )
      
      # get the combined effect for interaction terms
      if( ! is.null( intervar ) ) {
        
        # set the name of the interaction coefficient
        interVarName <- res$vCov_crude %>% filter( grepl( paste0( "exposure:", intervar ), variable ) ) %>% select( variable ) %>% unname() %>% unlist()
        
        # variance of the exposure term
        var_exp_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( exposure ) %>% unname() %>% unlist()
        var_exp_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( exposure ) %>% unname() %>% unlist()
        
        # variance of the interaction term
        var_inter_crude <- res$vCov_crude %>% filter( variable == interVarName ) %>% select( !! interVarName ) %>% unname() %>% unlist()
        var_inter_adj <- res$vCov_adj %>% filter( variable == interVarName ) %>% select( !! interVarName ) %>% unname() %>% unlist()
        
        # covariance between exposure and the interaction term
        cov_exp_inter_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( !! interVarName ) %>% unname() %>% unlist()
        cov_exp_inter_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( !! interVarName ) %>% unname() %>% unlist()
        
        # calculate half the width of the CI
        halfCI_crude <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "crude" ] ) * 
          sqrt( var_exp_crude + var_inter_crude + 2 * cov_exp_inter_crude ) # student t distribution
        halfCI_adj <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "adjusted" ] ) * 
          sqrt( var_exp_adj + var_inter_adj + 2 * cov_exp_inter_adj ) # student t distribution
        
        # calculate the combined estimate and CI bounds
        resInter_crude <- res$regModCoeffs %>% 
          filter( variable %in% c( "exposure", !! interVarName ), analysis == "crude" ) %>%
          summarize( variable = paste0( "combined_", interVarName ),
                     Estimate = sum( Estimate ),
                     lci = Estimate - halfCI_crude,
                     uci = Estimate + halfCI_crude ) %>%
          mutate( analysis = "crude" )
        
        resInter_adj <- res$regModCoeffs %>% 
          filter( variable %in% c( "exposure", !! interVarName ), analysis == "adjusted" ) %>%
          summarize( variable = paste0( "combined_", interVarName ),
                     Estimate = sum( Estimate ),
                     lci = Estimate - halfCI_adj,
                     uci = Estimate + halfCI_adj ) %>%
          mutate( analysis = "adjusted" )
        
        # add results to coefficients list
        res$regModCoeffs <- rbind( resInter_crude, resInter_adj ) %>%
          full_join( res$regModCoeffs, by = c( "variable", "analysis", "Estimate", "lci", "uci" ) ) %>%
          arrange( desc( analysis ) )
        
      }
      
      # get the post median exposure CI when using segmented regression
      if( expType == "comm" & segReg == TRUE ) { # & ! out == "incidence"
        
        # get variance of each exposure estimate and their covariance
        var_exp_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( exposure ) %>% unname() %>% unlist()
        var_expSegReg_crude <- res$vCov_crude %>% filter( variable == "exposure_segReg" ) %>% select( exposure_segReg ) %>% unname() %>% unlist()
        cov_exp_expSegReg_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( exposure_segReg ) %>% unname() %>% unlist()
        
        var_exp_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( exposure ) %>% unname() %>% unlist()
        var_expSegReg_adj <- res$vCov_adj %>% filter( variable == "exposure_segReg" ) %>% select( exposure_segReg ) %>% unname() %>% unlist()
        cov_exp_expSegReg_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( exposure_segReg ) %>% unname() %>% unlist()
        
        # calculate half the width of the CI
        halfCI_crude <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "crude" ] ) * 
          sqrt( var_exp_crude + var_expSegReg_crude + 2 * cov_exp_expSegReg_crude )
        halfCI_adj <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "adjusted" ] ) * 
          sqrt( var_exp_adj + var_expSegReg_adj + 2 * cov_exp_expSegReg_adj )
        
        # sum the exposure coefficients and calculate the CI bounds
        resPostMed_crude <- res$regModCoeffs %>% 
          filter( variable %in% c( "exposure", "exposure_segReg" ), analysis == "crude" ) %>%
          summarize( variable = "exposure_postMed",
                     Estimate = sum( Estimate ),
                     lci = Estimate - halfCI_crude,
                     uci = Estimate + halfCI_crude ) %>%
          mutate( analysis = "crude" )
        
        resPostMed_adj <- res$regModCoeffs %>% 
          filter( variable %in% c( "exposure", "exposure_segReg" ), analysis == "adjusted" ) %>%
          summarize( variable = "exposure_postMed",
                     Estimate = sum( Estimate ),
                     lci = Estimate - halfCI_adj,
                     uci = Estimate + halfCI_adj ) %>%
          mutate( analysis = "adjusted" )
        
        resPostMed <- rbind( resPostMed_crude, resPostMed_adj )
        
        # get the combined postMed effect for interaction terms
        if( ! is.null( intervar ) ) {
          
          interVarNames <- res$vCov_crude %>% filter( grepl( paste0( ":", intervar ), variable ) ) %>% select( variable ) %>% unname() %>% unlist()
          interVarName_exp <- interVarNames[ grepl( "exposure:", interVarNames ) ]
          interVarName_expSegReg <- interVarNames[ grepl( "exposure_segReg:", interVarNames ) ]
          
          # variance of the interaction terms
          var_interExp_crude <- res$vCov_crude %>% filter( variable == interVarName_exp ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          var_interExp_adj <- res$vCov_adj %>% filter( variable == interVarName_exp ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          
          var_interExpSegReg_crude <- res$vCov_crude %>% filter( variable == interVarName_expSegReg ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          var_interExpSegReg_adj <- res$vCov_adj %>% filter( variable == interVarName_expSegReg ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          
          # covariance between exposure and interaction terms
          cov_exp_interExp_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          cov_exp_interExp_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          
          cov_exp_interExpSegReg_crude <- res$vCov_crude %>% filter( variable == "exposure" ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          cov_exp_interExpSegReg_adj <- res$vCov_adj %>% filter( variable == "exposure" ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          
          cov_expSegReg_interExp_crude <- res$vCov_crude %>% filter( variable == "exposure_segReg" ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          cov_expSegReg_interExp_adj <- res$vCov_adj %>% filter( variable == "exposure_segReg" ) %>% select( !! interVarName_exp ) %>% unname() %>% unlist()
          
          cov_expSegReg_interExpSegReg_crude <- res$vCov_crude %>% filter( variable == "exposure_segReg" ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          cov_expSegReg_interExpSegReg_adj <- res$vCov_adj %>% filter( variable == "exposure_segReg" ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          
          # covariance between interaction terms
          cov_interExp_interExpSegReg_crude <- res$vCov_crude %>% filter( variable == interVarName_exp ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          cov_interExp_interExpSegReg_adj <- res$vCov_adj %>% filter( variable == interVarName_exp ) %>% select( !! interVarName_expSegReg ) %>% unname() %>% unlist()
          
          # calculate half the width of the CI
          halfCI_crude <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "crude" ] ) *
            sqrt( var_exp_crude + var_expSegReg_crude + var_interExp_crude + var_interExpSegReg_crude + 
                    2 * ( cov_exp_expSegReg_crude + cov_exp_interExp_crude + 
                            cov_exp_interExpSegReg_crude + cov_expSegReg_interExp_crude +
                            cov_expSegReg_interExpSegReg_crude + cov_interExp_interExpSegReg_crude ) ) # student t distribution
          
          halfCI_adj <- qt( 0.975, res$regModInfo$residualDf[res$regModInfo$analysis == "adjusted" ] ) *
            sqrt( var_exp_adj + var_expSegReg_adj + var_interExp_adj + var_interExpSegReg_adj +
                    2 * ( cov_exp_expSegReg_adj + cov_exp_interExp_adj + 
                            cov_exp_interExpSegReg_adj + cov_expSegReg_interExp_adj +
                            cov_expSegReg_interExpSegReg_adj + cov_interExp_interExpSegReg_adj ) ) # student t distribution
          
          # calculate the combined estimate and CI bounds
          resInter_segReg_crude <- res$regModCoeffs %>%
            filter( variable %in% c( "exposure", "exposure_segReg", !! interVarNames ), analysis == "crude" ) %>%
            summarize( variable = paste0( "combined_exposure_postMed", sub( "exposure_segReg", "", interVarName_expSegReg ) ),
                       Estimate = sum( Estimate ),
                       lci = Estimate - halfCI_crude,
                       uci = Estimate + halfCI_crude ) %>%
            mutate( analysis = "crude" )
          
          resInter_segReg_adj <- res$regModCoeffs %>%
            filter( variable %in% c( "exposure", "exposure_segReg", !! interVarNames ), analysis == "adjusted" ) %>%
            summarize( variable = paste0( "combined_exposure_postMed", sub( "exposure_segReg", "", interVarName_expSegReg ) ),
                       Estimate = sum( Estimate ),
                       lci = Estimate - halfCI_adj,
                       uci = Estimate + halfCI_adj ) %>%
            mutate( analysis = "adjusted" )
          
          # add results to resPostMed
          resPostMed <- rbind( resInter_segReg_crude, resInter_segReg_adj ) %>%
            full_join( resPostMed, by = c( "variable", "analysis", "Estimate", "lci", "uci" ) )
          
        }
        
        # add the post median exposure estimate to the coefficients dataframe
        res$regModCoeffs <- full_join( resPostMed, res$regModCoeffs, by = c( "variable", "analysis", "Estimate", "lci", "uci" ) ) %>%
          arrange( desc( str_detect( variable, "exposure" ) ), desc( analysis ) ) %>%
          mutate( variable = ifelse( str_detect( variable, "exposure" ) & ! str_detect( variable, "segReg|postMed" ), 
                                     str_replace( variable, "exposure", "exposure_preMed"), variable ) )
        
      }
      
      # calculate prevalence ratios from coefficients
      res$regModPRs <- res$regModCoeffs %>%
        select( variable, analysis, Estimate, lci, uci ) %>%
        mutate( across( c( Estimate, lci, uci ), ~ exp( .x ) ) )
      
      # get predicted outcomes for community stigma exposures
      if( expType == "comm" & segReg == TRUE & ! out == "incidence" & predictions == TRUE ) {
        
        paste( "start crude predictions:", Sys.time() ) %>% print()
        predRes_crude <- fn_predictions( nSamples = nSamples,
                                         data = dat_crude,
                                         medExp = medExp_crude,
                                         formula = formula_crude,
                                         intervar = intervar,
                                         intervarLevel1 = intervarLevel1,
                                         intervarLevel0 = intervarLevel0,
                                         nCores = nCores ) %>%
          lapply( function( x ) { x %>% mutate( analysis = "crude" ) } )
        
        paste( "start adjusted predictions:", Sys.time() ) %>% print()
        predRes_adj <- fn_predictions( nSamples = nSamples,
                                       data = dat_analysis,
                                       medExp = medExp_crude,
                                       formula = formula_analysis,
                                       intervar = intervar,
                                       intervarLevel1 = intervarLevel1,
                                       intervarLevel0 = intervarLevel0,
                                       nCores = nCores ) %>%
          lapply( function( x ) { x %>% mutate( analysis = "adjusted" ) } )
        
        res$predEst <- full_join( predRes_crude$predEst, predRes_adj$predEst, by = intersect( names( predRes_crude$predEst ), names( predRes_adj$predEst ) ) ) %>%
          relocate( analysis, .before = 1 )
        
        res$predDiff <- full_join( predRes_crude$predDiff, predRes_adj$predDiff, by = intersect( names( predRes_crude$predDiff ), names( predRes_adj$predDiff ) ) ) %>%
          relocate( analysis, .before = 1 )
      }
      
      return( res )
      
    } ) %>%
      set_names( outcomes )
    
  } ) %>%
    set_names( expAll )
  
  if( save == TRUE ) {
    
    saveName <- ifelse( is.null( intervar ), "regression-model-results_", paste0( "regression-model-results_inter-", intervar, "_" ) )
    saveRDS( resList, file = paste0( save_file_path, saveName,  Sys.Date(), ".rds" ) )
    
  }
  
  return( resList )
  
}


# ---------------------------- #
# ---- 8. Predictions fn  ----
# ---------------------------- #

fn_predictions <- function( nSamples, data, medExp, formula, intervar, 
                            intervarLevel1, intervarLevel0, nCores ) {
  
  # Set up //
  
  # set cutoff values for the predictions
  cutOffs <- c( medExp, seq( 0, 1, by = 0.05 ) ) %>% # instead of only 0 and 1, pretend that everyone has the exposure level of 0.1; 0.2; 0.3; 0.4; 0.5...
    sort()
  
  # initialize clusters
  library( doParallel )
  
  nCores_ <- ifelse( is.null( nCores ), 4, nCores )
  cl <- makeCluster( nCores_ )
  
  registerDoParallel( cl )
  
  clusterExport( cl, varlist = c( "cutOffs" ), envir = environment() )
  
  clusterEvalQ( cl, { 
    library( geepack )
    library( dplyr )
    library( rlang )
    library( tidyr )
  })
  
  exportVars <- c( "cutOffs", "data", "formula", "medExp" )
  if( ! is.null( intervar ) ) { exportVars <- c( exportVars, "intervar", "intervarLevel1", "intervarLevel0" ) }
  clusterExport( cl, varlist = exportVars, envir = environment() )
  
  # bootstrap 
  bootRes_list <- foreach::foreach( i = 1:nSamples, .combine = 'list', .multicombine = TRUE ) %dopar% {
    
    # sample PSU and subset the data
    sampledPSUs <- sample( unique( data[[ "psu_u" ]] ),
                           length( unique( data[["psu_u"]] ) ), replace = T )
    
    dat_sampled <- data[ data[[ "psu_u" ]] %in% sampledPSUs, ] %>%
      mutate( across( where( is.factor ), droplevels ) ) %>% # drop unused levels of factor variables after subsetting the data
      arrange( psu_u ) # ensure the data is ordered by psu (to run the gee correctly)
    
    regMod <- geeglm( data = dat_sampled,
                      formula = as.formula( formula ),
                      id = psu_u,
                      corstr = "exchangeable",
                      family = poisson,
                      scale.fix = TRUE )
    
    predEst <- if( is.null( intervar ) ) {
      
      lapply( cutOffs, function( x ) { 
        
        dat_sampled_temp <- dat_sampled %>%
          mutate( exposure = x,
                  exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
        
        return( mean( predict( regMod, dat_sampled_temp, type = "response" ) ) )
        
      } ) %>%
        unlist()
      
    } else {
      
      lapply( cutOffs, function( x ) {
        
        dat_sampled_temp_inter1 <- dat_sampled %>%
          filter( !! parse_expr( paste( intervar, "==", shQuote( intervarLevel1 ) ) ) ) %>%
          mutate( exposure = x,
                  exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
        
        dat_sampled_temp_inter0 <- dat_sampled %>%
          filter( !! parse_expr( paste( intervar, "==", shQuote( intervarLevel0 ) ) ) ) %>%
          mutate( exposure = x,
                  exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
        
        predEst_inter1 <- mean( predict( regMod, dat_sampled_temp_inter1, type = "response" ) )
        
        predEst_inter0 <- mean( predict( regMod, dat_sampled_temp_inter0, type = "response" ) )
        
        return( data.frame( predEst_inter1, predEst_inter0 ) )
        
      } ) %>%
        bind_rows()
      
    }
    
    predDiff <- if( is.null( intervar ) ) {
      
      dat_sampled_zero <- dat_sampled %>%
        mutate( exposure = 0,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      dat_sampled_100 <- dat_sampled %>%
        mutate( exposure = 1,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      predDiff_ <- rbind( mean( predict( regMod, dat_sampled_zero, type = "response" ) - predict( regMod, dat_sampled, type = "response" ) ),
                          mean( predict( regMod, dat_sampled_zero, type = "response" ) - predict( regMod, dat_sampled_100, type = "response" ) ) )
      
      predDiff <- rbind(
        data.frame(type = "current->0", predDiff = predDiff_[1, ]),
        data.frame(type = "100->0", predDiff = predDiff_[2, ])
      )
      
    } else {
      
      dat_sampled_inter1 <- dat_sampled %>%
        filter( !! parse_expr( paste( intervar, "==", shQuote( intervarLevel1 ) ) ) ) 
      
      dat_sampled_inter0 <- dat_sampled %>%
        filter( !! parse_expr( paste( intervar, "==", shQuote( intervarLevel0 ) ) ) ) 
      
      dat_sampled_zero_inter1 <- dat_sampled_inter1 %>%
        mutate( exposure = 0,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      dat_sampled_zero_inter0 <- dat_sampled_inter0 %>%
        mutate( exposure = 0,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      dat_sampled_100_inter1 <- dat_sampled_inter1 %>%
        mutate( exposure = 1,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      dat_sampled_100_inter0 <- dat_sampled_inter0 %>%
        mutate( exposure = 1,
                exposure_segReg = ifelse( exposure < medExp, 0, exposure - medExp ) )
      
      predDiff_inter1 <- rbind( mean( predict( regMod, dat_sampled_zero_inter1, type = "response" ) - predict( regMod, dat_sampled_inter1, type = "response" ) ),
                                mean( predict( regMod, dat_sampled_zero_inter1, type = "response" ) - predict( regMod, dat_sampled_100_inter1, type = "response" ) ) )
      
      predDiff_inter0 <- rbind( mean( predict( regMod, dat_sampled_zero_inter0, type = "response" ) - predict( regMod, dat_sampled_inter0, type = "response" ) ),
                                mean( predict( regMod, dat_sampled_zero_inter0, type = "response" ) - predict( regMod, dat_sampled_100_inter0, type = "response" ) ) )
      
      
      predDiff <- rbind( data.frame( type = "current->0", predDiff_inter1 = predDiff_inter1[1, ], predDiff_inter0 = predDiff_inter0[1, ] ),
                         data.frame( type = "100->0", predDiff_inter1 = predDiff_inter1[2, ], predDiff_inter0 = predDiff_inter0[2, ] ) )
    }
    
    return( list(predEst = data.frame(cutOffs = cutOffs, predEst), predDiff = data.frame( predDiff ) ) )    
  }
  
  parallel::stopCluster(cl)
  
  bootRes_predEst <- lapply(bootRes_list, function(x) x$predEst)
  bootRes_predDiff <- lapply(bootRes_list, function(x) x$predDiff)
  
  predEst <- bind_rows( bootRes_predEst ) %>%
    group_by( cutOffs ) %>%
    summarise( across( .cols = everything(), 
                       list( mean = ~ mean( . ), 
                             median = ~ quantile( ., 0.5 ),
                             lci = ~ quantile( ., 0.025 ),
                             uci = ~ quantile( ., 0.975 ) ), 
                       .names = "{.col}_{.fn}" ) )
  
  predDiff <- bind_rows( bootRes_predDiff ) %>% 
    group_by( type ) %>% 
    summarise( across( everything(), list( mean = ~ mean( . ),
                                           median = ~ quantile( ., 0.5 ),
                                           lci = ~ quantile( ., 0.025 ),
                                           uci = ~ quantile( ., 0.975 ) ),
                       .names = "{.col}_{.fn}" ) )
  
  if( ! is.null( intervar ) ){
    
    predEst <- predEst %>%
      pivot_longer(
        cols = contains( "inter" ), 
        names_to = c( "intervarLevel", ".value" ), 
        names_pattern = "predEst_inter(\\d)_(.*)" ) %>%
      mutate( intervarLevel = ifelse( intervarLevel == 1, paste( intervarLevel1 ), paste( intervarLevel0 ) ) )
    
    predDiff <- predDiff %>%
      pivot_longer(
        cols = contains( "inter" ), 
        names_to = c( "intervarLevel", ".value" ), 
        names_pattern = "predDiff_inter(\\d)_(.*)" ) %>%
      mutate( intervarLevel = ifelse( intervarLevel == 1, paste( intervarLevel1 ), paste( intervarLevel0 ) ) )
    
  } else {
    
    predEst <- predEst %>%
      rename_with( ~ str_remove( ., "^predEst_" ) )
    
    predDiff <- predDiff %>%
      rename_with( ~ str_remove( ., "^predDiff_" ) )
    
  }
  
  
  return( list( predEst = predEst, predDiff = predDiff ) )
  
}
