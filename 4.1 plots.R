#### ---- Set up ---- ####

# * load libraries ----
library( dplyr )
library( tidyr )
library( ggplot2 )
library( patchwork )
library( gridExtra )
library( grid )
library( stringr )
library( wesanderson )
library( metafor )
library( purrr )
library( corrplot )

# * set file paths ----
path_rds <- here::here("outputs/R-regression-results//")
path_out <- here::here("outputs/figs//")
path_data <- here::here("data//")

# * load results ----
fileNames <- list.files( path_rds )
files <- fileNames[ grep( "MAIN|EMM|SA|simulations", fileNames ) ]

resList <- lapply( paste0( path_rds, files ), readRDS )
names( resList ) <- sub("_\\d{4}-\\d{2}-\\d{2}\\.rds$", "", files)

# * load dataset ----
df <- readRDS( paste0( path_data, "all_surveys_2025-04-03.rds" ) )

# set exposure variable names
exposures_comm <- c( "stig_avg", "stig_perc_avg", "social_judge_avg" )
exposures_ind <- c( "stig_anticip", "med_stig", "ant_exp_stig" )
# exposures_ind <- c( "ant_exp_stig" )

# * create list to store plots ----
plots <- list()

# * figure configurations ----
# theme
theme <- theme_bw() +
  theme( plot.tag = element_text( size = 16, face = "bold" ),
         plot.tag.position = "topleft",
         plot.title = element_text( size = 16, face = "bold" ),
         axis.title.x = element_text( margin = margin( t = 10 ), size = 16 ),
         axis.title.y = element_text( margin = margin( r = 10, l = 10 ), size = 16 ),
         axis.text.x = element_text( size = 16, angle = 90, hjust = 1, color = "black" ),
         axis.text.y = element_text( size = 16, color = "black" ),
         strip.background = element_rect( fill = "grey90", color = "black", linewidth = 1 ),
         strip.text.y = element_text( angle = 270, hjust = 0.5 ),
         strip.text.x = element_text( margin = margin( t = 2, r = 1, b = 2, l = 1 ) ),
         strip.text = element_text( size = 14 ),
         strip.placement = "outside",
         legend.position = 'right',
         legend.background = element_rect( fill = "white" ),
         legend.text = element_text( size = 14, margin = margin( t = 4, r = 1, b = 4, l = 1 ) ),
         legend.key.height = unit( 0.5, "cm" ),
         legend.key = element_rect(fill = "white", color = "white"), 
         legend.title = element_text( size = 14, face = "bold" ) )

theme_forest <- theme_bw() +
  theme( plot.tag = element_text( size = 14, face = "bold" ),
         plot.tag.position = "topleft",
         plot.title = element_text( size = 14, face = "bold" ),
         axis.title.x = element_text( margin = margin( t = 10 ), size = 14 ),
         axis.title.y = element_text( margin = margin( r = 10, l = 10 ), size = 14 ),
         axis.text.x = element_text( size = 14, angle = 90, hjust = 1, color = "black" ),
         axis.text.y = element_text( size = 14, color = "black" ),
         strip.background = element_rect( fill = "grey90", color = "black", linewidth = 1 ),
         strip.text.y = element_text( angle = 270, hjust = 0.5 ),
         strip.text.x = element_text( margin = margin( t = 2, r = 1, b = 2, l = 1 ) ),
         strip.text = element_text( size = 12 ),
         strip.placement = "outside",
         legend.position = 'right',
         legend.background = element_rect( fill = "white" ),
         legend.text = element_text( size = 12, margin = margin( t = 4, r = 1, b = 4, l = 1 ) ),
         legend.key.height = unit( 0.5, "cm" ),
         legend.key = element_rect(fill = "white", color = "white"), 
         legend.title = element_text( size = 12, face = "bold" ) )

table_theme <- ttheme_default(
  core = list(
    fg_params = list(cex = 1),  # Adjust text size
    padding = unit(c(3, 3), "mm")  # Reduce padding (vertical, horizontal)
  ),
  colhead = list(
    fg_params = list(cex = 1),  # Adjust header text size
    padding = unit(c(3, 3), "mm")  # Reduce header padding 
    ) )

# axes
axis_x <- scale_x_continuous( limits = c( 0, 1 ),
                              breaks = seq( 0, 1, by = 0.10 ) )
axis_y <- scale_y_continuous( limits = c( 0, 1 ),
                              breaks = seq( 0, 1, by = 0.20 ) )

axis_x_forest <- scale_x_continuous( limits = c( 0.6, 1.35 ),
                                     breaks = seq( 0.5, 1.5, by = 0.10 ) )

# plot types
plotLine <- geom_line( linewidth = 0.5 )
plotPoint <- geom_point( size = 2, position = position_dodge( width = -0.5 ) )

# configs
labels_stigma <- c( "stig_avg" = "Discriminatory attitudes",
                   "stig_perc_avg" = "Perceived stigma",
                   "social_judge_avg" = "Shame of association",
                   "stig_anticip" = "Anticipated stigma\nin a healthcare setting",
                   "med_stig" = "Experienced discrimination\nin a healthcare setting",
                   "ant_exp_stig" = "Anticipated or experienced stigma\nin a healthcare setting",
                   "bias_corr" = "Anticipated or experienced stigma\n(bias-corrected: Bias OR=2)" )

names_emm <- c(
  "reg_alt" = "Region",
  "sex" = "Sex",
  "restype" = "Residence\ntype",
  "wealth_dichot" = "Wealth\nscore",
  "age_dichot" = "Age",
  "edu_dichot" = "Education"
)

# plot labels

x_lab = "Community-level prevalence"

# ribbon

ribbon <- geom_ribbon( aes( ymin = lci, ymax = uci ), alpha = 0.4, linewidth = 0 )

# analysis type configs

colours_type <- c( "adjusted" = "#B40F20",
                  "crude" = "#46ACC8" )

labels_type <- c( "adjusted" = "Adjusted",
                 "crude" = "Crude" )
labeller_type <- as_labeller( labels_type )

labels_emm <- c( "CW" = "CW",
                 "ES" = "ES",
                 "male" = "Male",
                 "female" = "Female",
                 "Rural" = "Rural",
                 "Urban" = "Urban",
                 "3+" = "Wealth score\n≥ 3",
                 "<3" = "Wealth score\n< 3",
                 "25+" = "Age\n≥ 25",
                 "<25" = "Age\n< 25",
                 "Secondary+" = "≥ Secondary",
                 "<Secondary" = "≤ Primary" )

labeller_emm <- as_labeller( labels_emm )

colours_emm <- c( "CW" = "#273046",
                  "ES" = "#FAD510",
                  "male" = "#CB2314",
                  "female" = "#354823",
                  "Rural" = "#0B775E",
                  "Urban" = "#DC863B",
                  "3+" = "#7294D4",
                  "<3" = "#E6A0C4",
                  "25+" = "#9986A5",
                  "<25" = "#CCBA72",
                  "Secondary+" = "#F98400",
                  "<Secondary" = "#5BBCD6" )

# * figure functions ----

makePredictionsPlot <- function( datlst, y_lab, title, int.var = NULL ){
  
  # set medians and nObs
  
  median <- datlst$regModInfo %>% filter( analysis == "crude" ) %>% select( medExp ) %>% as.numeric()

  # Make table 
  
  # Define column names for table dynamically
  table_names <- c(
    "Analysis" = "analysis",
    "n" = "nObs",
    "N" = "nSurveys"
  )
  
  table_names[ paste0( "PR (95% CI)\n< Median ", if ( !is.null( int.var ) ) min( datlst$predRes$intervarLevel ) else "" ) ] <- "preMed"
  table_names[ paste0( "PR (95% CI)\n≥ Median ", if ( !is.null( int.var ) ) min( datlst$predRes$intervarLevel ) else "" ) ] <- "postMed"
  
  PRtable <- datlst$regModPRs %>% 
    filter( grepl( "exposure", variable ) & ! grepl( "segReg", variable ) & ! grepl( ":", variable ) | grepl( "combined", variable ) ) %>% 
      mutate( PR = paste0( sprintf("%.2f", exp ( log( Estimate ) * 0.1 ) ), " (", sprintf( "%.2f", exp( log( lci ) * 0.1 ) ), "-", sprintf( "%.2f", exp( log( uci ) * 0.1 ) ), ")" ),
              variable = case_when( ! is.null( int.var ) ~ str_replace_all( variable, setNames( rep("", length( c( "exposure_", paste0( "_", !! int.var ) ) ) ), 
                                                                                                c( "exposure_", paste0( "_", !! int.var ) ) ) ),
                                    is.null( int.var ) ~ gsub( "exposure_", "", variable ) ) ) %>% 
    select( - c( Estimate, lci, uci ) ) %>% 
    pivot_wider( names_from = variable,
                 values_from = PR,
                 names_prefix = "" ) %>% 
    left_join( datlst$regModInfo[, c( "analysis", "nObs", "medExp", "nSurveys" ) ], by = "analysis" ) %>% 
    mutate( nObs = format( nObs, big.mark = "," ),
            medExp = sprintf( "%.2f", medExp ),
            analysis = str_to_title( analysis ) ) %>% 
    select( analysis, preMed, postMed, any_of( if ( ! is.null( int.var ) ) contains( "combined_preMed" ) ), any_of( if ( ! is.null( int.var ) ) contains ("combined_postMed") ), nObs, nSurveys ) %>% 
    rename( !!! table_names ) %>% 
    rename_with(
      ~ case_when(
        grepl("^combined_preMed:", .) ~ sprintf("PR (95%% CI)\n< Median %s", max(datlst$predRes$intervarLevel)),
        grepl("^combined_postMed:", .) ~ sprintf("PR (95%% CI)\n≥ Median %s", max(datlst$predRes$intervarLevel)),
        TRUE ~ .
      )
    )
  
  if( ! is.null( int.var ) ) {
    
    PRtable <- PRtable %>%
      filter( Analysis == "Adjusted" ) %>%  # Keep only the Adjusted row
      select( -Analysis ) %>%  # Remove Analysis column
      pivot_longer( cols = -c( n, N ), names_to = "Category", values_to = "PR (95% CI)") %>% 
      mutate( Type = case_when( grepl( "< Median", Category ) ~ "aPR (95% CI)\n< Median",
                                grepl( "≥ Median", Category ) ~ "aPR (95% CI)\n≥ Median" ),
              Category = case_when( grepl( "CW", Category ) ~ "CW",
                                    grepl( "ES", Category ) ~ "ES",
                                    grepl( "female", Category ) ~ "Female",
                                    grepl( "male", Category ) ~ "Male",
                                    grepl( "Rural", Category ) ~ "Rural",
                                    grepl( "Urban", Category) ~ "Urban",
                                    grepl( "<3", Category ) ~ "Wealth score < 3",
                                    grepl( "3+", Category ) ~ "Wealth score ≥ 3",
                                    grepl( "<25", Category ) ~ "Age < 25",
                                    grepl( "25+", Category ) ~ "Age ≥ 25",
                                    grepl( "<Secondary", Category ) ~ "≤ Primary",
                                    grepl( "Secondary+", Category ) ~ "≥ Secondary" ) ) %>%
      select( Category, Type, `PR (95% CI)`, N ) %>%
      pivot_wider( names_from = Type, values_from = `PR (95% CI)` ) %>% 
      select( Category, `aPR (95% CI)\n< Median`, `aPR (95% CI)\n≥ Median`)
  }
  
    plot_tab <- ggplot() +
      annotation_custom( tableGrob( PRtable, rows = NULL, theme = table_theme ) ) +
      theme_void()
  
  # Make plot
  
  # First, make plot data
    
    plot_data <- if ( !is.null( int.var ) ) {
      
      ( if ( !is.null( datlst$predRes ) ) datlst$predRes else datlst$predEst ) %>%
        filter( analysis != "crude" )
      
    } else {
      
      ( if ( !is.null(datlst$predRes ) ) datlst$predRes else datlst$predEst ) %>%
        rename_with(~ gsub("^predEst_", "", .x ) )
      
    }
    
  plot <-
    plot_data %>% 
    mutate(analysis = factor( analysis, levels = c( "crude", "adjusted" ) ) ) %>% 
    ggplot( aes( x = cutOffs,
                 y = mean,
                 colour = if( ! is.null( int.var) ) { intervarLevel } else { analysis },
                 fill = if( ! is.null( int.var) ) { intervarLevel } else { analysis }) ) +
    
    # add medians to plots
    geom_vline( xintercept = median, colour = "darkgrey", linetype = "dashed", size = 1 ) +

    plotLine +
    ribbon +
    theme +
    axis_x +
    axis_y +
    ggtitle( title ) +
    xlab( x_lab ) +
    ylab( y_lab ) 
  
  # add linetype scale and legend customisation
  if ( ! is.null( int.var ) ) {
    plot <- plot +
      scale_colour_manual( values = colours_emm, labels = labels_emm, name = names_emm[[ int.var ]] ) +
      scale_fill_manual( values = colours_emm, labels = labels_emm, name = names_emm[[ int.var ]] ) 
  } else {
    plot <- plot +
      scale_colour_manual( values = colours_type, labels = labels_type, name = "Analysis" ) +
      scale_fill_manual( values = colours_type, labels = labels_type, name = "Analysis" ) 
  }
  
  plot_tab$vp <- viewport( y = 0.5 )
  
  plot / plot_tab + plot_layout( heights = c( 1, 0.5 ) )
  
}

makeForestPlot <- function( data, exposures, outcome, x_lab_forest, bias.corr = TRUE, 
                            bias.or = if( bias.corr == TRUE ) { bias.or } else { NULL }, 
                            int.var = NULL,
                            int.level0 = if( is.null( int.var ) ) { NULL }, 
                            int.level1 = if( is.null( int.var ) ) { NULL } ) {
  
  # configure text for forest plot
  Nsurveys <- geom_text( aes( x = 0.55, label = nSurveys ), position = position_dodge( -0.5 ), show.legend = FALSE, size = 5 )
  
  Nparticipants <-  geom_text(aes(x = 0.75, label = N), position = position_dodge( -0.5 ), show.legend = FALSE, size = 5 )
  
  PR <- geom_text( aes(x = 1.2, label = `PR ` ), position = position_dodge( -0.5 ), show.legend = FALSE, size = 5 ) 
  
  if( !is.null( int.var ) ) { category <- geom_text(aes(x = 0.6, label = labeller_emm( Category ) ), position = position_dodge( -0.5 ), show.legend = FALSE, size = 5 ) }
  
  subtitles <- annotate( "text", x = c( 0.55, 0.75, 1.2 ), y = length( exposures ) + if ( bias.corr ) 1.5 else 0.5, label = c("Surveys", "N", "PR (95%CI)"), fontface = 2, size = 5, colour = "black" )
  
  subtitles_emm <- annotate( "text", x = c( 1.2 ), y = length( exposures ) + if ( bias.corr ) 1.5 else 0.5, label = c( "aPR (95%CI)" ), fontface = 2, size = 5, colour = "black" )
  
  forestData <- lapply( exposures, function( exposure ) {
    # Define column names for table dynamically
    
    table_names <- c( N = "nObs" )
    table_names[paste0("PR ", if (!is.null(int.var)) min( int.level0, int.level1 ) else "")] <- "exposure"
    
    dat_ <- data[[ exposure ]] [[ outcome ]]$regModPRs %>%  filter( grepl( "exposure", variable ) & ! grepl( "segReg", variable ) & ! grepl( ":", variable ) ) %>% 
      mutate( PR = paste0( sprintf("%.2f", Estimate), " (", sprintf( "%.2f", lci ), "-", sprintf( "%.2f", uci ), ")" ),
              variable = case_when( ! is.null( int.var ) ~ str_replace_all( variable, setNames( rep("", length( c( "exposure_", paste0( "_", !! int.var) ) ) ), 
                                                                                                c( "exposure_", paste0( "_", !! int.var ) ) ) ),
                                    is.null( int.var ) ~ gsub("exposure_", "", variable ) ) ) %>% 
      select( - c( Estimate, lci, uci ) ) %>% 
      pivot_wider( names_from = variable,
                   values_from = PR,
                   names_prefix = "" ) %>% 
      left_join( data[[ exposure ]] [[ outcome ]]$regModInfo[, c( "analysis", "nObs", "nSurveys" ) ], by = "analysis" ) %>% 
      mutate( nObs = format( nObs, big.mark = "," ),
              variable = !! exposure ) %>% 
      select( variable, analysis, nSurveys, exposure, any_of( if ( ! is.null( int.var ) ) "combined" ), nObs ) %>% 
      rename(
        PR = exposure,
        !!! table_names )
    
    if( ! is.null( int.var ) ) {
      dat_ <- dat_ %>% 
        rename_with( ~ str_replace_all( ., c( "combined" = paste0( "PR ", max( int.level0, int.level1 ) ) ) ), 
                     any_of( c( "combined" ) ) )
    }
    
    return( dat_ )
    
  } )
  
  forestData <- bind_rows( forestData ) 
  
  if( ! is.null( int.var ) ) {
    
    forestData <- forestData %>%
      filter( analysis == "adjusted" ) %>%  # Keep only the Adjusted row
      select( -N, -nSurveys ) %>%  # Remove Analysis column
      pivot_longer( cols = -c( variable, analysis ), names_to = "Category", values_to = "PR") %>% 
      mutate( Category = gsub( "PR ", "", Category ) ) %>%
      select(variable, analysis, Category, PR) %>% 
      separate(PR, into = c("Estimate", "CI"), sep = " \\(", remove = FALSE) %>%
      separate(CI, into = c("lci", "uci"), sep = "-", remove = TRUE) %>%
      mutate( uci = sub("\\)", "", uci),
              across( c( Estimate, lci, uci ), as.numeric ) )
    
  } else {
    
    forestData <- as.data.frame(forestData) %>% 
      separate(`PR `, into = c("Estimate", "CI"), sep = " \\(", remove = FALSE) %>%
      separate(CI, into = c("lci", "uci"), sep = "-", remove = TRUE) %>%
      mutate( uci = sub("\\)", "", uci),
              across( c( Estimate, lci, uci ), as.numeric ) )
  }
  
  if( bias.corr == TRUE ) {
    
    forestData <- data$simulations$ant_exp_stig[[ outcome ]] %>% 
      filter( sim == bias.or ) %>% 
      select( pr, lci, uci ) %>% 
      mutate( variable = "bias_corr",
              analysis = "adjusted",
              PR = sprintf("%.2f (%.2f-%.2f)", pr, lci, uci) ) %>% 
      rename( Estimate = pr ) %>% 
      bind_rows( forestData ) %>% 
      select( variable, analysis, nSurveys, PR, N, Estimate, lci, uci )
    
  }
  
  forestPlot <- forestData %>% 
    mutate( analysis = factor( analysis, levels = c( "crude", "adjusted" ) ),
            variable = factor( variable, levels = c( "bias_corr",  "ant_exp_stig", "med_stig", "stig_anticip", "social_judge_avg", "stig_perc_avg", "stig_avg" ) ) ) %>% 
    ggplot( aes( x = Estimate,
                 y = variable,
                 colour = if( ! is.null( int.var ) ) { Category } else { analysis } ) ) +
    geom_vline( xintercept = 1, linetype = "dashed" ) +
    plotPoint +
    geom_linerange( aes( xmin = lci, xmax = uci), position = position_dodge( -0.5 ) ) +
    theme_forest +
    theme( panel.border = element_blank(),
           axis.title.x = element_text( hjust = 1)) +
    axis_x_forest +
    xlab( x_lab_forest ) +
    ylab( NULL ) +
    PR +
    scale_y_discrete( labels = labels_stigma )
  
  if ( ! is.null( int.var ) ) {
    
    forestPlot <- forestPlot +
      scale_colour_manual( values = colours_emm, labels = labels_emm, name = names_emm[[ int.var ]] ) +
      subtitles_emm
    
  } else {
    forestPlot <- forestPlot +
      scale_colour_manual( values = colours_type, labels = labels_type, name = "Analysis\n" ) +
      Nsurveys +
      Nparticipants +
      subtitles
  }
  
  return( forestPlot )
}

#### ---- Plots ---- ####

# * MAIN predictions figure -----

## HIV testing analysis
plots$predProb_MAIN$test12m <- lapply( exposures_comm, function( x ) { 
  data_ <- resList$`MAIN_regression-model-results`[[x]]$test12m
  title_ <- ifelse( x == "stig_avg", "Discriminatory attitudes and\nHIV testing", 
                    ifelse( x == "stig_perc_avg", "Perceived stigma and\nHIV testing", 
                            "Shame of association and\nHIV testing" ) )
  makePredictionsPlot( datlst = data_, 
                       y_lab = "Predicted probability of HIV testing",
                       title = title_ ) 
  } )

## ARV among PLHIV analysis
plots$predProb_MAIN$arv <- lapply( exposures_comm, function( x ) { 
  data_ <- resList$`MAIN_regression-model-results`[[x]]$arv
  title_ <- ifelse( x == "stig_avg", "Discriminatory attitudes and\nART use", 
                    ifelse( x == "stig_perc_avg", "Perceived stigma and\nART use", 
                            "Shame of association and\nART use" ) )
  makePredictionsPlot( datlst = data_, 
                       y_lab = "Predicted probability of ART use",
                       title = title_ ) 
  } )

## Viral suppression among PLHIV analysis
plots$predProb_MAIN$vlsup <- lapply( exposures_comm, function( x ) {
  data_ <- resList$`MAIN_regression-model-results`[[x]]$vlsup
  title_ <- ifelse( x == "stig_avg", "Discriminatory attitudes and\nviral suppression", 
                    ifelse( x == "stig_perc_avg", "Perceived stigma and\nviral suppression", 
                            "Shame of association and\nviral suppression" ) )
  makePredictionsPlot( datlst = data_, 
                       y_lab = "Predicted probability of VLS",
                       title = title_ ) 
  } )

# Combine the plots using patchwork
combined_plot_preds <- wrap_plots( c(
  plots$predProb_MAIN$test12m, 
  plots$predProb_MAIN$arv, 
  plots$predProb_MAIN$vlsup ), 
  ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds, filename = "combined_predPlot.png",
        path = path_out, width = 17, height = 17, dpi = 300 )

# * SA predictions figure -----

## Excluding PLHIV from HIV testing analyses
plots$predProb_SA_noPLHIV$test12m <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`SA_noPLHIV_regression-model-results`[[x]]$test12m
  title_ <- ifelse( x == "stig_avg", "Discriminatory attitudes", 
                    ifelse( x == "stig_perc_avg", "Perceived stigma", 
                            "Shame of association" ) )
  makePredictionsPlot( datlst = data_, 
                       y_lab = "Predicted probability of HIV testing",
                       title = title_ ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_SA_noPLHIV <- wrap_plots( c(
  plots$predProb_SA_noPLHIV$test12m ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "" )
  ) )

ggsave( combined_plot_preds_SA_noPLHIV, filename = "combined_predPlot_SA_noPLHIV.png",
        path = path_out, width = 18, height = 5, dpi = 300 )

# * EMM predictions figures -----

# *** EMM by region ----

## HIV testing analysis
plots$predProb_EMM$test12m$reg_alt <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-reg_alt`[[x]]$test12m
  title_ <- ifelse( x == "stig_avg", "Discriminatory attitudes", 
                    ifelse( x == "stig_perc_avg", "Perceived stigma", 
                            "Shame of association" ) )
  makePredictionsPlot( datlst = data_, 
                       y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "reg_alt" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_reg_alt <- wrap_plots( c(
  plots$predProb_EMM$test12m$reg_alt), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_reg_alt, filename = "combined_predPlot_EMM_reg_alt.png",
        path = path_out, width = 18, height = 5, dpi = 300 )

# *** EMM by sex ----

## HIV testing analysis
plots$predProb_EMM$test12m$sex <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-sex`[[x]]$test12m
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "sex" ) 
  } )

## ARV among PLHIV analysis
plots$predProb_EMM$arv$sex <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-sex`[[x]]$arv
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of ART use",
                       title = title_,
                       int.var = "sex" ) 
  } )


## Viral suppression among PLHIV analysis
plots$predProb_EMM$vlsup$sex <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-sex`[[x]]$vlsup
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of viral suppression",
                       title = title_,
                       int.var = "sex" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_sex <- wrap_plots( c(
  plots$predProb_EMM$test12m$sex, 
  plots$predProb_EMM$arv$sex, 
  plots$predProb_EMM$vlsup$sex ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_sex, filename = "combined_predPlot_EMM_sex.png",
        path = path_out, width = 15, height = 15, dpi = 300 )

# *** EMM by restype ----

## HIV testing analysis
plots$predProb_EMM$test12m$restype <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-restype`[[x]]$test12m
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "restype" ) 
  } )

## ARV among PLHIV analysis
plots$predProb_EMM$arv$restype <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-restype`[[x]]$arv
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of ART use",
                       title = title_,
                       int.var = "restype" ) 
  } )


## Viral suppression among PLHIV analysis
plots$predProb_EMM$vlsup$restype <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-restype`[[x]]$vlsup
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of viral suppression",
                       title = title_,
                       int.var = "restype" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_restype <- wrap_plots( c(
  plots$predProb_EMM$test12m$restype, 
  plots$predProb_EMM$arv$restype, 
  plots$predProb_EMM$vlsup$restype ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_restype, filename = "combined_predPlot_EMM_restype.png",
        path = path_out, width = 15, height = 15, dpi = 300 )

# *** EMM by wealth_dichot ----

## HIV testing analysis
plots$predProb_EMM$test12m$wealth_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-wealth_dichot`[[x]]$test12m
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "wealth_dichot" ) 
  } )

## ARV among PLHIV analysis
plots$predProb_EMM$arv$wealth_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-wealth_dichot`[[x]]$arv
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of ART use",
                       title = title_,
                       int.var = "wealth_dichot" ) 
  } )


## Viral suppression among PLHIV analysis
plots$predProb_EMM$vlsup$wealth_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-wealth_dichot`[[x]]$vlsup
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of viral suppression",
                       title = title_,
                       int.var = "wealth_dichot" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_wealth_dichot <- wrap_plots( c(
  plots$predProb_EMM$test12m$wealth_dichot, 
  plots$predProb_EMM$arv$wealth_dichot, 
  plots$predProb_EMM$vlsup$wealth_dichot ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_wealth_dichot, filename = "combined_predPlot_EMM_wealth_dichot.png",
        path = path_out, width = 15, height = 15, dpi = 300 )

# *** EMM by age ----

## HIV testing analysis
plots$predProb_EMM$test12m$age_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-age_dichot`[[x]]$test12m
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "age_dichot" ) 
  } )

## ARV among PLHIV analysis
plots$predProb_EMM$arv$age_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-age_dichot`[[x]]$arv
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of ART use",
                       title = title_,
                       int.var = "age_dichot" ) 
  } )


## Viral suppression among PLHIV analysis
plots$predProb_EMM$vlsup$age_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-age_dichot`[[x]]$vlsup
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of viral suppression",
                       title = title_,
                       int.var = "age_dichot" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_age_dichot <- wrap_plots( c(
  plots$predProb_EMM$test12m$age_dichot, 
  plots$predProb_EMM$arv$age_dichot, 
  plots$predProb_EMM$vlsup$age_dichot ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_age_dichot, filename = "combined_predPlot_EMM_age_dichot.png",
        path = path_out, width = 15, height = 15, dpi = 300 )

# *** EMM by edu_dichot ----

## HIV testing analysis
plots$predProb_EMM$test12m$edu_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-edu_dichot`[[x]]$test12m
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of HIV testing",
                       title = title_,
                       int.var = "edu_dichot" ) 
  } )

## ARV among PLHIV analysis
plots$predProb_EMM$arv$edu_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-edu_dichot`[[x]]$arv
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of ART use",
                       title = title_,
                       int.var = "edu_dichot" ) 
  } )


## Viral suppression among PLHIV analysis
plots$predProb_EMM$vlsup$edu_dichot <- lapply( exposures_comm, function( x ) { 
  
  data_ <- resList$`EMM_regression-model-results_inter-edu_dichot`[[x]]$vlsup
  title_ <- ifelse(x == "stig_avg", "Discriminatory attitudes", ifelse(x == "stig_perc_avg", "Perceived stigma", "Shame of association"))
  makePredictionsPlot( datlst = data_, y_lab = "Predicted probability of viral suppression",
                       title = title_,
                       int.var = "edu_dichot" ) 
  } )

# Combine the plots using patchwork
combined_plot_preds_edu_dichot <- wrap_plots( c(
  plots$predProb_EMM$test12m$edu_dichot, 
  plots$predProb_EMM$arv$edu_dichot, 
  plots$predProb_EMM$vlsup$edu_dichot ), ncol = 3 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation( tag_levels = list(
    c( "a)", "", "b)", "", "c)", "",
       "d)", "", "e)", "", "f)", "",
       "g)", "", "h)", "", "i)", "")
  ))

ggsave( combined_plot_preds_edu_dichot, filename = "combined_predPlot_EMM_edu_dichot.png",
        path = path_out, width = 15, height = 15, dpi = 300 )

# * MAIN forest plot ----

# By default we will also plot the bias-corrected result, with Bias OR = 2
bias.corr = TRUE
bias.or = 2

# Set up data for function - by combining results from main and sensitivity analyses
res_all <- c( resList$`MAIN_regression-model-results`, 
               resList$`SA_indAntAndExp_regression-model-results`, 
               list(simulations = resList$`simulations-collider-stratification.rds` ) )

# Apply function to exposures and outcomes

# ART analysis
plots$forest_MAIN$arv <- makeForestPlot( data = res_all, 
                                         exposures = exposures_ind,
                                         outcome = "arv",
                                         x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                         bias.or = bias.or,
                                         int.var = NULL)

# Viral suppression analysis
plots$forest_MAIN$vlsup <- makeForestPlot( data = res_all, 
                                           exposures = exposures_ind,
                                           outcome = "vlsup",
                                           x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                           bias.or = bias.or)

# Combine the plots using patchwork
combined_plot_forest <- wrap_plots(list(
  plots$forest_MAIN$arv,
  plots$forest_MAIN$vlsup ), ncol = 1 ) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest, filename = "combined_forestPlot.png",
        path = path_out, width = 9, height = 2 * length( exposures_ind ) + if( bias.corr ) 2 else { 1 }, dpi = 300 )

# * SA forest plot ----

## VLS among PLHIV on ART

plots$forest_SA_onART$vlsup <- makeForestPlot( data = resList$`SA_onART_regression-model-results`, 
                                                   exposures = "ant_exp_stig",
                                                   outcome = "vlsup",
                                                   x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                                   bias.corr = FALSE,
                                                   int.var = NULL )

ggsave( plots$forest_SA_onART$vlsup, filename = "forestPlot_SA_onART.png",
        path = path_out, width = 9, height = 2, dpi = 300 )


# * EMM forest plots ----

# *** EMM by sex ----

# ART analysis
plots$forest_EMM$sex$arv <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-sex`, 
                                         exposures = "ant_exp_stig",
                                         outcome = "arv",
                                         x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                         bias.corr = FALSE,
                                         int.var = "sex",
                                         int.level0 = "female",
                                         int.level1 = "male")

# Viral suppression analysis
plots$forest_EMM$sex$vlsup <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-sex`, 
                                            exposures = "ant_exp_stig",
                                            outcome = "vlsup",
                                            x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                            bias.corr = FALSE,
                                            int.var = "sex",
                                            int.level0 = "female",
                                            int.level1 = "male")

# Combine the plots using patchwork
combined_plot_forest_emm_sex <- wrap_plots(list(
  plots$forest_EMM$sex$arv,
  plots$forest_EMM$sex$vlsup ), ncol = 1) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest_emm_sex, filename = "combined_forestPlot_EMM_sex.png",
        path = path_out, width = 8, height = 4 , dpi = 300 )

# *** EMM by age ----

# ART analysis
plots$forest_EMM$age_dichot$arv <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-age_dichot`, 
                                            exposures = "ant_exp_stig",
                                            outcome = "arv",
                                            x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                            bias.corr = FALSE,
                                            int.var = "age_dichot",
                                            int.level0 = "0",
                                            int.level1 = "1")

# Viral suppression analysis
plots$forest_EMM$age_dichot$vlsup <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-age_dichot`, 
                                              exposures = "ant_exp_stig",
                                              outcome = "vlsup",
                                              x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                              bias.corr = FALSE,
                                              int.var = "age_dichot",
                                              int.level0 = "0",
                                              int.level1 = "1")

# Combine the plots using patchwork
combined_plot_forest_emm_age_dichot <- wrap_plots(list(
  plots$forest_EMM$age_dichot$arv,
  plots$forest_EMM$age_dichot$vlsup ), ncol = 1) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest_emm_age_dichot, filename = "combined_forestPlot_EMM_age_dichot.png",
        path = path_out, width = 8, height = 4 , dpi = 300 )

# *** EMM by restype ----

# ART analysis
plots$forest_EMM$restype$arv <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-restype`, 
                                                   exposures = "ant_exp_stig",
                                                   outcome = "arv",
                                                   x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                                   bias.corr = FALSE,
                                                   int.var = "restype",
                                                   int.level0 = "Urban",
                                                   int.level1 = "Rural")

# Viral suppression analysis
plots$forest_EMM$restype$vlsup <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-restype`, 
                                                     exposures = "ant_exp_stig",
                                                     outcome = "vlsup",
                                                     x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                                     bias.corr = FALSE,
                                                     int.var = "restype",
                                                     int.level0 = "Urban",
                                                     int.level1 = "Rural")

# Combine the plots using patchwork
combined_plot_forest_emm_restype <- wrap_plots(list(
  plots$forest_EMM$restype$arv,
  plots$forest_EMM$restype$vlsup ), ncol = 1) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest_emm_restype, filename = "combined_forestPlot_EMM_restype.png",
        path = path_out, width = 8, height = 4 , dpi = 300 )

# *** EMM by wealth_dichot ----

# ART analysis
plots$forest_EMM$wealth_dichot$arv <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-wealth_dichot`, 
                                                exposures = "ant_exp_stig",
                                                outcome = "arv",
                                                x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                                bias.corr = FALSE,
                                                int.var = "wealth_dichot",
                                                int.level0 = "0",
                                                int.level1 = "1")

# Viral suppression analysis
plots$forest_EMM$wealth_dichot$vlsup <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-wealth_dichot`, 
                                                  exposures = "ant_exp_stig",
                                                  outcome = "vlsup",
                                                  x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                                  bias.corr = FALSE,
                                                  int.var = "wealth_dichot",
                                                  int.level0 = "0",
                                                  int.level1 = "1")

# Combine the plots using patchwork
combined_plot_forest_emm_wealth_dichot <- wrap_plots(list(
  plots$forest_EMM$wealth_dichot$arv,
  plots$forest_EMM$wealth_dichot$vlsup ), ncol = 1) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest_emm_wealth_dichot, filename = "combined_forestPlot_EMM_wealth_dichot.png",
        path = path_out, width = 8, height = 4 , dpi = 300 )

# *** EMM by edu_dichot ----

# ART analysis
plots$forest_EMM$edu_dichot$arv <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-edu_dichot`, 
                                                      exposures = "ant_exp_stig",
                                                      outcome = "arv",
                                                      x_lab_forest = "Prevalence ratio (PR) for impact on ART use",
                                                      bias.corr = FALSE,
                                                      int.var = "edu_dichot",
                                                      int.level0 = "0",
                                                      int.level1 = "1")

# Viral suppression analysis
plots$forest_EMM$edu_dichot$vlsup <- makeForestPlot( data = resList$`EMM_regression-model-results_inter-edu_dichot`, 
                                                        exposures = "ant_exp_stig",
                                                        outcome = "vlsup",
                                                        x_lab_forest = "Prevalence ratio (PR) for impact on viral suppression",
                                                        bias.corr = FALSE,
                                                        int.var = "edu_dichot",
                                                        int.level0 = "0",
                                                        int.level1 = "1")

# Combine the plots using patchwork
combined_plot_forest_emm_edu_dichot <- wrap_plots(list(
  plots$forest_EMM$edu_dichot$arv,
  plots$forest_EMM$edu_dichot$vlsup ), ncol = 1) + 
  plot_layout( guides = "collect" ) +
  plot_annotation(tag_levels = list(
    c( "a)", "b)" )
  ) )

ggsave( combined_plot_forest_emm_edu_dichot, filename = "combined_forestPlot_EMM_edu_dichot.png",
        path = path_out, width = 9, height = 4 , dpi = 300 )


# * HETEROGENEITY forest plots ----
resHetero <- resList$`SA_heterogeneity_regression-model-results`

# Initialize list to store meta-analysis results
meta_results <- list()

# Loop through each exposure
for (exposure in unique(unlist(map(resHetero, ~ names(.))))) {
  # Loop through each outcome for the current exposure
  for (outcome in unique(unlist(map(resHetero, ~ names(.x[[exposure]]))))) {
    
    # Initialize a list to store PR data for the current exposure-outcome combination across surveys
    pooled_data_list <- list()
    
    # Loop through each survey to extract adjusted PR data
    for (survey in names(resHetero)) {
      # Check if the current survey has the specified exposure and outcome
      if (exposure %in% names(resHetero[[survey]]) && outcome %in% names(resHetero[[survey]][[exposure]])) {
        
        # Extract the PR data
        regModCoeffs <- resHetero[[survey]][[exposure]][[outcome]]$regModCoeffs
        
        # Filter rows where analysis == "adjusted" and variable == "exposure"
        adjusted_data <- regModCoeffs %>%
          filter(analysis == "adjusted" & variable == "exposure") %>%
          mutate(surveyid = survey)
        
        # Add to pooled data list if any rows meet the criteria
        if (nrow(adjusted_data) > 0) {
          pooled_data_list[[survey]] <- adjusted_data %>%
            select(surveyid, Estimate, lci, uci, Std.err) %>% 
            mutate(
              Estimate = Estimate * 0.1,
              lci = lci * 0.1,
              uci = uci * 0.1,
              Std.err = Std.err * 0.1
            )
        }
      }
    }
    
    # Combine the pooled data for this exposure-outcome combination
    pooled_data <- bind_rows(pooled_data_list)
    
    # Check if we have any data to pool
    if (nrow(pooled_data) > 0) {
      # Rename Std.err to se for consistency
      pooled_data <- pooled_data %>%
        rename(se = Std.err )
      
      pooled_data <- escalc(measure = "RR", yi = Estimate, sei = se, data = pooled_data, slab = surveyid)
      
      # Perform meta-analysis (pool log(PRs) across surveys)
      res_re <- rma(yi = yi, vi = vi, data = pooled_data)
      res_fe <- rma(yi = yi, vi = vi, data = pooled_data, method = "FE")
      
      # Store the result in the meta_results list
      meta_results[[paste(exposure, outcome, sep = "_")]] <- list( res_re, res_fe )
    }
  }
}

for (name in names(meta_results)) {
  res_re <- meta_results[[name]][[ 1 ]]
  res_fe <- meta_results[[name]][[ 2 ]]
  
  # Determine exposure and outcome labels
  exposure <- case_when(
    grepl("stig_avg", name) ~ "Discriminatory attitudes towards PLHIV",
    grepl("stig_perc_avg", name) ~ "Perceived HIV stigma",
    grepl("social_judge_avg", name) ~ "Shame of association",
    grepl("stig_anticip", name) ~ "Anticipated stigma",
    grepl("med_stig", name) ~ "Experienced discrimination",
    grepl("ant_exp_stig", name) ~ "Anticipated/experienced stigma",
    TRUE ~ "Unknown exposure"
  )
  
  outcome <- case_when(
    grepl("test12m", name) ~ "HIV testing",
    grepl("arv", name) ~ "ART use",
    grepl("vlsup", name) ~ "Viral suppression",
    TRUE ~ "Unknown outcome"
  )
  
  if (grepl("avg", name)) {
    alim <- c(0.8, 1.3)
  } else {
    alim <- c(0.9, 1.1)
  }
  
  if (grepl("avg", name)) {
    xlim <- c(0.4, 1.7)
  } else {
    xlim <- c(0.8, 1.2)
  }
  
  # Define file name
  filename <- paste0(path_out, "heterogeneity/forestplot_", name, ".png")
  
  # Get number of studies
  num_studies <- length(res_re$yi)
  
  # Set dynamic height: Base height 5 inches, plus 0.3 inches per study
  plot_height <- max(2, 2 + (num_studies * 0.35))  # Ensure minimum height of 5 inches
  
  # Save as PNG with dynamic height
  png(filename, width = 6, height = plot_height, units = "in", res = 300)
  
  par(oma = c(2, 2, 2, 2))  # Reduces outer margins (bottom, left, top, right)
  par(mar = c(4, 4, 2, 2))  # Reduces inner margins (bottom, left, top, right)
  par(las = 1, mgp = c(2, 0.5, 0))  # Adjust label spacing
  
  # Create the forest plot
  efac_value <- 0.2#max(0.5, min(3, 3 * (5 / num_studies)))
  forest(res_re, 
        # main = paste0("Exposure: ", exposure, "\nOutcome: ", outcome),
         xlab = "Prevalence Ratio (PR)",
         transf = exp,
         xlim = xlim,
         alim = alim,
         at = if (grepl("avg", name)) {
           seq(0, 5, 0.1)
         } else {
           seq(0, 5, 0.05)
         },
         refline = 1,
         rows = num_studies + 3,
         addfit = FALSE,
         efac = c( efac_value, efac_value) )  
  mtext(side = 3, line = -1, adj = 0.5, cex = 1.2, text = paste0("Exposure: ", exposure, "\nOutcome: ", outcome), font = 2)
  
  abline( h = 3)
  # Get plot coordinate limits
  usr <- par("usr")  # Get plot boundaries
  
  par( xpd = TRUE )
  
  # Calculate y-coordinate for "RE Model" row
  y_pos <- 0  # "RE Model" is usually one row below the last study
  
  # X-position for I² statistic (aligning with left side of the plot)
  x_pos <- usr[1] + (usr[2] - usr[1]) * 0.18
  
  # Add I² statistic at the same y-position as "RE Model"
  text(x = x_pos, y = 2, 
       labels = paste0("(I² = ", round(res_re$I2, 0), "%)"), 
       pos = 4, cex = 1)
  
  # text(x = x_pos, y = 1, # This is I-squared for fixed-effects, we won't show
  #      labels = paste0("(I² = ", round(res_fe$I2, 0), "%)"), 
  #      pos = 4, cex = 1)
  
  addpoly( res_re, row = 2,
           efac = c(efac_value, efac_value) )
  addpoly( res_fe, row = 1,
           efac = c(efac_value, efac_value) )
  
  dev.off()  # Close the PNG device
}


# * RIDGE PLOT of distribution of community-level stigma ----
df$surveyid <- as.character(df$surveyid)
df$surveyid <- factor(df$surveyid, levels = rev(sort(unique(df$surveyid))))

df_psu <- df %>%
  distinct(surveyid, psu_u, .keep_all = TRUE)

median_stig <- df_psu %>%
  group_by(surveyid) %>%
  summarise(median_value_stig = as.numeric(median(stig_avg, na.rm = TRUE)))

df_psu <- merge(df_psu, median_stig, by = "surveyid")

median_perc <- df_psu %>%
  group_by(surveyid) %>%
  summarise(median_value_perc = as.numeric(median(stig_perc_avg, na.rm = TRUE)))
df_psu <- merge(df_psu, median_perc, by = "surveyid")

median_social <- df_psu %>%
  group_by(surveyid) %>%
  summarise(median_value_social = as.numeric(median(social_judge_avg, na.rm = TRUE)))
df_psu <- merge(df_psu, median_social, by = "surveyid")

# discriminatory attitudes
p1 <- ggplot(df_psu, aes(x = stig_avg, y = surveyid, fill = median_value_stig)) +
      ggridges::geom_density_ridges(scale = 1.4, alpha = 0.8, 
                                    linewidth = 0.2, rel_min_height = 0.02) +
      xlim(0, 1) +  
      theme_minimal() +
      labs(title = NULL, x = "Community-level discriminatory attitudes", y = NULL) +
      scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) + 
      guides(fill = "none")
ggsave(paste0(path_out, "ridges_discriminatory-attitudes.png"), plot = p1, width = 8, height = 40, dpi = 300)

# perceived stigma
p2 <- ggplot(df_psu, aes(x = stig_perc_avg, y = surveyid, fill = median_value_perc)) +
      ggridges::geom_density_ridges(scale = 0.6, alpha = 0.8,
                                    linewidth = 0.2, rel_min_height = 0.02) +
      xlim(0, 1) +  
      theme_minimal() +
      labs(title = NULL, x = "Community-level perceived stigma", y = NULL) +
      scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) + 
      guides(fill = "none")
ggsave(paste0(path_out, "ridges_perceived-stigma.png"), plot = p2, width = 8, height = 40, dpi = 300)

# perceived stigma
p3 <- ggplot(df_psu, aes(x = social_judge_avg, y = surveyid, fill = median_value_social)) +
      ggridges::geom_density_ridges(scale = 0.6, alpha = 0.8, 
                                    linewidth = 0.2, rel_min_height = 0.02) +
      xlim(0, 1) +  
      theme_minimal() +
      labs(title = NULL, x = "Community-level shame of association", y = NULL) +
      scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) + 
      guides(fill = "none")
ggsave(paste0(path_out, "ridges_shame-association.png"), plot = p3, width = 8, height = 40, dpi = 300)

library(patchwork)
p2_ <- p2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p3_ <- p3 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
combined_plot <- p1 + p2_ + p3_ + 
                plot_layout(ncol = 3, guides = "collect")
ggsave(paste0(path_out, "ridges_combined_plot.png"), 
       plot = combined_plot, width = 15, height = 30, dpi = 300)

# * Correlation of community-level stigma ----
corrStigmaVars_comm_a <- df %>% select( exposures_comm[1:2] ) %>% na.omit() %>% cor() %>% data.frame() %>% mutate( variable = exposures_comm[1:2] )
corrStigmaVars_comm_b <- df %>% select( exposures_comm[2:3] ) %>% na.omit() %>% cor() %>% data.frame() %>% mutate( variable = exposures_comm[2:3] )
corrStigmaVars_comm_c <- df %>% select( exposures_comm[c(1,3)] ) %>% na.omit() %>% cor() %>% data.frame() %>% mutate( variable = exposures_comm[c(1,3)] )
corrStigmaVars_ind <- df %>% select( exposures_ind ) %>% na.omit() %>% cor()

corrStigmaVars_comm <- full_join( corrStigmaVars_comm_a, corrStigmaVars_comm_b ) %>%
  relocate( variable, .before = everything() )
corrStigmaVars_comm$stig_avg[ corrStigmaVars_comm$variable == "social_judge_avg" ] <- 
  corrStigmaVars_comm_c$stig_avg[ corrStigmaVars_comm_c$variable == "social_judge_avg" ]
corrStigmaVars_comm$social_judge_avg[ corrStigmaVars_comm$variable == "stig_avg" ] <- 
  corrStigmaVars_comm_c$social_judge_avg[ corrStigmaVars_comm_c$variable == "stig_avg" ]
corrStigmaVars_comm <- corrStigmaVars_comm %>% select( - variable ) %>% as.matrix()
  
labels_corrPlot_comm <- labels_stigma[ names( labels_stigma ) %in% exposures_comm ]
labels_corrPlot_ind <- sub( "\n.*", "", labels_stigma[ names( labels_stigma ) %in% exposures_ind ] )

rownames( corrStigmaVars_comm ) <- colnames( corrStigmaVars_comm ) <- labels_corrPlot_comm
rownames( corrStigmaVars_ind ) <- colnames( corrStigmaVars_ind ) <- labels_corrPlot_ind

png( filename = paste0( path_out, "corrStigmaVars_comm.png" ), width = 1400, height = 1200, res = 150 )
corrplot( corrStigmaVars_comm, method = "ellipse", type = "upper", addCoef.col ='black', 
          order = "original", tl.srt = 45,  tl.col = "black" )
dev.off()

png( filename = paste0( path_out, "corrStigmaVars_ind.png" ), width = 1400, height = 1200, res = 150 )
corrplot( corrStigmaVars_ind, method = "ellipse", type = "upper", addCoef.col ='black', 
          order = "original", tl.srt = 45,  tl.col = "black" )
dev.off()


