
library( geepack )
library( openxlsx )
library( purrr )

# code to simulate the impact of collider-stratification bias
path_data <- here::here( "data" )
path_out <- here::here( "outputs/R-regression-results/" )

# source functions
source( "0.0-functions.R" )

# read in the data 
dat_original <- as.data.frame( readRDS( paste0( path_data, "/all_surveys_2025-03-18.rds" ) ) )
nrow(dat_original)

# create list for results
all_res <- list()

# ---- anticipated stigma in healthcare -----
# ---- arv analysis ----
# we replicate the adjusted analysis "sex", "age", "school", "wealth", "stig_avg", "surveyid" 
# we run the original analysis (not adjusted for collider-stratification bias)
dat <- dat_original[!is.na(dat_original$arv) & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat_unadjusted <- subset(dat, select = c("stig_anticip", 
                              "psu_u", "arv", 
                              "sex", "age", "school", "wealth", "surveyid", "stig_avg"))#,
                               #"comp_know", "restype", 
                               #"stig_avg"))#, "social_judge_avg", "stig_perc_avg"))
summary(dat_unadjusted)
nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted <- na.omit(dat_unadjusted); nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted[] <- lapply(dat_unadjusted, function(x) if (is.factor(x)) droplevels(x) else x)

fit_crude <- geeglm(arv ~ stig_anticip + sex + age + school + wealth + 
                            #stig_avg + #comp_know + social_judge_avg + stig_perc_avg + 
                            surveyid,
                          data = dat_unadjusted,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "poisson" )
summary(fit_crude)
original <- data.frame(sim = "original", 
                  pr = exp(coef(fit_crude)[2]),
                  lci = exp(coef(fit_crude)[2] - qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  uci = exp(coef(fit_crude)[2] + qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  est = coef(fit_crude)[2], se = sqrt(vcov(fit_crude)[2, 2]) )
original

# imputation of anticipated stigma 
# we restrict to the sample of plhiv with biomarkers for VLS
# we also exclude 3 surveys that did not collect the exposure
dat <- dat_original[!is.na(dat_original$arv) & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat <- dat[!(dat$surveyid %in% c("LS2014DHS", "MZ2015AIS", "ZA2017SABSSM")), ]
dat <- dat[order(dat$psu_u), ]
dat <- subset(dat, select = c("stig_anticip", 
                              "psu_u", "arv", "arv", "nondisc", "evertest", "stig_avg",
                              "sex", "age", "restype", "school", "wealth", "surveyid"))
dat[] <- lapply(dat, function(x) if (is.factor(x)) droplevels(x) else x)
nrow(dat); length(unique(dat$surveyid))
dat_nona <- subset(dat, select = c("stig_anticip", "psu_u",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat_nona <- na.omit(dat_nona)
nrow(dat_nona); length(unique(dat_nona$surveyid))
dat_nona[] <- lapply(dat_nona, function(x) if (is.factor(x)) droplevels(x) else x)
dat_nona <- dat_nona[order(dat_nona$psu_u), ]
nrow(dat_nona); length(unique(dat_nona$surveyid))

# we predict anticipated stigma
fit_stigma <- geeglm(stig_anticip ~ sex + age + school + wealth + surveyid + stig_avg,
                          data = dat_nona,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "binomial" )
# we predict everyone's outcome
dat$surveyid <- factor(dat$surveyid, levels = levels(dat_nona$surveyid))
dat$stigma_pred <- predict(fit_stigma, dat)
# we assumed that if the exposure is missing (collider-stratification), 
# that they are X more likely to have experienced stigma

# We simulate the datasets
set.seed(2025)
n_sim <- 20
pr_collider <- c(1.25, 1.50, 1.75, 2.00, 2.50, 3.00)
results <- NULL
val <- NULL

dat$exp_imputed <- ifelse(is.na(dat$stig_anticip), 1, 0); nrow(dat)
# we restrict to plhiv who likely know their status (evertest == 1)
dat_sim <- dat[!is.na(dat$stigma_pred), ]; nrow(dat_sim)
dat_sim <- dat_sim[!is.na(dat_sim$evertest) & dat_sim$evertest == 1, ]; nrow(dat_sim)

for (i in 1:length(pr_collider)) {
  pr_collider_i <- pr_collider[i]
  dat_sim$stigma_pred_new <- plogis(dat_sim$stigma_pred + log(pr_collider_i))
  
    for (j in 1:n_sim) {
        dat_sim$stigma_pred_rnd <- rbinom(nrow(dat_sim), size = 1, prob = dat_sim$stigma_pred_new)
        dat_sim$stigma_new <- dat_sim$stig_anticip
        dat_sim$stigma_new[dat_sim$exp_imputed == 1] <- dat_sim$stigma_pred_rnd[dat_sim$exp_imputed == 1]
        
        dat_adj <- subset(dat_sim, select = c("stigma_new", 
                                      "psu_u", "arv",
                                      "sex", "age", "school", "wealth", "surveyid", "stig_avg"))
        dat_adj <- na.omit(dat_adj); nrow(dat_adj)
        fit_adj <- geeglm(arv ~ stigma_new + sex + age + school + wealth + stig_avg + surveyid,
                                  data = dat_adj,
                                  id = psu_u,
                                  corstr = "exchangeable",
                                  family = "poisson" )
        
        res <- data.frame(sim = j, pr = exp(coef(fit_adj)[2]),
                          lci = exp(coef(fit_adj)[2] - qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          uci = exp(coef(fit_adj)[2] + qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          est = coef(fit_adj)[2], 
                          se = sqrt(vcov(fit_adj)[2, 2]))
      results <- rbind(results, res)
      }

      pooled <- mean(results$est)
      pooled_var_within <- mean(results$se^2)
      pooled_var_between <- sum((results$est - pooled)^2) / (nrow(results) - 1)
      pooled_se <- sqrt(pooled_var_within + pooled_var_between)
      pooled <- data.frame(sim = pr_collider_i, 
                        pr = exp(pooled),
                        lci = exp(pooled - qnorm(0.975) * pooled_se),
                        uci = exp(pooled + qnorm(0.975) * pooled_se),
                        est = pooled, se = pooled_se )
    val <- rbind(val, pooled)
  }

name_val <- data.frame(sim = "anticipated stigma -> arv", pr = NA, lci = NA, uci = NA, est = NA, se = NA)
all_res$stig_anticip$arv <- rbind(original, val)
rownames(all_res$stig_anticip$arv) <- NULL
all_res$stig_anticip$arv
      

# ---- vls analysis ----
# we replicate the adjusted analysis
# we run the original analysis (not adjusted for collider-stratification bias)
dat <- dat_original[!is.na(dat_original$vlsup) & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat_unadjusted <- subset(dat, select = c("stig_anticip", 
                              "psu_u", "vlsup", 
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))#,
                               #"comp_know", 
                               #"stig_avg"))#, "social_judge_avg", "stig_perc_avg"))
summary(dat_unadjusted)
nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted <- na.omit(dat_unadjusted); nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted[] <- lapply(dat_unadjusted, function(x) if (is.factor(x)) droplevels(x) else x)

fit_crude <- geeglm(vlsup ~ stig_anticip + sex + age + school + wealth + stig_avg +
                            #stig_avg + #comp_know + social_judge_avg + stig_perc_avg + 
                            surveyid,
                          data = dat_unadjusted,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "poisson" )
summary(fit_crude)
original <- data.frame(sim = "original", 
                  pr = exp(coef(fit_crude)[2]),
                  lci = exp(coef(fit_crude)[2] - qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  uci = exp(coef(fit_crude)[2] + qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  est = coef(fit_crude)[2], se = sqrt(vcov(fit_crude)[2, 2]) )
original

# imputation of anticipated stigma 
# we restrict to the sample of plhiv with biomarkers for VLS
# we also exclude 3 surveys that did not collect the exposure
dat <- dat_original[!is.na(dat_original$vlsup) & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat <- dat[!(dat$surveyid %in% c("LS2014DHS", "MZ2015AIS", "ZA2017SABSSM")), ]
dat <- dat[order(dat$psu_u), ]
dat <- subset(dat, select = c("stig_anticip", 
                              "psu_u", "arv", "vlsup", "nondisc", "evertest",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat[] <- lapply(dat, function(x) if (is.factor(x)) droplevels(x) else x)
nrow(dat); length(unique(dat$surveyid))
dat_nona <- subset(dat, select = c("stig_anticip", "psu_u",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat_nona <- na.omit(dat_nona)
nrow(dat_nona); length(unique(dat_nona$surveyid))
dat_nona[] <- lapply(dat_nona, function(x) if (is.factor(x)) droplevels(x) else x)
dat_nona <- dat_nona[order(dat_nona$psu_u), ]
nrow(dat_nona); length(unique(dat_nona$surveyid))

# we predict anticipated stigma
fit_stigma <- geeglm(stig_anticip ~ sex + age + school + wealth + stig_avg + surveyid,
                          data = dat_nona,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "binomial" )
# we predict everyone's outcome
dat$surveyid <- factor(dat$surveyid, levels = levels(dat_nona$surveyid))
dat$stigma_pred <- predict(fit_stigma, dat)
# we assumed that if the exposure is missing (collider-stratification), 
# that they are X more likely to have experienced stigma

# We simulate the datasets
set.seed(2025)
n_sim <- 20
pr_collider <- c(1.25, 1.50, 1.75, 2.00, 2.50, 3.00)
results <- NULL
val <- NULL

dat$exp_imputed <- ifelse(is.na(dat$stig_anticip), 1, 0); nrow(dat)
# we restrict to plhiv who likely know their status (evertest == 1)
dat_sim <- dat[!is.na(dat$stigma_pred), ]; nrow(dat_sim)
dat_sim <- dat_sim[!is.na(dat_sim$evertest) & dat_sim$evertest == 1, ]; nrow(dat_sim)

for (i in 1:length(pr_collider)) {
  pr_collider_i <- pr_collider[i]
  dat_sim$stigma_pred_new <- plogis(dat_sim$stigma_pred + log(pr_collider_i))
  
    for (j in 1:n_sim) {
        dat_sim$stigma_pred_rnd <- rbinom(nrow(dat_sim), size = 1, prob = dat_sim$stigma_pred_new)
        dat_sim$stigma_new <- dat_sim$stig_anticip
        dat_sim$stigma_new[dat_sim$exp_imputed == 1] <- dat_sim$stigma_pred_rnd[dat_sim$exp_imputed == 1]
        
        dat_adj <- subset(dat_sim, select = c("stigma_new", 
                                      "psu_u", "vlsup",
                                      "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
        dat_adj <- na.omit(dat_adj); nrow(dat_adj)
        fit_adj <- geeglm(vlsup ~ stigma_new + sex + age + school + wealth + stig_avg + surveyid,
                                  data = dat_adj,
                                  id = psu_u,
                                  corstr = "exchangeable",
                                  family = "poisson" )
        
        res <- data.frame(sim = j, pr = exp(coef(fit_adj)[2]),
                          lci = exp(coef(fit_adj)[2] - qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          uci = exp(coef(fit_adj)[2] + qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          est = coef(fit_adj)[2], 
                          se = sqrt(vcov(fit_adj)[2, 2]))
      results <- rbind(results, res)
      }

      pooled <- mean(results$est)
      pooled_var_within <- mean(results$se^2)
      pooled_var_between <- sum((results$est - pooled)^2) / (nrow(results) - 1)
      pooled_se <- sqrt(pooled_var_within + pooled_var_between)
      pooled <- data.frame(sim = pr_collider_i, 
                        pr = exp(pooled),
                        lci = exp(pooled - qnorm(0.975) * pooled_se),
                        uci = exp(pooled + qnorm(0.975) * pooled_se),
                        est = pooled, se = pooled_se )
    val <- rbind(val, pooled)
  }

name_val <- data.frame(sim = "anticipated stigma -> vls", pr = NA, lci = NA, uci = NA, est = NA, se = NA)
all_res$stig_anticip$vlsup <- rbind(original, val)
rownames(all_res$stig_anticip$vlsup) <- NULL
all_res$stig_anticip$vlsup




# ---- experienced/anticipated stigma in healthcare -----
# ---- arv analysis ----
# we replicate the adjusted analysis
# we run the original analysis (not adjusted for collider-stratification bias)
dat <- dat_original[!is.na(dat_original$arv)  & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat_unadjusted <- subset(dat, select = c("ant_exp_stig", 
                              "psu_u", "arv", 
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))#,
                               #"comp_know", 
                               #"stig_avg"))#, "social_judge_avg", "stig_perc_avg"))
summary(dat_unadjusted)
nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted <- na.omit(dat_unadjusted); nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted[] <- lapply(dat_unadjusted, function(x) if (is.factor(x)) droplevels(x) else x)

fit_crude <- geeglm(arv ~ ant_exp_stig + sex + age + school + wealth + stig_avg +
                            #stig_avg + #comp_know + social_judge_avg + stig_perc_avg + 
                            surveyid,
                          data = dat_unadjusted,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "poisson" )
summary(fit_crude)
original <- data.frame(sim = "original", 
                  pr = exp(coef(fit_crude)[2]),
                  lci = exp(coef(fit_crude)[2] - qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  uci = exp(coef(fit_crude)[2] + qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  est = coef(fit_crude)[2], se = sqrt(vcov(fit_crude)[2, 2]) )
original

# imputation of anticipated stigma 
# we restrict to the sample of plhiv with biomarkers for VLS
# we also exclude 3 surveys that did not collect the exposure
dat <- dat_original[!is.na(dat_original$arv) & dat_original$hivstatus == 1 & (dat_original$incidence == 0 | is.na(dat_original$incidence)), ]; nrow(dat)
dat <- dat[!(dat$surveyid %in% c("LS2014DHS", "MZ2015AIS", "ZA2017SABSSM")), ]
dat <- dat[order(dat$psu_u), ]
dat <- subset(dat, select = c("ant_exp_stig", 
                              "psu_u", "arv", "vlsup", "nondisc", "evertest",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat[] <- lapply(dat, function(x) if (is.factor(x)) droplevels(x) else x)
nrow(dat); length(unique(dat$surveyid))
dat_nona <- subset(dat, select = c("ant_exp_stig", "psu_u",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat_nona <- na.omit(dat_nona)
nrow(dat_nona); length(unique(dat_nona$surveyid))
dat_nona[] <- lapply(dat_nona, function(x) if (is.factor(x)) droplevels(x) else x)
dat_nona <- dat_nona[order(dat_nona$psu_u), ]
nrow(dat_nona); length(unique(dat_nona$surveyid))

# we predict anticipated stigma
fit_stigma <- geeglm(ant_exp_stig ~ sex + age + school + wealth + stig_avg + surveyid,
                          data = dat_nona,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "binomial" )
# we predict everyone's outcome
dat$surveyid <- factor(dat$surveyid, levels = levels(dat_nona$surveyid))
dat$stigma_pred <- predict(fit_stigma, dat)
# we assumed that if the exposure is missing (collider-stratification), 
# that they are X more likely to have experienced stigma

# We simulate the datasets
set.seed(2025)
n_sim <- 20
pr_collider <- c(1.25, 1.50, 1.75, 2.00, 2.50, 3.00)
results <- NULL
val <- NULL

dat$exp_imputed <- ifelse(is.na(dat$ant_exp_stig), 1, 0); nrow(dat)
# we restrict to plhiv who likely know their status (evertest == 1)
dat_sim <- dat[!is.na(dat$stigma_pred), ]; nrow(dat_sim)
dat_sim <- dat_sim[!is.na(dat_sim$evertest) & dat_sim$evertest == 1, ]; nrow(dat_sim)

for (i in 1:length(pr_collider)) {
  pr_collider_i <- pr_collider[i]
  dat_sim$stigma_pred_new <- plogis(dat_sim$stigma_pred + log(pr_collider_i))
  
    for (j in 1:n_sim) {
        dat_sim$stigma_pred_rnd <- rbinom(nrow(dat_sim), size = 1, prob = dat_sim$stigma_pred_new)
        dat_sim$stigma_new <- dat_sim$ant_exp_stig
        dat_sim$stigma_new[dat_sim$exp_imputed == 1] <- dat_sim$stigma_pred_rnd[dat_sim$exp_imputed == 1]
        
        dat_adj <- subset(dat_sim, select = c("stigma_new", 
                                      "psu_u", "arv",
                                      "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
        dat_adj <- na.omit(dat_adj); nrow(dat_adj)
        fit_adj <- geeglm(arv ~ stigma_new + sex + age + school + wealth + stig_avg + surveyid,
                                  data = dat_adj,
                                  id = psu_u,
                                  corstr = "exchangeable",
                                  family = "poisson" )
        
        res <- data.frame(sim = j, pr = exp(coef(fit_adj)[2]),
                          lci = exp(coef(fit_adj)[2] - qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          uci = exp(coef(fit_adj)[2] + qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          est = coef(fit_adj)[2], 
                          se = sqrt(vcov(fit_adj)[2, 2]))
      results <- rbind(results, res)
      }

      pooled <- mean(results$est)
      pooled_var_within <- mean(results$se^2)
      pooled_var_between <- sum((results$est - pooled)^2) / (nrow(results) - 1)
      pooled_se <- sqrt(pooled_var_within + pooled_var_between)
      pooled <- data.frame(sim = pr_collider_i, 
                        pr = exp(pooled),
                        lci = exp(pooled - qnorm(0.975) * pooled_se),
                        uci = exp(pooled + qnorm(0.975) * pooled_se),
                        est = pooled, se = pooled_se )
    val <- rbind(val, pooled)
  }

name_val <- data.frame(sim = "experienced/anticip stigma -> arv", pr = NA, lci = NA, uci = NA, est = NA, se = NA)
all_res$ant_exp_stig$arv <- rbind(original, val)
rownames(all_res$ant_exp_stig$arv) <- NULL
all_res$ant_exp_stig$arv
      

# ---- vls analysis ----
# we replicate the adjusted analysis
# we run the original analysis (not adjusted for collider-stratification bias)
dat <- dat_original[!is.na(dat_original$vlsup), ]; nrow(dat)
dat_unadjusted <- subset(dat, select = c("ant_exp_stig", 
                              "psu_u", "vlsup", 
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))#,
                               #"comp_know", 
                               #"stig_avg"))#, "social_judge_avg", "stig_perc_avg"))
summary(dat_unadjusted)
nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted <- na.omit(dat_unadjusted); nrow(dat_unadjusted); length(unique(dat_unadjusted$surveyid))
dat_unadjusted[] <- lapply(dat_unadjusted, function(x) if (is.factor(x)) droplevels(x) else x)

fit_crude <- geeglm(vlsup ~ ant_exp_stig + sex + age + school + wealth + stig_avg +
                            #stig_avg + #comp_know + social_judge_avg + stig_perc_avg + 
                            surveyid,
                          data = dat_unadjusted,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "poisson" )
summary(fit_crude)
original <- data.frame(sim = "original", 
                  pr = exp(coef(fit_crude)[2]),
                  lci = exp(coef(fit_crude)[2] - qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  uci = exp(coef(fit_crude)[2] + qnorm(0.975) * sqrt(vcov(fit_crude)[2, 2])),
                  est = coef(fit_crude)[2], se = sqrt(vcov(fit_crude)[2, 2]) )
original

# imputation of anticipated stigma 
# we restrict to the sample of plhiv with biomarkers for VLS
# we also exclude 3 surveys that did not collect the exposure
dat <- dat_original[!is.na(dat_original$vlsup), ]; nrow(dat)
dat <- dat[!(dat$surveyid %in% c("LS2014DHS", "MZ2015AIS", "ZA2017SABSSM")), ]
dat <- dat[order(dat$psu_u), ]
dat <- subset(dat, select = c("ant_exp_stig", 
                              "psu_u", "arv", "vlsup", "nondisc", "evertest",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat[] <- lapply(dat, function(x) if (is.factor(x)) droplevels(x) else x)
nrow(dat); length(unique(dat$surveyid))
dat_nona <- subset(dat, select = c("ant_exp_stig", "psu_u",
                              "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
dat_nona <- na.omit(dat_nona)
nrow(dat_nona); length(unique(dat_nona$surveyid))
dat_nona[] <- lapply(dat_nona, function(x) if (is.factor(x)) droplevels(x) else x)
dat_nona <- dat_nona[order(dat_nona$psu_u), ]
nrow(dat_nona); length(unique(dat_nona$surveyid))

# we predict anticipated stigma
fit_stigma <- geeglm(ant_exp_stig ~ sex + age + school + wealth + stig_avg + surveyid,
                          data = dat_nona,
                          id = psu_u,
                          corstr = "exchangeable",
                          family = "binomial" )
# we predict everyone's outcome
dat$surveyid <- factor(dat$surveyid, levels = levels(dat_nona$surveyid))
dat$stigma_pred <- predict(fit_stigma, dat)
# we assumed that if the exposure is missing (collider-stratification), 
# that they are X more likely to have experienced stigma

# We simulate the datasets
set.seed(2025)
n_sim <- 20
pr_collider <- c(1.25, 1.50, 1.75, 2.00, 2.50, 3.00)
results <- NULL
val <- NULL

dat$exp_imputed <- ifelse(is.na(dat$ant_exp_stig), 1, 0); nrow(dat)
# we restrict to plhiv who likely know their status (evertest == 1)
dat_sim <- dat[!is.na(dat$stigma_pred), ]; nrow(dat_sim)
dat_sim <- dat_sim[!is.na(dat_sim$evertest) & dat_sim$evertest == 1, ]; nrow(dat_sim)

for (i in 1:length(pr_collider)) {
  pr_collider_i <- pr_collider[i]
  dat_sim$stigma_pred_new <- plogis(dat_sim$stigma_pred + log(pr_collider_i))
  
    for (j in 1:n_sim) {
        dat_sim$stigma_pred_rnd <- rbinom(nrow(dat_sim), size = 1, prob = dat_sim$stigma_pred_new)
        dat_sim$stigma_new <- dat_sim$ant_exp_stig
        dat_sim$stigma_new[dat_sim$exp_imputed == 1] <- dat_sim$stigma_pred_rnd[dat_sim$exp_imputed == 1]
        
        dat_adj <- subset(dat_sim, select = c("stigma_new", 
                                      "psu_u", "vlsup",
                                      "sex", "age", "school", "wealth", "stig_avg", "surveyid"))
        dat_adj <- na.omit(dat_adj); nrow(dat_adj)
        fit_adj <- geeglm(vlsup ~ stigma_new + sex + age + school + wealth + stig_avg + surveyid,
                                  data = dat_adj,
                                  id = psu_u,
                                  corstr = "exchangeable",
                                  family = "poisson" )
        
        res <- data.frame(sim = j, pr = exp(coef(fit_adj)[2]),
                          lci = exp(coef(fit_adj)[2] - qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          uci = exp(coef(fit_adj)[2] + qnorm(0.975) * sqrt(vcov(fit_adj)[2, 2])),
                          est = coef(fit_adj)[2], 
                          se = sqrt(vcov(fit_adj)[2, 2]))
      results <- rbind(results, res)
      }

      pooled <- mean( results$est )
      pooled_var_within <- mean( results$se^2 )
      pooled_var_between <- sum( ( results$est - pooled )^2 ) / ( nrow(results) - 1 )
      pooled_se <- sqrt( pooled_var_within + pooled_var_between )
      pooled <- data.frame( sim = pr_collider_i, 
                        pr = exp( pooled ),
                        lci = exp( pooled - qnorm(0.975) * pooled_se ),
                        uci = exp( pooled + qnorm(0.975) * pooled_se ),
                        est = pooled, se = pooled_se )
    val <- rbind( val, pooled )
  }

name_val <- data.frame(sim = "experienced/anticip stigma -> vls", pr = NA, lci = NA, uci = NA, est = NA, se = NA)
all_res$ant_exp_stig$vlsup <- rbind(original, val)
rownames(all_res$ant_exp_stig$vlsup) <- NULL
all_res$ant_exp_stig$vlsup

saveRDS( all_res, file = paste0( path_out, "/simulations-collider-stratification", ".rds" ) )

# Flatten the list and construct sheet names
flat_all_res <- map2(names(all_res), all_res, function(parent_name, child_list) {
  map2(names(child_list), child_list, function(child_name, df) {
    list(name = paste0(parent_name, "_", child_name), data = df)
  })
}) %>% flatten()

# Create a named list for writing to Excel
excel_data <- setNames(map(flat_all_res, "data"), map_chr(flat_all_res, "name"))
write.xlsx(excel_data, file = paste0(path_out, "/simulations-collider-stratification.xlsx"))

