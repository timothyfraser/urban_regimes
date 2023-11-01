#' @name 05_models.R
#' @author Tim Fraser

# 3. Models

# Format Dataset ######################################

library(tidyverse)
library(broom)
library(lmtest)

read_rds("raw_data/indices.rds")  %>%
  mutate(LDP_Komeito_house = voteshare_LDP_house + voteshare_Komeito_house,
         LDP_Komeito_pref = voteshare_LDP_pref + voteshare_Komeito_pref) %>%
  select(year, muni_code, pref, contains("regime"),
         pop, inhabitable_area, rev, income, age_elder, dis_restoration, dis_relief, 
         rev, rev_external, rev_to_exp,  damages, deaths, tsunami,
         migration, college, unemployed, social_capital, bonding, bridging, linking, 
         #LDP_Komeito_house, # too much missing data ~ 20%
         LDP_Komeito_pref) %>%
  mutate_at(vars(contains("regime")), list(~as.numeric(.))) %>%
  #LDP_Komeito, voter_turnout_pref, 
  #voter_turnout_house, ) %>%
  # A few cases are missing. Omit them so you can do likelihood ratio tests
  filter(muni_code != "01209") %>%
  mutate(year = factor(year)) %>%
  saveRDS("raw_data/dataset.rds") # Outlier control


# Do this just once!
library(mice)
read_rds("raw_data/dataset.rds") %>%
  mice(data = ., m = 5, seed = 12345) %>%
  saveRDS("raw_data/mi.rds")



# Table Functions ######################################
library(tidyverse)
library(broom)
library(texreg)
library(car)
library(mice)
library(gtools)

# write a function to get a refined texreg object for each
get_tex = function(mymodel){
  require(tidyverse)
  require(texreg)
  require(mice)
  # Presenting multiple imputation models and normal models together
  # is hard, because texreg puts their goodness of fit stats on different rows.
  # Let's clean it up below.
  # If it's a multiple imputation object, 
  if("mira" %in% class(mymodel)){
    # Pool and extrac the results
    mytab <- mymodel %>% mice::pool() %>% texreg::extract()
    # cut the n, imputations, & (identical to R2 here)  
    mytab@gof.names <- mytab@gof.names[-c(1:2, 4)]
    mytab@gof.decimal <- mytab@gof.decimal[-c(1:2, 4)]
    mytab@gof <- mytab@gof[-c(1:2, 4)]
    return(mytab) 
  }else{
    # If it's a normal model, then just extract the results
    mytab <- mymodel %>% texreg::extract()
    # and cut the n & adj. R2 (identical to R2 here),
    mytab@gof.names <- mytab@gof.names[-c(2,3)]
    mytab@gof.decimal <- mytab@gof.decimal[-c(2,3)]
    mytab@gof <- mytab@gof[-c(2,3)]
    return(mytab) 
  }
}

# Write an allpurpose VIF function
get_vif = function(model){
  require(tidyverse)
  require(car)
  require(mice)
  # Get a function to calculate appropriate VIF
  calculate = function(model){
    if((length(model$model) - 1) == 1){
      # If there's just one predictor in your model, 
      # then just return a blank, since no VIF can be calculated.
      return(" - ")
    }else{
      # If multiple predictors, then go for it!
      vifscore <- car::vif(model)
      # If model includes categorical variables,
      # car package reports GVIF^(1/2) statistic.
      # This needs to be **squared** in order to compare with standard VIF benchmarks.
      if(is.matrix(vifscore)){
        vifscore[,3]^2 %>% max() %>% round(2) %>% return()
      }else{vifscore %>% max() %>% round(2) %>% return()}
    }
  }
  # Last, if the model is multiple imputation
  if("mira" %in% class(model)){
    # Grab the worst (highest) VIF stat from among all imputations
    model$analyses %>%
      map(~calculate(.)) %>% unlist() %>% max() %>% return()
  }else{
    # Otherwise, just grab the one
    calculate(model) %>% return()
  }
}

# Write a function to get f-statistic, df, sigma, and N
get_gof = function(model){
  require(tidyverse)
  require(broom)
  require(mice)
  # Here's a quick function for extracting main goodness of fit stats
  gof = function(model){
    model %>% broom::glance() %>%
      select(statistic, p_value = p.value, df, sigma, nobs) %>%
      mutate(stars = gtools::stars.pval(p_value)) %>%
      mutate(f = paste(round(statistic, 1), stars, " (", df, ")", sep = ""),
             sigma = round(sigma, 2)) %>%
      select(statistic, f, p_value, sigma, nobs) %>%
      return()
  }
  # If multiple imputed model, grab gof stats for LEAST significant model
  if("mira" %in% class(model)){
    model$analyses %>%
      map_dfr(~gof(.)) %>% filter(statistic == min(statistic)) %>%
      select(-statistic) %>% return()
  }else{
    # Otherwise, just do the one
    gof(model) %>% select(-statistic) %>% return()
  }
}

# Get a linear hypothesis test comparing model improvement
get_lh = function(m){
  require(tidyverse)
  require(car)
  require(broom)
  require(gtools)
  require(mice)
  # Write a wrapper to run a linear hypothesis test
  # comparing any model (m[[i+1]]) to a more limited previous specification (m[[i]])
  lh = function(m = m, i = i){
    # Write a short function to extract just one imputation, if imputed
    just_one = function(mymodel){
      # If it's a multiple imputation model,
      # we're going to find the imputed model with the WORST deviance
      if("mira" %in% class(mymodel)){
        worst <- mymodel$analyses %>%
          map_dfr(~broom::glance(.) %>% select(deviance), .id = "model") %>%
          filter(deviance == max(deviance)) %>%
          with(model) %>%
          as.numeric()
        # And return that one, to be conservative
        mymodel$analyses[[worst]] %>% return()
      }else{
        # Otherwise, just return the original model
        return(mymodel)
      }
    }
    # Extract just the most conservative model, if imputed
    model2 <- just_one(m[[i+1]])
    
    # Extract coefficients names from the new model
    newcoef <- names(model2$coefficients)
    # Extract coefficient names from the old model
    oldcoef <- just_one(m[[i]])$coefficients %>% names()
    
    # Check: Are the predictors exactly same as the previous model? 
    if(identical(newcoef, oldcoef)){
      # As long as i is not the very first model,
      if(i > 1){
        # Notify user
        paste("Model ", i+1, " uses the same coefficients as comparison model ", 
              i, ". ", "Comparing against model ", i-1, ".", sep = "") %>% print()
        # Extract coefficient names from a model 1 step earlier
        oldcoef <- just_one(m[[ (i-1) ]])$coefficients %>% names()
        
        # Are they still identical? If so, stop and move on to the next model.
        if(identical(newcoef, oldcoef)){ 
          paste("ERROR: ",
                "Can't compare Model ", i+1, " against models ", i, " or ", i-1, ". ",
                "Predictors are identical.", sep = "") %>% print()
          # Return Blank Value
          return(" - ")  }
      }else{
        # If i is the very first model, then freeze the process and let them know
        paste("ERROR: ",
              "Can't compare Model ", i+1, " against model ", i, ". ",
              "Predictors are identical. No other models to compare against.", sep = "") %>% print()
        # Return Blank Value
        return(" - ")}
    }
    
    # Identify the new variables added to the model
    testcoef <- newcoef[!newcoef %in% oldcoef]
    # Test whether adding these new variables improved the residual sum of squares
    test <- car::linearHypothesis(model2, testcoef)
    
    # Grab the amount of improvement in the residual sum of squares, p-value, and df
    data.frame(
      stat = test[2, "RSS"] - test[1, "RSS"],
      p_value = test$`Pr(>F)`[2],
      df = test$Df[2]) %>%
      # Get the sign for the change; negative is good; means we improved model fit
      mutate(sign = case_when(
        stat > 0 ~ "+",
        stat == 0 ~ "",
        stat < 0 ~ "-"),
        stars = p_value %>% gtools::stars.pval(),
        label = paste(sign, round(abs(stat), 1), stars, " (", df, ")", sep = "")) %>%
      with(label) %>%
      return()  
    
  }
  
  # calculate the number of models in list
  k <- length(m) - 1
  # create an object to hold the results
  output = rep(" - ", k)
  # Run a for-loop to go through all the models
  for(i in 1:k){
    output[i+1] <- lh(m = m, i = i)
  }
  # return the result! yay!
  return(output)
  
}


get_missing = function(data, mymodel, units){
  require(tidyverse)
  require(mice)
  # If your object is a multiply imputed dataset,
  if("mira" %in% class(mymodel)){
    # Just grab the first one. It doesn't matter which, since it will have complete data.
    mymodel <- mymodel$analyses[[1]]
    # And assign the corresponding rownames from the original dataset to the imputed full dataset
    rownames(mymodel$model) <- rownames(data)
    
  }
  
  # Get number of variables (outcome + predictors)
  k <- mymodel$model %>% length()
  # Get the variables (leaving out the weird ".", which is the k+1th entry)
  myvars <- all.vars(mymodel$call)[1:k]
  
  data %>% 
    select(units, myvars) %>%
    summarize(
      # How many cases in total?
      ncomplete = nrow(.),
      # How many data points in total?
      total = ncomplete * k,
      # How many data points are missing?
      missing = select(., myvars) %>% is.na() %>% sum(),
      # How many valid observations in this model?
      nobs = nobs(mymodel),
      # How many observations did we lose to missing data?
      obs_lost = ncomplete - nobs,
      # How many unique cities left over?
      cities = mymodel$model %>% rownames() %>% str_remove("[-][0-9]{4}") %>% unique() %>% length(),
      # How many unique cities left over?
      years = mymodel$model %>% rownames() %>% str_remove("[0-9]{5}[-]") %>% unique() %>% length()
    ) %>%
    mutate(obs_cities_years = paste(cities, " | ", years, sep = ""),
           missing_rows_values =  paste(round(obs_lost / nobs*100, 1),   " | ",   
                                        round(missing / total*100, 1), sep = "")) %>%
    return()
}

# Write a funciton to run any general linear hypothesis test!
get_glh = function(model, term){
  require(tidyverse)
  require(broom)
  require(mice)
  # Write a subfunction to calculate and extract the linear hypothesis test result
  get_test = function(model, term = ...){
    multcomp::glht(model = model, linfct = term) %>%
      broom::tidy() %>%
      magrittr::set_colnames(value = names(.) %>% str_replace_all("[.]", "_") %>% str_remove("adj_")) %>%
      mutate(contrast = paste(term, collapse = "; "),
             stars = gtools::stars.pval(p_value)) %>%
      select(contrast, estimate, std_error, statistic, p_value, stars) %>%
      return()
  }
  
  if(!"mira" %in% class(model)){
    # If your model is NOT a multiple imputation model, then just return the result.
    get_test(model = model, term = term) %>% return()
  }else{
    # Get number of imputations
    m <- length(model$analyses)
    # Get size of full sample
    n = model$analyses[[1]]$model %>% nrow()
    # Get degrees of freedom (same across all imputations)
    df = glance(model$analyses[[1]])$df %>% unname()
    k = 1 # set number of parameters we're going to create (usually just 1)
    
    model$analyses %>%
      # For each imputed model, run the linear hypothesis test
      map_dfr(~get_test(model = ., term = term)) %>%
      # Now attach sample size,  number of imputations, and variance (se^2)
      mutate(variance = std_error^2) %>%
      with(mice::pool.scalar(Q = .$estimate, U = .$variance, n = n - df, k = k, rule = "rubin1987")) %>%
      with(data.frame(estimate = qbar, std_error = sqrt(t), df = df)) %>%
      mutate(statistic = estimate / std_error,
             p_value = 2*pt(-abs(statistic), df = df, lower.tail = TRUE),
             contrast = paste(term, collapse = "; "),
             stars = gtools::stars.pval(p_value)) %>%
      select(contrast, estimate, std_error, statistic, p_value, stars, df) %>%
      return()
  }
}

save(get_tex, get_gof, get_vif, get_lh, get_missing, get_glh, file = "table/table_functions.RData")

rm(list = ls())



# Social Welfare ######################################


# Lag all continuous predictors
mi <- read_rds("raw_data/mi.rds")

m1 <- mi %>%
  with(lm(formula = regime_soc ~ year))

m2 <- mi %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp))
m3 <- mi %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief))
m4 <- mi %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_dev + regime_mid))
m5 <- mi %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_dev + regime_mid + 
            college + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking))
m6 <- mi %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_dev + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref))

# Add lag for just dependent variable
m7 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>% 
  mutate(ylag = lead(regime_soc, 1)) %>% ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_dev + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Specify covariates to be lagged
mylags = c("pop", "inhabitable_area", "income", "age_elder",
           "rev", "rev_external", "rev_to_exp", 
           "deaths", "damages", "tsunami", "dis_restoration", "dis_relief",
           "college", "unemployed", "migration", "LDP_Komeito_house", "LDP_Komeito_pref", 
           "bonding", "bridging", "linking")
# Add lags for ALL covariates
m8 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_soc, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_soc, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_soc ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_dev + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Add transformations
m9 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_soc, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_soc, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_soc ~ year +
            I(pop^.1) + log(inhabitable_area) + sqrt(rev) + log(income) + 
            log(age_elder / (1 - age_elder)) + log(rev_external / (1 - rev_external)) + rev_to_exp +
            I(deaths^.1) + I(damages^.1) + tsunami +
            I(dis_restoration^.1)  + I(dis_relief^.1) + pref +
            regime_dev + regime_mid + 
            log(college / (1 - college)) + log((unemployed+0.01) / (1 - (unemployed+0.01))) + log(migration) + 
            log((LDP_Komeito_house + 0.01) / (1 - (LDP_Komeito_house + 0.01) )) + 
            log((LDP_Komeito_pref + 0.01) / (1 - (LDP_Komeito_pref + 0.01) )) + 
            log(bonding / (1 - bonding)) + ntile(bridging, 4) + log(linking / (1 - linking)) +
            ylag) )


### Social Welfare
load("table/table_functions.RData")

# Get VIF scores
myvif <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map(~get_vif(.)) %>% unlist() 
# Get Linear-Hypothesis test of added covariates
mylh <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  get_lh()
# Get goodness of fit stats
mygof <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map_dfr(~get_gof(.)) %>%
  # Combine into one data.frame
  mutate(vif = myvif, lh = mylh)
goflist <- list("N (city-years)" = mygof$nobs, "Max VIF" = mygof$vif, "F-statistic (df)" = mygof$f,
                "Change in Deviance (df)" = mygof$lh, "Sigma (Avg. Error)" = mygof$sigma)
# Get texreg objects
mytex <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>% map(~get_tex(.))
outcome = "Social Welfare"
save(mytex, mygof, goflist, outcome, file = "table/table_soc.RData")

# Save the best fit model at the end, because it's enormous.
m9 %>% saveRDS("model/model_soc.RData")

rm(list=ls())


# Developmental ######################################


# Lag all continuous predictors
mi <- read_rds("raw_data/mi.rds")

m1 <- mi %>%
  with(lm(formula = regime_dev ~ year))

m2 <- mi %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp))
m3 <- mi %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief))
m4 <- mi %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_mid))
m5 <- mi %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking))
m6 <- mi %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref))

# Add lag for just dependent variable
m7 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>% 
  mutate(ylag = lead(regime_dev, 1)) %>% ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Specify covariates to be lagged
mylags = c("pop", "inhabitable_area", "income", "age_elder",
           "rev", "rev_external", "rev_to_exp", 
           "deaths", "damages", "tsunami", "dis_restoration", "dis_relief",
           "college","unemployed", "migration", "LDP_Komeito_house", "LDP_Komeito_pref", 
           "bonding", "bridging", "linking")
# Add lags for ALL covariates
m8 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_dev, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_dev, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_dev ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_mid + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Add transformations
m9 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_dev, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_dev, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_dev ~ year +
            I(pop^.1) + log(inhabitable_area) + sqrt(rev) + log(income) + 
            log(age_elder / (1 - age_elder)) + log(rev_external / (1 - rev_external)) + rev_to_exp +
            I(deaths^.1) + I(damages^.1) + tsunami +
            I(dis_restoration^.1)  + I(dis_relief^.1) + pref +
            regime_soc + regime_mid + 
            log(college / (1 - college)) + log((unemployed+0.01) / (1 - (unemployed+0.01))) + log(migration) + 
            log((LDP_Komeito_house + 0.01) / (1 - (LDP_Komeito_house + 0.01) )) + 
            log((LDP_Komeito_pref + 0.01) / (1 - (LDP_Komeito_pref + 0.01) )) + 
            log(bonding / (1 - bonding)) + ntile(bridging, 4) + log(linking / (1 - linking)) +
            ylag) )


load("table/table_functions.RData")

# Get VIF scores
myvif <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map(~get_vif(.)) %>% unlist() 
# Get Linear-Hypothesis test of added covariates
mylh <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  get_lh()
# Get goodness of fit stats
mygof <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map_dfr(~get_gof(.)) %>%
  # Combine into one data.frame
  mutate(vif = myvif, lh = mylh)
goflist <- list("N (city-years)" = mygof$nobs, "Max VIF" = mygof$vif, "F-statistic (df)" = mygof$f,
                "Change in Deviance (df)" = mygof$lh, "Sigma (Avg. Error)" = mygof$sigma)
# Get texreg objects
mytex <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>% map(~get_tex(.))
outcome = "Developmental"
save(mytex, mygof, goflist, outcome, file = "table/table_dev.RData")

# Save the best fit model at the end, because it's enormous.
m9 %>% saveRDS("model/model_dev.RData")

rm(list=ls())



# Middle-Class ######################################

# Lag all continuous predictors
mi <- read_rds("raw_data/mi.rds")

m1 <- mi %>%
  with(lm(formula = regime_mid ~ year))

m2 <- mi %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp))
m3 <- mi %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief))
m4 <- mi %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_dev))
m5 <- mi %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_dev + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking))
m6 <- mi %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_dev + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref))

# Add lag for just dependent variable
m7 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>% 
  mutate(ylag = lead(regime_mid, 1)) %>% ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_dev + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Specify covariates to be lagged
mylags = c("pop", "inhabitable_area", "income", "age_elder",
           "rev", "rev_external", "rev_to_exp", 
           "deaths", "damages", "tsunami", "dis_restoration", "dis_relief",
           "college", "unemployed", "migration", "LDP_Komeito_house", "LDP_Komeito_pref", 
           "bonding", "bridging", "linking")
# Add lags for ALL covariates
m8 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_mid, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_mid, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_mid ~ year +
            pop + inhabitable_area + income + rev + age_elder  + rev_external + rev_to_exp +
            deaths + damages + tsunami + dis_restoration + dis_relief + 
            regime_soc + regime_dev + 
            college + unemployed + migration + LDP_Komeito_house + LDP_Komeito_pref +
            bonding + ntile(bridging, 4) + linking + pref + 
            ylag))

# Add transformations
m9 <- read_rds("raw_data/mi.rds") %>% 
  mice::complete(action = "long", include = TRUE) %>%
  group_by(.imp, muni_code) %>%
  mutate(ylag = lead(regime_mid, 1)) %>%
  mutate_at(vars(contains("regime"), -regime_mid, mylags), list(~lead(., 1))) %>% 
  ungroup() %>% mice::as.mids() %>%
  with(lm(formula = regime_mid ~ year +
            I(pop^.1) + log(inhabitable_area) + sqrt(rev) + log(income) + 
            log(age_elder / (1 - age_elder)) + log(rev_external / (1 - rev_external)) + rev_to_exp +
            I(deaths^.1) + I(damages^.1) + tsunami +
            I(dis_restoration^.1)  + I(dis_relief^.1) + pref +
            regime_soc + regime_dev + 
            log(college / (1 - college)) + log((unemployed+0.01) / (1 - (unemployed+0.01))) + log(migration) + 
            log((LDP_Komeito_house + 0.01) / (1 - (LDP_Komeito_house + 0.01) )) + 
            log((LDP_Komeito_pref + 0.01) / (1 - (LDP_Komeito_pref + 0.01) )) + 
            log(bonding / (1 - bonding)) + ntile(bridging, 4) + log(linking / (1 - linking)) +
            ylag) )


load("table/table_functions.RData")

# Get VIF scores
myvif <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map(~get_vif(.)) %>% unlist() 
# Get Linear-Hypothesis test of added covariates
mylh <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  get_lh()
# Get goodness of fit stats
mygof <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>%
  map_dfr(~get_gof(.)) %>%
  # Combine into one data.frame
  mutate(vif = myvif, lh = mylh)
goflist <- list("N (city-years)" = mygof$nobs, "Max VIF" = mygof$vif, "F-statistic (df)" = mygof$f,
                "Change in Deviance (df)" = mygof$lh, "Sigma (Avg. Error)" = mygof$sigma)
# Get texreg objects
mytex <- list(m2,m3,m4,m5,m6,m7,m8,m9) %>% map(~get_tex(.))
outcome = "Middle Class"
save(mytex, mygof, goflist, outcome, file = "table/table_mid.RData")

# Save the best fit model at the end, because it's enormous.
m9 %>% saveRDS("model/model_mid.RData")

rm(list=ls())




# Captions ######################################


mycoefnames = list(
  "pop" = "Population",
  "I(pop^0.1)" = "Population",
  "inhabitable_area" = "Inhabitable Area (ha)",
  "log(inhabitable_area)" = "Inhabitable Area (ha)",
  "age_elder" = "% Over Age 65",
  "log(age_elder/(1 - age_elder))" = "% Over Age 65",
  "income" = "Income per capita (1000s of yen)",
  "log(income)" = "Income per capita (1000s of yen)",
  "rev" = "Revenue per capita (1000s of yen)",
  "sqrt(rev)" = "Revenue per capita (1000s of yen)",
  "rev_external" = "% National & Prefectural Funding",
  "log(rev_external/(1 - rev_external))" = "% National & Prefectural Funding",
  "rev_to_exp" = "Real Term Budget Balance (+/-)",
  "deaths" = "Disaster Deaths (per 100,000)", 
  "I(deaths^0.1)" = "Disaster Deaths (per 100,000)", 
  "damages" = "Disaster Damage (per 100,000)", 
  "I(damages^0.1)" = "Disaster Damage (per 100,000)",
  "tsunami" = "Hit by 2011 tsunami (1/0)",
  "dis_restoration" = "Disaster Recovery Spending Rate",
  "I(dis_restoration^0.1)" = "Disaster Recovery Spending Rate",
  "dis_relief" = "Disaster Relief Spending Rate",
  "I(dis_relief^0.1)" = "Disaster Relief Spending Rate",
  "regime_soc" = "Social Welfare Index",
  "regime_mid" = "Middle Class Regime Index",
  "regime_dev" = "Developmental Regime Index",
  "LDP_Komeito_house" = "% LDP Coalition Votes: Lower House",
  "log(LDP_Komeito_house + 0.01/(1 - LDP_Komeito_house + 0.01))" = "% LDP Coalition Votes: Lower House",
  "LDP_Komeito_pref" = "% LDP Coalition Votes: Prefecture",
  "log(LDP_Komeito_pref + 0.01/(1 - LDP_Komeito_pref + 0.01))" = "% LDP Coalition Votes: Prefecture",
  "bonding" = "Bonding Social Capital (0-1)",
  "log(bonding/(1 - bonding))" = "Bonding Social Capital (0-1)",
  "bridging" = "Bridging Social Capital (Quartiles)",
  "ntile(bridging, 4)" = "Bridging Social Capital (Quartiles)",
  "linking" = "Linking Social Capital (0-1)",
  "log(linking/(1 - linking))" = "Linking Social Capital (0-1)",
  "college" = "% College Educated",
  "log(college/(1 - college))" = "% College Educated",
  "unemployed" = "% Unemployed",
  "log((unemployed + 0.01)/(1 - (unemployed + 0.01)))" = "% Unemployed",
  "migration" = "Total Migration (per capita)",
  "log(migration)" = "Total Migration (per capita)",
  "ylag" = "Lagged Outcome (1 year prior)",
  "(Intercept)" = "Constant")

mycolumns = c("Basic Controls", "Disaster Controls", 
              "Other Regimes", "Collective Action", 
              "Prefecture Effects<sup>1</sup>", "Lagged Outcome<sup>2</sup>", 
              "Lagged Controls<sup>3</sup>", "Transformed<sup>4</sup>")


get_ktable = function(tableno, multiplier = 1){
  
  # Write a function to take the contents of parentheses and put it at the end.
  switch_order = function(data){
    for(i in 2:9){
      se = str_extract(data[,i] %>% unlist(), pattern = "[(].*[)] ")
      beta = str_remove(data[,i] %>% unlist(), pattern = " [(].*[)] ")
      label = if_else(!is.na(se), paste(beta, se, sep = " "), beta)
      # Overwrite original  
      data[,i] <- label
    }
    return(data)
  }
  
  tab <- texreg::matrixreg(
    #file = "/cloud/project/viz/table_soc.html", 
    l = mytex, caption.above = TRUE, include.adjr = FALSE,  #custom.note = mynote, 
    single.row = TRUE, bold = 0.10, stars = c(0.001, 0.01, 0.05, 0.10),
    custom.model.names = mycolumns, custom.gof.rows = goflist,
    custom.coef.map = mycoefnames) %>% #groups = mygroups
    as_tibble() %>%
    magrittr::set_colnames(value = slice(., 1)) %>%
    .[-1,] %>%
    switch_order()
  
  get_bold = function(data, column){
    unlist(tab[1:30,column]) %>% str_detect("[*]+") %>% c(., rep(FALSE, 6)) %>%
      return()
  }
  
  tab[30,][1] <- "R2"
  
  # Get headers matching your multiplier
  headers = rep(1,9) %>% 
    set_names(
      nm = paste0("Model ", (1:8)+(multiplier-1)*8) %>%
        c(" ", .))
  
  id = str_extract(tableno, "tab:.*") %>% str_remove("[}]")
  
  tab %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "\\addlinespace",
        # You can write LaTEX in the caption, as long as you use \\, not \
        caption = paste0(tableno, ": ", "\\label{", id, "}{", "\\textbf{OLS Models of ", outcome, " Regimes.}", "} ",
                         "\\newline \\normalsize 
        \\textit{Dependent Variable}: ", outcome, " Regime Index (Z-score). 
        \\newline \\normalsize 
        \\textit{Unit of Observation}: ", mygof$nobs %>% max(), " Japanese municipality-years (2000-2018), with annual fixed effects.", sep = "")) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE, font_size = 10) %>%
    column_spec(column = 1, width = "6cm") %>%
    column_spec(2, bold = get_bold(tab, 2), width = "1.6cm") %>%
    column_spec(3, bold = get_bold(tab, 3), width = "1.6cm") %>%
    column_spec(4, bold = get_bold(tab, 4), width = "1.6cm") %>%
    column_spec(5, bold = get_bold(tab, 5), width = "1.6cm") %>%
    column_spec(6, bold = get_bold(tab, 6), width = "1.6cm") %>%
    column_spec(7, bold = get_bold(tab, 7), width = "1.6cm") %>%
    column_spec(8, bold = get_bold(tab, 8), width = "1.6cm") %>%
    column_spec(9, bold = get_bold(tab, 9), width = "1.6cm") %>%
    pack_rows("\nDemographics", 1, 4) %>%
    pack_rows("\nRevenue", 5, 7, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nDisaster Conditions", 8, 10, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nDisaster Spending", 11, 12, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nOther Urban Regimes", 13, 14, hline_before = TRUE, latex_gap_space = "0.25cm") %>% 
    pack_rows("\nPolitical Parties", 15, 16, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nCollective Action", 17, 19, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nExtra Controls", 20, 24, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nModel Fit", 25, 30, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    add_header_above(
      header = headers
      
    ) %>%
    add_header_above(header = c(" " = 8, "Best Model")) %>%
    footnote(
      general = "Statistical Significance: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10. All p-values and asterisks reflect two-tailed hypothesis tests. (F-statistic is one-tailed by default.)",
      number = c(
        
        paste0("Annual Fixed Effects included in every model. Prefectural effects added starting in Model ",
               5+(multiplier-1)*8, 
               ". Excluded from table to conserve space."),
        
        paste0("Lagged Outcome by 1 year in Models ",
               6+(multiplier-1)*8, "-", 8+(multiplier-1)*8,
               ", to control for path dependence and any temporal correlation. Constrains final models to 2001-2018."),
        
        paste0("Lagged Controls: All other numeric predictors lagged by 1 year in Models ",
               7+(multiplier-1)*8, "-", 8+(multiplier-1)*8,
               " to avoid endogeneity bias. Despite the 1% drop in R<sup>2</sup>, lagging controls ensures more conservative estimates."),
        
        paste0(
          "Transformations: In Model ",
          8+(multiplier-1)*8, ", predictors were log-, logit-, or root-transformed to fit their distribution and nonlinear trends, adding a small constant where necessary. These made statistically significant improvements in log-likelihood compared to Model ", 7+(multiplier-1)*8, " (p < 0.001). Area, income, and migration were logged. Revenue used the square root; the 10th root was used for Population (to avoid colinearity with spending), disaster deaths, damages, recovery spending, and relief spending (since the distributions have frequent, meaningful zeros). Age, voteshares, social capital, education, and unemployment were logit tranformed, since they are bounded at 0 and 1. Bridging social capital was split into quartiles, to avoid collinearity with regime indicators.")
      ),
      threeparttable = TRUE) %>%
    str_replace_all(c("<sup>" = "$^{",   "</sup>" = "}$",
                      "R2" = "R$^{2}$", "<b>" = "\textbf{",  "</b>" = "}",
                      "<sqrt>" = "$sqrt{", "</sqrt>" = "}$")) %>%
    knitr::asis_output() %>%
    return()
}

save(mycoefnames, mycolumns, get_ktable, file = "table/captions.RData")
