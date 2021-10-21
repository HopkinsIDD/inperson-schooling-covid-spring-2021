##'Function that aggregates key variables to the national level 
##'by week (default) or other aggregation variable using survey weights
##'
##' @param data
##' @param variables variables to aggregate
##' @param aggregate_by column to aggregate by
##' @param return_unweighted whether or not to return N and unweighted percent
##'
##' @return aggregated df long by variables with mean, upper, and lower
aggregate_national <- function(data, 
                               variables = c('Child_IP_any', 'Child_IP_full_bin',
                                             'n_interventions', 'any_vaccine'),
                               aggregate_by = 'wk',
                               return_unweighted = FALSE) {
  
  # convert Yes/No factors to logical
  for (i in variables) {
    # catch factor and character variables
    if (class(data[[i]])=='factor' | class(data[[i]])=='character') {
      # set Yes to TRUE
      if ('Yes' %in% data[[i]]) {
        data <- data %>% 
          mutate_at(i, function(x) ifelse(x=='Yes', TRUE, FALSE))
      } else {
        # otherwise stop and warn
       stop('Function converts Yes/No variables to logical. Please make "', 
            i, '" Yes/No, logical, numeric, or edit the function.')
      }
    }
  }
  
  # set survey design
  options(survey.lonely.psu="remove")
  svy_d <- data%>%
    srvyr::as_survey_design(weights=weight, strata=State)
  
  # weighted aggregation
  ag <- svy_d %>%
    # group by week, or other indicated variable
    group_by_at(aggregate_by) %>%
    # take the weighted mean/proportion of each key variable
    srvyr::summarise_at(variables, survey_mean, na.rm=TRUE, vartype='ci') %>%
    # melt all non-ID variables
    reshape2::melt(id.vars=aggregate_by)
  
  # create separate columns for mean, upper, and lower
  ag <- full_join(
    # mean
    ag%>%filter(!grepl('_upp|_low', variable))%>%rename(mean=value),
    # lower
    ag%>%filter(grepl('_low', variable))%>%rename(lower=value)%>%
      mutate(variable = gsub('_low', '', variable))
  ) %>%
    # upper
    full_join(
      ag%>%filter(grepl('_upp', variable))%>%rename(upper=value)%>%
      mutate(variable = gsub('_upp', '', variable))
  )
  
  # unweighted aggregation
  if(return_unweighted) {
    
    # number
    ag <- data %>%
      # group by week, or other indicated variable
      group_by_at(aggregate_by) %>%
      # get sample size and unweighted mean
      dplyr::summarise_at(all_of(variables), length) %>%
      # melt all non-ID variables
      reshape2::melt(id.vars=aggregate_by, value.name = 'N') %>%
      # add to data
      left_join(ag)
    
    # percent
    ag <- data %>%
      # group by week, or other indicated variable
      group_by_at(aggregate_by) %>%
      # get sample size and unweighted mean
      dplyr::summarise_at(all_of(variables), mean, na.rm=TRUE) %>%
      # melt all non-ID variables
      reshape2::melt(id.vars=aggregate_by, value.name = 'pct_unweighted') %>%
      # add to data
      left_join(ag)
  }
  
  # return data frame long by variables
  return(ag)
}


##'Function that does a survey weighted regression on data.
##' doing this so we calculate the subsets correctly
##'
##' @param data
##' @param formula
##' @param ... other arguments to surveyglm
##'
##' @return results of survey glm
survey_reg <- function(data, formula, wts=data$weight, ...) {
  options(survey.lonely.psu="remove")
  
  svy_d <- data%>%
    mutate(weight=wts)%>%
    srvyr::as_survey_design(weights=weight, strata=State)
  
  rc <- svyglm(formula, design=svy_d, family=quasibinomial, ...)
  
  
  return(rc)
}


##' Function that does a fully adjusted analysis and unadjusted analysis foe regression on
##' postive cases and returns a data frame of the combined results.
##' 
##' @param df the data frame to use for the regression
##' @param outcome the outcome of the regression to use
##' @param as_df should the outcome be a data frame.
##' @param cnty_cov include county level covariates other than AR
##' @param n_mit include number of mitigatoin measures
##' @param state include state as a coviariate
##' 
##' @return a series for regrsion summary tables or no
pos_regs <- function(data, outcome="tst", as_df=FALSE,
                     cnty_cov = TRUE, n_mit=FALSE,
                     state=FALSE, variant=FALSE,
                     include_unadj = TRUE, cum_inc = FALSE) {
  
  rc <- list()
  
  
  if(outcome=="tst") {
    data <- data%>%filter(COVID_tested=="Yes")%>%
      mutate(pos=tst_pos)
    outcome_nm <-"Test+"
  } else if(outcome=="tst2") {
    data <- data%>%mutate(pos=tst_pos)
    outcome_nm <- "Overall Test+"
  } else if (outcome=="cli") {
    data<- data%>%mutate(pos=cli_pos)
    outcome_nm <- "COVID like illness"
  } else if (outcome=="cli2") {
    data<- data%>%mutate(pos=cli2_pos)
    outcome_nm <- "Loss of taste/smell"
  } else if (outcome=="hh_cli") {
    data <- data%>%mutate(pos=cliHH_pos)
    outcome_nm <- "COVID like illness in HH"
  } else if (outcome=="hh_pos") {
    data <- data%>%mutate(pos=cntct_HH_test_pos)
    outcome_nm <- "HH member Test+"
  } else if (outcome=="tst_ind"){
    data <- data%>%mutate(pos=tst_pos_ind)
    outcome_nm <- "Indicated Test+"
  } else if(outcome=="tst_rout"){
    data <- data%>%mutate(pos=tst_pos_routine)
    outcome_nm <- "Non-indicated Test+"
  } else if(outcome=="bp") {
    data <- data%>%mutate(pos=prior_bloodprss)
    outcome_nm <- "High Blood Pressure"
  } else {
    stop("Unknown outcome")
  }
  
  ##First look at the raw weighted relationship
  if (include_unadj) {
    reg_ip_unadj <- data%>%
      survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin)
    
    rc$rtbl_ip_unadj <- 
      gtsummary::tbl_regression(reg_ip_unadj, exponentiate = TRUE)
  }

  ##Full adjustment
  if(cum_inc){
    if(variant) {
      if (state) {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )
          }
        }
        
      } else {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           n_interventions+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin+
                           cum_inc
              )
          }
        }
      }
    } else {
      if (state) {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           n_interventions+
                           cum_inc
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           cum_inc
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           cum_inc
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           cum_inc
              )
          }
        }
        
      } else {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           n_interventions+
                           any_vaccine+
                           cum_inc
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           cum_inc
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           cum_inc
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           cum_inc
              )
          }
        }
      }
    }
  } else {
    if(variant) {
      if (state) {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )
          }
        }
        
      } else {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           n_interventions+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine+
                           variant_fit+
                           variant_fit*Child_IP_full_bin+
                           variant_fit*Child_IP_part_bin
              )
          }
        }
      }
    } else {
      if (state) {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine+
                           n_interventions
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           State+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine
              )
          }
        }
        
      } else {
        if (cnty_cov) {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           n_interventions+
                           any_vaccine
              )   
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           Population+
                           White+
                           GINI+
                           Poverty+
                           Description+
                           any_vaccine
              )   
          }
        } else {
          if (n_mit) {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ +
                           n_interventions+
                           any_vaccine
              )
          } else {
            reg_ip_adjfull<- data%>%
              survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                           rel_avg_biwk_AR+
                           IsMale+Age+OccupationRed+
                           Educational_Level+
                           HH_sz+num_kid_recode+
                           mask_level+trav_out_state+
                           bar_rest_cafe_activ +
                           large_event_activ+
                           pub_transit_activ+
                           any_vaccine
              )
          }
        }
      }
    }
  }
  
  
  
  rc$rtbl_ip_adjfull <- 
    gtsummary::tbl_regression(reg_ip_adjfull, exponentiate = TRUE)
  
  tmp <- NULL
  
  if (as_df) {
    
    if (include_unadj) {
      tmp <- rc$rtbl_ip_unadj$table_body%>%
        mutate(adjusted=FALSE,
               outcome=outcome_nm)
    }
    
    tmp <-  rc$rtbl_ip_adjfull$table_body%>%
                mutate(adjusted=TRUE,
                outcome=outcome_nm)%>%
      bind_rows(tmp)
    
    rc<-tmp
  }
  
  return(rc)
}


##' Function to do a standard recode of all of the data.
##' 
##' @param df the data frame to recode
##' 
##' @return a data frame with all of the standard recodes done.
##'
standard_recode <- function(df, min_HH=2) {
  ##Recode COVID Tested
  df <- df%>%mutate(COVID_tested = recode(COVID_tested,  `(Missing)`="No"),
                    tst_pos = COVID_positive=="Yes",
                    COVID_tst_status=if_else(COVID_tested=="No", "Not Tested",
                                             ifelse(tst_pos, "Tested +", "Tested -")))
  
  
  ##CLI 1 recode
  df<- df%>%mutate(cli_pos=ifelse(CLIv1=="(Missing)", NA, CLIv1=="Yes"))
  
  
  ##CLI 2 recode
  df<- df%>%mutate(cli2_pos=ifelse(CLIv2=="(Missing)", NA, CLIv2=="Yes"))
  
  ## HH CLI recode
  ## NOTE: CLIv1_HH is just whether reported symptoms in HH
  ##       cliHH_pos is whether HH or individual CLI symptoms responded
  df <- df %>% mutate(CLIv1_HH = as.factor(ifelse(df$feverHH=="Yes" & (df$coughHH =="Yes" | df$shrtbreathHH=="Yes" | df$diffbreathHH =="Yes"), 
                                                  "Yes", 
                                                  "No")))
  df <- df%>%mutate(cliHH_pos=ifelse(is.na(CLIv1_HH), NA, CLIv1_HH=="Yes" | cli_pos))
  
  ## HH test pos recode
  df <- df %>% mutate(cntct_HH_test_pos = cntct_covidpos=="Yes" & cntct_covidpos_fam=="Yes")
  
  ## Test indicators
  ## indicated = sick/in contact with sick person
  ## 'routine' = for work or medical procedure + not indicated
  ## then make covid outcomes: 
  ## test_pos_ind - positive if indicated, neg/not tested, NA otherwise
  ## test_pos_routine - positive/negative if routine, NA otherwise
  df <- df %>%
    mutate(tested_indic = ifelse(COVID_tested=="Yes",
                                 testd_felt_sick=="Yes" | testd_cntct_someone_sick=="Yes",
                                 NA),
           tested_routine = ifelse(COVID_tested=="Yes",
                                   !tested_indic & (testd_work_req=="Yes" | testd_other_medcare=="Yes"),
                                   NA),
           tst_pos_ind = case_when(tested_indic & tst_pos ~ TRUE,
                                   tested_indic ~ FALSE,
                                   tst_pos ~ NA,
                                   TRUE ~ FALSE),
           tst_pos_routine = case_when(tested_routine & tst_pos ~ TRUE,
                                       tested_routine ~ FALSE,
                                       TRUE ~ NA)
    )
  
  ##Add is make variable
  df <- df%>%mutate(IsMale=Gender=="Male")
  
  ##Recode occupatoin
  df <- df%>%
    mutate(Occupation=ifelse(!is.na(Occupation),as.character(Occupation),
                             ifelse(Employed=="No", "Not Employed", NA)))
  
  df <- df%>%
        mutate(Employed = ifelse(Employed=="(Missing)", NA, Employed))
  
  ##Group all occupations with <10000 respondents into other
  df <- df%>%
    group_by(Occupation)%>%
    mutate(OccupationRed = ifelse(n()<10000, "Other",Occupation))%>%
    ungroup()
  
  ##HH Sizer recode
  # first recalculate to consider negatives, NAs, drop non-whole numbers, etc. then categorize
  df<-df%>%mutate(num_kids = case_when(num_kids %/% 1 != num_kids ~ NA_real_,
                                       num_kids<0 ~ NA_real_,
                                       num_kids>=100 ~ NA_real_,
                                       TRUE ~ num_kids),
                  num_adults = case_when(num_adults %/% 1 != num_adults ~ NA_real_,
                                         num_adults<0 ~ NA_real_,
                                         num_adults>=100 ~ NA_real_,
                                         TRUE ~ num_adults),
                  num_65plus = case_when(num_65plus %/% 1 != num_65plus ~ NA_real_,
                                         num_65plus<0 ~ NA_real_,
                                         num_65plus>=100 ~ NA_real_,
                                         TRUE ~ num_65plus)) %>%
            mutate(total_ppl_recoded = ifelse(is.na(num_kids) & is.na(num_adults) & is.na(num_65plus),
                                             NA,
                                             rowSums(select(.,num_kids, num_adults, num_65plus), na.rm=TRUE)),
                  HH_sz = ifelse(total_ppl_recoded<10, total_ppl_recoded, "10+"),
                  HH_sz=factor(HH_sz, c(as.character(min_HH:9),"10+")))
  
  ##Number of kids/adults/65+ recode
  df <- df%>%mutate(num_kid_recode=ifelse(num_kids>=7,"7+",num_kids),
                    num_adult_recode=ifelse(num_adults>=10,"10+",num_adults),
                    num_65plus_recode=ifelse(num_65plus>=10,"10+",num_65plus))
  
  ##Sick contacts recode
  df <- df%>%mutate(known_sick_cat=ifelse(known_sick>=10,"10+", floor(known_sick)))
  
  ## Recode individual-level risk behavior variables to set NA to "No"
  df <- df %>% mutate(trav_out_state=ifelse(is.na(trav_out_state), 'No', 'Yes'),
                      bar_rest_cafe_activ=ifelse(is.na(bar_rest_cafe_activ), 'No', 'Yes'),
                      large_event_activ=ifelse(is.na(large_event_activ), 'No', 'Yes'),
                      pub_transit_activ=ifelse(is.na(pub_transit_activ), 'No', 'Yes'),
                      mask_always=ifelse(is.na(mask_always), 'No', 'Yes'),
                      mask_most=ifelse(is.na(mask_most), 'No', 'Yes'),
                      mask_some=ifelse(is.na(mask_some), 'No', 'Yes'),
                      mask_little=ifelse(is.na(mask_little), 'No', 'Yes'),
                      mask_never=ifelse(is.na(mask_never), 'No', 'Yes'),
                      never_in_public=ifelse(is.na(never_in_public), 'No', 'Yes'))
  
  ## Mask level recode
  df<-df%>%mutate(mask_level=ifelse(mask_always=="Yes","Always",
                                    ifelse(mask_most=="Yes", "Mostly",
                                           ifelse(mask_some=="Yes","Sometimes",
                                                  ifelse(mask_little=="Yes","Rarely",
                                                         ifelse(mask_never=="Yes","Never",
                                                                ifelse(never_in_public=="Yes", "Never in Public",
                                                                NA)))))))
  
  ## Recode of the social gathering and shopping data
  df <- df%>%mutate(`Social Gathering`=cut(soc_cnct_gath, c(0,1,5,Inf), 
                                           right = F, labels=c("0","1-4","5+")),
                    `Shopping`=cut(soc_cnct_shop, c(0,1,5,Inf), 
                                   right = F, labels=c("0","1-4","5+")))
  
  ## Recode avoiding social contact variable
  df <- df %>% mutate(avoidsoc = case_when(avoidsoc_always=="Yes" ~ "Always",
                                           avoidsoc_most=="Yes" ~ "Mostly",
                                           avoidsoc_some=="Yes" ~ "Sometimes",
                                           avoidsoc_none=="Yes" ~ "Never",
                                           TRUE ~ NA_character_))
  
  ## Recode outside activity variables (categorical masked/unmasked/none)
  df <- df %>%
    mutate(work_outside_activ_msk = factor(case_when(work_outside_activ=="Yes" & work_outside_msk=="Yes" ~ "Masked",
                                                     work_outside_activ=="Yes" & work_outside_msk=="No" ~ "Unmasked",
                                                     work_outside_activ=="No" ~ "No activity"),
                                           levels=c("No activity", "Masked", "Unmasked")),
           mkt_grocery_pharm_activ_msk = factor(case_when(mkt_grocery_pharm_activ=="Yes" & mkt_grocery_pharm_msk=="Yes" ~ "Masked",
                                                          mkt_grocery_pharm_activ=="Yes" & mkt_grocery_pharm_msk=="No" ~ "Unmasked",
                                                          mkt_grocery_pharm_activ=="No" ~ "No activity"),
                                                levels=c("No activity", "Masked", "Unmasked")),
           bar_rest_cafe_activ_msk = factor(case_when(bar_rest_cafe_activ=="Yes" & bar_rest_cafe_msk=="Yes" ~ "Masked",
                                                      bar_rest_cafe_activ=="Yes" & bar_rest_cafe_msk=="No" ~ "Unmasked",
                                                      bar_rest_cafe_activ=="No" ~ "No activity"),
                                            levels=c("No activity", "Masked", "Unmasked")),
           ext_person_activ_msk = factor(case_when(ext_person_activ=="Yes" & ext_person_msk=="Yes" ~ "Masked",
                                                   ext_person_activ=="Yes" & ext_person_msk=="No" ~ "Unmasked",
                                                   ext_person_activ=="No" ~ "No activity"),
                                         levels=c("No activity", "Masked", "Unmasked")),
           large_event_activ_msk = factor(case_when(large_event_activ=="Yes" & large_event_msk=="Yes" ~ "Masked",
                                                    large_event_activ=="Yes" & large_event_msk=="No" ~ "Unmasked",
                                                    large_event_activ=="No" ~ "No activity"),
                                          levels=c("No activity", "Masked", "Unmasked")),
           pub_transit_activ_msk = factor(case_when(pub_transit_activ=="Yes" & pub_transit_msk=="Yes" ~ "Masked",
                                                    pub_transit_activ=="Yes" & pub_transit_msk=="No" ~ "Unmasked",
                                                    pub_transit_activ=="No" ~ "No activity"),
                                          levels=c("No activity", "Masked", "Unmasked")))
  
  ##Recode missing and NAs to Nos
  df <- df%>%mutate(Pre_K=replace_na(Pre_K, "No"),
                    Grades_15= replace_na(Grades_15,"No"),
                    Grades_68=replace_na(Grades_68, "No"),
                    Grades_912=replace_na(Grades_912,"No"))
  
  
  
  ##Recode school mitigation variales as binary
  df<- df%>%mutate_at(vars(matches("sch_")), 
                      function(x){ifelse(x=="Yes",TRUE,ifelse(x=="(Missing)",NA,FALSE))})
  
  
  
  ##recode Missing and don'g know for Child IP levels to NAs
  df <- df%>%
    mutate(Child_IP_full_bin=ifelse(Child_IP_full=="(Missing)"|Child_IP_full=="Don't know", NA, Child_IP_full=="Yes"),
           Child_IP_part_bin=ifelse(Child_IP_part=="(Missing)"|Child_IP_part=="Don't know", NA, Child_IP_part=="Yes"),
           Age=na_if(Age,"(Missing)"),
           Age=relevel(Age, ref="35.44"),
           Educational_Level=na_if(Educational_Level,"(Missing)"),
           Educational_Level=relevel(Educational_Level, ref="HS"),
           OccupationRed=relevel(as.factor(OccupationRed), 
                                 ref="Office and admin support"))
  
  
  ##give make a variable that is specifically for folks with paid work
  ##outside th ehome vs. everyone else.
  df <- df%>%mutate(work_outside_paid = ifelse(Employed=="No", FALSE,
                                               work_payed=="Yes"))
  
  ## set number of interventions
  dat<-as.data.frame(df)
  index<-grep("sch_", colnames(dat))
  temp<-dat[,index[-length(index)]]
  df$n_interventions<-rowSums(temp, na.rm=T)
  df$tot_pol<-rowSums(!is.na(temp), na.rm=T)
  df$no_pol<-df$tot_pol-df$n_interventions
  df$n_interventions[df$tot_pol==0]<-NA # Set those who reported no data on interventions to NA
  df$n_interventions[df$n_interventions==14]<-NA # Set those who reported all 14 interventions to NA
  df$n_interventions[df$Child_IP_any!="Yes"]<-NA # Set those who reported no in person schooling to NA
  
  ## Create categorical variable of number of interventions 
  df$cat_int<-NA
  df$cat_int[df$n_interventions==0 & df$Child_IP_any=="Yes"]<-0
  df$cat_int[df$n_interventions>=1 & df$n_interventions<=3 &  df$Child_IP_any=="Yes"]<-1
  df$cat_int[df$n_interventions>=4 & df$n_interventions<=6 &  df$Child_IP_any=="Yes"]<-2
  df$cat_int[df$n_interventions>=7 & df$n_interventions<=9 &  df$Child_IP_any=="Yes"]<-3
  df$cat_int[df$n_interventions>=10 &  df$Child_IP_any=="Yes"]<-4
  
  ## set state by FIPS code
  df <- df %>%
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
    mutate(State = cdlTools::fips(substr(fips,1,2), to = "Abbreviation"))
  
  ## create a single vaccination/vaccine doses variable
  df <- df %>% 
    mutate(any_vaccine=ifelse(had_vaccine=='No', FALSE,
                              ifelse(had_vaccine=='Yes', TRUE,  NA)))
  
  ## add epi week, month, and period for second semester analysis
  df <- df%>%
    mutate(Date=as.Date(Date))%>%
    filter(Date>="2021-01-12", Date<="2021-06-12")%>%
    mutate(wk=lubridate::epiweek(Date))%>%
    mutate(month=lubridate::month(Date,label=TRUE))%>%
    mutate(period=ifelse(Date<"2021-03-01","jan_feb",
                         ifelse(Date<"2021-05-01", "mar_apr", "may_jun")))

  return(df)
}


##' Function to do merge with the CSSE data 
##' and for each week include the average biweekly attack rate 
##' in the past 4 weeks (current week + 3 preceding)
##' 
##' @param df the data frame
##' @param csse the csse data
##' 
##' @return a data frame merged with this data.
##' 
merge_csse <- function(df, csse) {
  
  ##Get the relevant JHU CSSE data.
  ##Subset to the epi weeks comprised in the updated dataset, plus the preceding 3 weeks
  obs_cases <- csse%>%
    filter(Update>="2020-12-13", Update<="2021-06-12")%>%
    mutate(year=lubridate::epiyear(Update),
           wk=lubridate::epiweek(Update))%>%
    group_by(year, wk, FIPS)%>%
    summarize(cases=sum(incidI))%>%
    ungroup()%>%
    rename(fips=FIPS)
  
  
  ##Note that the following calls require your census API key be installed
  pops <- tidycensus::get_estimates(geography="county", product="population")%>%
    filter(variable=="POP")%>%
    rename(population=value, fips=GEOID)%>%
    select(fips,population)
  
  
  cmb_cases<-obs_cases%>%
    inner_join(pops)%>%
    group_by(fips, population)%>%
    summarize(wk=wk,
              avg_biwk_case=RcppRoll::roll_sum(cases, 2, fill=NA, align="right"),
              biwk_AR=avg_biwk_case/mean(population))%>%
    ungroup() %>%
    group_by(fips, population)%>%
    summarize(wk=wk,
              avg_biwk_case=avg_biwk_case,
              avg_biwk_AR=(RcppRoll::roll_sum(biwk_AR, 4, fill=NA, align="right"))/4)%>%
    ungroup() %>%
    filter(!is.na(avg_biwk_AR))
  
  
  df <- df%>%mutate(fips=str_pad(fips,5,"left",0))%>%
    left_join(cmb_cases)%>%
    mutate(rel_avg_biwk_AR = log2(avg_biwk_AR*1000+1),
           month = ceiling(wk/4)) # add "epi month"
  
  # Merge in cumulative case counts and calc incidence
  csse_cum <- csse %>%
    rename(Date = Update, cum_cases = Confirmed, fips = FIPS) %>%
    dplyr::select(Date, fips, cum_cases)
  
  df <- df %>%
    left_join(csse_cum) %>%
    mutate(cum_inc = cum_cases/Population)
  
  rm(csse_cum)
  
  return(df)
  
}


##' Function that wrangles regression output 
##' for adjusted analyses by period
##' then plots during GT
##' 
##' @param reg_tbl regression table object, filtered to variable of interest
##' @param var_name name to diplay for variable of interest in the table
##' 

reg_tbl_period_wrangle <- function(reg_tbl, var_name, expo = TRUE) {
  
  # setting up object for tables
  reg_sum <- reg_tbl %>%
    mutate(adjusted=adjusted=="TRUE",
           outcome=recode(outcome,
                          `Covid like illness`="COVID like illness",
                          `Loss taste/smell`="Loss of taste/smell",
                          `Test+`="Overall Test+"))%>%
    select(period, outcome, adjusted, 
           label, estimate, conf.low, conf.high)%>%
    mutate(estimate=round(exp(estimate), 2),
           conf.low=round(exp(conf.low), 2),
           conf.high=round(exp(conf.high), 2))%>%
    drop_na()%>%
    pivot_wider(names_from=c(adjusted, outcome),
                values_from=c(estimate,conf.low,conf.high)) %>%
    mutate(period = factor(period, 
                           levels = c('Jan-Jun', 'Jan-Feb', 'Mar-Apr', 'May-Jun')))
  
  if(!expo) {
    reg_sum[,3:ncol(reg_sum)] <- round(log(reg_sum[,3:ncol(reg_sum)]), 2)
  }
  
  # printing regression table
  reg_disp_tbl <- reg_sum%>%
    select(-period)%>%
    gt::gt()%>%
    gt::tab_spanner(
      label=c("Test+"),
      columns = ends_with("Test+")
    )%>% 
    gt::tab_spanner(
      label=c("COVID like illness"),
      columns = ends_with("COVID like illness")
    )%>%
    gt::tab_spanner(
      label=c("Loss of taste/smell"),
      columns = ends_with("taste/smell")
    )%>%
    gt::cols_merge_range(
      col_begin=vars(`conf.low_TRUE_Loss of taste/smell`),
      col_end=vars(`conf.high_TRUE_Loss of taste/smell`)
    )%>%
    gt::cols_merge_range(
      col_begin=vars(`conf.low_TRUE_Overall Test+`),
      col_end=vars(`conf.high_TRUE_Overall Test+`)
    )%>%
    gt::cols_merge_range(
      col_begin=vars(`conf.low_TRUE_COVID like illness`),
      col_end=vars(`conf.high_TRUE_COVID like illness`)
    )%>%
    gt::cols_label(
      `conf.low_TRUE_Loss of taste/smell`="95% CI",
      `conf.low_TRUE_Overall Test+`="95% CI",
      `conf.low_TRUE_COVID like illness`="95% CI",
      `estimate_TRUE_Loss of taste/smell`="adj. OR",
      `estimate_TRUE_Overall Test+`="adj OR",
      `estimate_TRUE_COVID like illness`="adj OR",
      label=var_name
    )%>%
    gt::tab_options(
      row_group.font.weight   = "lighter",
      column_labels.font.weight = "bold"
    )%>%
    gt::tab_row_group(label = 'January to June',
                      rows = which(reg_sum$period=='Jan-Jun'))%>%
    gt::tab_row_group(label = 'January to February',
                      rows = which(reg_sum$period=='Jan-Feb'))%>%
    gt::tab_row_group(label = 'March to April',
                      rows = which(reg_sum$period=='Mar-Apr'))%>%
    gt::tab_row_group(label = 'May to June',
                      rows = which(reg_sum$period=='May-Jun'))%>%
    gt::row_group_order(c('January to June',
                          'January to February',
                          'March to April',
                          'May to June'))%>%
    gt::cols_align("left")
  
  # end function
  return(reg_disp_tbl)
  
}


##' Function that runs the COVID-19 risk by grade level analyses
##' 
##' @param time_period Months to run analysis for
##' @param df Data frame with survey data
##' @param outcomes COVID-19-related outcomes to run analysis for
##' 
run_grade_analysis <- function(time_period, df,
                               outcomes = c("tst2","cli","cli2")) {
  
  ## Set up the single grade data frame
  ## This filters for respondents who report a child in
  ## only one of the available grade classes (PreK-K, 1-5, 6-8, 9-12)
  single_grade_df<- df%>% mutate(Pre_K=Pre_K=="Yes",
                                 Grades_15=Grades_15=="Yes",
                                 Grades_68=Grades_68=="Yes",
                                 Grades_912=Grades_912=="Yes")%>%
    mutate(n_grades = rowSums(select(.,Pre_K,Grades_15,Grades_68,Grades_912)))%>%
    filter(n_grades==1)%>%
    pivot_longer(c(Pre_K,Grades_15,Grades_68,Grades_912),
                 values_to = "InGrade",
                 names_to = "Grade")%>%
    filter(InGrade)
  
  ## creating object for output
  ip_regs_all <- NULL
  
  for(outcome in outcomes) {
    ##Overall
    ip_regs_all <-  pos_regs(df%>%filter(period %in% time_period), 
                             outcome, as_df=TRUE)%>%
      mutate(Grade="overall")%>%
      bind_rows(ip_regs_all)
    
    ##Loop over grades doing regressions among each
    grades<- c("Pre_K","Grades_15","Grades_68","Grades_912")
    grade_names <- c("K or under", "1 to 5", "6 to 8", 
                     "9 to 12")
    for (i in 1:length(grades)) {  
      ip_regs_all <- single_grade_df %>%
        filter(Grade==grades[i])%>%
        filter(period %in% time_period)%>%
        pos_regs(outcome, as_df=TRUE)%>%
        mutate(Grade=grade_names[i])%>%
        bind_rows(ip_regs_all)
    }
  }
  
  # add time period label
  ip_regs_all <- ip_regs_all %>%
    mutate(period=ifelse(length(time_period)>1, 'Jan-Jun',
                         ifelse(time_period=='jan_feb', 'Jan-Feb', 
                                ifelse(time_period=='mar_apr', 'Mar-Apr', 'May-Jun'))))
  
  # return data frame with model results
  return(ip_regs_all)
}


##' Function that runs the COVID-19 risk by type of school-based mitigation measures
##' 
##' @param time_period Months to run analysis for
##' @param df Data frame with survey data
##' @param outcomes COVID-19-related outcomes to run analysis for
##' @param outcome_nms Readable names for the COVID-19 outcomes
##' 
mit_type_analysis <- function(time_period, df,
                              outcomes = c('tst_pos', 'cli_pos', 'cli2_pos'),
                              outcome_nms = c('Overall Test+','Covid like illness','Loss taste/smell')) {
  
  message(time_period)
  
  # in person data frame
  inperson_df <- df%>% 
    filter(Child_IP_any=="Yes",
           period%in%time_period)%>%
    replace_na(
      list(sch_maskMand_st=FALSE,
           sch_maskMand_tch=FALSE,
           sch_sameTch=FALSE,
           sch_sameSt=FALSE,
           sch_outdoorInstr=FALSE,
           sch_restEntry=FALSE,
           sch_redClassSize=FALSE,
           sch_closedCafe=FALSE,
           sch_closedPlay=FALSE,
           sch_deskShields=FALSE,
           sch_xdeskSpace=FALSE,
           sch_noExtraCurr=FALSE,
           sch_noShareSupp=FALSE,
           sch_dailySymptScr=FALSE)
    ) %>%
    filter(n_interventions<=13)
  
  # covariates
  covs <- c('sch_maskMand_st',
            'sch_maskMand_tch',
            'sch_sameTch',
            'sch_sameSt',
            'sch_outdoorInstr',
            'sch_restEntry',
            'sch_redClassSize',
            'sch_closedCafe',
            'sch_closedPlay',
            'sch_deskShields',
            'sch_xdeskSpace',
            'sch_noExtraCurr',
            'sch_noShareSupp',
            'sch_dailySymptScr',
            'Child_IP_part_bin',
            'rel_avg_biwk_AR', 
            'IsMale', 
            'Age', 
            'OccupationRed',
            'Educational_Level',
            'HH_sz', 'num_kid_recode', 
            'Population', 
            'White', 
            'GINI', 
            'Poverty', 
            'Description',
            'mask_level', 
            'trav_out_state',
            'bar_rest_cafe_activ',
            'large_event_activ',
            'pub_transit_activ',
            'any_vaccine')
  
  # object to store results
  inter_reg_tbl <- NULL
  
  # loop over outcomes
  for (i in 1:length(outcomes)) {
    
    message(outcomes[[i]])
    
    # regression
    inter_reg <- inperson_df %>%
      survey_reg(as.formula(paste0(outcomes[[i]], '~', paste0(covs, collapse = '+'))))
    
    # combining + formatting regression outputs
    inter_reg_tbl <- 
      gtsummary::tbl_regression(inter_reg,exponentiate=T)$table_body%>%
      mutate(outcome=outcome_nms[[i]], adjustment="adjusted")%>%
      bind_rows(inter_reg_tbl)
    
  }
  
  # add period
  inter_reg_tbl <- inter_reg_tbl %>%
    mutate(period=ifelse(length(time_period)>1, 'Jan-Jun',
                         ifelse(time_period=='jan_feb', 'Jan-Feb', 
                                ifelse(time_period=='mar_apr', 'Mar-Apr', 'May-Jun'))))
  
  # return model results
  return(inter_reg_tbl)
}


##' Function that runs the COVID-19 risk by number of school-based mitigation measures
##' 
##' @param time_period Months to run analysis for
##' @param df Data frame with survey data
##' @param outcomes COVID-19-related outcomes to run analysis for
##' @param outcome_nms Readable names for the COVID-19 outcomes
##' 
run_nmit_analysis <- function(time_period, df,
                              outcomes = c('tst_pos', 'cli_pos', 'cli2_pos'),
                              outcome_nms = c('Overall Test+','Covid like illness','Loss taste/smell')) {
  
  df_mit <- df%>% 
    # exclude those with 14 interventions because these data seem unreliable 
    filter(n_interventions<=13 | Child_IP_any=="No") %>%
    # subset to period for analysis
    filter(period %in% time_period) %>%
    # make intervention and in-person schooling interaction variable
    mutate(n_int_bin=cut(n_interventions,c(0,1,4,7,10,14), right=FALSE),
           ft_cat=ifelse(Child_IP_full_bin,
                         sprintf("FULL_%s", n_int_bin),
                         "None"),
           pt_cat=ifelse(Child_IP_part_bin,
                         sprintf("PART_%s", n_int_bin),
                         "None"),
           ft_cat=relevel(factor(ft_cat),ref="None"),
           pt_cat=relevel(factor(pt_cat),ref="None"))
  
  # base covariates
  covars <- c('rel_avg_biwk_AR', 
              'IsMale', 
              'Age', 
              'OccupationRed',
              'Educational_Level',
              'HH_sz', 'num_kid_recode', 
              'Population', 
              'White', 
              'GINI', 
              'Poverty', 
              'Description',
              'mask_level', 
              'trav_out_state',
              'bar_rest_cafe_activ',
              'large_event_activ',
              'pub_transit_activ',
              'any_vaccine')
  
  # overall covariates
  covs_o <- c('Child_IP_full_bin', 'Child_IP_part_bin', covars)
  
  # mitigation analysis covariates
  covs_m <- c('ft_cat', 'pt_cat', covars)
  
  # objects to store results in
  overall_reg_tbl <- NULL
  mit_reg_tbl <- NULL
  
  # loop over outcomes for overall analysis
  for (i in 1:length(outcomes)) {
    
    # overall analysis
    all_mdl <- survey_reg(df_mit, as.formula(paste0(outcomes[[i]], '~', paste0(covs_o, collapse = '+'))))
    
    # combine results
    overall_reg_tbl<- 
      gtsummary::tbl_regression(all_mdl,exponentiate=T)$table_body%>%
      mutate(outcome=outcome_nms[[i]], adjustment="adjusted")%>%
      bind_rows(overall_reg_tbl)
    
  }
  
  # for mitigation measures analysis, exclude those with both full and part time schooling
  df_mit <- df_mit %>%
    filter(!(Child_IP_full_bin & Child_IP_part_bin))
  
  # loop over outcomes for mitigation analysis
  for (i in 1:length(outcomes)) {
    
    # number of mitigation measures analysis 
    mit_mdl <- survey_reg(df_mit, as.formula(paste0(outcomes[[i]],'~', paste0(covs_m, collapse = '+'))))
    
    # combine results
    mit_reg_tbl<- 
      gtsummary::tbl_regression(mit_mdl,exponentiate=T)$table_body%>%
      mutate(outcome=outcome_nms[[i]], adjustment="adjusted")%>%
      bind_rows(mit_reg_tbl)
    
  }
  
  # clean up model summaries for mitigation analysis
  mit_reg_plt <- mit_reg_tbl%>%
    filter(variable%in%c("ft_cat","pt_cat"),
           label!="ft_cat",
           label!="pt_cat",
           label!="None")%>%
    mutate(
      label=str_sub(label,start=6),
      label=factor(label, levels=c("[0,1)",
                                   "[1,4)",
                                   "[4,7)",
                                   "[7,10)",
                                   "[10,14)"),
                   labels = c("0","1-3","4-6","7-9","10+")),
      period=ifelse(length(time_period)>1, 'Jan-Jun',
                    ifelse(time_period=='jan_feb', 'Jan-Feb',
                           ifelse(time_period=='mar_apr', 'Mar-Apr', 'May-Jun'))))
  
  # clean up model summaries for overall analysis
  overall_reg_plt <- overall_reg_tbl%>%
    filter(variable%in%c("Child_IP_full_bin", "Child_IP_part_bin"),
           label!="Child_IP_full_bin",
           label!="Child_IP_part_bin")%>%
    mutate(variable=recode(variable,
                           Child_IP_full_bin = 'ft_cat',
                           Child_IP_part_bin = 'pt_cat'),
           label='Overall',
           period=ifelse(length(time_period)>1, 'Jan-Jun',
                         ifelse(time_period=='jan_feb', 'Jan-Feb',
                                ifelse(time_period=='mar_apr', 'Mar-Apr', 'May-Jun'))))
  
  # return a combine data frame
  return(rbind(mit_reg_plt, overall_reg_plt))
}


##' Function that runs the COVID-19 risk analyses by in-person schooling
##' and number of vaccine doses
##' 
##' @param data Data frame with individual-level survey data
##' @param covs Covariates to use in the regressions
##'
vax_reg <- function(data, covs) {
  
  vax_reg_tbl <- NULL
  
  vax_tst <- survey_reg(data, as.formula(paste0('tst_pos~', paste0(covs, collapse = '+'))))
  vax_cli <- survey_reg(data, as.formula(paste0('cli_pos~', paste0(covs, collapse = '+'))))
  vax_cli2 <- survey_reg(data, as.formula(paste0('cli2_pos~', paste0(covs, collapse = '+'))))
  
  vax_reg_tbl <- 
    gtsummary::tbl_regression(vax_tst,exponentiate=T)$table_body%>%
    mutate(outcome="Overall Test+", adjustment="adjusted")%>%
    bind_rows(vax_reg_tbl)
  
  vax_reg_tbl <- 
    gtsummary::tbl_regression(vax_cli,exponentiate=T)$table_body%>%
    mutate(outcome="Covid-like illness", adjustment="adjusted")%>%
    bind_rows(vax_reg_tbl)
  
  vax_reg_tbl <- 
    gtsummary::tbl_regression(vax_cli2,exponentiate=T)$table_body%>%
    mutate(outcome="Loss taste/smell", adjustment="adjusted")%>%
    bind_rows(vax_reg_tbl)
  
  return(vax_reg_tbl)
}