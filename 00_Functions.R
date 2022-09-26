
#### Summary table using weights ####
# Creates a table of cohort summaries using weights
# For categorical variables, the sum of the weights and the % is calculated
# For numerical variables, the weighed mean and weighted sd are calculated, as well as
# the weighted median and weighted IQR

# Input:
# - data = the dataset (must have weights named eave_weight)
# - dependent = a character of the dependent variables name
# - explanatory = a string of characters of the explanatory variables

# Output:
# A table with each explanatory variable as a row (multiple rows for each category if categorical)
# with two columns of the weighted summaries for the levels in the dependent variable

summary_factorlist_wt <- function(data, dependent, explanatory){
  # Create list to put in summaries into each element
  summary_tbl_list <- list()
  
  for(i in 1:length(explanatory)){
    
    # Extract variable
    n <- data %>%
      pull(!!sym(explanatory[i]))
    
    # If the variable has a nice name, then we use that
    variable_label = data %>% select(!!sym(explanatory[i]))
    variable_label = variable_label %>% var_label(unlist = TRUE)
    variable_label = as.vector(unlist(variable_label))
    variable_label = ifelse(variable_label == "", explanatory[i], variable_label)

    # If numeric then make weighted mean
    if(is.numeric(n)) {
      z_mean <- data %>%
        group_by(!!sym(dependent)) %>%
        summarise(mean = round(weighted.mean(!!sym(explanatory[i]), w = eave_weight, na.rm = TRUE),1),
                  sd = round(sqrt(spatstat.geom::weighted.var(!!sym(explanatory[i]), w = eave_weight)),1)) %>%
        mutate(mean.sd = paste0(mean, " (",sd,")")) %>%
        select(-mean, -sd) %>%
        mutate("characteristic" = explanatory[i]) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = mean.sd) %>%
        relocate(characteristic) %>%
        mutate(levels = "mean.sd")
      
      
      z_median <- data %>%
        group_by(!!sym(dependent)) %>%
        summarise(median = spatstat.geom::weighted.median(!!sym(explanatory[i]), w = eave_weight),
                  q1 = spatstat.geom::weighted.quantile(!!sym(explanatory[i]), w = eave_weight, probs = 0.25),
                  q3 = spatstat.geom::weighted.quantile(!!sym(explanatory[i]), w = eave_weight, probs = 0.75)) %>%
        mutate("characteristic" = explanatory[i]) %>%
        mutate(iqr = q3 -q1) %>%
        mutate(median.iqr = paste0(median, " (",iqr,")")) %>%
        select(-q1, -q3, -median, -iqr) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = median.iqr) %>%
        relocate(characteristic) %>%
        mutate(levels = "median.iqr")
      
      # Combine!!
      summary_tbl_list[[i]] <- full_join(z_mean, z_median)
      
      
      # Else get sum of weights of each level
    } else if (length(unique(data %>% pull(!!sym(dependent) ) ) ) ==1) {
      # This is for when there is only one level in the dependent variable
      summary_tbl_list[[i]] <- data %>%
        group_by(!!sym(explanatory[i])) %>%
        dplyr::summarise(n = sum(eave_weight)) %>%
        ungroup() %>%
        mutate(perc = sprintf("%.1f",round(n/sum(n)*100,1))) %>%
        mutate_if(is.numeric, ~formatC(round(.,0), format = "f", big.mark = ",", drop0trailing = TRUE)) %>%
        dplyr::mutate(n_perc := paste0(n, " (", perc,"%)")) %>%
        select(-n, -perc) %>%
        dplyr::rename("levels"= explanatory[i], !!dependent := n_perc) %>%
        mutate(characteristic = variable_label) %>%
        relocate(characteristic)
      
    } else {
      summary_tbl_list[[i]] <- data %>%
        group_by(!!sym(explanatory[i]), !!sym(dependent)) %>%
        dplyr::summarise(n = sum(eave_weight)) %>%
        ungroup() %>%
        group_by(!!sym(dependent)) %>%
        mutate(perc = sprintf("%.1f",round(n/sum(n)*100,1))) %>%
        mutate_if(is.numeric, ~formatC(round(.,0), format = "f", big.mark = ",", drop0trailing = TRUE)) %>%
        dplyr::mutate(n_perc := paste0(n, " (", perc,"%)")) %>%
        select(-n, -perc) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = n_perc) %>%
        dplyr::rename("levels"=explanatory[i]) %>%
        mutate(characteristic = variable_label) %>%
        relocate(characteristic)
      
    }
  }
  
  # Combine list together to make dataset
  summary_tbl_wt <- summary_tbl_list %>%
    reduce(full_join)
  
  summary_tbl_wt
}


fun_extract_clogit <- function(z) {
  #z is clogit fit
  z_se <-  sqrt(diag(z$var))
  z_out <- cbind.data.frame(RR = z$coefficients, LCL= z$coefficients - 1.96*z_se, UCL= z$coefficients + 1.96*z_se )
  z_out <- exp(z_out)
  z_out
}

fun_plot_tnd_cc <- function(z_est, z_vt, ul=2, ll = 0){
  #z_est is output of fun_extract_clogit
  #z_vt <- "Mo"
  z_title <- case_when(z_vt=="PB" ~ "BNT162b2", z_vt=="AZ" ~ "ChAdOx1", z_vt=="Mo" ~ "mRNA-1273")
  z1 <- z_est %>% filter(grepl(z_vt, rownames(z_est))) %>% 
    mutate(name = gsub(paste0("\\_",z_vt),"", rownames(.))) %>% 
    mutate(name = gsub("^vt","",name))
  z1 <- bind_rows(data.frame(RR=1, LCL=1,UCL=1, name="uv"), z1)
  z1 <- z1 %>% mutate(across(RR:UCL, ~ if_else(. > ul, ul, .)))
  z1 <- z1 %>% mutate(across(RR:UCL, ~ if_else(. < ll, ll, .)))
  
  z1 <- z1 %>% mutate(name=factor(name,levels=name))
  #
  x_labels = c("None", substring(z1$name, 5)[-1])
  x_labels = gsub(":", "-", x_labels)
  
  dose_labels = c("Dose 1", "Dose 2", "Dose 3")
  
  g1 <- z1 %>% ggplot(aes(x=name, y=RR)) + geom_point() + 
    geom_errorbar(aes(ymin=LCL, ymax=UCL), width=0.4) +
    theme(axis.text.x=element_text(angle=45, size=14), axis.text.y=element_text(size=14), plot.margin = unit(c(1, 1, 4, 1), "lines")) + 
    labs(x="Vaccine Status", y="Risk Ratio") + theme(axis.title = element_text(size = 16), axis.title.x = element_text(hjust=0)) +
    scale_x_discrete(labels=x_labels) + annotate(geom = "text", x = 4.5, y = -0.25, label = "Dose 1", size = 5) +  
    annotate(geom = "text", x = 10, y = -0.25, label = "Dose 2", size = 5) +
    coord_cartesian(ylim = c(ll, ul + 0.2), expand = FALSE, clip = "off") +
    geom_hline(yintercept = 1) + geom_segment(aes(x = 2, xend = 7, y = -0.18, yend = -0.18)) +
    geom_segment(aes(x = 8, xend = 12, y = -0.18, yend = -0.18))
  
  if ("v3_0:1" %in% z_est$vs) {
    g1 = g1 + annotate(geom = "text", x = 13.5, y = -0.25, label = "Dose 3", size = 5) +
    geom_segment(aes(x = 13, xend = 14, y = -0.18, yend = -0.18))
  } 
  #g1 = g1 + ylim(ul, ll)
  g1
  
}

run_model = function(eqn, df, event_name) {
  options(knitr.kable.NA = '')
  
  z_df <- df 
  
  z <- clogit(eqn, data=z_df, method="efron")
  
  #termplot(z, terms="pspline(ageYear)", se= TRUE)
  z_est <- fun_extract_clogit(z)
  z_g <- fun_plot_tnd_cc(z_est,"")
  
  z1 <- z_est %>% filter(grepl("vs", rownames(z_est))) %>% 
    mutate(vs = gsub("^vs","", rownames(.)))
  #z1 <- bind_rows(data.frame(RR=1, LCL=1,UCL=1, vs="uv"), z1)
  
  z2 <- z_df %>% group_by(vs) %>% dplyr::summarise(N=n(), R=sum(!!sym(event_name))) %>% as.data.frame()
  z3 <- z2 %>% left_join(z1, by="vs") %>% 
    mutate(ve=100*(1-RR), lcl=100*(1-UCL), ucl=100*(1-LCL))
  z3 <- z3 %>% dplyr::select(vs, N, R, ve, lcl, ucl)
  ret = list(z_g = z_g, z3 = z3, z1=z1)
  return(ret)
}
