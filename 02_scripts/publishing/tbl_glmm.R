tbl_glmm_results <- tibble(Estimate = NA, `Std..Error` = NA, `z.value` = NA, `Pr...z..` = NA, AIC = NA, BIC = NA, logLik = NA, deviance = NA, df.resid = NA) %>%
    bind_rows(
        data.frame(
            model = "Model 1",
            rbind(summary(glmm_mem_basic)$AICtab)) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(
            model = "Model 1",
            summary(glmm_mem_basic)$coefficients) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 2a", rbind(summary(glmm_mem_dur)$AICtab)) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 2a", summary(glmm_mem_dur)$coefficients) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 2b", rbind(summary(glmm_mem_num)$AICtab)) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 2b", summary(glmm_mem_num)$coefficients) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 3b", rbind(summary(glmm_mem_num_fix)$AICtab)) %>%
            rownames_to_column()) %>%
    bind_rows(
        data.frame(model = "Model 3b", summary(glmm_mem_num_fix)$coefficients) %>%
            rownames_to_column()) %>%
    select(
        model, Coefficient = rowname,
        # coeficeints
        Estimate, SE = Std..Error, z = z.value, p = Pr...z..,
        # AIC tab
        AIC, BIC, DIC = deviance, df = df.resid ) %>% # without `Log-Likelihood` = logLik,
    mutate(
        Coefficient = case_when(
            Coefficient == "(Intercept)" ~ "    INTERCEPT",
            Coefficient == "group_idmem" ~ "    GROUP",
            Coefficient == "prop_dur_z" ~ "    DURATION",
            Coefficient == "prop_num_z" ~ "    NUMBER",
            Coefficient == "fix_idobject_uncued" ~ "    OBJECT ROLE",
            Coefficient == "group_idmem:prop_dur_z" ~ "    GROUP*DURATION",
            Coefficient == "group_idmem:prop_num_z" ~ "    GROUP*NUMBER",
            Coefficient == "group_idmem:fix_idobject_uncued" ~ "    GROUP*OBJECT ROLE",
            Coefficient == "prop_num_z:fix_idobject_uncued" ~ "    NUMBER*OBJECT ROLE",
            Coefficient == "group_idmem:prop_num_z:fix_idobject_uncued" ~ "    GROUP*NUMBER*OBJECT ROLE"),
        Coefficient = ifelse(is.na(Coefficient), model, Coefficient),
        p = case_when(p < .001 ~ "< .001",
                      p < .01 ~ "< .01",
                      p < .05 ~ "< .05",
                      p >= .05 ~ round(p, 3) %>%
                          as.character() %>%
                          str_sub(2,5))) %>%
    mutate_at(c("Estimate", "SE", "z"),
              ~round(., 2)) %>%
    mutate_at(c("AIC", "BIC", "DIC"), # "Log-Likelihood",
              ~round(., 0)) %>%
    mutate_all(as.character) %>%
    drop_na(model) %>%
    mutate_all(~ifelse(is.na(.), " ", .)) %>%
    select(-model)
