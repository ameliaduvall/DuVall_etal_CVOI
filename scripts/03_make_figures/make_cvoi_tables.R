## CVOI results tables

## assp ####
assp_boot <- readRDS(here("results", "assp_bootstrapped.RDS"))

assp_table <- data.frame(Hypothesis_number = assp_boot$hypothesis,
                         Hypothesis=assp_boot$name,
                         CVoI=paste0(formatC(assp_boot$mean_cvoi, digits=2, format="f"),
                               " (", formatC(sqrt(assp_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(assp_boot$mean_red, digits=2, format="f"),
                              " (", formatC(sqrt(assp_boot$var_red), digits=2, format="f"),")"))
write.csv(assp_table, file=here("figures", "tables", "assp_cvoi_results.csv"))

## brpe ####
brpe_boot <- readRDS(here("results", "brpe_bootstrapped.RDS"))

brpe_table <- data.frame(Hypothesis_number = brpe_boot$hypothesis,
                         Hypothesis=brpe_boot$name,
                         CVoI=paste0(formatC(brpe_boot$mean_cvoi, digits=2, format="f"),
                                     " (", formatC(sqrt(brpe_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(brpe_boot$mean_red, digits=2, format="f"),
                                    " (", formatC(sqrt(brpe_boot$var_red), digits=2, format="f"),")"))
write.csv(brpe_table, file=here("figures", "tables", "brpe_cvoi_results.csv"))

## caau ####
caau_boot <- readRDS(here("results", "caau_bootstrapped.RDS"))

caau_table <- data.frame(Hypothesis_number = caau_boot$hypothesis,
                         Hypothesis=caau_boot$name,
                         CVoI=paste0(formatC(caau_boot$mean_cvoi, digits=2, format="f"),
                                     " (", formatC(sqrt(caau_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(caau_boot$mean_red, digits=2, format="f"),
                                    " (", formatC(sqrt(caau_boot$var_red), digits=2, format="f"),")"))
write.csv(caau_table, file=here("figures", "tables", "caau_cvoi_results.csv"))

## scmu ####
scmu_boot <- readRDS(here("results", "scmu_bootstrapped.RDS"))

scmu_table <- data.frame(Hypothesis_number = scmu_boot$hypothesis,
                         Hypothesis=scmu_boot$name,
                         CVoI=paste0(formatC(scmu_boot$mean_cvoi, digits=2, format="f"),
                                     " (", formatC(sqrt(scmu_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(scmu_boot$mean_red, digits=2, format="f"),
                                    " (", formatC(sqrt(scmu_boot$var_red), digits=2, format="f"),")"))
write.csv(scmu_table, file=here("figures", "tables", "scmu_cvoi_results.csv"))

## snpl ####
snpl_boot <- readRDS(here("results", "snpl_bootstrapped.RDS"))

snpl_table <- data.frame(Hypothesis_number = snpl_boot$hypothesis,
                         Hypothesis=snpl_boot$name,
                         CVoI=paste0(formatC(snpl_boot$mean_cvoi, digits=2, format="f"),
                                     " (", formatC(sqrt(snpl_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(snpl_boot$mean_red, digits=2, format="f"),
                                    " (", formatC(sqrt(snpl_boot$var_red), digits=2, format="f"),")"))
write.csv(snpl_table, file=here("figures", "tables", "snpl_cvoi_results.csv"))

## wegu ####
wegu_boot <- readRDS(here("results", "wegu_bootstrapped.RDS"))

wegu_table <- data.frame(Hypothesis_number = wegu_boot$hypothesis,
                         Hypothesis=wegu_boot$name,
                         CVoI=paste0(formatC(wegu_boot$mean_cvoi, digits=2, format="f"),
                                     " (", formatC(sqrt(wegu_boot$var_cvoi), digits=2, format="f"),")"), 
                         Red=paste0(formatC(wegu_boot$mean_red, digits=2, format="f"),
                                    " (", formatC(sqrt(wegu_boot$var_red), digits=2, format="f"),")"))
write.csv(wegu_table, file=here("figures", "tables", "wegu_cvoi_results.csv"))

## combine
assp_table2 <- assp_table %>% mutate(species = c("assp"))
brpe_table2 <- brpe_table %>% mutate(species = c("brpe"))
caau_table2 <- caau_table %>% mutate(species = c("caau"))
scmu_table2 <- scmu_table %>% mutate(species = c("scmu"))
wegu_table2 <- wegu_table %>% mutate(species = c("wegu"))
snpl_table2 <- snpl_table %>% mutate(species = c("snpl"))

all_scores <- bind_rows(assp_table2, brpe_table2, caau_table2, scmu_table2, wegu_table2, snpl_table2)