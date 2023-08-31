library(rforce)

# Salesforce ----
refresh.data = FALSE

cases.q = pull.data(query.list$cases, refresh.data)
npi.q = pull.data(query.list$npi, refresh.data)
rach.q = pull.data(query.list$rach, refresh.data)
cf.q = pull.data(query.list$contributing.factor, refresh.data) %>% rename_with(~str_replace(.x,"^contributing.factor", "cf")) %>% rename(case.id = cf.case)


.queries.updated <- file.info(list.files("data/", pattern = "^rforce*", full.names = T)) %>%
  rownames_to_column("file") %>%
  mutate(file = str_remove(file, "data/")) %>%
  select(file, mtime)


# External Data ----














## ------------------------------ Next: process-data ------------------------------ ##
