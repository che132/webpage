library(tidyverse)
library(googledrive)
library(googlesheets4)

lowN_field <- 
  "https://docs.google.com/spreadsheets/d/12Y6q0vNTgNUDVz8UICXLZJD8L1OO4GI4NgCf9yIqyW4/edit#gid=1107220663" %>%
  as_id() %>%
  range_read(., sheet = "PSU_LowN")

lowN_field_base <- lowN_field %>%
  filter(Person == "R") %>%
  select(Row = `Plot #`, Treatment, Genotype = Line) %>%
  mutate(
    Treatment = ifelse(Treatment == "HN", "High-N", "Low-N"),
    Genotype = case_when(
      Genotype == "AMF_MUT" ~ "M",
      Genotype == "AMF_WT" ~ "WT",
      Genotype == "INV4 B73" ~ "CTRL",
      grepl("INV4 Mi21", Genotype) ~ gsub("INV4 Mi21", "INV4", Genotype),
      T ~ Genotype
    )
  ) 

lowP_field <- 
  "https://docs.google.com/spreadsheets/d/12Y6q0vNTgNUDVz8UICXLZJD8L1OO4GI4NgCf9yIqyW4/edit#gid=1107220663" %>%
  as_id() %>%
  range_read(., sheet = "PSU_P_field")

lowP_field_base <- lowP_field %>%
  select(Row = `PHO22-`, Treatment, Genotype = Description) %>%
  mutate(rn = rep_len(c(1:3), n())) %>%
  filter(rn == 2) %>%
  select(-rn)

labels_Yield <- bind_rows(lowN_field_base, lowP_field_base) %>%
  rownames_to_column("ID") %>%
  mutate(Measurement = "Yield") %>%
  mutate(label = paste(Row, Genotype, Treatment, Measurement, sep ="_"))

labels_ears <- bind_rows(lowN_field_base, lowP_field_base) %>%
  mutate(Measurement = list(paste0("ear.", c(1:3)))) %>%
  unnest(Measurement) %>%
  rownames_to_column("ID") %>%
  mutate(label = paste(Row, Genotype, Treatment, Measurement, sep ="_"))

work_book <- createWorkbook()

tibble(
  what = c("labels_Yield", "labels_ears"),
  data = list(labels_Yield, labels_ears)
  ) %>%
  mutate(wsh = map(
    .x = what,
    .f = ~ addWorksheet(work_book, sheetName = .x)
  )) %>%
  mutate(wsh = map2(
    .x = what,
    .y = data,
    .f = ~ writeData(work_book, sheet = .x, x = .y)
  ))

saveWorkbook(
  work_book,
  file= "Yield_nutrient_use_labels_with_INV4.xlsx",
             overwrite = TRUE)

  
