
# X_Friterature Review

library(gsheet); library(magrittr); library(tidyverse)

LitReview <- gsheet2tbl("https://docs.google.com/spreadsheets/d/15XXz_ZfBm-qEc39RDxQvJDyQ2iJ8xXSiMhK6hIijwK0/edit?usp=sharing")

LitReview %<>% as.data.frame

LitReview %<>% rename_all(~.x %>% str_trim %>% str_replace_all(" ", "_"))

LitReview %>% head

LitReview %>% pull(YNM) %>% table

LitReview %>% pull(YNM) %>% table %>% prop.table

LitReview %>% filter(YNM == "Y")

LitReview %>% filter(YNM == "M")

library(cowplot)

theme_set(theme_cowplot())

# Exploring changes through time ####

LitReview %>% 
  filter(YNM == "Y") %>% 
  count(Publication_Year) %>% 
  ggplot(aes(Publication_Year, n)) +
  geom_point()

LitReview %>% 
  filter(YNM == "Y") %>% 
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  ggplot(aes(Publication_Year, n)) +
  geom_point()

LitReview %>% 
  filter(YNM == "Y") %>% 
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  ggplot(aes(Publication_Year, n)) +
  geom_line()

LitReview %>% 
  filter(YNM == "M") %>% 
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  ggplot(aes(Publication_Year, n)) +
  geom_line()

LitReview %>% 
  filter(YNM == "N") %>% 
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  ggplot(aes(Publication_Year, n)) +
  geom_line()

LitReview %>% 
  # filter(YNM == "N") %>% 
  group_by(YNM) %>%
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  ggplot(aes(Publication_Year, n, colour = YNM)) +
  geom_line()

LitReview %>% 
  # filter(YNM == "N") %>% 
  group_by(YNM) %>% 
  count(Publication_Year) %>% mutate_at("n", cumsum) %>% 
  mutate_at("n", ~scales::rescale(.x, c(0, 1))) %>% 
  ggplot(aes(Publication_Year, n, colour = YNM)) +
  geom_line()

# Saving DOIs to save time ####

LitReview$DOI %>% is.na %>% table

LitReview$DOI %>% table %>% table

LitReview %>% 
  dplyr::select(Article_Title, DOI) %>% 
  is.na %>% colSums()

LitReview$Article_Title %>% table %>% table

Done <- 
  LitReview %>% #slice(1:100) %>% 
  dplyr::select(Article_Title, DOI)

LitReview-1 <- #LitReview
  gsheet2tbl("AgeingNewLitReview.html or whatever")

LitReview2 %>% 
  # anti_join(Done, by = c("DOI")) %>%
  nrow

# Looking at some abstract sifting ####

Maybes <- LitReview %>% mutate(Row = 1:n()) %>% 
  filter(YNM == "M")

Maybes %>% 
  filter(str_detect(Abstract, "Mammal|mammal"))

Maybes %>% 
  filter(str_detect(Abstract, "nfect"))

Maybes$Abstract %>% str_count("nfect") %>% sum(na.rm = T)

Maybes$Abstract %>% str_count("athogen") %>% sum(na.rm = T)

Maybes %>% 
  mutate(Output = paste0(Row, ": ", Authors, ": ", Article_Title, ": ", DOI, "\nAbstract:", Abstract)) %>% 
  # pull(Output) %>% 
  # paste(sep = "\n\n") %>% 
  dplyr::select(Output) %>% 
  write_delim("Abstracts.txt")

#Cross referencing parasite types with initial filter ####

LitReview2 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/19iy1VxbECUiWeBVj2mEpFNczjnI9bSOfaexgs0WfEDM/edit?usp=sharing")

LitReview2 %<>% rename_all(~.x %>% str_trim %>% str_replace_all(" ", "_"))

LitReview2 %>% 
  dplyr::select(Article_Title, DOI) %>% 
  is.na %>% colSums()

LitReview2$Article_Title %>% table %>% table

New <- 
  LitReview2 %>%
  dplyr::select(Article_Title, DOI)

Integrated <- LitReview2 %>% anti_join(Done, by = c("DOI"))

# Combining the two initial senescence searches ####

SenescenceResults1 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/15XXz_ZfBm-qEc39RDxQvJDyQ2iJ8xXSiMhK6hIijwK0/edit#gid=235659555")

SenescenceResults2 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/15XXz_ZfBm-qEc39RDxQvJDyQ2iJ8xXSiMhK6hIijwK0/edit#gid=23140384")

SenescenceDF <- 
  SenescenceResults1 %>% 
  bind_rows(SenescenceResults2) %>% 
  data.frame

SenescenceDF$Age.Categorization %>% table

SenescenceDF$YNM %>% table

SenescenceDF %>% filter(YNM == "Y") %>% 
  pull(Publication.Year) %>% qplot

#Cross referencing age with sensec searches ####

LitReview3 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/15XXz_ZfBm-qEc39RDxQvJDyQ2iJ8xXSiMhK6hIijwK0/edit?gid=1506073965#gid=1506073965")

LitReview3 %<>% rename_all(~.x %>% str_trim %>% str_replace_all(" ", "_"))

LitReview3 %>% 
  dplyr::select(Article_Title, DOI) %>% 
  is.na %>% colSums()

LitReview3$Article_Title %>% table %>% table

New2 <- 
  LitReview3 %>%
  dplyr::select(Article_Title, DOI)

Integrated2 <- LitReview3 %>% anti_join(SenescenceDF, by = c("DOI"))

library("readr")
write.csv(Integrated2,"age.csv")

