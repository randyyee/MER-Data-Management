## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: Main for Clinical Cascade Completeness
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: Long Dataset 
## CREATION DATE: 6/8/2020
## UPDATE: 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

source("MSD_FXNS.R")

## ============================== TEST ==============================

test <- msd_import(choose.files())

test1 <- msd_convert_long(test)

test2 <- recode_qtrs(test1)

test3 <- filter_ind_disagg(test2, "TX_CURR","Total Numerator")

test4 <- site_reporting(test3)

test4a <- mech_matrix(test3)

test4b <- primep_matrix(test3)

test4c <- agency_matrix(test3)

## ============================== MAIN ==============================


df<-test4 %>%
  mutate(Report = case_when(value != 0 ~ 1,
                            value == 0 ~ NA_real_)) %>%
  filter(psnu == "Hhohho")

test4 %>%
  mutate(Report = case_when(value != 0 ~ 1,
                            value == 0 ~ NA_real_)) %>%
  filter(psnu == "Hhohho") %>%
  na_if(0) %>%
  #filter(!is.na(Qtr)) %>%
  ggplot(aes(x = period, y = sitename,
             group = sitename, col = Report)) + 
  geom_line() + 
  geom_point(shape = 15) + 
  theme_minimal() +
  labs(title = "Site Reporting Completeness Timelines",
       subtitle = "Note: Orange indicates value reported as 0") +
  theme(axis.text.y = element_text(size=9), 
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) #+
  #scale_x_continuous(expand=c(0.01,0.01)) +
  #scale_color_gradient(na.value = "orange")

df2 <- test4a %>% 
  select(-value)%>%
  pivot_wider(names_from = "period", values_from = "mech_name")%>%
  mutate(`2019QTR1` = case_when(`2019 qtr1` == "NULL" ~NA_real_,
                                `2019 qtr1` != "NULL" ~1),
         `2019QTR2` = case_when(`2019 qtr1` == "NULL" & `2019 qtr2` == "NULL" ~NA_real_,
                                 `2019 qtr1`==`2019 qtr2` ~1,
                                 `2019 qtr1`!=`2019 qtr2` ~2,
                                TRUE ~3),
         `2019QTR3` = case_when(`2019 qtr2` == "NULL" & `2019 qtr3` == "NULL" ~NA_real_,
                                `2019 qtr3` == "NULL" ~NA_real_,
                                `2019 qtr3` == "NULL" ~NA_real_,
                                 `2019 qtr2`==`2019 qtr3` ~1,
                                 `2019 qtr2`!=`2019 qtr3` ~2,
                                TRUE ~3),
         `2019QTR4` = case_when(`2019 qtr3` == "NULL" | `2019 qtr4` == "NULL" ~NA_real_,
                                `2019 qtr4` == "NULL" ~NA_real_,
                                 `2019 qtr3`==`2019 qtr4` ~1,
                                 `2019 qtr3`!=`2019 qtr4` ~2,
                                TRUE ~3),
         `2020QTR1` = case_when(`2019 qtr4` == "NULL" | `2020 qtr1` == "NULL" ~NA_real_,
                                `2020 qtr1` == "NULL" ~NA_real_,
                                 `2019 qtr4`==`2020 qtr1` ~1,
                                 `2019 qtr4`!=`2020 qtr1` ~2,
                                TRUE ~3),
         `2020QTR2` = case_when(`2020 qtr1` == "NULL" | `2020 qtr2` == "NULL" ~NA_real_,
                                `2020 qtr2` == "NULL" ~NA_real_,
                                 `2020 qtr1`==`2020 qtr2` ~1,
                                 `2020 qtr1`!=`2020 qtr2` ~2,
                                TRUE ~3),
         `2020QTR3` = case_when(`2020 qtr2` == "NULL" | `2020 qtr3` == "NULL" ~NA_real_,
                                `2020 qtr3` == "NULL" ~NA_real_,
                                `2020 qtr2`==`2020 qtr3` ~1,
                                `2020 qtr2`!=`2020 qtr3` ~2,
                                TRUE ~3)
         )
