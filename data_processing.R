source("setup.R")

parties <- c('SDP', 'NCP', 'FP', 'CPF', 'GL', 'LA', 'SPP', 'CD')

oldnames <- c(paste('PARTY_SUPPORTER_', as.character(1:8), sep = ""), 
              paste('POL_party_closeness_', as.character(1:8), sep = ""),
              paste(paste('PARTY_EVAL_', as.character(1:8), sep = ""), '_1', sep = ""),
              paste('SOS_DIS_1_', as.character(1:8), sep = ""),
              paste('SOS_DIS_2_', as.character(1:8), sep = ""))

newnames <- c(paste('support_', parties, sep = ""), 
              paste('closeness_', parties, sep = ""), 
              paste('like_', parties, sep = ""),
              paste('socdis1_', parties, sep = ""),
              paste('socdis2_', parties, sep = ""))

closeness_levels <- c('Hyvin kaukaiseksi' = 1, 'Jokseenkin kaukaiseksi' = 2,
                      'En läheiseksi enkä kaukaiseksi' = 3, 'Jokseenkin läheiseksi' = 4, 'Hyvin läheiseksi' = 5,
                      'En osaa sanoa' = 99)

support_levels <- c('Vastustan vahvasti' = 1, 'Vastustan' = 2, 'En kumpaakaan' = 3, 'Kannatan' = 4, 'Kannatan vahvasti' = 5, 'En osaa sanoa' = 99)

df <- read_excel(data_path) %>% 
  
  dplyr::filter(Finished == 1) %>% # Only include respondents who finished the survey
  rename(language = MotherTongue, birth_year = SOS_1, gender = SOS_3, education = SOS_4, political_interest = POL_interest, leftright = 'POL_v-o_1', libcon = 'POL_LIB-TRAD_1', vote_bool = POL_vote_1, voted_party = POL_vote_2) %>%
  rename_at(all_of(oldnames), ~ newnames) %>%
  
  mutate(birth_year = as.numeric(birth_year)) %>%
  mutate(age = 2021 - birth_year) %>%
  mutate(gender = recode_factor(gender, 'Mies' = 'male', 'Nainen' = 'female', 'Muu' = 'other', 
                                .missing = NA_character_)) %>%
  mutate(education = recode_factor(education, 
                                   "Tieteellinen jatkotutkinto" = 'phd',
                                   'Yliopistotutkinto' = 'university',
                                   "Ammattikorkeakoulututkinto tai vastaava" = 'polytechnic',
                                   "Ylioppilastutkinto" = 'ylioppilastutkinto',
                                   "Opistotason ammatillinen tutkinto" = 'vocational education',
                                   "Lyhyt ammatillinen koulutus (ammattikoulu, -kurssi)" = 'vocational education',
                                   "Peruskoululuokat 7-9, 10, keskikoulu" = 'elementary school',
                                   "Peruskoululuokat 1-6, kansakoulu" = 'elementary school',
                                   "Ei koulutusta" = 'no education',
                                   .missing = NA_character_)) %>%
  mutate(political_interest = recode_factor(political_interest,
                                            "Hyvin kiinnostunut" = 'very interested',
                                            "Jonkin verran kiinnostunut" = 'somewhat interested',
                                            "Vain vähän kiinnostunut" = 'not very interested',
                                            "En lainkaan kiinnostunut" = 'not interested',
                                            "En osaa sanoa" = NA_character_,
                                            .missing = NA_character_)) %>%
  
  mutate(across(starts_with('support_'), ~ as.numeric(dplyr::recode(.x, !!!support_levels)))) %>%
  mutate(across(starts_with('closeness_'), ~ as.numeric(dplyr::recode(.x, !!!closeness_levels)))) %>%
  
  mutate(voted_party = dplyr::recode_factor(voted_party, 'En muista' = "Don't remember", 
                                            "Muu, mikä?" = 'Other',
                                            'Suomen Keskusta (Kesk.)' = 'CPF', 
                                            'Suomen ruotsalainen kansanpuolue (RKP)' = 'SPP',
                                            "Vasemmistoliitto (Vas.)" = 'LA',
                                            "Kansallinen Kokoomus (Kok.)" = 'NCP',
                                            "Perussuomalaiset (PS)" = 'FP',
                                            "Suomen Kristillisdemokraatit (KD)" = 'CD',
                                            "Suomen Sosialidemokraattinen Puolue (SDP)" = 'SDP',
                                            "Vihreä liitto (Vihr.)" = 'GL',
                                            .missing = 'missing')) %>%
  
  rename(val_multiculturality = VAL_1_1, val_christ = VAL_2_1,
         val_genderequality = VAL_3_1,
         val_sexualminorities = VAL_4_1, val_laworder = VAL_5_1,
         val_nuclearfamily = VAL_6_1, val_immigration = VAL_7_1,
         val_environment = VAL_8_1, val_eu = VAL_9_1,
         val_market = VAL_10_1, val_tax = VAL_11_1,
         val_incdifferences = VAL_12_1, val_pubsector = VAL_13_1,
         val_language = VAL_14_1)