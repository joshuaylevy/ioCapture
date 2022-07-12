#Preamble

setwd('C:/Users/Joshualevy/Documents/ioCapture/lancieri_posner_zingales/amici_matching/07-12-22 amici-direction')
library(tidyverse)
library(stargazer)
library(readxl)
library(hrbrthemes)

data_in <- read_excel('antitrust_amici_per_case v3.xlsx')



governments_vector <- c(
  'Bigger City Government',
  'City government',
  'Government',
  'Smaller City Government',
  'government'
)

companies_vector <- c(
  'Bigger company',
  'Bigger Company',
  'Company',
  'Smaller company',
  'Smaller Company'
) 

associations_vector <- c(
  'Professional association',
  'State-sanctioned professional organization',
  'State Bar Association',
  'Union'
)

data_mod <- data_in %>%
  rename('brief_direction' = 'direction:\r\n1 = petitioner\r\n2 = neither\r\n3 = Respondent',
         'brief_favors' = 'TypeAmici\r\n')

# data_mod <- data_mod %>%
#   mutate(brief_favors_type = case_when(
#            brief_direction == '1' ~ party1,
#            brief_direction == '3' ~ party2,
#            brief_direction == '2' ~ 'Favors neither party',
#            is.na(brief_direction) ~ 'none'
#            ),
#          brief_favors_type = case_when(
#            brief_favors_type %in% governments_vector ~ 'Government',
#            brief_favors_type %in% companies_vector ~ 'Company',
#            brief_favors_type %in% associations_vector ~ 'Association',
#            TRUE ~ brief_favors_type
#          ),
#          pre_1975 = ifelse(opinionYear <= 1975, 'Pre-1975', 'Post-1975')) %>%
#   filter(!is.na(opinionYear))

data_mod <- data_in %>%
  rename('brief_direction' = 'direction:\r\n1 = petitioner\r\n2 = neither\r\n3 = Respondent',
         'brief_favors' = 'TypeAmici\r\n') %>%
  mutate(brief_favors_party_type = case_when(
      brief_direction == '1' ~ party1,
      brief_direction == '3' ~ party2,
      brief_direction ==  '2' ~ 'Favors neither',
      is.na(brief_direction) ~ 'None'),
    brief_favors_party_type = case_when(
      brief_favors_party_type %in% c('Bigger company', 'Bigger Company') ~ 'Bigger Company',
      brief_favors_party_type %in% c('Smaller company', 'Smaller Company') ~ 'Smaller Company',
      brief_favors_party_type %in% governments_vector ~ 'Government',
      TRUE ~ brief_favors_party_type),
    party1 = case_when(
      party1 %in% c('Bigger company', 'Bigger Company') ~ 'Bigger Company',
      party1 %in% c('Smaller company', 'Smaller Company') ~ 'Smaller Company',
      party1 %in% governments_vector ~ 'Government',
      TRUE ~ party1
    ),
    party2 = case_when(
      party2 %in% c('Bigger company', 'Bigger Company') ~ 'Bigger Company',
      party2 %in% c('Smaller company', 'Smaller Company') ~ 'Smaller Company',
      party2 %in% governments_vector ~ 'Government',
      TRUE ~ party2
    ),
    brief_favors_enforcement_type = case_when(
      brief_favors_party_type %in% companies_vector & (party1 == 'Government' | party2 == 'Government') ~ "Opposes enforcement",
      brief_favors_party_type == 'Bigger Company' & (party1 == 'Smaller Company' | party2 == 'Smaller Company') ~ "Opposes enforcement",
      brief_favors_party_type %in% companies_vector & (party1 == 'Individual' | party2 == 'Individual') ~ 'Opposes enforcement',
      brief_favors_party_type %in% associations_vector & (party1 %in% c('Government', 'Individual') | party2 %in% c('Government', 'Individual')) ~ 'Opposes enforcement',
      brief_favors_party_type == 'Government' & (party1 %in% companies_vector | party2 %in% companies_vector) ~ 'Favors enforcement',
      brief_favors_party_type == 'Smaller Company' & (party1 == 'Bigger Company' | party2 == 'Bigger Company') ~ 'Favors enforcement',
      brief_favors_party_type == 'Individual' & (party1 %in% companies_vector | party2 %in% companies_vector) ~ 'Favors enforcement',
      brief_favors_party_type %in% c('Government', 'Individual') & (party1 %in% associations_vector | party2 %in% associations_vector) ~ 'Favors enforcement',
      brief_favors_party_type == 'Favors neither' ~ 'Neither favors nor opposes'),
    pre_post_1975 = ifelse(year <= 1975, '1953-1974', '1975-2012')) %>%
  select(Title, year, party1, party2, brief_direction, brief_favors_party_type, brief_favors_enforcement_type, pre_post_1975)

data_mod %>% 
  filter(is.na(brief_favors_enforcement_type)) %>%
  view()







ggplot(data_mod %>%
         filter(!is.na(brief_favors_enforcement_type)))+
  geom_bar(aes(x=year, group=brief_favors_enforcement_type, fill=brief_favors_enforcement_type))+
  scale_fill_discrete(name='Brief type:')+
  
  xlab('Year')+
  ylab('Count')+
  labs(title="1953-2012 -- Distribution of amici curiae briefs' anti-trust enforcment stances")+
  theme_minimal()+
  facet_wrap(~pre_post_1975, scales='free_x')

ggsave(filename='fig1_without_na.png', plot=last_plot())

ggplot(data_mod)+
  geom_bar(aes(x=year, group=brief_favors_enforcement_type, fill=brief_favors_enforcement_type))+
  scale_fill_discrete(name='Brief type:')+
  
  xlab('Year')+
  ylab('Count')+
  labs(title="1953-2012 -- Distribution of amici curiae briefs' anti-trust enforcment stances")+
  theme_minimal()+
  facet_wrap(~pre_post_1975, scales='free_x')

ggsave(filename='fig1_with_na.png', plot=last_plot())


ggplot(data_mod)+
  geom_bar(aes(y=pre_post_1975, group=brief_favors_enforcement_type, fill=brief_favors_enforcement_type))+
  theme_minimal()+
  scale_fill_discrete(guide='none')+
  xlab('Count')+
  ylab('Period')+
  labs(title="1953-2012 -- Count of amici curiae briefs' anti-trust enforcement stances")+
  facet_wrap(~brief_favors_enforcement_type)

ggsave(filename='fig2.png', plot=last_plot())

