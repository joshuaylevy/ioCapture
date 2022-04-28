library(tidyverse)
library(stargazer)


`%notin%` <- Negate(`%in%`)
setwd('~')
setwd('ioCapture')

adhoc_path <- 'adhoc_material/Reports/04-21/'


df <- read_csv('00_Scraping/indicator_count_df.csv')

df <- df %>%
  filter(year %notin% c(1990, 2021))


ggplot(df %>%
         group_by(year, publication) %>%
         mutate(market_power_all = sum(market_power_indicator),
                antitrust_all = sum(anti_trust_indicator),
                publication = toupper(publication)))+
  geom_line(aes(x=year, y=market_power_all, color=publication), size=1.25)+
  labs(title="'Market Power' in abstract (count)")+
  ylab('Articles')+
  scale_color_discrete(name='Publication')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'marketpower_count.png'), plot=last_plot())

ggplot(df %>%
         group_by(year, publication) %>%
         mutate(market_power_all = sum(market_power_indicator),
                antitrust_all = sum(anti_trust_indicator),
                publication = toupper(publication)))+
  geom_line(aes(x=year, y=antitrust_all, color=publication), size=1.25)+
  labs(title="'Antitrust' in abstract (count)")+
  ylab('Articles')+
  scale_color_discrete(name='Publication')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'antitrust_count.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication = toupper(publication)))+
  geom_line(aes(x=year, y=K_code/count, color=publication), size=1.25)+
  scale_color_discrete(name='Publication')+
  ggtitle('KXX-Code share by journal')+
  ylab('Share of articles with a KXX code')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'KXX_code_share.png'), plot=last_plot())

ggplot(df %>%
         mutate(publication = toupper(publication)))+
  geom_line(aes(x=year, y=L_code/count, color=publication), size=1.25)+
  scale_color_discrete(name='Publication')+
  ggtitle('LXX-Code share by journal')+
  ylab('Share of articles with a LXX code')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'LXX_code_share.png'), plot=last_plot())



indicators_df <- read_csv('adhoc_material/Reports/04-21/L_papers_indicators.csv') %>%
  mutate(publication = toupper(publication)) %>%
  filter(year %notin% c(1990, 2021))

sum_func <- function(x, na.rm=TRUE) (sum(x))

indicators_count_df <- indicators_df %>%
  group_by(publication, year) %>% 
  mutate(L_code_count = sum(L_code))%>%
  mutate_at(c(
    # 'L_code',
    'L0_code',
    'L1_code',
    'L2_code',
    'L3_code',
    'L4_code',
    'L5_code',
    'L6_code',
    'L7_code',
    'L8_code',
    'L9_code',
    'anti_trust_indicator',
    'market_power_indicator',
    'contains_theory'), sum_func) %>%
  distinct(publication, year, .keep_all = TRUE)%>% 
  select(-c(index, date, title, abstract,jel_code, L_code, K_code, D4_code, O3_code, G34_code,L40_code, L41_code, L42_code, L43_code, L44_code, L49_code))%>% 
  pivot_longer(!c(publication, year, L_code_count), names_to='indicator', values_to='count') 

#LX codes by journal by year (COUNT)
ggplot(indicators_count_df %>%
         filter(indicator %notin% c('anti_trust_indicator', 'contains_theory', 'K21_code', 'market_power_indicator')))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  scale_fill_discrete(breaks = c('L0_code',
                                'L1_code',
                                'L2_code',
                                'L3_code',
                                'L4_code',
                                'L5_code',
                                'L6_code',
                                'L7_code',
                                'L8_code',
                                'L9_code'
                                ),
                     labels = c('L0: General',
                                'L1: Market structure, firm strategy, and market performance',
                                'L2: Firm objectives, organization, and behavior',
                                'L3: Nonprofit organizations and public enterprise',
                                'L4: Antitrust issues and policy',
                                'L5: Regulation and industrial policy',
                                'L6: Industry studies: manufacturing',
                                'L7: Industry studeis: primary products and construction',
                                'L8: Industry studies: services',
                                'L9: Industry studies: transportation and utilities'
                     ),
                     name='JEL Code:')+
  scale_color_discrete(guide="none")+
  theme_minimal()+
  labs(title = 'Number of articles labelled with LX JEL Codes',
       caption = 'Note: Articles can mention more than one LX code, so the publication-year count may exceed the number of L-code articles')+
  ylab('Articles')+
  facet_wrap('publication')
ggsave(filename=paste(adhoc_path, 'LX-breakdown-count-by-journal.png'), plot=last_plot())

#LX codes by year (COUNT)
ggplot(indicators_count_df %>%
         group_by(year, indicator)%>%
         mutate(count = sum(count))%>%
         filter(indicator %notin% c('anti_trust_indicator', 'contains_theory', 'K21_code', 'market_power_indicator', 'publication'))%>%
         distinct(year, indicator, .keep_all = TRUE))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  scale_fill_discrete(breaks = c('L0_code',
                                 'L1_code',
                                 'L2_code',
                                 'L3_code',
                                 'L4_code',
                                 'L5_code',
                                 'L6_code',
                                 'L7_code',
                                 'L8_code',
                                 'L9_code'),
                      labels = c('L0: General',
                                 'L1: Market structure, firm strategy, and market performance',
                                 'L2: Firm objectives, organization, and behavior',
                                 'L3: Nonprofit organizations and public enterprise',
                                 'L4: Antitrust issues and policy',
                                 'L5: Regulation and industrial policy',
                                 'L6: Industry studies: manufacturing',
                                 'L7: Industry studeis: primary products and construction',
                                 'L8: Industry studies: services',
                                 'L9: Industry studies: transportation and utilities'),
                      name='JEL Code:')+
  scale_color_discrete(guide="none")+
  theme_minimal()+
  labs(title = 'Number of articles labelled with LX JEL Codes',
       caption = 'Note: Articles can mention more than one LX code, so the publication-year count may exceed the number of L-code articles')+
  ylab('Articles')
ggsave(filename=paste(adhoc_path, 'LX-breakdown-count.png'), plot=last_plot())


#ANTI-TRUST IN ABSTRACT by year (COUNT)
ggplot(indicators_count_df %>%
         group_by(year, indicator)%>%
         mutate(count = sum(count))%>%
         filter(indicator %notin% c('contains_theory', 'K21_code', 'market_power_indicator', 'publication'))%>%
         ungroup() %>%
         mutate(anti_trust_indicator_count = ifelse(indicator == 'anti_trust_indicator', count, 0),
                L_code_less_anti_trust = L_code_count - anti_trust_indicator_count) %>%
         select(-c('indicator', 'count')) %>%
         distinct(year, publication, .keep_all = TRUE) %>%
         pivot_longer(!c(publication, year, 'L_code_count'), names_to='indicator', values_to = 'count') %>%
         # view() %>%
         group_by(year, indicator) %>%
         mutate(count = sum(count)) %>%
         distinct(year, indicator, .keep_all = TRUE)
       )+
  scale_fill_discrete(breaks=c('anti_trust_indicator_count',
                               'L_code_less_anti_trust'),
                      labels=c('Anti-trust',
                               'Non-anti-trust LX'),
                      name='Type:')+
  scale_color_discrete(guide='none')+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  theme_minimal()+
  labs(title = 'Number of articles that mention "anti-trust" in the abstract',
       caption = 'Note: Articles without an LX code can still mention antitrust in an abstract, so total counts may not be accurate')+
  ylab('Aritlces')
ggsave(filename=paste(adhoc_path, 'anti-trust-in-abstract.png'), plot=last_plot())


#ANTI-TRUST IN ABSTRACT by year (COUNT)
ggplot(indicators_count_df %>%
         group_by(year, indicator)%>%
         mutate(count = sum(count))%>%
         filter(indicator %notin% c('contains_theory', 'K21_code', 'market_power_indicator', 'publication'))%>%
         ungroup() %>%
         mutate(anti_trust_indicator_count = ifelse(indicator == 'anti_trust_indicator', count, 0),
                L_code_less_anti_trust = L_code_count - anti_trust_indicator_count) %>%
         select(-c('indicator', 'count')) %>%
         distinct(year, publication, .keep_all = TRUE) %>%
         pivot_longer(!c(publication, year, 'L_code_count'), names_to='indicator', values_to = 'count') %>%
         # view() %>%
         group_by(year, publication, indicator) %>%
         mutate(count = sum(count)))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  scale_fill_discrete(breaks=c('anti_trust_indicator_count',
                               'L_code_less_anti_trust'),
                      labels=c('Anti-trust',
                               'Non-anti-trust LX'),
                      name='Type:')+
  scale_color_discrete(guide='none')+
  theme_minimal()+
  facet_wrap('publication')+
  labs(title = 'Number of articles that mention "anti trust" in the abstract',
       caption = 'Note: Articles without an LX code can still mention antitrust in an abstract, so total counts may not be accurate')
ggsave(filename=paste(adhoc_path, 'anti-trust-in-abstract-by-journal.png'), plot=last_plot())


ggplot(indicators_count_df %>%
         filter(indicator %in% c('L4_code')) %>%
         group_by(year)%>%
         mutate(count=sum(count),
                L_code_count=sum(L_code_count)) %>%
         mutate(L4_indicator_count = ifelse(indicator == 'L4_code', count, 0),
                L_code_less_L4 = L_code_count - L4_indicator_count) %>%
         select(-c('indicator', 'count')) %>%
         distinct(year, .keep_all = TRUE) %>%
         pivot_longer(!c(publication, year, L_code_count), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  labs(title='Share of LX articles that are L4 (antitrust issues and policies)')+
  scale_fill_discrete(breaks=c('L_code_less_L4',
                                'L4_indicator_count'),
                       labels=c('LXX articles',
                                'L4 articles'),
                       name="Type:",
                      direction=-1)+
  scale_color_discrete(guide="none",
                       direction=-1)+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'L4-vs-LXX.png'), plot=last_plot())

ggplot(indicators_count_df %>%
         filter(indicator %in% c('L4_code', 'L_code')) %>%
         ungroup() %>%
         group_by(year, publication) %>%
         mutate(L4_indicator_count = ifelse(indicator == 'L4_code', count, 0),
                L_code_less_L4 = L_code_count - L4_indicator_count) %>%
         select(-c('L_code_count', 'indicator', 'count'))%>%
         pivot_longer(!c('publication', 'year'), names_to='indicator', values_to='count')%>%
         distinct(publication, year, indicator, .keep_all = TRUE))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('L_code_less_L4',
                               'L4_indicator_count'),
                      labels=c('LXX articles',
                               'L4 articles'),
                      name="Type:",
                      direction=-1)+
  scale_color_discrete(guide="none",
                       direction=-1)+
  labs(title='Share of LX articles that are L4 (antitrust issues and policies), by journal')+
  ylab('Articles')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste(adhoc_path, 'L4-vs-LXX-by_journal.png'), plot=last_plot())


ggplot(indicators_count_df %>%
         filter(indicator == 'contains_theory')%>%
         ungroup() %>%
         group_by(year, publication) %>%
         mutate(contains_theory_indicator_count = ifelse(indicator== 'contains_theory', count, 0),
                L_code_less_theory = L_code_count - contains_theory_indicator_count) %>%
         ungroup() %>%
         select(-c('L_code_count', 'indicator', 'count')) %>%
         pivot_longer(!c('publication', 'year'), names_to='indicator', 'values_to'='count') %>%
         group_by(year, indicator) %>%
         mutate(count = sum(count)) %>%
         distinct(year, indicator, .keep_all = TRUE))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  scale_fill_discrete(breaks=c('contains_theory_indicator_count',
                               'L_code_less_theory'),
                      labels=c('Theory articles',
                               'LX articles'),
                      name='Type:')+
  scale_color_discrete(guide='none')+
  labs(title='Share of LXX articles that also contain theory* JEL codes',
       caption='*Theory codes include: C7, D11, D5, D21, D85, and D86')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, "theory-vs-LXX.png"), plot=last_plot())


ggplot(indicators_count_df %>%
         filter(indicator == 'contains_theory')%>%
         ungroup() %>%
         group_by(year, publication) %>%
         mutate(contains_theory_indicator_count = ifelse(indicator== 'contains_theory', count, 0),
                L_code_less_theory = L_code_count - contains_theory_indicator_count) %>%
         select(-c('L_code_count', 'indicator', 'count')) %>%
         pivot_longer(!c('publication', 'year'), names_to='indicator', 'values_to'='count') %>%
         distinct(publication, year, indicator, .keep_all = TRUE))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator))+
  scale_fill_discrete(breaks=c('contains_theory_indicator_count',
                               'L_code_less_theory'),
                      labels=c('Theory articles',
                               'LX articles'),
                      name='Type:')+
  scale_color_discrete(guide='none')+
  labs(title='Share of LXX articles taht also contain theory* JEL codes, by journal',
       caption='*Theory codes include: C7, D11, D5, D85, and D86')+
  ylab('Articles')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste(adhoc_path, 'theory-vs-LXX-by-journal.png'), plot=last_plot())



# I'm omitting this figure because there are too many 0s in the data-set that make normalization functionally useless
# ggplot(indicators_df %>%
#          select('year', 'publication', 'L_code', 'L4_code') %>%
#          group_by(year, publication) %>%
#          mutate(L4_over_L = sum(L4_code)/sum(L_code)) %>% 
#          distinct(year, publication, L4_over_L, .keep_all = TRUE)%>%
#          ungroup() %>% 
#          group_by(publication) %>%
#          arrange(year, .by_group=TRUE, .desc=TRUE)%>%
#          mutate(L4_over_L_base_91 = first(L4_over_L),
#                 # L4_over_L_normalized = (L4_over_L - L4_over_L_base_91)/L4_over_L_base_91,
#                 L4_over_L_normalized = L4_over_L / L4_over_L_base_91,
#                 L4_over_L_normalized = case_when(is.na(L4_over_L_normalized) ~ 0,
#                                                  L4_over_L_normalized == Inf ~ L4_over_L,
#                                                  TRUE ~ L4_over_L_normalized
#                                                  )) %>% 
#          view())+
#   geom_line(aes(x=year, y=L4_over_L_normalized, color=publication, group=publication), size=1.25)+
#   theme_minimal()


ggplot(indicators_df %>%
         select('year', 'publication', 'L_code', 'L4_code') %>%
         group_by(year) %>%
         mutate(L4_over_L = sum(L4_code)/sum(L_code)) %>%
         distinct(year, publication, L4_over_L, .keep_all = TRUE)%>%
         ungroup() %>%
         arrange(year, .by_group=TRUE, .desc=TRUE)%>%
         mutate(L4_over_L_base_91 = first(L4_over_L),
                # L4_over_L_normalized = (L4_over_L - L4_over_L_base_91)/L4_over_L_base_91,
                L4_over_L_normalized = L4_over_L / L4_over_L_base_91,
                L4_over_L_normalized = case_when(is.na(L4_over_L_normalized) ~ 0,
                                                 L4_over_L_normalized == Inf ~ L4_over_L,
                                                 TRUE ~ L4_over_L_normalized
                )))+
  geom_line(aes(x=year, y=L4_over_L_normalized,), size=1.25)+
  labs(title='Share of L4 articles relative to LXX articles by year',
       subtitle='Normalized to 1991')+
  ylab('L4 vs LXX share')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'L4-vs-LXX-share-by-year-normalized'), plot=last_plot())


ggplot(indicators_df %>%
         select('year', 'publication', 'L4_code', 'L40_code', 'L41_code', 'L42_code', 'L43_code', 'L44_code', 'L49_code') %>%
         group_by(year) %>%
         mutate_at(c('L4_code', 'L40_code', 'L41_code', 'L42_code', 'L43_code', 'L44_code', 'L49_code'),
                   sum_func) %>%
         distinct(year, .keep_all = TRUE) %>%
         select(-publication) %>%
         pivot_longer(!c(year), names_to='indicator', values_to='count')%>%
         filter(indicator != 'L4_code'))+
  geom_col(aes(x=year, y=count, group=indicator, color=indicator, fill=indicator))+
  scale_fill_discrete(breaks=c('L40_code',
                               'L41_code',
                               'L42_code',
                               'L43_code',
                               'L44_code',
                               'L49_code'),
                      labels=c('L40: General',
                               'L41: Monopolization; Horizontal anticompetitive practices',
                               'L42: Vertical restraints; Resale price maintenance; Quantity discounts',
                               'L43: Legal monopoloes and regulation or deregulation',
                               'L44: Antitrust policy and public enterprises, nonprofit institutions, and professional organizations',
                               'L49: Other'),
                      name='L4 (Antitrust Issues and Policies) Type:'
                      )+
  scale_color_discrete(guide='none')+
  theme_minimal()
ggsave(filename=paste(adhoc_path, 'L4-breakdown-count.png'), plot=last_plot())
  






table_df <- read_csv('00_Scraping/labels_for_table.csv') %>%
  rename('Publication' = '...1')

stargazer(table_df,
          rownames=FALSE,
          font.size='footnotesize',
          summary=FALSE)
