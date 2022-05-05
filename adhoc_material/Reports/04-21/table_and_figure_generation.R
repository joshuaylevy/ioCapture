### PREAMBLE
library(tidyverse)
library(stargazer)
library(pals)

`%notin%` <- Negate(`%in%`)
setwd('~')
setwd('ioCapture')

adhoc_path <- 'adhoc_material/Reports/04-21/'

code_col <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','Y','Z'
)

desc_col <- c(
  'General Economics and Teaching',
  'History of Economic Through, Methodology and Heterodox Approaches',
  'Mathematical and Quantitative Methods',
  'Microeconomics',
  'Macroeconomics and Monetary Economics',
  'International Economics',
  'Financial Economics',
  'Public Economics',
  'Health, Education, and Welfare',
  'Labor and Demographic Economics',
  'Law and Economics',
  'Industrial Organization',
  'Business Administratio and Business Economics; Marketing; Accounting; Personnel Economics',
  'Economic History',
  'Economic Development, Innovation, Technologicla Change, and Growth',
  'Economic Systems',
  'Agricultural and Natural Resource Econoimcs; Environmental and Ecological Economics',
  'Urban, Rural, Regional, Real Estate, and Transportation Economics',
  'Miscellaneous Categories',
  'Other Special Topics'
)


jel_alpha_descriptions <- data.frame(desc_col, code_col)



#### General indicators
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
ggsave(filename=paste0(adhoc_path, 'marketpower_count.png'), plot=last_plot())

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
ggsave(filename=paste0(adhoc_path, 'antitrust_count.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication = toupper(publication)))+
  geom_line(aes(x=year, y=K_code/count, color=publication), size=1.25)+
  scale_color_discrete(name='Publication')+
  ggtitle('KXX-Code share by journal')+
  ylab('Share of articles with a KXX code')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'KXX_code_share.png'), plot=last_plot())

ggplot(df %>%
         mutate(publication = toupper(publication)))+
  geom_line(aes(x=year, y=L_code/count, color=publication), size=1.25)+
  scale_color_discrete(name='Publication')+
  ggtitle('LXX-Code share by journal')+
  ylab('Share of articles with a LXX code')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'LXX_code_share.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication=toupper(publication))%>%
         group_by(year) %>%
         mutate(L_code = sum(L_code),
                count = sum(count)) %>%
         select(year, L_code, count) %>%
         distinct(year, .keep_all=TRUE)%>%
         pivot_longer(!c(year), names_to='indicator', values_to='count')%>%
         view())+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L_code'),
                      labels=c('Top 5 (and RAND)',
                               'Industrial Organization (LXX)'),
                      direction=-1,
                      name='Type:')+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='IO vs Top 5 (and RAND) Publications')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'LXX-code-share-area.png'), plot=last_plot())


library(scales)
io_share_table <- df %>%
  mutate(publication=toupper(publication))%>%
  group_by(year) %>%
  mutate(L_code = sum(L_code),
         count = sum(count)) %>%
  select(year, L_code, count) %>%
  distinct(year, .keep_all=TRUE)%>%
  ungroup() %>%
  mutate(share = percent(L_code/count,
                         accuracy=0.1)) %>%
  column_to_rownames('year') %>%
  rename('Top 5 (and RAND)' = 'count',
         'IO (LXX)' = 'L_code',
         'IO Share' = 'share')
stargazer(io_share_table,
          summary=FALSE,
          rownames=TRUE,
          font.size='footnotesize',
          out=paste0(adhoc_path, 'tex_tables/io_share_by_year.tex'))





ggplot(df %>%
         mutate(publication=toupper(publication))%>%
         group_by(year) %>%
         mutate(L_code = sum(L_code),
                count = sum(count)) %>%
         select(year, L_code, count) %>%
         distinct(year, .keep_all=TRUE)%>%
         pivot_longer(!c(year), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L_code'),
                      labels=c('Top 5 (and RAND)',
                               'Industrial Organization (LXX)'),
                      direction=-1,
                      name='Type:')+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='IO as a share of Top 5 (and RAND) Publications')+
  ylab('Share')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'LXX-code-share-area-normalized.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication=toupper(publication))%>%
         group_by(year, publication) %>%
         mutate(L_code = sum(L_code),
                count = sum(count)) %>%
         select(year, L_code, count, publication) %>%
         distinct(year, .keep_all=TRUE)%>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L_code'),
                      labels=c('Top 5 (and RAND)',
                               'Industrial Organization (LXX)'),
                      direction=-1,
                      name='Type:')+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='IO vs Top 5 (and RAND) Publications, by journal')+
  ylab('Articles')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste0(adhoc_path, 'LXX-code-share-area-by-journal.png'), plot=last_plot())

io_share_journal_table <- df %>%
  mutate(publication=toupper(publication))%>%
  group_by(year, publication) %>%
  mutate(L_code = sum(L_code),
         count = sum(count)) %>%
  select(year, L_code, count, publication) %>%
  pivot_wider(names_from='publication',
              values_from=c('count', 'L_code')) %>%
  mutate(AER_io_share =  percent(L_code_AER / count_AER, accuracy=0.01),
         ECA_io_share =  percent(L_code_ECA / count_ECA, accuracy=0.01),
         JPE_io_share =  percent(L_code_JPE / count_JPE, accuracy=0.01),
         QJE_io_share =  percent(L_code_QJE / count_QJE, accuracy=0.01),
         RES_io_share =  percent(L_code_RES / count_RES, accuracy=0.01),
         RJE_io_share =  percent(L_code_RJE / count_RJE, accuracy=0.01),
         ) %>%
  select(-c(L_code_AER,
            L_code_ECA,
            L_code_JPE,
            L_code_QJE,
            L_code_RES,
            L_code_RJE,)) %>%
  rename('AER IO Share' = 'AER_io_share',
         'ECA IO Share' = 'ECA_io_share',
         'JPE IO Share' = 'JPE_io_share',
         'QJE IO Share' = 'QJE_io_share',
         'RES IO Share' = 'RES_io_share',
         'RJE IO Share' = 'RJE_io_share',
         'AER' = 'count_AER',
         'ECA' = 'count_ECA',
         'JPE' = 'count_JPE',
         'QJE' = 'count_QJE',
         'RJE' = 'count_RJE',
         'RES' = 'count_RES'
         ) %>%
  select(c('year',
           'AER',
           'AER IO Share',
           'ECA',
           'ECA IO Share',
           'JPE',
           'JPE IO Share',
           'QJE',
           'QJE IO Share',
           'RES',
           'RES IO Share',
           'RJE',
           'RJE IO Share',
           )) %>%
  column_to_rownames('year') %>%
  view()

stargazer(io_share_journal_table,
          summary=FALSE,
          rownames=TRUE,
          font.size='footnotesize',
          column.sep.width = '2pt',
          out=paste0(adhoc_path, 'tex_tables/io_share_journal_by_year.tex'))



ggplot(df %>%
         mutate(publication=toupper(publication))%>%
         group_by(year, publication) %>%
         mutate(L_code = sum(L_code),
                count = sum(count)) %>%
         select(year, L_code, count, publication) %>%
         distinct(year, .keep_all=TRUE)%>%
         view()%>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L_code'),
                      labels=c('Top 5 (and RAND)',
                               'Industrial Organization (LXX)'),
                      direction=-1,
                      name='Type:')+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='IO as a share of Top 5 (and RAND) Publications, by journal')+
  ylab('Share')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste0(adhoc_path, 'LXX-code-share-area-normalized-by-journal.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication=toupper(publication)) %>%
         group_by(year) %>%
         mutate(J3_code = sum(J3_code),
                count = sum(count)) %>%
         select(year, J3_code, count) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'J3_code'),
                      labels=c('Top 5 (and RAND)',
                               'J3 (Wages, compensation, and labor costs)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Number of Top 5 (and RAND) publications that are wage-related (J3)')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'J3-vs-top5.png'), plot=last_plot())

ggplot(df %>%
         mutate(publication=toupper(publication)) %>%
         group_by(year) %>%
         mutate(J3_code = sum(J3_code),
                count = sum(count)) %>%
         select(year, J3_code, count) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'J3_code'),
                      labels=c('Top 5 (and RAND)',
                               'J3 (Wages, compensation, and labor costs)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Number of Top 5 (and RAND) publications that are wage-related (J3)')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'J3-vs-top5-normalized.png'), plot=last_plot())


ggplot(df %>%
         mutate(publication = toupper(publication)) %>%
         group_by(year) %>%
         mutate(J3_code = sum(J3_code),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         select(year, J3_code, L4_code, count) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'J3_code',
                               'L4_code'),
                      labels=c('Top 5 (and RAND)',
                               'J3 (Wages, compensation, and labor costs)',
                               'L4 (Anti-trust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Top 5, J3 (wages) and L4 (anti-trust) articles')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'j3-l4-top5.png'), plot=last_plot())



ggplot(df %>%
         mutate(publication = toupper(publication)) %>%
         group_by(year) %>%
         mutate(J3_code = sum(J3_code),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         select(year, J3_code, L4_code, count) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'J3_code',
                               'L4_code'),
                      labels=c('Top 5 (and RAND)',
                               'J3 (Wages, compensation, and labor costs)',
                               'L4 (Anti-trust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Top 5, J3 (wages) and L4 (anti-trust) articles')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'j3-l4-top5-normalized.png'), plot=last_plot())








#### SPECIFIC INDICATORS

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
  view()
  distinct(publication, year, .keep_all = TRUE)%>% 
  select(-c(index, date, title, abstract,jel_code, L_code, K_code, D4_code, O3_code, G34_code,L40_code, L41_code, L42_code, L43_code, L44_code, L49_code))%>% 
  pivot_longer(!c(publication, year, L_code_count), names_to='indicator', values_to='count') %>%

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
ggsave(filename=paste0(adhoc_path, 'LX-breakdown-count-by-journal.png'), plot=last_plot())

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
ggsave(filename=paste0(adhoc_path, 'LX-breakdown-count.png'), plot=last_plot())


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
ggsave(filename=paste0(adhoc_path, 'anti-trust-in-abstract.png'), plot=last_plot())


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
ggsave(filename=paste0(adhoc_path, 'anti-trust-in-abstract-by-journal.png'), plot=last_plot())


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
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX.png'), plot=last_plot())

ggplot(indicators_count_df %>%
         filter(indicator %in% c('L4_code')) %>%
         group_by(year, publication)%>%
         mutate(count=sum(count),
                L_code_count=sum(L_code_count)) %>%
         mutate(L4_indicator_count = ifelse(indicator == 'L4_code', count, 0),
                L_code_less_L4 = L_code_count - L4_indicator_count) %>%
         select(-c('indicator', 'count')) %>%
         distinct(year, publication, .keep_all = TRUE) %>%
         pivot_longer(!c(publication, year, L_code_count), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, fill=indicator, color=indicator),
            position=position_stack(reverse=TRUE))+
  labs(title='L4 (antitrust issues and policies) vs LXX (IO) articles')+
  scale_fill_discrete(breaks=c('L_code_less_L4',
                               'L4_indicator_count'),
                      labels=c('LXX articles',
                               'L4 articles'),
                      name="Type:",
                      direction=-1)+
  scale_color_discrete(guide="none",
                       direction=-1)+
  ylab('Articles')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX-by-journal.png'), plot=last_plot())







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
            position=position_fill(reverse=TRUE))+
  labs(title='Share of LX articles that are L4 (antitrust issues and policies)')+
  scale_fill_discrete(breaks=c('L_code_less_L4',
                               'L4_indicator_count'),
                      labels=c('LXX articles',
                               'L4 articles'),
                      name="Type:",
                      direction=-1)+
  scale_color_discrete(guide="none",
                       direction=-1)+
  ylab('Share')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX-normalized.png'), plot=last_plot())





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
            position=position_fill(reverse=TRUE))+
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
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX-normalized-by_journal.png'), plot=last_plot())


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
            position=position_fill(reverse=TRUE))+
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
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX-normalized-by_journal.png'), plot=last_plot())



ggplot(df %>%
         select(year, publication, L4_code, count)%>%
         group_by(year) %>%
         mutate(publication=toupper(publication),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, color=indicator, fill=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L4_code'),
                      labels=c('Top 5',
                               'L4 (antitrust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Number of Top 5 (and RAND) publications that are anti-trust (L4)')+
  ylab("Aritlces")+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'L4-vs-top5.png'), plot=last_plot())


ggplot(df %>%
         select(year, publication, L4_code, count)%>%
         group_by(year) %>%
         mutate(publication=toupper(publication),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         distinct(year, .keep_all=TRUE) %>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, color=indicator, fill=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L4_code'),
                      labels=c('Top 5',
                               'L4 (antitrust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Share of Top 5 (and RAND) publications that are anti-trust (L4)')+
  ylab("Share")+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'L4-vs-top5-normalized.png'), plot=last_plot())

ggplot(df %>%
         select(year, publication, L4_code, count)%>%
         group_by(year) %>%
         mutate(publication=toupper(publication),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         distinct(year, publication, .keep_all=TRUE) %>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count')%>%
         view())+
  geom_area(aes(x=year, y=count, group=indicator, color=indicator, fill=indicator),
            position=position_stack(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L4_code'),
                      labels=c('Top 5',
                               'L4 (antitrust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Number of Top 5 (and RAND) publications that are anti-trust (L4)')+
  ylab("Aritlces")+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste0(adhoc_path, 'L4-vs-top5-by-journal.png'), plot=last_plot())


ggplot(df %>%
         select(year, publication, L4_code, count)%>%
         group_by(year) %>%
         mutate(publication=toupper(publication),
                L4_code = sum(L4_code),
                count = sum(count)) %>%
         distinct(year, publication, .keep_all=TRUE) %>%
         pivot_longer(!c(year, publication), names_to='indicator', values_to='count'))+
  geom_area(aes(x=year, y=count, group=indicator, color=indicator, fill=indicator),
            position=position_fill(reverse=TRUE))+
  scale_fill_discrete(breaks=c('count',
                               'L4_code'),
                      labels=c('Top 5',
                               'L4 (antitrust issues and policies)'),
                      name='Type:',
                      direction=-1)+
  scale_color_discrete(guide='none',
                       direction=-1)+
  labs(title='Share of Top 5 (and RAND) publications that are anti-trust (L4)')+
  ylab("Share")+
  theme_minimal()+
  facet_wrap('publication')
ggsave(filename=paste0(adhoc_path, 'L4-vs-top5-normalized.png'), plot=last_plot())







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
ggsave(filename=paste0(adhoc_path, "theory-vs-LXX.png"), plot=last_plot())


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
ggsave(filename=paste0(adhoc_path, 'theory-vs-LXX-by-journal.png'), plot=last_plot())



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
ggsave(filename=paste0(adhoc_path, 'L4-vs-LXX-share-by-year-normalized'), plot=last_plot())


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
                               'L43: Legal monopolies and regulation or deregulation',
                               'L44: Antitrust policy and public enterprises, nonprofit institutions, and professional organizations',
                               'L49: Other'),
                      name='L4 (Antitrust Issues and Policies) Type:'
                      )+
  scale_color_discrete(guide='none')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'L4-breakdown-count.png'), plot=last_plot())
  





publications_year_df <- read_csv('adhoc_material/Reports/04-21/publications_year.csv')

top5_year_df <- publications_year_df %>%
  group_by(year) %>%
  mutate(count = sum(count)) %>%
  ungroup()%>%
  distinct(year, count, .keep_all = TRUE)

ggplot(publications_year_df %>%
         mutate(publication = toupper(publication)))+
  geom_col(aes(x=year, y=count, color=publication, fill=publication))+
  scale_fill_discrete(name='Publication')+
  scale_color_discrete(guide='none')+
  labs(title='Top 5 (and RAND) publications over time')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'top_5_over_time_col.png'), plot=last_plot())

ggplot(publications_year_df %>%
         mutate(publication = toupper(publication)))+
  geom_col(aes(x=year, y=count, color=publication, fill=publication), position='fill')+
  scale_fill_discrete(name='Publication')+
  scale_color_discrete(guide='none')+
  labs(title='Top 5 (and RAND) publication shares over time')+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'top_5_over_time_col_shares.png'), plot=last_plot())

ggplot(publications_year_df %>%
         mutate(publication = toupper(publication)))+
  geom_line(aes(x=year, y=count, color=publication), size=1.25)+
  scale_color_discrete(name='Publication')+
  labs(title="Top 5 (and RAND) publications over time")+
  ylab('Articles')+
  theme_minimal()
ggsave(filename=paste0(adhoc_path, 'top5_over_time.png'), plot=last_plot())

publications_year_df_wide <- publications_year_df %>%
  mutate(publication = toupper(publication)) %>%
  pivot_wider(names_from='publication', values_from='count')%>%
  group_by(year)%>%
  rename('Year'= 'year') %>% 
  mutate(TOTAL = sum(AER, ECA, JPE, QJE, RES, RJE))
stargazer(publications_year_df_wide %>% column_to_rownames(var='Year'),
          rownames=TRUE,
          font.size='footnotesize',
          summary=FALSE,
          out=paste00(adhoc_path, 'tex_tables/top5_publications_by_year.tex'))





jel_normalized_df <- read_csv(paste0(adhoc_path, 'top_five_jel_breakdown.csv'))
jel_normalized_unnested_df <- read_csv(paste0(adhoc_path, 'top_five_jel_unnested.csv'))

ggplot(jel_normalized_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         mutate(publication = toupper(publication)) %>%
         na.omit() %>%
         select(title, year, publication, predom_jel) %>%
         group_by(year, publication) %>%
         count(predom_jel) %>% 
         rename('predom_jel_count'  = 'n') %>%
         distinct(year, predom_jel, .keep_all = TRUE) %>%
         pivot_wider(names_from='year',
                     values_from = 'predom_jel_count',
                     values_fill = 0) %>%
         pivot_longer(!c('publication', 'predom_jel'),
                      names_to = 'year',
                      values_to = 'predom_jel_count') %>%
         mutate(year = as.numeric(year)))+
  geom_area(aes(x=year, y=predom_jel_count, group=predom_jel, color=predom_jel, fill=predom_jel))+
  scale_fill_manual(values=unname(alphabet(n=length(unique(jel_normalized_df$predom_jel %>% na.omit())))),
                    breaks = jel_alpha_descriptions$code_col,
                    labels = jel_alpha_descriptions$desc_col,
                    name='Predominant Category')+
  scale_color_discrete(guide='none')+
  labs(title='Distribution of topics (by JEL code) over time, by publication',
       subtitle='Category determined by predominant category listed')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(paste0(adhoc_path, 'jel_predom_by_journal.png'), plot=last_plot(), height=8.5, width=11, units='in')



  ggplot(jel_normalized_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         na.omit() %>% 
         select(title, year, publication, predom_jel) %>%
         group_by(year) %>%
         count(predom_jel) %>% 
         rename('predom_jel_count'  = 'n') %>%
         distinct(year, predom_jel, .keep_all = TRUE))+
  geom_area(aes(x=year, y=predom_jel_count, group=predom_jel, color='black', fill=predom_jel),
            position=position_stack())+
  theme_minimal()


ggplot(jel_normalized_unnested_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         mutate(publication = toupper(publication)) %>%
         na.omit() %>% 
         group_by(year, publication, jel_list_pre) %>%
         mutate(weighted_jel_count = sum(jel_list_elem_weight)) %>%
         distinct(year, publication, jel_list_pre, .keep_all = TRUE) %>%
         select(publication, year, jel_list_pre, weighted_jel_count) %>%
         rename('jel_alpha' = 'jel_list_pre') %>%
         pivot_wider(names_from ='year',
                     values_from = 'weighted_jel_count',
                     values_fill = 0) %>%
         pivot_longer(!c('jel_alpha', 'publication'),
                      names_to = 'year',
                      values_to = 'weighted_jel_count') %>%
         mutate(year = as.numeric(year)))+
  geom_area(aes(x=year, y=weighted_jel_count, group=jel_alpha, fill=jel_alpha, color=jel_alpha),
            position = position_fill())+
  scale_color_discrete(guide='none')+
  scale_fill_manual(
    values = unname(alphabet(n=length(unique(jel_normalized_unnested_df$jel_list_pre %>% na.omit())))),
    breaks = jel_alpha_descriptions$code_col,
    labels = jel_alpha_descriptions$desc_col,
    name= 'JEL category:')+
  labs(title='Distribution of topics (by JEL code) over time, by publication',
       subtitle='Frequency weighted by number of JEL codes per article')+
  ylab('Share')+
  theme_minimal()+
  facet_wrap('publication')
ggsave(paste0(adhoc_path, 'jel_weighted_normalized_by_journal.png'), plot=last_plot(), height=8.5, width=11, units='in')


ggplot(jel_normalized_unnested_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         na.omit() %>% 
         group_by(year, jel_list_pre) %>%
         mutate(weighted_jel_count = sum(jel_list_elem_weight)) %>%
         distinct(year, jel_list_pre, .keep_all = TRUE) %>%
         select(year, jel_list_pre, weighted_jel_count) %>%
         rename('jel_alpha' = 'jel_list_pre') %>%
         pivot_wider(names_from = 'year',
                     values_from = 'weighted_jel_count',
                     values_fill = 0) %>%
         pivot_longer(!c('jel_alpha'),
                      names_to = 'year',
                      values_to = 'weighted_jel_count') %>%
         mutate(year = as.numeric(year)))+
  geom_area(aes(x=year, y=weighted_jel_count, group=jel_alpha, color='black', fill=jel_alpha),
           position=position_fill())+
  scale_color_discrete(guide='none')+
  scale_fill_manual(
    values = unname(alphabet(n=length(unique(jel_normalized_unnested_df$jel_list_pre %>% na.omit())))),
    breaks = jel_alpha_descriptions$code_col,
    labels = jel_alpha_descriptions$desc_col,
    name= 'JEL category:')+
  labs(title='Distribution of topics (by JEL code) over time',
       subtitle='Frequency weighted by number of JEL codes per article')+
  ylab('Share')+
  theme_minimal()
ggsave(paste0(adhoc_path, 'jel_weighted_normalized.png'), plot=last_plot(), height=8.5, width=11, units='in')






ggplot(jel_normalized_unnested_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         na.omit() %>% 
         group_by(year, jel_list_pre) %>%
         mutate(weighted_jel_count = sum(jel_list_elem_weight)) %>%
         distinct(year, jel_list_pre, .keep_all = TRUE) %>%
         select(year, jel_list_pre, weighted_jel_count) %>%
         rename('jel_alpha' = 'jel_list_pre') %>%
         pivot_wider(names_from = 'year',
                     values_from = 'weighted_jel_count',
                     values_fill = 0) %>%
         pivot_longer(!c('jel_alpha'),
                      names_to = 'year',
                      values_to = 'weighted_jel_count') %>%
         mutate(year = as.numeric(year)) %>%
         group_by(year) %>%
         mutate(weighted_jel_totals = sum(weighted_jel_count),
                weighted_jel_share = weighted_jel_count / weighted_jel_totals) %>%
         ungroup() %>%
         view())+
  geom_line(aes(x=year, y=weighted_jel_share, group=jel_alpha, color=jel_alpha),
            size=1.25)+
  scale_color_manual(
    values = unname(alphabet(n=length(unique(jel_normalized_unnested_df$jel_list_pre %>% na.omit())))),
    breaks = jel_alpha_descriptions$code_col,
    labels = jel_alpha_descriptions$desc_col,
    name= 'JEL category:')+
  labs(title='Distribution of topics (by JEL code) over time',
       subtitle='Frequency weighted by number of JEL codes per article')+
  ylab('Share')+
  theme_minimal()
ggsave(paste0(adhoc_path, 'jel_weighted_normalized_line.png'), plot=last_plot(), height=8.5, width=11, units='in')


ranking_table_df <- jel_normalized_unnested_df %>%
  filter(year %notin% c(1990, 2021)) %>%
  mutate(publication = toupper(publication)) %>%
  na.omit() %>%
  group_by(year, jel_list_pre, publication) %>%
  mutate(weighted_jel_count = sum(jel_list_elem_weight)) %>%
  distinct(year, jel_list_pre, .keep_all = TRUE) %>%
  select(year, publication, jel_list_pre, weighted_jel_count) %>%
  group_by(year, publication) %>%
  arrange(desc(weighted_jel_count), .by_group = TRUE) %>%
  mutate(year_pub_jel_rank = row_number()) %>% 
  filter(year_pub_jel_rank <=5| jel_list_pre == 'L') %>%
  left_join(jel_alpha_descriptions, by = c("jel_list_pre" = "code_col")) %>%
  select(year, publication, year_pub_jel_rank, jel_list_pre, desc_col) %>%
  ungroup() %>%
  mutate(label = paste(jel_list_pre, desc_col, sep=': '),
         label = paste(year_pub_jel_rank, label, sep='. ')) %>% 
  filter(year %in% c(1991, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
  select(-c(jel_list_pre, desc_col)) %>% 
  mutate(year_pub_jel_rank = ifelse(year_pub_jel_rank >5,
                                    'Other',
                                    year_pub_jel_rank)) %>%
  pivot_wider(names_from = c('publication', 'year'), values_from = 'label') %>%
  select(-c(year_pub_jel_rank)) %>%
  view()


for (pub in c('AER', 'ECA', 'JPE', 'QJE', 'RES')) {
  stargazer(ranking_table_df %>%
              select(starts_with(pub)), 
            rownames = FALSE,
            summary = FALSE,
            out=paste0(adhoc_path, 'tex_tables/jel_ranking_table_', pub, '.tex'))
    
}










ggplot(jel_normalized_df %>%
         filter(year %notin% c(1990, 2021)) %>%
         na.omit() %>%
         group_by(year, publication) %>%
         select(title, year, publication, first_jel) %>%
         group_by(year) %>%
         count(first_jel) %>%
         rename('first_jel_count' = 'n') %>%
         distinct(year, first_jel, .keep_all = TRUE) %>%
         view())+
  geom_area(aes(x=year, y=first_jel_count, group=first_jel, fill=first_jel))+
  theme_minimal()




table_df <- read_csv('00_Scraping/labels_for_table.csv') %>%
  rename('Publication' = '...1')

stargazer(table_df,
          rownames=FALSE,
          font.size='footnotesize',
          summary=FALSE)
