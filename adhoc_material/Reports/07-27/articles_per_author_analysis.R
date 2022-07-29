library(tidyverse)
library(stargazer)

print(getwd())
setwd('adhoc_material/Reports/07-27')

consultants_read <- read_csv('../../../01_Database_Construction/consultants/nber_consultants.csv')

consultants_df <- consultants_read %>%
    filter(consults_at_all==1) %>% 
    select(name, nber_program, scopus_id) %>%
    mutate(scopus_id = as.numeric(str_replace_all(scopus_id, '\"', ''))) %>%
    rename('sc_author_id' = scopus_id) %>%
    mutate(
        nber_symbol = case_when(
            nber_program == 'Law and Economics' ~ '\\dag',
            nber_program == 'Industrial Organization' ~ '*',
            nber_program == 'Labor Studies' ~ '\\ddag',
            nber_program == 'Corporate Finance' ~ '\\mathsection',
            TRUE ~ 'asdf'
    )) %>%
    group_by(sc_author_id) %>% 
    summarise(nber_symbol_string = paste(nber_symbol, collapse = '')) %>% 
    filter(!is.na(sc_author_id)) %>%
    view(0)



per_author_read <- read_csv('tables/input/articles_per_author.csv')

pub_codes_df <- data.frame(
    sc_pub_code = c('AER', 'ATB', 'ALJ', 'ECA', 'JEM', 'JOF', 'JFE', 'JHR', 'JOL', 'JLE', 'JLO', 'JPE', 'QJE', 'RJE', 'RES', 'RFS'),
    sc_pub_name = sort(unique(per_author_read$sc_pub_name))
)


### The top 10 most-published authors across our database
per_author_df <- per_author_read %>% 
    select(c(sc_author_id, a_per_author_total, sc_author_given_name, sc_author_last_name)) %>%
    distinct(sc_author_id, .keep_all = TRUE) %>%
    arrange(desc(a_per_author_total)) %>%
    ungroup() %>% 
    slice_max(a_per_author_total, n=10) %>%
    mutate(rank = dense_rank(desc(a_per_author_total))) %>%
    left_join(consultants_df, by='sc_author_id') %>%
    mutate(Name = paste(sc_author_given_name, sc_author_last_name, sep= " ")) %>%
    view() 

for (row in 1:nrow(per_author_df)){
    old_name = per_author_df[row, 'Name']

    new_name = iconv(old_name, from='utf-8', to='ASCII//TRANSLIT')
    print(new_name)
    per_author_df[row, 'Name'] = new_name
}
view(per_author_df)

stargazer(per_author_df %>%
    mutate(
        Name = paste(Name, nber_symbol_string, sep='')
    ) %>% 
    rename(
        'Articles' = a_per_author_total,
        'Rank' = rank) %>%
    select(c('Name', 'Articles', 'Rank')) %>%
    view(),
    type = "latex",
    title = "Most Published Authors (all journals)",
    style = "AER",
    out = "tables/input/articles_per_author.tex",
    summary = FALSE,
    rownames = FALSE,
    out.header = FALSE,
        notes="Symbols indicate being a consultant and affiiated with an NBER program: $^{\\dag}$: Law and Economics; $^{*}$: Industrial Organization; $^{\\dag}$: Labor Studies; $^{\\mathsection}$: Corporate Finance."
)




### The top 10- most-published authors per journal in our database
per_author_per_journal <- per_author_read %>%
    select(c(sc_author_id, sc_issn, a_per_author_per_journal, sc_author_given_name, sc_author_last_name, sc_pub_name)) %>%
    group_by(sc_issn) %>% 
    arrange(desc(a_per_author_per_journal)) %>%
    slice_max(a_per_author_per_journal, n=10) %>%
    left_join(consultants_df, by='sc_author_id') %>%
    mutate(
        Rank = dense_rank(desc(a_per_author_per_journal)),
        Name = paste(sc_author_given_name, sc_author_last_name, sep=' '),
        Name = as.character(Name))%>%
    rename('Articles' = a_per_author_per_journal) %>% 
    view()

for (row in 1:nrow(per_author_per_journal)){
    old_name = per_author_per_journal[row, 'Name']

    new_name = iconv(old_name, from='utf-8', to='ASCII//TRANSLIT')
    print(new_name)
    per_author_per_journal[row, 'Name'] = new_name
}
view(per_author_per_journal)


ranking_tables <- per_author_per_journal %>%
    ungroup() %>%
    mutate(
        Name = paste(Name, nber_symbol_string, sep=''),
        Name_arts = paste(Name,' ', '(', Articles, ')', sep='')) %>%
    select(Name_arts, sc_pub_name, Rank) %>% 
    left_join(pub_codes_df, by='sc_pub_name') %>%
    select(Name_arts, sc_pub_code, Rank) %>%
    pivot_wider(
        names_from = 'sc_pub_code',
        values_from = 'Name_arts'
    ) %>%
    view()


top5_ranking_table <- ranking_tables %>%
    select(c(Rank, AER, ECA, JPE, QJE, RES)) %>%
    group_by(Rank) %>%
    mutate_at(c('AER', 'ECA', 'JPE', 'QJE', 'RES'), toString) %>%
    view() %>%
    stargazer(
        type = "latex",
        title = "Most Published Authors (Top 5)",
        style = "AER",
        out = "tables/input/most_published_authors_top5.tex",
        font.size = "footnotesize",
        summary = FALSE,
        rownames = FALSE, 
        out.header = FALSE,
        notes="Symbols indicate being a consultant and affiiated with an NBER program: $^{\\dag}$: Law and Economics; $^{*}$: Industrial Organization; $^{\\dag}$: Labor Studies; $^{\\mathsection}$: Corporate Finance."
    )

io_law_ranking_table <- ranking_tables %>%
    select(c(Rank, ATB, ALJ, JEM, JHR, JLE, JLO, RJE)) %>%
    group_by(Rank) %>%
    mutate_at(c('ATB', 'ALJ', 'JEM', 'JHR', 'JLE', 'JLO', 'RJE'), toString) %>%
    view() %>%
    stargazer(
        type = "latex",
        title = "Most Published Authors (IO \\& Law and Economics)",
        style = "AER",
        out = "tables/input/most_published_authors_io_and_law.tex",
        font.size = "scriptsize",
        summary = FALSE,
        rownames = FALSE, 
        out.header = FALSE,
        notes="Symbols indicate being a consultant and affiiated with an NBER program: $^{\\dag}$: Law and Economics; $^{*}$: Industrial Organization; $^{\\dag}$: Labor Studies; $^{\\mathsection}$: Corporate Finance."
    )

finance_labor_ranking_table <- ranking_tables %>%
    select(c(Rank, JOF, JFE, RFS, JOL)) %>%
    group_by(Rank) %>%
    mutate_at(c('JOF', 'JFE', 'RFS', 'JOL'), toString) %>%
    view() %>%
    stargazer(
        type = "latex",
        title = "Most Published Authors (Finance and Labor)",
        style = "AER",
        out = "tables/input/most_published_authors_finance_labor.tex",
        font.size = "footnotesize",
        summary = FALSE,
        rownames = FALSE, 
        out.header = FALSE,
        notes="Symbols indicate being a consultant and affiiated with an NBER program: $^{\\dag}$: Law and Economics; $^{*}$: Industrial Organization; $^{\\dag}$: Labor Studies; $^{\\mathsection}$: Corporate Finance."
    )

