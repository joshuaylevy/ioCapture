library(tidyverse)
library(stargazer)
library(scales)

print(getwd())
setwd("adhoc_material/Reports/07-27/figures")
path_prepend_00_scraping <- "../../../../00_Scraping/"
path_prepend_01_database <- "../../../../01_Database_Construction/"

### Generates figures that illustrate the fuzzy matching reports. Relies on a .csv in 01_Database_Construction_papers
fuzzy_reports_read <- read_csv(paste0(path_prepend_01_database, "papers/fuzzy_match_results.csv", sep = ""))
fuzzy_reports_df <- fuzzy_reports_read %>%
    mutate(total_eligible_obs = naive_match_obs + fuzzy_match_obs + remaining_scopus_obs + remaining_econlit_obs,
           remaining_scopus_share = percent(remaining_scopus_obs/total_eligible_obs, accuracy=0.1),
           remaining_scopus_share = as.character(remaining_scopus_share),
           remaining_econlit_share = percent(remaining_econlit_obs/total_eligible_obs, acuracy=0.1),
           remaining_econlit_share = as.character(remaining_econlit_share),
           remaining_shares_text = paste(remaining_econlit_share, remaining_scopus_share, sep='||')) %>%
    select(-c(remaining_scopus_share, remaining_econlit_share)) %>%
    pivot_longer(cols = -c(pub_code, remaining_shares_text),
                 names_to = "indicator", values_to = "value") %>%
    filter(indicator %in% c(
        "naive_match_obs",
        "fuzzy_match_obs",
        "remaining_econlit_obs",
        "remaining_scopus_obs"
    )) %>%
    mutate(
        pub_type = case_when(
            pub_code %in% c("AER", "ECA", "JPE", "QJE", "RES") ~ "Top 5",
            pub_code %in% c("ATB", "JEM", "JHR", "JLE", "JLO", "RJE") ~ "IO & Law",
            pub_code %in% c("JFE", "JOF", "RFS") ~ "Finance",
            pub_code %in% c("JOL") ~ "Labor"
        ),
        pub_code = as.factor(pub_code),
        pub_code = fct_relevel(pub_code, "AER", "ECA", "JPE", "QJE", "RES", "ATB", "JEM", "JHR", "JLE", "JLO", "RJE", "JFE", "JOF", "RFS", "JOL")
    )

ggplot(fuzzy_reports_df) +
    geom_col(aes(x = pub_code, y = value, fill = indicator, group = pub_code),
        position = "stack",
        color = "white",
        width = 0.6
    ) +
    geom_label_repel(
        data= fuzzy_reports_df %>%
            group_by(pub_code) %>%
            mutate(remaining_shares_text = ifelse(row_number()==1, remaining_shares_text, NA)),
        aes(x=pub_code, y= value, label=remaining_shares_text, group=pub_code),
                     nudge_y = 1500)+
    scale_fill_discrete(
        breaks = c(
            "naive_match_obs",
            "fuzzy_match_obs",
            "remaining_scopus_obs",
            "remaining_econlit_obs"
        ),
        labels = c(
            "Naively Matched",
            "Fuzzily Matched",
            "Unmatched Scopus",
            "Unmatched EconLit"
        ),
        name = "Type:"
    ) +
    scale_y_continuous(labels = comma) +
    ggtitle("Results of Scopus-EconLit Matching") +
    xlab("") +
    ylab("Observations (articles)") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", color = NA),
        legend.position = c(0.9, 0.8)
    )
ggsave(filename = "../figures/scopus_econlit_matching_results_labels.png", plot = last_plot(), width = 297, height = 210, units = "mm")

### Generates .tex table that summarizes the fuzzy matching reports. Relies on a .csv in 01_Database_Construction_papers
stargazer(fuzzy_reports_read %>%
    select(-c(scopus_obs, econlit_obs)) %>%
    mutate(
        pub_code = as.factor(pub_code),
        pub_code = fct_relevel(pub_code, "AER", "ECA", "JPE", "QJE", "RES", "ATB", "JEM", "JHR", "JLE", "JLO", "RJE", "JFE", "JOF", "RFS", "JOL")
    )  %>%
    arrange(pub_code) %>%
    mutate(pub_code = as.character(pub_code),
        total_matched = naive_match_obs + fuzzy_match_obs) %>%
    rename('Journal' = pub_code,
        'Naive matches' = naive_match_obs, 
        'Fuzzy matches' = fuzzy_match_obs, 
        'Scopus only' = remaining_scopus_obs, 
        'EconLit only' = remaining_econlit_obs,
        'Total' = total_matched),
    type = "latex",
    title = "Results of Scopus-EconLit Matching",
    style = "AER",
    out = "../tables/input/scopus_econlit_matching_results_in.tex",
    summary = FALSE,
    rownames = FALSE,
    out.header = FALSE)
