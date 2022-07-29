library(tidyverse)
library(stargazer)
library(scales)

print(getwd())
setwd("~")
print(getwd())
setwd("ioCapture/adhoc_material/Reports/07-27")

consultants_authorship_read <- read_csv("tables/input/consultants_authorship.csv")

consultants_authorship_df <- consultants_authorship_read %>%
    group_by(sc_abstract_api_endpoint) %>%
    mutate(
        any_author_consultant = ifelse(sum(consults_at_all, na.rm = TRUE) > 0, 1, 0),
        any_author_nber = ifelse(
            any(!is.na(consults_at_all)) && (sum(consults_at_all, na.rm = TRUE) == 0),
             1,
              0)
    ) %>%
    ungroup() %>%
    distinct(sc_abstract_api_endpoint, .keep_all = TRUE) %>%
    view()


consultants_authorship_agg_df <- consultants_authorship_df %>%
    group_by(sc_issn) %>%
    mutate(
        articles_in_journal = n(),
        articles_by_consultants = sum(any_author_consultant),
        articles_by_nber = sum(any_author_nber),
        articles_share_consultants = articles_by_consultants / articles_in_journal,
        articles_share_nber = articles_by_nber / articles_in_journal
    ) %>%
    distinct(sc_issn, sc_pub_name, articles_in_journal, articles_by_consultants, articles_by_nber, articles_share_consultants, articles_share_nber) %>%
    distinct(sc_issn, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(
        articles_share_consultants = percent(articles_share_consultants, accuracy = 0.01),
        articles_share_nber = percent(articles_share_nber, accuracy = 0.01)
    ) %>%
    mutate(
        pub_type = case_when(
            sc_pub_name %in% c(
                "American Economic Review",
                "Econometrica",
                "Journal of Political Economy",
                "Quarterly Journal of Economics",
                "Review of Economic Studies"
            ) ~ "Top 5",
            sc_pub_name %in% c(
                "Journal of Financial Economics",
                "Journal of Finance",
                "Review of Financial Studies") ~ "Finance",
            sc_pub_name %in% c("Journal of Labor Economics") ~ "Labor",
            TRUE ~ "IO & Law and Econ"
        ),
        pub_type = as.factor(pub_type),
        pub_type = fct_relevel(
            pub_type,
            "Top 5",
            "IO & Law and Econ",
            "Finance",
            "Labor")
    ) %>%
    group_by(pub_type) %>%
    arrange(pub_type, sc_pub_name, .by_group = TRUE) %>%
    ungroup() %>%
    arrange(pub_type) %>%
    view()


stargazer(consultants_authorship_agg_df %>%
    select(sc_pub_name, articles_in_journal, articles_by_consultants, articles_share_consultants, articles_by_nber, articles_share_nber) %>%
    rename(
        "Journal" = sc_pub_name,
        "Count" = articles_in_journal,
        "Consultants (count)" = articles_by_consultants,
        "Consultants (share)" = articles_share_consultants,
        "NBER (count)" = articles_by_nber,
        "NBER (share)" = articles_share_nber
    ),
type = "latex",
title = "Consultants' Share of Publications",
style = "AER",
out = "tables/input/consultants_nber_journals_share_in.tex",
summary = FALSE,
rownames = FALSE,
out.header = FALSE
)


pub_codes_df <- data.frame(
    pub_code = c("AER", "ECA", "JPE", "QJE", "RES", "JFE", "JOF", "RFS", "ATB", "ALJ", "JEM", "JHR", "JLO", "JLE", "RJE", "JOL"),
    sc_pub_name = c("American Economic Review", "Econometrica", "Journal of Political Economy", "Quarterly Journal of Economics", "Review of Economic Studies", "Journal of Financial Economics", "Journal of Finance", "Review of Financial Studies", "Antitrust Bulletin", "Antitrust Law Journal", "Journal of Economics and Management Strategy", "Journal of Human Resources", "Journal of Law, Economics, and Organization", "Journal of Law and Economics", "RAND Journal of Economics", "Journal of Labor Economics")
)


consultants_authorship_vis_df <- consultants_authorship_agg_df %>%
    left_join(pub_codes_df, by = "sc_pub_name") %>%
    select(
        pub_code,
        articles_in_journal,
        articles_by_consultants,
        articles_by_nber
    ) %>%
    mutate(unaffiliated = articles_in_journal - articles_by_consultants - articles_by_nber) %>%
    pivot_longer(!pub_code,
        names_to = "indicator",
        values_to = "value"
    ) %>%
    filter(indicator != "articles_in_journal") %>%
    ungroup() %>%
    mutate(
        indicator = as.factor(indicator),
        indicator = fct_relevel(indicator,
            "articles_by_consultants",
            "articles_by_nber",
            "unaffiliated"),
        pub_code = as.factor(pub_code),
        pub_code = fct_relevel(pub_code,
            "AER",
            "ECA",
            "JPE",
            "QJE",
            "RES",
            "ATB",
            "ALJ",
            "JEM",
            "JHR",
            "JLE",
            "JLO",
            "RJE",
            "JFE",
            "JOF",
            "RFS",
            "JOL")
    ) %>%
    view()


ggplot(consultants_authorship_vis_df) +
    geom_col(aes(x = pub_code, y = value, fill = indicator),
        color = "white",
        position = "stack"
    ) +
    scale_fill_discrete(
        name = "Articles by:",
        labels = c("Consultants", "NBER-affiliated", "Rest of World"),
    ) +
    scale_y_continuous(labels = comma) +
    ggtitle("Consultants' Share of Publications") +
    xlab("") +
    ylab("Articles") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", color = NA),
        legend.position = c(0.85, 0.8)
    )
ggsave("figures/consultants_nber_journal_share.png", plot = last_plot(), width = 297, height = 210, units = "mm")
