### Table 14-1.02 Summary of End of Study Data

# library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(assertthat)

# library(pharmaRTF)
library(tibble)

source('~/phuse-GT-Package-for-Publication-Quality-Tables-from-R/Table 14 1 02/config.R')
source('~/phuse-GT-Package-for-Publication-Quality-Tables-from-R/Table 14 1 02/funcs.R')

#Read in Source and order factors
adsl <- read_xpt('~/phuse-GT-Package-for-Publication-Quality-Tables-from-R/Table 14 1 02/data/adam/adsl.xpt')
adsl$COMP24FL <- ordered(adsl$COMP24FL, c("Y", "N", NA))
adsl$ARM <- ordered(adsl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adsl$DCREASCD <- ordered(adsl$DCSREAS, c("Adverse Event",
                                          "Death",
                                          "Lack of Efficacy",
                                          "Lost to Follow-up",
                                          "Withdrew Consent",
                                          "Physician Decision",
                                          "I/E Not Met",
                                          "Protocol Violation",
                                          "Sponsor Decision"))


#### Completion Status Table
comp_stat <- adsl %>%
  group_by(COMP24FL, ARM) %>%
  summarise(n = n())

#Make data.frame for table, unnamed so the cols are named correctly
comp_df <- data.frame(
  "Placebo" = n_pct(unlist(comp_stat[c(1,4), "n"]), sum(unlist(comp_stat[c(1,4), "n"])), mark_lt=FALSE),
  "Xanomeline Low Dose" = n_pct(unlist(comp_stat[c(2,5), "n"]), sum(unlist(comp_stat[c(2,5), "n"])), mark_lt=FALSE),
  "Xanomeline High Dose" = n_pct(unlist(comp_stat[c(3,6), "n"]), sum(unlist(comp_stat[c(3,6), "n"])), mark_lt=FALSE),
  "Total" = c(n_pct(sum(comp_stat[1:3, "n"]), sum(comp_stat[,"n"]), mark_lt=FALSE),
              n_pct(sum(comp_stat[4:6, "n"]), sum(comp_stat[,"n"]), mark_lt=FALSE)),
  row.names = c("\tCompleted Week 24", "\tEarly Termination (prior to Week 24)"),
  #Stop data.frame from adding periods
  check.names = FALSE, stringsAsFactors = FALSE
)
# Add tabs to row.names

# Add missing row.
comp_df["\tMissing", ] <- "  0 (  0%)"

# p-value
comp_p <- fish_p(adsl, adsl$COMP24FL, adsl$ARM)
comp_df <- attach_p(comp_df, comp_p)


# sean version
# first, total by arm
arm_group <- adsl %>% 
  group_by(ARM) %>% 
  count()

# create table for early termination
term_df <- adsl %>% 
  filter(COMP24FL == "N") %>%
  group_by(DCREASCD, ARM) %>%
  mutate(DCREASCD = forcats::fct_expand(DCREASCD, "Missing")) %>% 
  count() %>% 
  ungroup() %>% 
  complete(DCREASCD, ARM, fill = list(n = 0)) %>% 
  rename(in_group = n) %>% 
  group_by(DCREASCD) %>% 
  add_tally(wt = in_group, name = "TOTAL") %>% 
  left_join(arm_group) %>% 
  mutate(pres_val = n_pct(in_group, n, mark_lt = FALSE)) %>%
  mutate(Total = n_pct(TOTAL, sum(n), mark_lt = FALSE)) %>% 
  select(DCREASCD, ARM, pres_val, Total) %>% 
  spread(ARM, pres_val) %>% 
  column_to_rownames("DCREASCD") %>% 
  relocate(Total, .after = last_col())

#### Reason for Early Termination Table
## By ARM
# term_reas <- adsl %>%
#   filter(COMP24FL == "N") %>%
#   group_by(DCREASCD, ARM) %>%
#   complete(nesting(DCREASCD, ARM)) %>%
#   #complete(DCREASCD, ARM) %>%
#   summarise(n = n())
# 
# ## Total
# term_reas_tot <- adsl %>%
#   filter(COMP24FL == "N", !is.na(DCDECOD)) %>%
#   group_by(DCREASCD) %>%
#   complete(nesting(DCREASCD, ARM)) %>%
#   summarise(n = n())
# 
# 
# term_df <- data.frame(
#   "Placebo" = n_pct(unlist(term_reas[seq(1, 27, 3), "n"]), sum(adsl %>% filter(ARM == "Placebo") %>% summarise(n = n()), na.rm=TRUE), mark_lt=FALSE),
#   "Xanomeline Low Dose" = n_pct(unlist(term_reas[seq(2, 27, 3), "n"]), sum(adsl %>% filter(ARM == "Xanomeline Low Dose") %>% summarise(n = n()), na.rm=TRUE), mark_lt=FALSE),
#   "Xanomeline High Dose" = n_pct(unlist(term_reas[seq(3, 27, 3), "n"]), sum(adsl %>% filter(ARM == "Xanomeline High Dose") %>% summarise(n = n()), na.rm=TRUE), mark_lt=FALSE),
#   "Total" = n_pct(unlist(term_reas_tot[, "n"]), sum(adsl %>% summarise(n = n())), mark_lt=FALSE),
#   row.names = c(
#     "\tAdverse Event",
#     "\tDeath",
#     "\tLack of Efficacy[2]",
#     "\tLost to Follow-up",
#     "\tSubject decided to withdraw",
#     "\tPhysician decided to withdraw subject",
#     "\tProtocol criteria not met",
#     "\tProtocol violation",
#     "\tSponsor decision"
#   ),
#   #Stop data.frame from adding periods
#   check.names = FALSE, stringsAsFactors = FALSE
# )
# term_df["\tMissing", ] <- "  0 (  0%)"



# p-value
term_p_1 <- adsl %>%
  select(ARM, DCREASCD) %>%
  mutate(loefl = ifelse(DCREASCD %in% "Adverse Event", 1, 0)) %>%
  fish_p(loefl, ARM, width = 6)
term_df <- attach_p(term_df, term_p_1)

term_p_2 <- adsl %>%
  select(ARM, DCREASCD) %>%
  mutate(loefl = ifelse(DCREASCD %in% "Lack of Efficacy", 1, 0)) %>%
  fish_p(ARM ,loefl, width = 6)
term_df$p[rownames(term_df) == "Lack of Efficacy"]  <- term_p_2



## Add Table labels
comp_df <- add_column(comp_df, " " = row.names(comp_df), .before = 1)
comp_df <- add_row(comp_df, " " = "Completion Status:", .before = 1)
comp_df <- add_row(comp_df, " " = "", .before = 1)

term_df <- add_column(term_df, " " = row.names(term_df), .before = 1)
term_df <- add_row(term_df, " " = "Reason for Early Termination (prior to Week 24):", .before = 1)
term_df <- add_row(term_df, " " = "", .before = 1)

combinedTable <- rbind(comp_df, term_df)
# Rename to get rid of period seperation
names(combinedTable)

headers <- adsl %>%
  group_by(ARM) %>%
  summarise(N = n())
headers_2 <- adsl %>%
  summarise(N = n()) %>%
  mutate(ARM = "Total")
headers_3 <- rbind(headers, headers_2) %>%
  mutate(labels = str_replace_all(str_wrap(glue('{ARM} (N={N})'), width=10), "\n", function(x) "\\line "))
headers_4 <- c(" ", headers_3$labels, "p-value [1]")
names(combinedTable) <- headers_4


#Stop here for GT
library(gt)

gt_tbl <- gt(combinedTable) %>%
  fmt_missing(
    columns = 1:6,
    missing_text = ""
  )

# Show the gt Table
gt_tbl





colnames(combinedTable) <- colnames(combinedTable) %>% gsub("\\line ", "", ., fixed = TRUE)


four_line_header <- function(data, lines) {
  
  tab_header(
    data,
    title = html(
      htmltools::tagList(
        htmltools::tags$div(
          style = htmltools::css(padding.top = px(0), padding.bottom = px(0)),
          htmltools::tags$p(
            style = htmltools::css(
              text.align = "left", font.family = "Courier", font.size = px(14)),
            htmltools::tags$strong(lines[1]),
            htmltools::br(),
            htmltools::tags$strong(lines[2])
          ),
          htmltools::tags$p(
            style = htmltools::css(
              text.align = "center", font.family = "Courier", font.size = px(14)),
            htmltools::tags$strong(lines[3]),
            htmltools::br(),
            htmltools::tags$strong(lines[4])
          )
        )
      ) %>% as.character()
    )
  )
}



combinedTable %>%
  gt() %>%
  cols_label(
    `Placebo(N=86)` = html("Placebo<br>(N=86)"),
    `XanomelineLow Dose(N=84)` = html("XanomelineLow Dose<br>(N=84)"),
    `XanomelineHigh Dose(N=84)` = html("XanomelineHigh Dose<br>(N=84)"),
    `Total(N=254)` = html("Total<br>(N=254)")
  ) %>%
  four_line_header(
    lines = c("Protocol: CSIDSCPILOT01", "Population: Intent-to-Treat",
              "Table 14-1.02", "Summary of End of Study Data")
  ) %>%
  tab_style(
    locations = list(cells_body(), cells_column_labels(columns = TRUE)), 
    style = cell_text(font = "Courier", size = px(12))
  ) %>%
  tab_style(
    locations = cells_column_labels(columns = TRUE), 
    style = cell_text(weight = "bold")
  ) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  opt_table_lines(extent = "none") %>%
  tab_options(
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table.width = px(1024), data_row.padding = px(2)
  ) %>%
  tab_source_note(
    source_note = html(
      htmltools::tagList(
        htmltools::tags$p(
          style = htmltools::css(font.family = "Courier", font.size = px(14)),
          "[1] Fisher's exact test.",
          htmltools::tags$br(),
          "[2] Based on either patient/caregiver perception or physician perception.",
        ),
        #htmltools::tags$br(),
        htmltools::tags$div(
          style = htmltools::css(font.family = "Courier", font.size = px(14)),
          htmltools::tags$span(
            style = htmltools::css(
              display = "block", float = "left", width = px(500)),
            "Source: C:\\csdic_pilot\\PROGRAMS\\DRAFT\\TFLs\\ads12.sas"
          ),
          htmltools::tags$span(
            style = htmltools::css(display = "block", float = "right", width = px(250)),
            "21:02 Monday, June 26, 2006"
          )
        )
      ) %>% as.character()
    ) 
  ) %>%
  tab_style(
    style = cell_text(indent = px(15)),
    locations = cells_body(columns = 1, rows = c(3:5, 8:17))
  )





ht <- combinedTable %>%
  huxtable::as_hux(add_colnames=TRUE)

huxtable::bottom_border(ht)[1, ] <- 1
huxtable::bold(ht)[1, ] <- TRUE
huxtable::align(ht)[1, ] <- 'center'
huxtable::align(ht)[, 6] <- "center"
huxtable::width(ht) <- 1.5
huxtable::escape_contents(ht) <- FALSE
huxtable::col_width(ht) <- c(.4, .12, .12, .12, .12, .12)
huxtable::bottom_padding(ht) <- 0
huxtable::top_padding(ht) <- 0
huxtable::valign(ht)[1,] <- "bottom"
ht[8,2] <- ""
ht <- huxtable::merge_cells(ht, 8, 1:2)


# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht) %>% titles_and_footnotes_from_df(
  from.file='./data/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-1.02') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top = 1)

# Write out the RTF
write_rtf(doc, file='./outputs/14-1.02.rtf')

