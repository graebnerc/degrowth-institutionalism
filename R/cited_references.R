here::i_am("R/cited_references.R")
library(here)
library(tidyr)
library(dplyr)
library(data.table)
library(countrycode)
# library(xtable)
library(readxl)

inst_journals <- c(
  "J ECON ISSUES",
  "J I ECON",
  "EVOL INST ECON REV",
  "AM J ECON SOCIOL",
  "J EVOL ECON",
  "East Econ J",
  "Forum Soc Econ",
  "Rev Soc Econ"
)

# Date setup---------------------------
journal_abbrevations <- fread(
  file = here("data/JournalAbbrevations.csv"))
cited_refs <- as_tibble(readRDS(
  file = here("data/cited_articles_full.Rds")))

info_sheet <- read_xlsx(path = here("data/Vars2Check.xlsx"), col_names = TRUE)
rel_cols <-  info_sheet %>%
  dplyr::filter(!is.na(DescriptiveAnalysis)) %>%
  dplyr::pull(Column)

rel_cols <- c(rel_cols, "Authors", "Author Full Names", "Article Title")

wos_result <- read_xls(here("data/literature_search.xls")) %>%
  dplyr::select(all_of(rel_cols))

rel_cols2 <- c(
  "Article Title", "Core",
  "SecStage Assessment of 1st stage no-core",
  "SecStageCore Classification  (Narrative)",
  "SecStageCore Classification  (Methodology)"
  )

evaluation <- read_xlsx(here("data/Qual-review.xlsx")) %>%
  dplyr::select(all_of(rel_cols2)) %>%
  dplyr::mutate(Core=ifelse(is.na(Core), "DirectDrop", Core)) %>%
  dplyr::filter(
    `Article Title` !="Bier hierhin gehen die Formeln",
    !is.na(`Article Title`))

wos_sample_full <- full_join(wos_result, evaluation, by=c("Article Title"))

relevant_sample <- wos_sample_full %>%
  dplyr::filter(
    Core == "Core" | `SecStage Assessment of 1st stage no-core` %in% c(
      "Not core - relevant", "Put into core")
  ) %>%
  dplyr::select(-all_of(c(
    "Core", "SecStage Assessment of 1st stage no-core",
    "SecStageCore Classification  (Narrative)",
    "SecStageCore Classification  (Methodology)"
  )))

core_sample <- wos_sample_full %>%
  dplyr::filter(
    Core == "Core" | `SecStage Assessment of 1st stage no-core` %in% c(
      "Put into core")
  ) %>%
  dplyr::rename(
    Narrative = `SecStageCore Classification  (Narrative)`,
    Methodology = `SecStageCore Classification  (Methodology)`
  ) %>%
  dplyr::select(-all_of(c("Core", "SecStage Assessment of 1st stage no-core")))

cited_refs_relevant <- cited_refs %>%
  dplyr::filter(citing_pub_title %in% relevant_sample$`Article Title`)

cited_refs_core <- cited_refs %>%
  dplyr::filter(citing_pub_title %in% core_sample$`Article Title`)

# Full sample--------------------------
cited_sources_full_count <- cited_refs["pub_id"] %>%
  rename(Journal=pub_id) %>%
  group_by(Journal) %>%
  count()

cited_sources_full_count_top50 <-  cited_sources_full_count %>%
  dplyr::filter(n>=50)

cited_inst_sources_full <- cited_sources_full_count %>%
  dplyr::filter( Journal %in% inst_journals)

cite_table_full <- rbind(
  cited_sources_full_count_top50, cited_inst_sources_full) %>%
  rename(`Citations`=n) %>%
  dplyr::mutate(
    Journal = countrycode(
      sourcevar = Journal,
      origin = "Abbrevation",
      destination = "Original",
      custom_dict = as.data.frame(journal_abbrevations))
  ) %>%
  arrange(-Citations)

writexl::write_xlsx(
  cite_table_full,
  path = here("output/journals_full_sample.xlsx"))

# Relevant sample----------------------
cited_sources_relevant_count <- cited_refs_relevant["pub_id"] %>%
  rename(Journal=pub_id) %>%
  group_by(Journal) %>%
  count()

cited_sources_relevant_count_top10 <-  cited_sources_relevant_count %>%
  arrange(-n) %>%
  head(10)

cited_inst_sources_relevant <- cited_sources_relevant_count %>%
  dplyr::filter( Journal %in% inst_journals)

cite_table_relevant <- rbind(
  cited_sources_relevant_count_top10, cited_inst_sources_relevant) %>%
  arrange(-n) %>%
  rename(`Citations`=n) %>%
  dplyr::mutate(
    Journal = countrycode(
      sourcevar = Journal,
      origin = "Abbrevation",
      destination = "Original",
      custom_dict = as.data.frame(journal_abbrevations))
  )

writexl::write_xlsx(
  cite_table_relevant,
  path = here("output/table-2b_relevant.xlsx"))

dplyr::filter(cited_refs_relevant, pub_id=="J ECON ISSUES")
dplyr::filter(cited_refs_core, pub_id=="J ECON ISSUES")


# Core sample--------------------------
cited_sources_core_count <- cited_refs_core["pub_id"] %>%
  rename(Journal=pub_id) %>%
  group_by(Journal) %>%
  count()

cited_sources_core_count_top10 <- cited_sources_core_count %>%
  arrange(-n) %>%
  head(10)

cited_inst_sources_core <- cited_sources_core_count %>%
  dplyr::filter( Journal %in% inst_journals)

cite_table_core <- rbind(
  cited_sources_core_count_top10, cited_inst_sources_core) %>%
  arrange(-n) %>%
  rename(`Citations`=n)%>%
  dplyr::mutate(
    Journal = countrycode(
      sourcevar = Journal,
      origin = "Abbrevation",
      destination = "Original",
      custom_dict = as.data.frame(journal_abbrevations))
  )

writexl::write_xlsx(
  cite_table_core,
  path = here("output/table-2b_core.xlsx"))
