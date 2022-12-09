# We look at the articles that were cited by those in our sample
# We need to identify the most important sources to check whether we miss
#  some important sources in our sample
# But also to check what the main sources of inspiration for our core articles
#  were
here::i_am("R/setup_data_cited_references.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
read_rows <- Inf # Reduce to read in only a subset of the data

col_names_1 <- fread(
  file = here("data/initial_sample.txt"),
  header = FALSE, nrows = 1)

init_sample_1 <- fread(
  file = here("data/initial_sample.txt"),
  nrows = read_rows, skip = 1, header = FALSE)

setnames(
  x = init_sample_1,
  old = names(init_sample_1),
  new = c(as.character(col_names_1[1,]), "V70")
  )

init_sample_1_sub <- init_sample_1[, .(AU, TI, SO, DI, UT, CR)]

# Extract references

rows_considered <- 1:nrow(init_sample_1_sub)
reference_list <- list()

for (i in rows_considered){
  print(paste0(i, "/", max(rows_considered)))
  row_ <- init_sample_1_sub[i, ]

  # Extract the cited references
  refs_i <- str_split(row_$CR, pattern = "; ", simplify = FALSE)
  refs_i <- str_squish(refs_i[[1]])

  if (refs_i != ""){# TODO check condition
    # Remove uncaptured titles (usually do not appear in the real ref list)
    refs_i <- refs_i[refs_i != "[No title captured]"]

    # Extract the surname of the first author
    first_authors_i <- tibble(
      "first_author"=tolower(str_remove(refs_i, ",.*$"))
    ) %>%
      dplyr::mutate(
        first_author=str_remove_all(first_author, "[[:punct:]]")
      )

    # Extract title/source
    pub_id_i <- tibble(
      "pub_id"=purrr::map_chr(
        .x = str_split(refs_i, ",", 3),
        .f = ~str_squish(str_remove(.x[3], ",.*$"))))

    # Extract the publication year
    pub_years_i <- as_tibble(str_extract_all(
      string = refs_i, pattern = "[0-9]", simplify = TRUE)[,1:4],
      .name_repair = "unique") %>%
      unite(col = "Year", sep = "", remove = TRUE)

    reference_frame <- cbind(first_authors_i, pub_years_i, pub_id_i) %>%
      dplyr::mutate(citing_pub_title=row_[["TI"]])

    implausibles <- reference_frame %>%
      dplyr::filter(
        is.na(pub_id) | nchar(Year)<4 | !str_detect(Year, "^[:digit:]+$") |
          str_detect(first_author, "\\d")
      )

    # Remove implausible entries:
    reference_frame_plausibles <- reference_frame %>%
      dplyr::filter(
        !is.na(pub_id), # There is a publication id
        nchar(Year)==4, # Column year has exactly four characters
        str_detect(Year, "^[:digit:]+$"), # Column year contains only digits
        !str_detect(first_author, "\\d") # Author does not contains digits
      )
    if ( (nrow(reference_frame_plausibles)<nrow(reference_frame)) &
         (length(refs_i[is.na(pub_id_i$pub_id)]) > 0) ){
      # Add at least those with missing publication id
      # Get vector of missings:
      special_cases <- refs_i[is.na(pub_id_i$pub_id)]
      # Extract the years:
      special_cases_year <- purrr::map_chr(
        .x = str_extract_all(special_cases, "[:digit:]"),
        .f = ~paste(.x, collapse = ""))
      # Remove the years:
      special_cases <- str_remove_all(special_cases, "[:digit:]")

      # Split the remainders by commas:
      remainders <- str_split(special_cases, pattern = ", ", simplify = FALSE)
      # Detect patterns where there are lower letters
      # -> this needs to be authors since sources only consist of upper letters
      subset_author_ids <- purrr::map(
        .x = remainders, .f = ~str_detect(.x, "[:lower:]"))
      # Detect the patterns that do not have lower letters and are at least of
      #  length 2: this need to be sources
      subset_source_ids <- purrr::map(
        .x = remainders,
        .f = function(.x) !str_detect(.x, "[:lower:]") & nchar(.x)>2)

      # Put those back into the respective vectors; if in doubt, write NA
      remaining_authors <- rep(NA, length(remainders))
      remaining_ids <- rep(NA, length(remainders))
      for (i in 1:length(remainders)){
        rem_author <- remainders[[i]][subset_author_ids[[i]]]
        rem_id <- remainders[[i]][subset_source_ids[[i]]]
        remaining_authors[i] <- ifelse(
          test = length(rem_author)==1,
          yes = rem_author, no = NA)
        remaining_ids[i] <- ifelse(
          test = length(rem_id)==1,
          yes = rem_id, no = NA)
      }
      # Construct tibble:
      fixed_entries <- tibble(
        "first_author"=tolower(remaining_authors),
        "Year"=special_cases_year,
        "pub_id"=remaining_ids
      )%>%
        dplyr::mutate(citing_pub_title=row_[["TI"]]) %>%
        dplyr::filter(
          !is.na(pub_id), !is.na(pub_id)
        )

      reference_frame <-rbind(reference_frame_plausibles, fixed_entries)
    }


    reference_list[[row_[["TI"]]]] <- reference_frame
  }
}

full_list <- rbindlist(reference_list)
saveRDS(object = full_list, file = here("data/cited_articles_full.Rds"))
