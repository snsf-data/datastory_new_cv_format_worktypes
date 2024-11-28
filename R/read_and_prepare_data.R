if (interactive()) {
  library(tidyverse)
}

# load data
gender_distribution <- read_csv("data/gender_distribution.csv")
works <- read_csv("data/works.csv")

n_application <- works |> distinct(ApplicationId) |> nrow()
n_applicant <- works |> distinct(PersonId) |> nrow()
n_cvs <- works |> distinct(CvId) |> nrow()

n_cvs_domain <-
  works |>
  distinct(CvId, ResearchArea) |>
  count(ResearchArea) |>
  mutate(pct = n / sum(n))

multiple_CVs <- works |>
  filter(!is.na(PersonId)) |>
  distinct(PersonId, CvId) |>
  count(PersonId) |>
  count("No. of CVs per researcher" = n)

n_multiple_CVs <- multiple_CVs |>
  filter(`No. of CVs per researcher` > 1) |>
  summarize(total = sum(n)) |>
  pull(total)

CV_reuse <- works |>
  distinct(ApplicationId, CvId) |>
  count(CvId) |>
  count("No. of times CV was used" = n)

n_cv_reuse <- CV_reuse |>
  filter(`No. of times CV was used` > 1) |>
  summarize(total = sum(n)) |>
  pull(total)

# count number of achievements and cited works per CV-application combination
# note that a few CVs will be counted more than once since they have been used
# for multiple applications

count_achievments_works <- works |>
  group_by(ApplicationId, CvId, PersonId, ResearchArea) |>
  summarise(
    n_achievements = n_distinct(CvAchievementId, na.rm = TRUE),
    n_works = n_distinct(CvWorkId, na.rm = TRUE),
  ) |>
  ungroup()

dist_number_achievements <- count_achievments_works |>
  group_by(ResearchArea) |>
  count(ResearchArea, n_achievements) |>
  mutate(freq_achievements = n / sum(n))

dist_number_achievements_overall <- count_achievments_works |>
  count(n_achievements) |>
  mutate(freq_achievements = n / sum(n))

dist_cited_works <-
  count_achievments_works |>
  count(ResearchArea, n_works) |>
  mutate(
    freq_works = n / sum(n),
    .by = ResearchArea
  )

dist_cited_works_overall <- count_achievments_works |>
  count(n_works) |>
  mutate(freq_works = n / sum(n))

distinct_work_types <- works |>
  drop_na(CvWorkId) |>
  select(ApplicationId, CvId, WorkTypeName, WorkCategory) |>
  mutate(
    n_distinct_work_types = n_distinct(WorkTypeName),
    n_distinct_work_categories = n_distinct(WorkCategory),
    .by = c(CvId, ApplicationId)
  )

dist_distinct_work_types <- distinct_work_types |>
  distinct(CvId, ApplicationId, n_distinct_work_types) |>
  count(n_distinct_work_types) |>
  mutate(freq_works = round(n / sum(n) * 100, 2))

dist_distinct_work_categories <- distinct_work_types |>
  distinct(CvId, ApplicationId, n_distinct_work_categories) |>
  count(n_distinct_work_categories) |>
  mutate(freq_categories = round(n / sum(n) * 100, 2))

singular_worktypes <- distinct_work_types |>
  filter(n_distinct_work_types == 1) |>
  count(WorkTypeName) |>
  arrange(-n)

dist_work_categories_by_area <-
  works |>
  drop_na(CvWorkId) |>
  count(WorkCategory, ResearchArea, name = "n_category") |>
  mutate(
    freq_category = n_category / sum(n_category),
    .by = ResearchArea
  )

work_categories_per_cv <-
  distinct_work_types |>
  summarize(
    WorkCategories = list(unique(WorkCategory)),
    .by = CvId
  ) |>
  rowwise() |>
  mutate(
    FlagPublication = any(unlist(WorkCategories) == "Publication"),
    FlagConference = any(unlist(WorkCategories) == "Conference"),
    FlagIP = any(unlist(WorkCategories) == "Intellectual Property"),
    FlagOther = any(unlist(WorkCategories) == "Other")
  ) |>
  ungroup()

n_cvs_categories <-
  work_categories_per_cv |>
  count(FlagConference, FlagIP, FlagOther) |>
  filter(if_any(FlagIP | FlagConference | FlagOther)) |>
  mutate(total = sum(n)) |>
  slice_head() |>
  pull(total)

publications_clusters <-
  works |>
  filter(WorkCategory == "Publication") |>
  count(PublicationSubcategory, ResearchArea, name = "n_publications") |>
  mutate(
    freq_publications = n_publications / sum(n_publications),
    .by = ResearchArea
  )

conference_by_research_area <-
  works |>
  filter(WorkCategory == "Conference") |>
  drop_na(WorkTypeName) |>
  count(ResearchArea, WorkTypeName, name = "n_conference") |>
  mutate(
    freq_conference = n_conference / sum(n_conference),
    .by = ResearchArea
  )

intellectual_property_by_research_area <-
  works |>
  filter(WorkCategory == "Intellectual Property") |>
  drop_na(WorkTypeName) |>
  count(ResearchArea, WorkTypeName, name = "n_property") |>
  mutate(freq_property = n_property / sum(n_property))
