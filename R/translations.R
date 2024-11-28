get_trsl <- function(fig, label, lang) {
  
  fig_translations |>
    filter(
      Figure == fig,
      en == label
    ) |>
    pull({{ lang }})
  
}

fig_translations <- read_csv(here::here("data", "figure_translations.csv"))

fig_labels <-
  fig_translations |>
  select(!Figure) |>
  distinct() |>
  group_by(en) |>
  group_split() |>
  map(\(x) list(en = x$en, de = x$de, fr = x$fr))

names(fig_labels) <- map_chr(fig_labels, \(x) x$en)

#==============================================================================#
# Figures meta ####
#==============================================================================#

n_work_ex <-
  switch(
    params$lang,
    en = fig_labels$`Number of work examples`$en,
    de = fig_labels$`Number of work examples`$de,
    fr = fig_labels$`Number of work examples`$fr
  )

n_cv <-
  switch(
    params$lang,
    en = fig_labels$`Number of CVs`$en,
    de = fig_labels$`Number of CVs`$de,
    fr = fig_labels$`Number of CVs`$fr
  )

#==============================================================================#
# Work types ####
#==============================================================================#

fig_4_labels <-
  list(
    "Publication" =
      switch(
        params$lang,
        en = fig_labels$Publication$en,
        de = fig_labels$Publication$de,
        fr = fig_labels$Publication$fr
      ),
    "Conference" =
      switch(
        params$lang,
        en = fig_labels$Conference$en,
        de = fig_labels$Conference$de,
        fr = fig_labels$Conference$fr
      ),
    "Intellectual Property" =
      switch(
        params$lang,
        en = fig_labels$`Intellectual Property`$en,
        de = fig_labels$`Intellectual Property`$de,
        fr = fig_labels$`Intellectual Property`$fr
      ),
    "Other" =
      switch(
        params$lang,
        en = fig_labels$Other$en,
        de = fig_labels$Other$de,
        fr = fig_labels$Other$fr
      )
  )

fig_5_labels <-
  list(
    "Journal" =
      switch(
        params$lang,
        en = fig_labels$Journal$en,
        de = fig_labels$Journal$de,
        fr = fig_labels$Journal$fr
      ),
    "Book" =
      switch(
        params$lang,
        en = fig_labels$Book$en,
        de = fig_labels$Book$de,
        fr = fig_labels$Book$fr
      ),
    "Other type of Article" =
      switch(
        params$lang,
        en = fig_labels$`Other type of Article`$en,
        de = fig_labels$`Other type of Article`$de,
        fr = fig_labels$`Other type of Article`$fr
      ),
    "Preprint" =
      switch(
        params$lang,
        en = fig_labels$Preprint$en,
        de = fig_labels$Preprint$de,
        fr = fig_labels$Preprint$fr
      ),
    "Online" =
      switch(
        params$lang,
        en = fig_labels$Online$en,
        de = fig_labels$Online$de,
        fr = fig_labels$Online$fr
      ),
    "Other" =
      switch(
        params$lang,
        en = fig_labels$Other$en,
        de = fig_labels$Other$de,
        fr = fig_labels$Other$fr
      ),
    "Dissertation" =
      switch(
        params$lang,
        en = fig_labels$Dissertation$en,
        de = fig_labels$Dissertation$de,
        fr = fig_labels$Dissertation$fr
      ),
    "Training and Supervision" =
      switch(
        params$lang,
        en = fig_labels$`Training and Supervision`$en,
        de = fig_labels$`Training and Supervision`$de,
        fr = fig_labels$`Training and Supervision`$fr
      ),
    "Textual Tool" =
      switch(
        params$lang,
        en = fig_labels$`Textual Tool`$en,
        de = fig_labels$`Textual Tool`$de,
        fr = fig_labels$`Textual Tool`$fr
      )
  )

fig_6_labels <-
  list(
    "Conference Paper" =
      switch(
        params$lang,
        en = fig_labels$`Conference Paper`$en,
        de = fig_labels$`Conference Paper`$de,
        fr = fig_labels$`Conference Paper`$fr
      ),
    "Conference Abstract" =
      switch(
        params$lang,
        en = fig_labels$`Conference Abstract`$en,
        de = fig_labels$`Conference Abstract`$de,
        fr = fig_labels$`Conference Abstract`$fr
      ),
    "Conference Poster" =
      switch(
        params$lang,
        en = fig_labels$`Conference Poster`$en,
        de = fig_labels$`Conference Poster`$de,
        fr = fig_labels$`Conference Poster`$fr
      )
  )

fig_7_labels <-
  list(
    "Patent" =
      switch(
        params$lang,
        en = fig_labels$Patent$en,
        de = fig_labels$Patent$de,
        fr = fig_labels$Patent$fr
      ),
    "Disclosure" =
      switch(
        params$lang,
        en = fig_labels$Disclosure$en,
        de = fig_labels$Disclosure$de,
        fr = fig_labels$Disclosure$fr
      ),
    "Registered Copyright" =
      switch(
        params$lang,
        en = fig_labels$`Registered Copyright`$en,
        de = fig_labels$`Registered Copyright`$de,
        fr = fig_labels$`Registered Copyright`$fr
      ),
    "License" =
      switch(
        params$lang,
        en = fig_labels$License$en,
        de = fig_labels$License$de,
        fr = fig_labels$License$fr
      ),
    "Trademark" =
      switch(
        params$lang,
        en = fig_labels$Trademark$en,
        de = fig_labels$Trademark$de,
        fr = fig_labels$Trademark$fr
      )
  )

fig_8_labels <-
  list(
    "All types" =
      switch(
        params$lang,
        en = fig_labels$`All types`$en,
        de = fig_labels$`All types`$de,
        fr = fig_labels$`All types`$fr
      ),
    "Technical Standard" =
      switch(
        params$lang,
        en = fig_labels$`Technical Standard`$en,
        de = fig_labels$`Technical Standard`$de,
        fr = fig_labels$`Technical Standard`$fr
      ),
    "Annotation" =
      switch(
        params$lang,
        en = fig_labels$Annotation$en,
        de = fig_labels$Annotation$de,
        fr = fig_labels$Annotation$fr
      ),
    "Artistic Performance" =
      switch(
        params$lang,
        en = fig_labels$`Artistic Performance`$en,
        de = fig_labels$`Artistic Performance`$de,
        fr = fig_labels$`Artistic Performance`$fr
      ),
    "Data Management Plan" =
      switch(
        params$lang,
        en = fig_labels$`Data Management Plan`$en,
        de = fig_labels$`Data Management Plan`$de,
        fr = fig_labels$`Data Management Plan`$fr
      ),
    "Data Set" =
      switch(
        params$lang,
        en = fig_labels$`Data Set`$en,
        de = fig_labels$`Data Set`$de,
        fr = fig_labels$`Data Set`$fr
      ),
    "Invention" =
      switch(
        params$lang,
        en = fig_labels$Invention$en,
        de = fig_labels$Invention$de,
        fr = fig_labels$Invention$fr
      ),
    "Lecture/Speech" =
      switch(
        params$lang,
        en = fig_labels$`Lecture/Speech`$en,
        de = fig_labels$`Lecture/Speech`$de,
        fr = fig_labels$`Lecture/Speech`$fr
      ),
    "Physical Object" =
      switch(
        params$lang,
        en = fig_labels$`Physical Object`$en,
        de = fig_labels$`Physical Object`$de,
        fr = fig_labels$`Physical Object`$fr
      ),
    "Research Technique" =
      switch(
        params$lang,
        en = fig_labels$`Research Technique`$en,
        de = fig_labels$`Research Technique`$de,
        fr = fig_labels$`Research Technique`$fr
      ),
    "Software" =
      switch(
        params$lang,
        en = fig_labels$Software$en,
        de = fig_labels$Software$de,
        fr = fig_labels$Software$fr
      ),
    "Spin-off Company" =
      switch(
        params$lang,
        en = fig_labels$`Spin-off Company`$en,
        de = fig_labels$`Spin-off Company`$de,
        fr = fig_labels$`Spin-off Company`$fr
      ),
    "Standards and Policy" =
      switch(
        params$lang,
        en = fig_labels$`Standards and Policy`$en,
        de = fig_labels$`Standards and Policy`$de,
        fr = fig_labels$`Standards and Policy`$fr
      ),
    "Other" =
      switch(
        params$lang,
        en = fig_labels$Other$en,
        de = fig_labels$Other$de,
        fr = fig_labels$Other$fr
      )
  )

#==============================================================================#
# Research area ####
#==============================================================================#

ssh <-
  switch(
    params$lang,
    en = "SSH",
    de = "GSW",
    fr = "SHS"
  )

mint <-
  switch(
    params$lang,
    en = "MINT",
    de = "MINT",
    fr = "MINT"
  )

ls <-
  switch(
    params$lang,
    en = "LS",
    de = "LW",
    fr = "SV"
  )

id <-
  switch(
    params$lang,
    en = "ID",
    de = "ID",
    fr = "ID"
  )

area_labels <- list(SSH = ssh, MINT = mint, LS = ls, ID = id)
