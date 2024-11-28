if (interactive()) {
  
  source("R/read_and_prepare_data.R")
  
  if (!require("snf.datastory")) {
    if (!require("devtools")) {
      install.packages("devtools")
      library(devtools)
    }
    install_github("snsf-data/snf.datastory")
    library(snf.datastory)
  }
  params <- NULL
  params$lang <- "fr"
  source("R/translations.R")
  
  library(tidyverse)
  library(waffle)
  library(cowplot)
  library(here)
  
}

make_figure_1 <- function() {
  
  knitr::include_graphics(
    here::here(
      switch(
        params$lang,
        en = "data/img/fig1_en.png",
        de = "data/img/fig1_de.png",
        fr = "data/img/fig1_fr.png"
      )
    )
  )
  
}

make_figure_2 <- function(height = NULL) {

  fig <- dist_cited_works |>
    mutate(n_works = as.factor(n_works)) |>
    ggplot(aes(x = n_works, y = n)) +
    geom_col(
      width = .9,
      position = position_stack(),
      fill = get_datastory_scheme()[1]
    ) +
    get_datastory_theme() +
    theme(
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9)
    ) +
    labs(
      x = n_work_ex,
      y = n_cv
    )
  
  return(fig)
  
}

make_figure_3 <- function() {
  
  knitr::include_graphics(
    here::here(
      switch(
        params$lang,
        en = "data/img/fig3_en.png",
        de = "data/img/fig3_de.png",
        fr = "data/img/fig3_fr.png"
      )
    )
  )
  
}

make_figure_4 <- function() {
  
  fig <-
    dist_work_categories_by_area |>
    mutate(
      WorkCategory =
        fct_relabel(WorkCategory, \(x) as.character(fig_4_labels[x])) |>
        fct_relevel(rev(fig_4_labels)),
      ResearchArea =
        fct_relabel(ResearchArea, \(x) as.character(area_labels[x])) |>
        fct_relevel(ssh, mint, ls, id)
    ) |>
    ggplot() +
    aes(
      x = ResearchArea,
      y = freq_category,
      fill = WorkCategory
    ) +
    geom_col(width = 1.6, position = position_dodge(width = 2.2)) +
    geom_text(
      aes(
        label =
          paste0("N = ", snf.datastory::print_num(n_category, params$lang)),
        hjust = if_else(freq_category > 0.5, 1, 0),
        y = if_else(
          freq_category > 0.5,
          freq_category - 0.0075,
          freq_category + 0.0075
        ),
        color = if_else(freq_category > 0.5, "white", "#4C4C4C")
      ),
      size = 3,
      position = position_dodge(width = 2.2),
    ) +
    coord_flip() +
    scale_fill_manual(
      values = rev(get_datastory_scheme(n_col = 5)[-4])
    ) +
    scale_color_identity() +
    scale_y_continuous(
      labels = scales::percent, expand = expansion(mult = c(0.0, 0.1))
    ) +
    get_datastory_theme() +
    facet_wrap(
      ~fct_relevel(ResearchArea, ssh, mint, ls, id),
      scales = "free_y",  ncol = 2
    ) +
    theme(
      strip.text =
        element_text(
          margin = margin(5, 5, 5),
          size = 10
        ),
      axis.text.x = element_text(size = 9),
      panel.spacing.x = unit(1.5, "lines"),
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(fig)
  
}

make_figure_5 <- function() {
  
  fig <-
    publications_clusters  |>
    arrange(desc(freq_publications)) |>
    mutate(
      order = row_number(),
      PublicationSubcategory =
        fct_relabel(
          PublicationSubcategory,
          \(x) as.character(fig_5_labels[x])
        ) |>
        fct_reorder(order, .desc = TRUE),
      ResearchArea =
        fct_relabel(ResearchArea, \(x) as.character(area_labels[x])) |>
        fct_relevel(ssh, mint, ls, id)
    ) |>
    ggplot() +
    aes(x = ResearchArea, y = freq_publications, fill = reorder(PublicationSubcategory, -order)) +
    geom_col(width = 1.8, position = position_dodge(width = 2.2)) +
    geom_text(
      aes(
        label =
          paste0("N = ", snf.datastory::print_num(n_publications, params$lang)),
        hjust = if_else(freq_publications > 0.5 & ResearchArea != ssh, 1, 0),
        y = if_else(
          freq_publications > 0.5 & ResearchArea != ssh,
          freq_publications - 0.01,
          freq_publications + 0.01
        ),
        color =
          if_else(
            freq_publications > 0.5 & ResearchArea != ssh,
            "white",
            "#4C4C4C"
          )
      ),
      size = 2.75,
      position = position_dodge(width = 2.2)
    ) +
    coord_flip() +
    scale_color_identity() +
    scale_fill_manual(values = rev(get_datastory_scheme(n_col = 8))) +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0.0, 0.1))
    ) +
    get_datastory_theme() +
    facet_wrap(
      ~fct_relevel(ResearchArea, ssh, mint, ls, id),
      scales = "free_y",
      ncol = 2,
    ) +
    theme(
      strip.text =
        element_text(
          margin = margin(5, 5, 5),
          size = 10
        ),
      axis.text.x = element_text(size = 9),
      panel.spacing.x = unit(1.5, "lines"),
      axis.text.y = element_blank(),
      legend.byrow = TRUE
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(fig)
  
}

make_figure_6 <- function() {
  
  fig <-
    conference_by_research_area |>
    arrange(desc(freq_conference)) |>
    mutate(
      order = row_number(),
      .by = ResearchArea
    ) |>
    mutate(
      ResearchArea =
        fct_relabel(ResearchArea, \(x) as.character(area_labels[x])) |>
        fct_relevel(ssh, mint, ls, id),
      WorkTypeName =
        fct_relabel(WorkTypeName, \(x) as.character(fig_6_labels[x])) |>
        fct_reorder(order, .desc = TRUE)
    ) |>
    ggplot() +
    aes(
      x = ResearchArea,
      y = freq_conference,
      fill = WorkTypeName
    ) +
    geom_col(width = 2, position = position_dodge(width = 2.2)) +
    geom_text(
      aes(
        label = paste0("N = ", n_conference),
        y = freq_conference + 0.005
      ),
      hjust = 0,
      size = 3,
      position = position_dodge(width = 2.2),
      color = "#4C4C4C"
    ) +
    coord_flip() +
    scale_fill_manual(values = rev(get_datastory_scheme(n_col = 3))) +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0.0, 0.1))
    ) +
    get_datastory_theme() +
    facet_wrap(
      ~ResearchArea,
      scales = "free_y",
      ncol = 1
    ) +
    theme(
      strip.text =
        element_text(
          margin = margin(5, 5, 5),
          size = 10
        ),
      axis.text.x = element_text(size = 9),
      panel.spacing.y = unit(0.5, "lines"),
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(fig)
  
}

make_figure_7 <- function() {
  
  fig <-
    intellectual_property_by_research_area |>
    ungroup() |>
    arrange(desc(freq_property)) |>
    mutate(
      order = row_number(),
      ResearchArea =
        fct_relabel(ResearchArea, \(x) as.character(area_labels[x])) |>
        fct_relevel(ssh, mint, ls, id),
      WorkTypeName =
        fct_relabel(WorkTypeName, \(x) as.character(fig_7_labels[x])) |>
        fct_reorder(order, .desc = TRUE)
    ) |>
    ggplot() +
    aes(
      x = ResearchArea,
      fill = WorkTypeName,
      values = n_property
    ) +
    geom_waffle(color = "white", size = .75, n_rows = 17, flip = TRUE) +
    scale_fill_manual(values = rev(get_datastory_scheme(n_col = 6)[-4])) +
    coord_equal() +
    get_datastory_theme(remove_plot_margin = TRUE) +
    facet_wrap(~ResearchArea) +
    theme(
      strip.text =
        element_text(
          margin = margin(5, 5, 5),
          size = 10
        ),
      plot.margin = margin(l = -95, r = 95),
      axis.text.x = element_blank(),
      panel.spacing.y = unit(0.5, "lines"),
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(fig)
  
}

make_figure_8 <- function() {
  
  dat <-
    works |>
    drop_na(WorkTypeName)  |>
    filter(WorkCategory == "Other") |>
    count(WorkTypeName, ResearchArea) |>
    add_row(WorkTypeName = "All types", n = 0) |>
    mutate(
      WorkTypeName =
        fct_relabel(WorkTypeName, \(x) as.character(fig_8_labels[x])) |>
        fct_reorder(n, sum, .desc = TRUE) |>
        fct_relevel(fig_8_labels$`All types`)
    )
  
  f1 <-
    dat |>
    mutate(
      WorkTypeName = fct_recode(WorkTypeName, " " = fig_8_labels$`All types`)
    ) |>
    ggplot(aes(x = n, y = WorkTypeName)) +
    geom_col(fill = get_datastory_scheme()[6]) +
    scale_x_continuous(expand = expansion(add = c(0, 0.05))) +
    get_datastory_theme(text_axis = "x") +
    facet_wrap(~WorkTypeName, ncol = 1, scales = "free_y") +
    theme(
      strip.text =
        element_text(
          face = "plain",
          size = 10,
          hjust = 0,
          color = "#4F4F4F",
          margin = margin(1, 1, 1)
        ),
      panel.spacing.y = unit(0.25, "lines")
    )
  
  f2_area_level <-
    dat |>
    drop_na() |>
    summarise(
      n = sum(n),
      .by = ResearchArea
    ) |>
    mutate(pct = n / sum(n)) |>
    select(!n) |>
    mutate(
      WorkTypeName = fct(fig_8_labels$`All types`, levels = levels(dat$WorkTypeName))
    )
  
  f2 <-
    dat |>
    drop_na() |>
    complete(ResearchArea, WorkTypeName, fill = list(n = 0)) |>
    filter(WorkTypeName != fig_8_labels$`All types`) |>
    mutate(
      pct = n / sum(n),
      .by = WorkTypeName
    ) |>
    bind_rows(f2_area_level) |>
    mutate(
      ResearchArea =
        fct_relabel(ResearchArea, \(x) as.character(area_labels[x])) |>
        fct_relevel(ssh, mint, ls, id),
    ) |>
    ggplot() +
    aes(x = pct, y = WorkTypeName, fill = ResearchArea) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_x_continuous(
      expand = expansion(add = c(0, 0.05)),
      labels = scales::percent
    ) +
    scale_fill_manual(values = get_datastory_scheme(n_col = 5)[-4]) +
    get_datastory_theme(text_axis = "x") +
    facet_wrap(~WorkTypeName, ncol = 1, scales = "free_y") +
    theme(
      strip.text =
        element_text(
          face = "plain",
          size = 10,
          hjust = 0,
          color = "#4F4F4F",
          margin = margin(1, 1, 1)
        ),
      panel.spacing.y = unit(0.25, "lines")
    )
  
  fig <- plot_grid(f1, f2, ncol = 2, align = "h")
  
  return(fig)
  
}
