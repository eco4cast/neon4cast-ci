# Interactive plots for the dashboard, and a helper that renders one tab per
# variable. Sourced by R/setup.R.

girafe_options <- list(
  ggiraph::opts_hover_inv(css = "opacity:0.20;"),
  ggiraph::opts_hover(css = "stroke-width:2;"),
  ggiraph::opts_zoom(max = 4)
)

# Forecast mean and 95% interval per model, faceted by site, with
# observations as points when there are any. NULL if there is nothing to plot.
forecast_plots <- function(df, ncol = NULL) {
  if (nrow(df) == 0) return(NULL)
  p <- ggplot(df) +
    ggiraph::geom_ribbon_interactive(
      aes(x = datetime, ymin = quantile02.5, ymax = quantile97.5, fill = model_id,
          data_id = model_id, tooltip = model_id),
      alpha = 0.2, show.legend = FALSE
    ) +
    ggiraph::geom_line_interactive(
      aes(datetime, mean, col = model_id, tooltip = model_id, data_id = model_id),
      show.legend = FALSE
    ) +
    labs(x = "datetime", y = "predicted") +
    facet_wrap(~site_id, scales = "free", ncol = ncol) +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()
  if ("observation" %in% names(df) && any(!is.na(df$observation))) {
    p <- p + geom_point(aes(datetime, observation), data = dplyr::filter(df, !is.na(observation)))
  }
  ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 4, options = girafe_options)
}

# Mean CRPS by model, by reference date and by horizon, stacked. `scores`
# holds mean CRPS and row counts (n) per model, reference date and horizon;
# the means are re-averaged weighted by n. NULL if there is nothing to plot.
leaderboard_plots <- function(scores) {
  scores <- dplyr::filter(scores, is.finite(crps))
  if (nrow(scores) == 0) return(NULL)
  average <- function(by) {
    dplyr::summarise(scores, crps = stats::weighted.mean(crps, n), .by = dplyr::all_of(by))
  }
  by_model <- average("model_id") |>
    dplyr::mutate(model_id = forcats::fct_reorder(model_id, crps, .desc = TRUE))
  model_order <- levels(by_model$model_id)
  ordered <- function(df) dplyr::mutate(df, model_id = factor(model_id, model_order))

  board1 <- ggplot(by_model, aes(model_id, crps, fill = model_id)) +
    ggiraph::geom_col_interactive(aes(tooltip = model_id, data_id = model_id),
                                  show.legend = FALSE) +
    coord_flip() +
    labs(x = NULL, y = "crps") +
    theme_bw() +
    theme(axis.text.y = element_blank())
  board2 <- ggplot(ordered(average(c("model_id", "reference_datetime"))),
                   aes(reference_datetime, crps, col = model_id)) +
    ggiraph::geom_line_interactive(aes(tooltip = model_id, data_id = model_id),
                                   show.legend = FALSE) +
    scale_y_log10() +
    labs(x = "reference datetime") +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()
  board3 <- ggplot(ordered(average(c("model_id", "horizon"))),
                   aes(horizon, crps, col = model_id)) +
    ggiraph::geom_line_interactive(aes(tooltip = model_id, data_id = model_id),
                                   show.legend = FALSE) +
    labs(x = "horizon (days)") +
    theme_bw()
  ggiraph::girafe(ggobj = patchwork::wrap_plots(board1, board2, board3, ncol = 1),
                  width_svg = 8, height_svg = 6, options = girafe_options)
}

# Render a panel-tabset with one tab per element of `tabs`. Each tab is a
# list with `title`, optional markdown `text`, and `widget` (a plot, a table,
# or NULL for "no data"). Call from a chunk with `results = "asis"`.
render_tabs <- function(tabs, empty = "No data are available yet.") {
  child <- c("```{r}", "if (is.null(widget)) cat(empty) else widget", "```")
  body <- purrr::map_chr(tabs, function(tab) {
    env <- new.env(parent = globalenv())
    env$widget <- tab$widget
    env$empty <- empty
    paste(c(paste("###", tab$title), "", tab$text, "",
            knitr::knit_child(text = child, envir = env, quiet = TRUE)),
          collapse = "\n")
  })
  cat("::: panel-tabset\n\n", paste(body, collapse = "\n\n"), "\n\n:::\n", sep = "")
}
