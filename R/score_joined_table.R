score_joined_table <- function (joined,
                             extra_groups = NULL,
                             include_summaries = TRUE)
{

  joined <- joined |>
    dplyr::mutate(family = case_match(family,
                                      "ensemble" ~ "sample",
                                      .default = family))

  grouping <- c(
    "model_id",
    "reference_datetime",
    "site_id",
    "datetime",
    "family",
    "variable",
    "pub_datetime",
    "project_id",
    "duration"
  )
  if(include_summaries){

    scores <- joined |>
      dplyr::group_by(dplyr::across(dplyr::any_of(grouping))) |>
      dplyr::summarise(
        observation = unique(observation), # grouping vars define a unique obs
        crps = score4cast:::generic_crps(family, parameter, prediction, observation),
        logs = score4cast:::generic_logs(family, parameter, prediction, observation),
        dist = score4cast:::infer_dist(family, parameter, prediction),
        .groups = "drop") |>
      dplyr::mutate(
        mean = as.numeric(mean(dist)),
        median = as.numeric(stats::median(dist)),
        sd = sqrt(as.numeric(distributional::variance(dist))),
        quantile97.5 = as.numeric(distributional::hilo(dist, 95)$upper),
        quantile02.5 = as.numeric(distributional::hilo(dist, 95)$lower),
        quantile90 = as.numeric(distributional::hilo(dist, 90)$upper),
        quantile10 = as.numeric(distributional::hilo(dist, 90)$lower)) |>
      dplyr::select(-dist)

  }else{
    scores <- joined |>
      dplyr::group_by(dplyr::across(dplyr::any_of(grouping))) |>
      dplyr::summarise(
        observation = unique(observation), # grouping vars define a unique obs
        crps = score4cast:::generic_crps(family, parameter, prediction, observation),
        logs = score4cast:::generic_logs(family, parameter, prediction, observation),
        .groups = "drop")
  }
  scores
}


