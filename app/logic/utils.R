box::use(
  dplyr,
  fs[path],
  magrittr[`%>%`],
  purrr[map_lgl],
  stringr[str_split],
  tibble[is_tibble],
)

#' @export
MAX_AGG_LEVELS <- 3 # nolint: object_name_linter

#' @export
AGG_LEVELS <- c("content_guid", "user_guid", "start_date") # nolint: object_name_linter

#' @export
AGG_TIME_LEVELS <- c("Daily" = "day", "Weekly" = "week", "Monthly" = "month") # nolint: object_name_linter

#' Maps the day of the week to an integer between 1 to 7
#' where 1 indicates a Monday and a 7 indicates a Sunday
#' @param dow day(or days) of the week as a text (i.e. "Monday")
#' @return integer from 1 to 7 where 1 indicates a monday
#' @export
map_wday <- function(dow) {
  dows <- c(
    "monday", "tuesday", "wednesday", "thursday", "friday",
    "saturday", "sunday"
  )
  return(match(tolower(dow), dows))
}

#' Retrieves the goal from a dataframe assuming the following structure
#' columns:
#'  freq <chr>:
#'      Contains the frequency for the goal.
#'      Can be one of day,week,month
#'  per <chr>:
#'      The aggregation levels for the goal as a comma separated list.
#'      Can be a combination of content_guid,user_guid,start_date
#'  goal<chr>:
#'      The value of the goal.
#'      Must be a numerical value even if the column data type is <chr>
#' @param goal_df a dataframe with the above specification
#' @param agg_levels vector of aggregation levels to filter the 'per' column.
#' Should contain a combination of content_guid,user_guid,start_date
#' @param date_agg character value to filter the 'freq' column.
#' Can be one of day,week,month
#' @param returns a single numerical value if a goal matches the arguments else NULL is returned
get_goal_spec <- function(goal_df, agg_levels, date_agg) {
  spec <- goal_df |>
    dplyr$filter(
      date_agg == freq,
      str_split(per, ",") |> map_lgl(~ identical(sort(agg_levels), sort(trimws(.x))))
    ) |>
    dplyr$pull(goal) |>
    as.integer()
  if (length(spec) == 0) {
    return(NULL)
  } else {
    return(spec)
  }
}

#' Filter dataframe by username
#' @param df A dataframe.
#' @param users A string array.
#' @return A filtered dataframe.
#' @export
filter_data_by_user <- function(df, users) {
  df %>% dplyr$filter(!(username %in% users))
}

#' Get average value of column grouped by another column in dataframe
#' @param agg_usage Aggregated usage data frame
#' @param group_by Column name to group data
#' @param column Column name to calculate average
#' @return Average value of column grouped by another column in dataframe
#' @export
get_grouped_average <- function(df, group_by, column) {
  grouped_df <- df %>%
    dplyr$group_by(.data[[group_by]]) %>%
    dplyr$summarise(
      avg_column = sum(.data[[column]])
    )

  mean(grouped_df$avg_column)
}

#' Process goal configuration for charts
#' @param goal Goal configuration (can be tibble or numeric)
#' @param agg_levels Vector of aggregation levels
#' @param date_aggregation Date aggregation setting
#' @return Processed goal value (integer or NULL)
#' @export
process_chart_goal <- function(goal, agg_levels, date_aggregation) {
  if (is_tibble(goal)) {
    goal <- get_goal_spec(
      goal,
      agg_levels,
      date_aggregation
    )
  } else if (!is.null(goal)) {
    goal <- as.integer(unlist(goal))
  }

  goal
}

#' Create an image path based on the given path.
#'
#' @param image_path The path to be used for creating the image path.
#' @return The created image path.
#'
#' @examples
#' create_image_path("photo.jpg")
#' # Output: "../static/images/photo.jpg"
#'
#' @export
create_image_path <- function(image_path) {
  base_path <- "static/images"

  if (nchar(base_path) < 1) {
    file_path <- path(image_path)
  } else {
    file_path <- path(
      base_path,
      image_path
    )
  }

  return(file_path)
}

#' Get titles for applications and replace empty titles with names
#' @param titles Vector of each application title
#' @param names  Vector of each application name
#' @return Vector of non-empty titles
#' @export
get_app_titles <- function(titles, names) {
  has_title <- !is.na(titles) & titles != ""
  ifelse(has_title, titles, names)
}
