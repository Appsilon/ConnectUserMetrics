box::use(
  testthat[expect_equal, expect_null, expect_true, test_that],
)

box::use(
  app/logic/utils,
)

impl <- attr(utils, "namespace")

test_that("map_wday gives monday as one", {
  expect_equal(utils$map_wday(c("Monday")), c(1))
})

test_that("map_wday is case insensitive", {
  expect_equal(utils$map_wday(c("Monday", "monday", "mOnDaY")), c(1, 1, 1))
})

test_that("map_wday sets sunday as 7", {
  expect_equal(utils$map_wday(c("Sunday")), c(7))
})

test_that("get_goal_spec works for basic example", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("start_date"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(test_goal_df, agg_levels = c("start_date"), date_agg = c("day"))
  )
})

test_that("get_goal_spec gives NULL for non existent goals", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("start_date"),
    goal = c("28")
  )
  expect_null(impl$get_goal_spec(test_goal_df, agg_levels = c("start_date"), date_agg = c("week")))
  expect_null(impl$get_goal_spec(test_goal_df, agg_levels = c("content_guid"), date_agg = c("day")))

  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("start_date,content_guid"),
    goal = c("28")
  )
  expect_null(impl$get_goal_spec(test_goal_df, agg_levels = c("start_date"), date_agg = c("day")))
  expect_null(impl$get_goal_spec(test_goal_df, agg_levels = c("content_guid"), date_agg = c("day")))
})

test_that("get_goal_spec captures comma separated lists", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("start_date,content_guid"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df, agg_levels = c("start_date", "content_guid"), date_agg = c("day")
    )
  )
})

test_that("get_goal_spec avoids whitespace in user input", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c(" start_date , content_guid "),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df, agg_levels = c("start_date", "content_guid"), date_agg = c("day")
    )
  )
})

test_that("get_goal_spec is robust to ordering of agg_level in input_df", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("content_guid,start_date"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df, agg_levels = c("start_date", "content_guid"), date_agg = c("day")
    )
  )

  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("content_guid,start_date,user_guid"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df,
      agg_levels = c("start_date", "content_guid", "user_guid"),
      date_agg = c("day")
    )
  )
})

test_that("get_goal_spec is robust to ordering of agg_level in func args", {
  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("start_date,content_guid"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df, agg_levels = c("content_guid", "start_date"), date_agg = c("day")
    )
  )

  test_goal_df <- data.frame(
    freq = c("day"),
    per = c("content_guid,start_date,user_guid"),
    goal = c("28")
  )
  expect_equal(
    28,
    impl$get_goal_spec(
      test_goal_df,
      agg_levels = c("start_date", "user_guid", "content_guid"),
      date_agg = c("day")
    )
  )
})

test_that("process_chart_goal handles numeric input", {
  expect_equal(
    42,
    utils$process_chart_goal(42, agg_levels = c("start_date"), date_aggregation = "day")
  )
})

test_that("process_chart_goal handles NULL input", {
  expect_null(
    utils$process_chart_goal(NULL, agg_levels = c("start_date"), date_aggregation = "day")
  )
})

test_that("get_grouped_average returns correct overall average for basic grouping", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4)
  )

  # For group "A": sum = 1 + 2 = 3; for group "B": sum = 3 + 4 = 7.
  # Overall average = (3 + 7) / 2 = 5.
  result <- utils$get_grouped_average(df, "group", "value")
  expect_equal(result, 5)
})

test_that("get_grouped_average returns the group sum as average when only one group is present", {
  df <- data.frame(
    group = c("A", "A", "A"),
    value = c(2, 3, 5)
  )

  # Group "A": sum = 2 + 3 + 5 = 10; overall average should be 10.
  result <- utils$get_grouped_average(df, "group", "value")
  expect_equal(result, 10)
})

test_that("get_grouped_average returns NA when missing values cause group sums to be NA", {
  df <- data.frame(
    group = c("A", "A", "B"),
    value = c(1, NA, 3)
  )
  result <- utils$get_grouped_average(df, "group", "value")
  expect_true(is.na(result))
})

test_that("create_image_path returns image path when image filename is given as an input", {
  image_filename <- "image.jpeg"

  expect_equal(
    as.character(utils$create_image_path(image_filename)),
    "static/images/image.jpeg"
  )
})

test_that("create_image_path returns image path when image is located in sub-folder", {
  image_filename <- "test/image.jpeg"

  expect_equal(
    as.character(utils$create_image_path(image_filename)),
    "static/images/test/image.jpeg"
  )
})

test_that("get_app_titles returns app titles if they exist", {
  apps <- data.frame(
    guid = c("test_guid_1", "test_guid_2", "test_guid_3"),
    name = c("test_name_1", "test_name_2", "test_name_3"),
    title = c("test_title_1", "test_title_2", "test_title_3")
  )

  expect_equal(
    c("test_title_1", "test_title_2", "test_title_3"),
    utils$get_app_titles(apps$title, apps$name)
  )
})

test_that("get_app_titles returns app names instead of NA titles", {
  apps <- data.frame(
    guid = c("test_guid_1", "test_guid_2", "test_guid_3"),
    name = c("test_name_1", "test_name_2", "test_name_3"),
    title = c("test_title_1", NA, "test_title_3")
  )

  expect_equal(
    c("test_title_1", "test_name_2", "test_title_3"),
    utils$get_app_titles(apps$title, apps$name)
  )
})

test_that("get_app_titles returns app names instead of empty titles", {
  apps <- data.frame(
    guid = c("test_guid_1", "test_guid_2", "test_guid_3"),
    name = c("test_name_1", "test_name_2", "test_name_3"),
    title = c("", "test_title_2", "")
  )

  expect_equal(
    c("test_name_1", "test_title_2", "test_name_3"),
    utils$get_app_titles(apps$title, apps$name)
  )
})
