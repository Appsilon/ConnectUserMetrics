box::use(
  testthat[skip_if_not],
  withr[with_locale],
)

skip_if_locale_unavailable <- function(locale) {
  available_locales <- system("locale -a", intern = TRUE)
  skip_if_not(locale %in% available_locales,
              message = paste(locale, "is not available on this system"))
}

locale_test_name <- function(locale_for_writing, locale_for_reading) {
  sprintf(
    ".yml written in %s encoding works under %s locale",
    locale_for_writing,
    locale_for_reading
  )
}

write_yaml_with_locale <- function(locale, yaml_lines, file_path) {
  with_locale(c(LC_CTYPE = locale), {
    writeLines(
      yaml_lines,
      file_path
    )
  })
}
