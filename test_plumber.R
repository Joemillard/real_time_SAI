library(plumber)
# 'plumber_test.R' is the location of the file shown above
pr("plumber_test.R") %>%
  pr_run(port=8000)
