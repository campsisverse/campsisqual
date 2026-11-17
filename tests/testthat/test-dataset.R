test_that("set_default_obs_cmt sets the compartment of all observations", {
  dataset <- campsis::Dataset() %>%
    add(campsis::Arm(subjects = 2) %>% add(campsis::Observations(times = c(1, 2)))) %>%
    add(campsis::Arm(subjects = 3) %>% add(campsis::Observations(times = c(3, 4)))) %>%
    add(campsis::Bolus(time = 0, amount = 100))

  updated <- dataset %>% set_default_obs_cmt(2)
  compartments <- updated@arms@list %>%
    purrr::map(~ .x@protocol@observations@list %>% purrr::map_chr(~ .x@compartment)) %>%
    unlist()
  expect_equal(compartments, c("2", "2"))

  # Index given as a double is accepted
  updated <- dataset %>% set_default_obs_cmt(3)
  expect_equal(updated@arms@list[[1]]@protocol@observations@list[[1]]@compartment, "3")
})
