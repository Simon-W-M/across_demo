library(NHSRdatasets)
library(tidyverse)

dat <- head(ae_attendances,100)

# please note I am going to be super sloppy with my
# sum/mean functions and not add in a na.rm = T

# add a column with the mean of the column
data_means <- dat |>
  mutate (mean_attendances = mean(attendances))

# thats great but if I want to do it across several columns
# I have to name each one

data_means <- dat |>
  mutate (mean_attendances = mean(attendances),
          mean_breaches = mean(breaches),
          mean_admissions = mean(admissions)
          )

# also with my sloppy typing going to make mistakes real fast
# and if I have lots to do, it could get boring real fast

# convert the attendances and breaches to mean of the column
data_across <- dat |>
  mutate(across(
    c(attendances, breaches),
    mean
  ))

# maybe want to be a little more dynamic and 
# do this across all numeric variables

data_across <- dat |>
  mutate(across(
    where(is.numeric),
    mean
  ))

# or if we have set up data up in a certain way we could use tidy select
data_across <- dat |>
  mutate(across(
    contains('es'),
    mean
  ))

# or we could do across all numerics but not breaches
# by putting into a vector
data_across <- dat |>
  mutate(across(
    c(where(is.numeric), -breaches),
    mean
  ))

# but maybe add a grouping variable
data_across <- dat |>
  mutate(
    across(
      where(is.numeric),
      mean
    ),
    .by = c(org_code, period)
  )

# but maybe add a grouping variable
# but also create a new columns
data_across <- dat |>
  mutate(
    across(where(is.numeric),
      mean,
      .names = "mean_{.col}"
    ),
    .by = c(org_code, period)
  )

# we can also get clever by using lamda functions
# note use of tilde and the '.'
# the '.' now denotes the column(s) that we are going to do operations across
data_across <- dat |>
  mutate(
    across(where(is.numeric),
      ~ sum(.),
      .names = "sum_{.col}"
    ),
    .by = c(org_code, period)
  )

# this then allows us to do even clever things by using the '.'
# more than once in a function
# such as 
data_across <- dat |>
  mutate(
    across(where(is.numeric),
      ~ (. / sum(.)) * 100,
      .names = "perc_{.col}"
    ),
    .by = c(org_code, period)
  )
# we can also do more than one thing at once
# if we want to show denominator, numerator and percentage
data_across <- dat |>
  mutate(
    across(
      where(is.numeric),
      list(
        ~ (. / sum(.)) * 100,
        ~ sum(.)
      )
    ),
    .by = c(org_code, period)
  )
# the 1 and the two in the naming is the number of the function
# ie 1 = first function, 2 = second function


# we have to get a little bit more funky if we want to
# give our new variables nice names
# (which of course we always should)
data_across <- dat |>
  mutate(
    across(
      where(is.numeric),
      list(
        "percentage" = ~ (. / sum(.)) * 100,
        "total" = ~ sum(.)
      )
    ),
    .by = c(org_code, period)
  )

# if you prefer naming conventions the other way around
data_across <- dat |>
  mutate(
    across(where(is.numeric),
      list(
        "percentage" = ~ (. / sum(.)) * 100,
        "total" = ~ sum(.)
      ),
      .names = "{.fn}_{.col}"
    ),
    .by = c(org_code, period)
  )

# we can also use some of these funky across like features in other commands

# introducing if_any & if_all designed for use in filtering

# in this case filter to where if breaches or admissions > 5
dat_filter <- dat |>
  filter(if_any(
    c(breaches, admissions),
    ~ . > 5
  ))

# or could filter to where both > 5
dat_filter <- dat |>
  filter(if_all(
    c(breaches, admissions),
    ~ . > 5
  ))

# if_any can also be used inside a mutate
# such as
data_if_all <- dat |>
  mutate(all_over_10 = if_else(
    if_all(
      where(is.numeric),
      ~ . > 10
    ),
    "All above 10",
    "Not all above 10"
  ))

# or we could do if any over 10
data_if_any <- dat |>
  mutate(any_over_1000 = if_else(
    if_any(
      where(is.numeric),
      ~ . > 1000
    ),
    "At least one above 1,000",
    "None above 1,000"
  ))

# across has some awesome features
# but 100% I would recommend doing this instead!
dat_tidy <- dat |>
  pivot_longer(cols = c(attendances, 
                        breaches, 
                        admissions))

# and a row wise demo

rowwise_demo <- dat |>
  rowwise() |>
  mutate(mean = mean(c_across(is.numeric))) |> 
  ungroup()

