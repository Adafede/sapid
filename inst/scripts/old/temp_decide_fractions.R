## run visualize-profiles

fraction_score <- profiles_consistent_sum |>
  group_by(name) |>
  summarize(sum = sum(value))
