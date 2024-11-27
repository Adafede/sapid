message("... paths and parameters \n")
source(file = "paths.R")
source(file = "params.R")

library(plotly)
library(tidyverse)

test <- read_delim("inst/extdata/fractions.tsv",
  "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  dplyr::select(label, parent, value = `mass [mg]`) |>
  data.frame()

fig <- plot_ly(
  labels = test$label,
  parents = test$parent,
  values = test$value,
  type = "sunburst",
  # level = "extract_dried_30g"
)

fig

## run scripts in SAPERE

test_2_corr <-
  full_join(
    test_2,
    test[temp_head:nrow(test), ] |> ## see if use this or not
      dplyr::filter(
        as.numeric(gsub(
          pattern = "M_",
          replacement = "",
          x = label
        )) >= min_fraction &
          as.numeric(gsub(
            pattern = "M_",
            replacement = "",
            x = label
          ) <= max_fraction)
      ),
    by = c("parent" = "label")
  ) |>
  mutate(value = as.numeric(value.x) * as.numeric(value.y)) |>
  dplyr::select(-value.x, -value.y, -parent.y)

test_2_alternative_corr <-
  full_join(
    test_2_alternative,
    test[temp_head:nrow(test), ] |> ## see if use this or not
      dplyr::filter(
        as.numeric(gsub(
          pattern = "M_",
          replacement = "",
          x = label
        )) >= min_fraction &
          as.numeric(gsub(
            pattern = "M_",
            replacement = "",
            x = label
          ) <= max_fraction)
      ),
    by = c("parent" = "label")
  ) |>
  mutate(value = as.numeric(value.x) * as.numeric(value.y)) |>
  dplyr::select(-value.x, -value.y, -parent.y)

test_3 <- rbind(
  c(
    label = "V_03_10g",
    parent = NA,
    value = 1
  ),
  test[temp_head:nrow(test), ] |> ## see if use this or not
    dplyr::filter(
      as.numeric(gsub(
        pattern = "M_",
        replacement = "",
        x = label
      )) >= min_fraction &
        as.numeric(gsub(
          pattern = "M_",
          replacement = "",
          x = label
        ) <= max_fraction)
    ),
  test_2_corr
) |>
  mutate(id = if_else(
    condition = !is.na(parent),
    true = paste(parent, label, sep = "-"),
    false = label
  )) |>
  mutate(parent = if_else(
    condition = parent != "V_03_10g",
    true = paste("V_03_10g",
      gsub(
        pattern = "-.*", replacement = "", id
      ),
      sep = "-"
    ),
    false = parent
  )) |>
  mutate(color = if_else(
    condition = grepl(pattern = "[0-9]", x = label),
    true = if_else(
      condition = grepl(pattern = "M", x = label),
      true = "toDo",
      false = "grey"
    ),
    false = label
  )) |>
  group_by(color) %>%
  mutate(color_2 = cur_group_id()) |>
  group_by(label, color) %>%
  mutate(color_3 = cur_group_id()) |>
  mutate(color = if_else(
    condition = color == "grey",
    true = "grey",
    false = if_else(
      condition = color == "toDo",
      true = colorRampPalette(c("#FFFFFF", "#71196E"))(n_fraction)[color_3],
      false = tableau20[color_2]
    )
  ))

test_3_alternative <- rbind(
  c(
    label = "V_03_10g",
    parent = NA,
    value = 1
  ),
  test[temp_head:nrow(test), ] |> ## see if use this or not
    dplyr::filter(
      as.numeric(gsub(
        pattern = "M_",
        replacement = "",
        x = label
      )) >= min_fraction &
        as.numeric(gsub(
          pattern = "M_",
          replacement = "",
          x = label
        ) <= max_fraction)
    ),
  test_2_alternative_corr
) |>
  mutate(id = if_else(
    condition = !is.na(parent),
    true = paste(parent, label, sep = "-"),
    false = label
  )) |>
  mutate(parent = if_else(
    condition = parent != "V_03_10g",
    true = paste("V_03_10g",
      gsub(
        pattern = "-.*", replacement = "", id
      ),
      sep = "-"
    ),
    false = parent
  )) |>
  mutate(color = if_else(
    condition = grepl(pattern = "[0-9]", x = label),
    true = if_else(
      condition = grepl(pattern = "M", x = label),
      true = "toDo",
      false = "grey"
    ),
    false = label
  )) |>
  group_by(color) %>%
  mutate(color_2 = cur_group_id()) |>
  group_by(label, color) %>%
  mutate(color_3 = cur_group_id()) |>
  mutate(color = if_else(
    condition = color == "grey",
    true = "grey",
    false = if_else(
      condition = color == "toDo",
      true = colorRampPalette(c("#FFFFFF", "#71196E"))(n_fraction)[color_3],
      false = tableau20[color_2]
    )
  ))

test_4_alternative <- test_3_alternative |>
  dplyr::filter(as.numeric(gsub(
    pattern = "M_",
    replacement = "",
    x = label
  )) < min_diluted_fraction |
    as.numeric(
      gsub(
        pattern = "M_",
        replacement = "",
        x = label
      ) > max_diluted_fraction
    )) |>
  dplyr::filter(as.numeric(gsub(
    pattern = "V_03_10g-M_",
    replacement = "",
    x = parent
  )) < min_diluted_fraction |
    as.numeric(
      gsub(
        pattern = "V_03_10g-M_",
        replacement = "",
        x = parent
      ) > max_diluted_fraction
    ))

wheel <- plot_ly() |>
  add_trace(
    data = test_3,
    ids = ~id,
    labels = ~label,
    parents = ~parent,
    values = ~value,
    marker = list(colors = ~color),
    domain = list(column = 0),
    sort = FALSE,
    type = "sunburst"
  ) |>
  add_trace(
    data = test_3_alternative,
    ids = ~id,
    labels = ~label,
    parents = ~parent,
    values = ~value,
    marker = list(colors = ~color),
    domain = list(column = 1),
    sort = FALSE,
    type = "sunburst"
  ) |>
  # add_trace(
  #   data = test_4_alternative,
  #   ids = ~id,
  #   labels = ~label,
  #   parents = ~parent,
  #   values = ~value,
  #   marker = list(colors = ~color),
  #   domain = list(column = 2),
  #   sort = FALSE,
  #   type = "sunburst"
  # ) |>
  plotly::layout(
    title = "Extract fractions tastes",
    annotations = list(
      list(
        x = 0.1,
        y = 1.02,
        text = "What is the taste of the fractions of my extract?",
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(size = 12)
      ),
      list(
        x = 0.9,
        y = 1.02,
        text = "Which fractions contribute to which \n taste within my extract?",
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(size = 12)
      ),
      list(
        x = 0.5,
        y = 1.05,
        text = "Fraction polarity [% MeOH]",
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(size = 12, color = "#71196E")
      )
    ),
    # shapes = list(
    #   list(
    #     type = "line",
    #     x0 = 0.66,
    #     y0 = 0.58,
    #     x1 = 0.75,
    #     y1 = 0.8,
    #     line = list(
    #       color = "black",
    #       dash = "dot"
    #     ),
    #     xref = "paper",
    #     yref = "paper"
    #   ),
    #   list(
    #     type = "line",
    #     x0 = 0.64,
    #     y0 = 0.35,
    #     x1 = 0.8,
    #     y1 = 0.15,
    #     line = list(
    #       color = "black",
    #       dash = "dot"
    #     ),
    #     xref = "paper",
    #     yref = "paper"
    #   )
    # ),
    grid = list(
      columns = 2, # 3
      rows = 1
    ),
    margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 100
    )
  )

wheel
