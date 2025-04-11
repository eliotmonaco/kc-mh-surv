number_to_time <- function(x) {
  paste0(
    substr(sprintf("%04d", x), 1, 2),
    ":",
    substr(sprintf("%04d", x), 3, 4)
  )
}

validate_time <- function(x) {
  h <- paste(sprintf("%02d", 0:23), collapse = "|")
  m <- paste(sprintf("%02d", 0:59), collapse = "|")
  p <- paste0("^(", h, "):(", m, ")$")
  grepl(pattern = p, x = x)
}

# Convert bedtime (`x`) and wake time (`y`) pairs to date objects
times_to_dates <- function(x, y) {
  # If y > x, assign x and y to the same day
  # If x > y, assign y to one day later than x
  xmin <- as.numeric(substr(x, 1, 2)) * 60 + as.numeric(substr(x, 4, 5))
  ymin <- as.numeric(substr(y, 1, 2)) * 60 + as.numeric(substr(y, 4, 5))
  xmin > ymin
  d1 <- as.POSIXct(paste(Sys.Date(), x), format = "%Y-%m-%d %H:%M")
  d2 <- dplyr::if_else(
    ymin > xmin,
    as.POSIXct(paste(Sys.Date(), y), format = "%Y-%m-%d %H:%M"),
    as.POSIXct(paste(Sys.Date() + 1, y), format = "%Y-%m-%d %H:%M")
  )
  list(
    date1 = d1,
    date2 = d2
  )
}

score_hse <- function(x) {
  score <- cut(
    x,
    breaks = c(0, 65, 75, 85, Inf),
    labels = c(3, 2, 1, 0),
    include.lowest = TRUE,
    right = FALSE
  )

  # Reverse factor order
  factor(score, levels = 0:3)
}

# `df`: the dataframe of all individual responses
# `var1`: the question response to summarize
# `var2`: an optional grouping variable, e.g., age or sex
# `cb_vals`: all possible response values from the codebook
summarize_results <- function(df, var1, var2 = NULL, cb_vals) {
  var1 <- var1

  # Drop responses where the value is NA
  df <- df |>
    dplyr::mutate({{ var1 }} := factor(
      .data[[var1]],
      levels = levels(cb_vals)
    )) |>
    tidyr::drop_na({{ var1 }})

  if (!is.null(var2)) {
    # Drop responses where the grouping variable value is NA
    df <- df |>
      tidyr::drop_na({{ var2 }})

    # Get denominators for each grouping variable value
    totals <- df |>
      dplyr::group_by(dplyr::pick({{ var2 }})) |>
      dplyr::count() |>
      dplyr::ungroup()

    df |>
      dplyr::group_by(dplyr::pick(tidyselect::all_of(c(var1, var2)))) |>
      dplyr::count(.drop = FALSE) |>
      dplyr::ungroup() |>
      dplyr::left_join(totals, by = var2, suffix = c("", "_total")) |>
      dplyr::mutate(pct = setmeup::pct(n, n_total, digits = 2))
  } else {
    n_total <- nrow(df)
    df |>
      dplyr::group_by(dplyr::pick({{ var1 }})) |>
      dplyr::count(.drop = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(n_total = n_total) |>
      dplyr::mutate(pct = setmeup::pct(n, n_total, digits = 2))
  }
}

mh_barplot1 <- function(df, yvar, ylab, legend_title) {
  hcl <- farver::decode_colour(
    colour = viridisLite::viridis(length(unique(df$defn))),
    to = "hcl"
  )
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  df |>
    ggplot2::ggplot(ggplot2::aes(x = pct, y = .data[[yvar]], fill = defn)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_discrete(
      labels = scales::label_wrap(20),
      limits = rev
    ) +
    ggplot2::scale_fill_viridis_d(
      labels = function(x) stringr::str_wrap(x, width = 30)
    ) +
    ggplot2::xlab("\n%") +
    ggplot2::ylab(paste0(ylab, "\n")) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(setmeup::round_ties_away(pct), "%"),
        color = defn
      ),
      position = ggplot2::position_stack(vjust = .5),
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = label_col) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, b = 10),
      legend.key.spacing.y = ggplot2::unit(5, units = "points")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = legend_title,
      byrow = TRUE
    ))
}

mh_barplot2 <- function(df, yvar, ylab, legend_title, plot_title, n_sample) {
  df |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[yvar]], y = pct, fill = defn)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = c("#ed5c5c", "#51abf5")
    ) +
    ggplot2::xlab(paste0("\n", ylab, "\n")) +
    ggplot2::ylab("%\n") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_text( # bar labels
      ggplot2::aes(label = paste0(setmeup::round_ties_away(pct), "%")),
      position = ggplot2::position_stack(vjust = .5)
    ) +
    ggplot2::ggtitle(label = stringr::str_wrap(plot_title, width = 60)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = .5,
        margin = ggplot2::margin(b = 40)
      ),
      panel.grid = ggplot2::element_blank(),
      axis.ticks.x.bottom = ggplot2::element_line(),
      plot.caption = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = legend_title,
      byrow = TRUE
    )) +
    ggplot2::labs(caption = paste0("Number of respondents = ", n_sample))
}
