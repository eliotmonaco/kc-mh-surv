# Demo breakdowns for Q11

mh$q11.01 |>
  group_by(birth_gender) |>
  count()

mh$q11.01 |>
  group_by(race2) |>
  count()

mh$q11.01 |>
  group_by(age_group) |>
  count()



# -------------------------------------------------------------------------



# Methodology for multi-part questions

# Q4: What percentage of respondents experienced anxiety in the past 30 days?
#
# Method 1: Count the number of respondents who answered 2-5 on any of the
# questions 4.1 to 4.4. Multiply by 100 and divide by the total number of
# respondents.

# Filter rows in which respondent answered "yes" (2-5) in any column, then get
# percentage of rows
fn <- function(df, pattern, total = nrow(df)) {
  df |>
    select(starts_with(pattern)) |>
    filter(if_any(everything(), ~ . %in% 2:5)) |>
    nrow() |>
    setmeup::pct(total = total)
}

fn(surv2, "q4_")

# Method 2: Calculate the percentage within each question, 4.1 to 4.4, of
# respondents who answered 2-5. Average the percentages for each of the four
# questions.

mean(c(
  fn(surv2, "q4_01"),
  fn(surv2, "q4_02"),
  fn(surv2, "q4_03"),
  fn(surv2, "q4_04")
))

# Dummy data set:
# - Q1: men answered 2-5, women answered 1 or 9
# - Q2: men answered 1 or 9, women answered 2-5

mw <- data.frame(
  sex = c(
    rep("male", 50),
    rep("female", 50)
  ),
  q1 = c(
    sample(2:5, 50, replace = T), # men
    sample(c(1, 9), 50, replace = T) # women
  ),
  q2 = c(
    sample(c(1, 9), 50, replace = T), # men
    sample(2:5, 50, replace = T) # women
  )
)

# Method 1

fn(mw, ".")

# Method 2

mean(c(
  fn(mw, "q1"),
  fn(mw, "q2")
))



# -------------------------------------------------------------------------



# stacked bar plot
# scale is never --> always (L to R)
# x-axis: left of 0 is no amount, right of 0 is some amount
# don't knows are dropped

data |>
  # Make the count of "1" responses negative so that "no amount" is left of 0
  # and "some amount" is right of 0
  mutate(
    n2 = if_else(val == "1", 0 - n, n),
    pct2 = if_else(val == "1", 0 - pct, pct)
  ) |>
  filter(val %in% 1:5) |>
  ggplot(aes(x = pct2, y = qtext2, fill = defn)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(labels = abs) +
  scale_y_discrete(
    labels = scales::label_wrap(20),
    limits = rev
  ) +
  scale_fill_viridis_d() +
  xlab("%") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.title.y = element_blank()
  ) +
  geom_text(
    aes(label = paste0(abs(pct2), "%")),
    position = position_stack(vjust = .5)
  ) +
  guides(fill = guide_legend(title = "Frequency")) +
  ggtitle(label = str_wrap(
    cbq |>
      filter(qnum == "4") |>
      slice(1) |>
      pull(qtext1),
    width = 80
  ))



# Create `rank` to order chart by highest percentage of Yes response
ranks <- data.frame(
  qnum = unique(data$qnum),
  rank = rank(data[which(data$val == 1), "pct", drop = TRUE])
)

data <- data |>
  left_join(ranks, by = "qnum")







