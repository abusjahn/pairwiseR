function(
  data,
  dep_vars,
  indep_var,
  gaussian,
  round_p = 3,
  round_desc = 2,
  range = FALSE,
  rangesep = " ",
  pretext = FALSE,
  mark = FALSE,
  n = FALSE,
  add_n = FALSE,
  singleline = TRUE,
  indentor = "   ",
  ci = FALSE,
  n_boot = 10^3
) {
  # Silence R CMD check notes about NSE column names
  . <- Group <- Value <- Variable <- desc_groups <- NULL
  # Long (multiline) output implies CI and n must be shown
  if (!singleline) {
    ci <- TRUE
    add_n <- TRUE
    n <- FALSE
  }
  # Pick descriptive function and test based on distribution assumption
  if (gaussian) {
    DESC <- wrappedtools::meansd
    COMP <- t_var_test
  } else {
    DESC <- median_quart
    COMP <- wilcox.test
  }
  # Reshape data to long format: one row per subject x dependent variable
  data_l <- dplyr::filter(
    tibble::as_tibble(
      mutate(
        pivot_longer(
          mutate(
            dplyr::select(
              data,
              Group = all_of(indep_var),
              all_of(dep_vars)
            ),
            Group = fct_drop(factor(Group))
          ),
          -Group,
          names_to = "Variable",
          values_to = "Value"
        ),
        Variable = forcats::fct_inorder(Variable)
      )
    ),
    !is.na(Group)
  )
  # This function only handles exactly 2 groups
  if (nlevels(data_l$Group) != 2) {
    stop(paste0(
      "Other than 2 groups provided for ",
      indep_var,
      ": ",
      paste(levels(data_l$Group), collapse = "/"),
      ". Look into function compare_n_numvars."
    ))
  }
  group_levels <- levels(data_l$Group)
  # n is required to build long-format tables; enable add_n if needed
  if (!singleline && n && !add_n) {
    add_n <- TRUE
    cat(
      "add_n will be set to TRUE to calculate n for long table format (singleline = FALSE)\n"
    )
  }
  # Loop over each dependent variable and build one output block per variable
  for (var_i in seq_along(dep_vars)) {
    # Restrict to the current variable and drop missing values
    tempdata <- dplyr::filter(
      filter(
        data_l,
        Variable == dep_vars[var_i]
      ),
      !is.na(Value)
    )
    # Sample sizes per group for this variable
    n_g1 <- sum(tempdata$Group == group_levels[1], na.rm = TRUE)
    n_g2 <- sum(tempdata$Group == group_levels[2], na.rm = TRUE)
    # Overall descriptive statistics (both groups combined)
    out_temp <- reframe(
      tempdata,
      Variable = dep_vars[var_i],
      n = sum(!is.na(Value)),
      `n g1` = n_g1,
      `n g2` = n_g2,
      desc_all = as.character(suppressWarnings(DESC(
        Value,
        roundDig = round_desc,
        range = range,
        rangesep = rangesep,
        add_n = add_n,
        ci = ci,
        singleline = singleline
      )))
    )
    # Descriptives for group 1 (NA if the group is empty)
    if (n_g1 == 0) {
      out_temp$g1 <- NA_character_
    } else {
      out_temp <- bind_cols(
        out_temp,
        reframe(
          filter(
            tempdata,
            Group == group_levels[1]
          ),
          g1 = as.character(suppressWarnings(DESC(
            Value,
            roundDig = round_desc,
            range = range,
            rangesep = rangesep,
            add_n = add_n,
            ci = ci,
            singleline = singleline
          )))
        )
      )
    }
    # Descriptives for group 2 (NA if the group is empty)
    if (n_g2 == 0) {
      out_temp$g2 <- NA_character_
    } else {
      out_temp <- bind_cols(
        out_temp,
        reframe(
          filter(
            tempdata,
            Group == group_levels[2]
          ),
          g2 = as.character(suppressWarnings(DESC(
            Value,
            roundDig = round_desc,
            range = range,
            rangesep = rangesep,
            add_n = add_n,
            ci = ci,
            singleline = singleline
          )))
        )
      )
    }
    out_temp$p <- NA_character_
    if (singleline) {
      # Two-group test; blank string if the test fails (e.g. zero variance)
      out_temp$p[1] <- tryCatch(
        {
          suppressWarnings(formatP(
            COMP(Value ~ Group,
              data = tempdata
            )$p.value,
            ndigits = round_p,
            pretext = pretext,
            mark = mark
          ))
        },
        error = function(e) {
          " "
        }
      )
      out_temp <- select(
        out_temp,
        Variable,
        "n",
        "desc_all",
        "n g1",
        "g1",
        "n g2",
        "g2",
        "p"
      )
    }
    if (!singleline) {
      # Long format: insert a header row with the p value and label
      # each statistic row (n, mean/median, SD/quartiles, range)
      out_temp <- rename(
        select(
          mutate(
            add_row(
              .data = out_temp,
              .before = 1,
              Variable = dep_vars[var_i],
              p = tryCatch(
                {
                  suppressWarnings(formatP(
                    COMP(Value ~ Group,
                      data = tempdata
                    )$p.value,
                    ndigits = round_p,
                    pretext = pretext,
                    mark = mark
                  ))
                },
                error = function(e) {
                  " "
                }
              )
            ),
            Stats = c(
              dep_vars[var_i],
              paste(indentor, "n"),
              ifelse(
                gaussian,
                paste(indentor, "Mean (95% CI)"),
                paste(indentor, "Median (95% CI)")
              ),
              ifelse(
                gaussian,
                paste(indentor, "SD"),
                paste(indentor, "Quartiles")
              ),
              na.omit(ifelse(
                range,
                paste(indentor, "Range"),
                NA_character_
              ))
            )
          ),
          Stats,
          desc_all,
          g1,
          g2,
          p
        ),
        Variable = Stats
      )
    }
    # Accumulate results across variables
    if (var_i == 1) {
      out <- out_temp
    } else {
      out <- bind_rows(out, out_temp)
    }
  }
  # Drop n columns unless requested
  if (!n) {
    out <- dplyr::select(out, -starts_with("n"))
  }
  # Replace NAs with blanks and name group columns after the group levels
  out <- rename(
    mutate(
      out,
      across(
        everything(),
        ~ replace_na(.x, "")
      )
    ),
    `:=`(
      !!glue::glue("{indep_var} {levels(data_l$Group)[1]}"),
      "g1"
    ),
    `:=`(
      !!glue::glue("{indep_var} {levels(data_l$Group)[2]}"),
      "g2"
    )
  )
  return(out)
}
