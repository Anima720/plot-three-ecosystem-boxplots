options(encoding = "UTF-8", warn = 2, stringsAsFactors = FALSE)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop(
    "用法：audit_statistics.R analysis_config.json seven_tables.json ",
    "audit_report.json"
  )
}

required_packages <- c("digest", "jsonlite", "readxl")
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]
if (length(missing_packages) > 0L) {
  stop("独立统计复核缺少 R 包：", paste(missing_packages, collapse = "、"))
}

config_path <- normalizePath(arguments[[1]], winslash = "/", mustWork = TRUE)
summary_path <- normalizePath(arguments[[2]], winslash = "/", mustWork = TRUE)
report_path <- normalizePath(arguments[[3]], winslash = "/", mustWork = FALSE)
config <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
config_raw <- jsonlite::read_json(config_path, simplifyVector = FALSE)

input_skip_value <- if (is.null(config$input_skip)) 0 else as.numeric(config$input_skip)
if (
  length(input_skip_value) != 1L ||
    !is.finite(input_skip_value) ||
    input_skip_value < 0 ||
    input_skip_value != floor(input_skip_value)
) {
  stop("独立复核发现 input_skip 不合法。")
}
input_skip <- as.integer(input_skip_value)
input_range <- if (
  is.null(config$input_range) ||
    !nzchar(trimws(as.character(config$input_range)))
) NULL else trimws(as.character(config$input_range))
if (!is.null(input_range) && input_skip != 0L) {
  stop("独立复核发现 input_range 与非零 input_skip 同时设置。")
}
source_header_row <- if (is.null(input_range)) {
  input_skip + 1L
} else {
  if (grepl("!", input_range, fixed = TRUE)) {
    range_sheet <- sub("!.*$", "", input_range)
    range_sheet <- sub("^'(.*)'$", "\\1", range_sheet)
    range_sheet <- gsub("''", "'", range_sheet, fixed = TRUE)
    if (!identical(range_sheet, as.character(config$input_sheet))) {
      stop("独立复核发现 input_range 工作表前缀与 input_sheet 不一致。")
    }
  }
  cell_range <- sub("^.*!", "", input_range)
  range_match <- regexec("^\\$?[A-Za-z]+\\$?([0-9]+):", cell_range)
  range_parts <- regmatches(cell_range, range_match)[[1]]
  if (length(range_parts) != 2L) {
    stop("独立复核无法解析 input_range 的标题行。")
  }
  as.integer(range_parts[[2]])
}

alpha_level <- 0.05
treatment_levels <- c("Meadow", "Steppe", "desert")
comparison_plan <- data.frame(
  group1 = c("Meadow", "Meadow", "Steppe"),
  group2 = c("Steppe", "desert", "desert"),
  direction = c(
    "Meadow - Steppe",
    "Meadow - desert",
    "Steppe - desert"
  ),
  stringsAsFactors = FALSE
)

assert_close <- function(actual, expected, label, tolerance = 2e-9) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (
    length(actual) != length(expected) ||
      any(is.na(actual) != is.na(expected))
  ) {
    stop(label, " 的长度或 NA 位置不一致。")
  }
  finite <- !is.na(expected)
  if (any(finite)) {
    scale <- pmax(1, abs(expected[finite]))
    difference <- abs(actual[finite] - expected[finite])
    if (any(difference > tolerance * scale)) {
      stop(
        label,
        " 不一致。actual=",
        paste(format(actual, digits = 17), collapse = ","),
        "；expected=",
        paste(format(expected, digits = 17), collapse = ",")
      )
    }
  }
  invisible(TRUE)
}

bias_corrected_skewness <- function(values) {
  n <- length(values)
  centered <- values - mean(values)
  m2 <- mean(centered^2)
  m3 <- mean(centered^3)
  if (n < 3L || !is.finite(m2) || m2 <= 0) {
    return(NA_real_)
  }
  sqrt(n * (n - 1)) / (n - 2) * m3 / (m2^(3 / 2))
}

unbiased_excess_kurtosis <- function(values) {
  n <- length(values)
  centered <- values - mean(values)
  m2 <- mean(centered^2)
  m4 <- mean(centered^4)
  if (n < 4L || !is.finite(m2) || m2 <= 0) {
    return(NA_real_)
  }
  biased_excess <- m4 / (m2^2) - 3
  (n - 1) / ((n - 2) * (n - 3)) *
    ((n + 1) * biased_excess + 6)
}

cliffs_delta <- function(first, second) {
  differences <- outer(first, second, "-")
  (
    sum(differences > 0) - sum(differences < 0)
  ) / (length(first) * length(second))
}

hc3_covariance_manual <- function(model) {
  design <- stats::model.matrix(model)
  residuals <- stats::residuals(model)
  leverage <- stats::hatvalues(model)
  bread <- solve(crossprod(design))
  weights <- residuals^2 / (1 - leverage)^2
  meat <- crossprod(design, design * weights)
  bread %*% meat %*% bread
}

run_python_strict <- function(python_executable, code, values) {
  output <- system2(
    python_executable,
    args = c("-c", shQuote(code), values),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(output, "status")
  if (!is.null(exit_status) && as.integer(exit_status) != 0L) {
    stop("独立 SciPy 复核失败：", paste(output, collapse = "\n"))
  }
  output
}

scipy_studentized_range <- function(q_values, df_values, python_executable) {
  code <- paste(
    "import sys, math",
    "from scipy.stats import studentized_range",
    "values = [float(value) for value in sys.argv[1:]]",
    "for index in range(0, len(values), 2):",
    "    q_value = values[index]",
    "    df_value = values[index + 1]",
    "    p_value = float(studentized_range.sf(q_value, 3, df_value))",
    "    q_critical = float(studentized_range.ppf(0.95, 3, df_value))",
    "    if not (math.isfinite(p_value) and math.isfinite(q_critical)):",
    "        raise SystemExit('non-finite studentized-range result')",
    "    print(f'{p_value:.17g}\\t{q_critical:.17g}')",
    sep = "\n"
  )
  values <- as.vector(rbind(
    format(q_values, digits = 17, scientific = TRUE, trim = TRUE),
    format(df_values, digits = 17, scientific = TRUE, trim = TRUE)
  ))
  output <- run_python_strict(python_executable, code, values)
  parsed <- strsplit(output, "\t", fixed = TRUE)
  list(
    p = as.numeric(vapply(parsed, `[[`, character(1), 1L)),
    q_critical = as.numeric(vapply(parsed, `[[`, character(1), 2L))
  )
}

dunn_components <- function(values, treatment) {
  pooled_ranks <- rank(values, ties.method = "average")
  mean_ranks <- tapply(pooled_ranks, treatment, mean)
  group_n <- table(treatment)
  total_n <- length(values)
  tie_counts <- table(values)
  tie_correction <- sum(tie_counts^3 - tie_counts)
  rank_variance <-
    total_n * (total_n + 1) / 12 -
    tie_correction / (12 * (total_n - 1))
  estimates <- mapply(
    function(group1, group2) mean_ranks[[group1]] - mean_ranks[[group2]],
    comparison_plan$group1,
    comparison_plan$group2
  )
  standard_errors <- mapply(
    function(group1, group2) {
      sqrt(rank_variance * (1 / group_n[[group1]] + 1 / group_n[[group2]]))
    },
    comparison_plan$group1,
    comparison_plan$group2
  )
  z_statistics <- estimates / standard_errors
  p_raw <- 2 * stats::pnorm(-abs(z_statistics))
  list(
    estimate = estimates,
    se = standard_errors,
    statistic = z_statistics,
    p_raw = p_raw,
    p_adjusted = stats::p.adjust(p_raw, method = "BH"),
    mean_ranks = mean_ranks
  )
}

source_file <- normalizePath(
  config$input_file,
  winslash = "/",
  mustWork = TRUE
)
source_sha256 <- toupper(digest::digest(
  file = source_file,
  algo = "sha256",
  serialize = FALSE
))
if (!identical(source_sha256, toupper(config$expected_sha256))) {
  stop("独立复核时源文件 SHA256 与配置不一致。")
}

source_data <- if (is.null(input_range)) {
  readxl::read_excel(
    source_file,
    sheet = config$input_sheet,
    skip = input_skip,
    .name_repair = "minimal"
  )
} else {
  readxl::read_excel(
    source_file,
    sheet = config$input_sheet,
    range = input_range,
    .name_repair = "minimal"
  )
}
if (anyDuplicated(names(source_data))) {
  stop("独立复核发现所选范围有重复表头。")
}
source_rows <- source_header_row + seq_len(nrow(source_data))
metrics <- unname(as.character(config$metric_columns))
derived_specs <- config_raw$derived_metrics
if (is.null(derived_specs)) {
  derived_specs <- list()
}
for (derived_name in names(derived_specs)) {
  if (derived_name %in% names(source_data)) {
    stop("独立复核发现源表已有同名派生指标：", derived_name)
  }
  specification <- derived_specs[[derived_name]]
  operation <- as.character(specification$operation)
  source_columns <- unlist(specification$columns, use.names = FALSE)
  if (
    length(operation) != 1L ||
      !operation %in% c("sum", "difference", "ratio", "product") ||
      length(source_columns) < 2L ||
      anyNA(source_columns) ||
      any(trimws(source_columns) == "") ||
      (
        operation %in% c("difference", "ratio") &&
          length(source_columns) != 2L
      )
  ) {
    stop("独立复核发现派生指标规格不合法：", derived_name)
  }
  missing_source_columns <- setdiff(source_columns, names(source_data))
  if (length(missing_source_columns) > 0L) {
    stop(
      "独立复核的派生基础列缺失：",
      paste(missing_source_columns, collapse = "、")
    )
  }
  source_values <- lapply(
    source_columns,
    function(column_name) source_data[[column_name]]
  )
  if (
    any(!vapply(source_values, is.numeric, logical(1))) ||
      any(vapply(
        source_values,
        function(values) anyNA(values) || any(!is.finite(values)),
        logical(1)
      ))
  ) {
    stop("独立复核发现派生基础列不是完整有限数值：", derived_name)
  }
  if (operation == "ratio" && any(source_values[[2]] == 0)) {
    stop("独立复核发现派生比值分母含 0：", derived_name)
  }
  derived_values <- switch(
    operation,
    sum = Reduce(`+`, source_values),
    difference = source_values[[1]] - source_values[[2]],
    ratio = source_values[[1]] / source_values[[2]],
    product = Reduce(`*`, source_values),
    stop("独立复核不支持派生操作：", operation)
  )
  source_data[[derived_name]] <- as.numeric(derived_values)
}

missing_columns <- setdiff(c(config$group_column, metrics), names(source_data))
if (length(missing_columns) > 0L) {
  stop("独立复核缺少列：", paste(missing_columns, collapse = "、"))
}
if (
  any(!vapply(source_data[metrics], is.numeric, logical(1))) ||
    anyNA(source_data[metrics]) ||
    any(!is.finite(as.matrix(source_data[metrics])))
) {
  stop("独立复核发现指标不是完整有限数值。")
}
if (!is.null(config$sample_id_column)) {
  if (!config$sample_id_column %in% names(source_data)) {
    stop("独立复核缺少样本 ID 列。")
  }
  sample_ids <- trimws(as.character(source_data[[config$sample_id_column]]))
  if (anyNA(sample_ids) || any(sample_ids == "") || anyDuplicated(sample_ids)) {
    stop("独立复核发现样本 ID 缺失、空白或重复。")
  }
} else {
  sample_ids <- paste0("SourceRow_", source_rows)
}

source_group <- trimws(as.character(source_data[[config$group_column]]))
treatment <- factor(
  ifelse(source_group %in% c("Desert", "desert"), "desert", source_group),
  levels = treatment_levels
)
if (anyNA(treatment)) {
  stop("独立复核发现无法映射的生态系统标签。")
}
expected_group_counts <- as.integer(
  unlist(config$expected_group_counts)[treatment_levels]
)
if (!identical(as.integer(table(treatment)), expected_group_counts)) {
  stop("独立复核的三个组样本量与配置不一致。")
}

expected_branches <- NULL
if (!is.null(config$expected_branches)) {
  expected_branches <- unlist(config$expected_branches, use.names = TRUE)
  if (!setequal(names(expected_branches), metrics)) {
    stop("独立统计复核发现 expected_branches 未逐一覆盖指标。")
  }
  expected_branches <- expected_branches[metrics]
}

summary_data <- jsonlite::fromJSON(summary_path, simplifyDataFrame = TRUE)
overview <- summary_data[["分析总览"]]
pairwise <- summary_data[["两两比较"]]
descriptive <- summary_data[["描述统计"]]
diagnostics <- summary_data[["残差诊断"]]
outliers <- summary_data[["异常值标记"]]
total_n <- nrow(source_data)
stopifnot(
  identical(as.character(overview[["指标"]]), metrics),
  nrow(pairwise) == 3L * length(metrics),
  nrow(descriptive) == 3L * length(metrics),
  nrow(diagnostics) == length(metrics),
  nrow(outliers) == total_n * length(metrics),
  all(outliers[["是否保留在分析中"]] %in% TRUE)
)
expected_metric_order <- rep(metrics, each = total_n)
expected_source_rows <- rep(source_rows, times = length(metrics))
expected_sample_ids <- rep(sample_ids, times = length(metrics))
expected_original_groups <- rep(source_group, times = length(metrics))
expected_display_groups <- rep(as.character(treatment), times = length(metrics))
expected_source_values <- unlist(
  lapply(metrics, function(metric) as.numeric(source_data[[metric]])),
  use.names = FALSE
)
stopifnot(
  identical(as.character(outliers[["指标"]]), expected_metric_order),
  identical(as.integer(outliers[["源行号"]]), as.integer(expected_source_rows)),
  identical(as.character(outliers[["样本ID"]]), expected_sample_ids),
  identical(as.character(outliers[["原始样本前缀"]]), expected_original_groups),
  identical(as.character(outliers[["显示处理"]]), expected_display_groups)
)
assert_close(
  outliers[["源值"]],
  expected_source_values,
  "异常值表源值追踪"
)

python_executable <- normalizePath(
  config$python_executable,
  winslash = "/",
  mustWork = TRUE
)
omnibus_p <- stats::setNames(numeric(length(metrics)), metrics)
audit_rows <- vector("list", length(metrics))
names(audit_rows) <- metrics

for (metric in metrics) {
  metric_data <- data.frame(
    Value = as.numeric(source_data[[metric]]),
    Treatment = treatment
  )
  model <- stats::lm(Value ~ Treatment, data = metric_data)
  model_summary <- summary(model)
  model_residuals <- stats::residuals(model)
  shapiro <- stats::shapiro.test(model_residuals)
  residual_group_mean <- ave(
    model_residuals,
    treatment,
    FUN = mean
  )
  absolute_deviation <- abs(model_residuals - residual_group_mean)
  levene_table <- stats::anova(
    stats::lm(absolute_deviation ~ treatment)
  )
  shapiro_p <- unname(shapiro$p.value)
  levene_f <- unname(levene_table[["F value"]][[1]])
  levene_p <- unname(levene_table[["Pr(>F)"]][[1]])
  skewness <- bias_corrected_skewness(model_residuals)
  kurtosis <- unbiased_excess_kurtosis(model_residuals)
  max_abs_rstudent <- max(abs(stats::rstudent(model)))
  guards <- c(
    total_n >= 25L,
    min(table(treatment)) >= 5L,
    abs(skewness) <= 1,
    abs(kurtosis) <= 2,
    max_abs_rstudent <= 3
  )
  branch <- if (shapiro_p >= alpha_level && levene_p >= alpha_level) {
    "ANOVA_TUKEY"
  } else if (shapiro_p >= alpha_level && levene_p < alpha_level) {
    "WELCH_GH"
  } else if (all(guards)) {
    "HC3_HOLM"
  } else {
    "KW_DUNN_BH"
  }
  if (
    !is.null(expected_branches) &&
      !identical(branch, unname(expected_branches[[metric]]))
  ) {
    stop(metric, " 的独立复核分支与锁定配置不一致。")
  }

  overview_row <- overview[overview[["指标"]] == metric, , drop = FALSE]
  pair_rows <- pairwise[pairwise[["指标"]] == metric, , drop = FALSE]
  descriptive_rows <- descriptive[
    descriptive[["指标"]] == metric,
    ,
    drop = FALSE
  ]
  diagnostic_row <- diagnostics[
    diagnostics[["指标"]] == metric,
    ,
    drop = FALSE
  ]
  outlier_rows <- outliers[outliers[["指标"]] == metric, , drop = FALSE]
  stopifnot(
    nrow(overview_row) == 1L,
    nrow(pair_rows) == 3L,
    nrow(descriptive_rows) == 3L,
    nrow(diagnostic_row) == 1L,
    nrow(outlier_rows) == total_n,
    identical(as.character(pair_rows[["比较方向"]]), comparison_plan$direction),
    identical(as.character(overview_row[["最终分支"]]), branch),
    all(pair_rows[["P原始"]] >= 0 & pair_rows[["P原始"]] <= 1),
    all(pair_rows[["P调整后"]] >= 0 & pair_rows[["P调整后"]] <= 1)
  )
  assert_close(overview_row[["残差ShapiroP"]], shapiro_p, paste(metric, "Shapiro P"))
  assert_close(overview_row[["均值中心LeveneP"]], levene_p, paste(metric, "Levene P"))
  assert_close(diagnostic_row[["Shapiro_W"]], shapiro$statistic, paste(metric, "Shapiro W"))
  assert_close(diagnostic_row[["均值中心Levene_F"]], levene_f, paste(metric, "Levene F"))
  assert_close(diagnostic_row[["均值中心Levene_P"]], levene_p, paste(metric, "Levene P表"))
  assert_close(diagnostic_row[["手工绝对离差_F"]], levene_f, paste(metric, "Levene双实现F"))
  assert_close(diagnostic_row[["手工绝对离差_P"]], levene_p, paste(metric, "Levene双实现P"))
  assert_close(diagnostic_row[["偏差校正偏度"]], skewness, paste(metric, "残差偏度"))
  assert_close(diagnostic_row[["无偏超额峰度"]], kurtosis, paste(metric, "残差峰度"))
  assert_close(
    diagnostic_row[["最大绝对外部学生化残差"]],
    max_abs_rstudent,
    paste(metric, "最大外部学生化残差")
  )

  group_values <- lapply(
    treatment_levels,
    function(group_name) metric_data$Value[metric_data$Treatment == group_name]
  )
  names(group_values) <- treatment_levels
  group_n <- vapply(group_values, length, integer(1))
  group_means <- vapply(group_values, mean, numeric(1))
  group_variances <- vapply(group_values, stats::var, numeric(1))

  pair_estimate <- pair_se <- pair_statistic <- pair_df <-
    pair_ci_lower <- pair_ci_upper <- pair_p_raw <- pair_p_adjusted <-
      rep(NA_real_, 3L)
  eta_squared <- omega_squared <- epsilon_squared <- NA_real_

  if (branch == "ANOVA_TUKEY") {
    anova_table <- stats::anova(model)
    omnibus_statistic <- unname(anova_table[["F value"]][[1]])
    omnibus_df1 <- unname(anova_table[["Df"]][[1]])
    omnibus_df2 <- unname(anova_table[["Df"]][[2]])
    omnibus_p[[metric]] <- unname(anova_table[["Pr(>F)"]][[1]])
    mse <- unname(anova_table[["Mean Sq"]][[2]])
    ss_between <- unname(anova_table[["Sum Sq"]][[1]])
    ss_within <- unname(anova_table[["Sum Sq"]][[2]])
    eta_squared <- ss_between / (ss_between + ss_within)
    omega_squared <- max(
      0,
      (ss_between - omnibus_df1 * mse) /
        (ss_between + ss_within + mse)
    )
    for (index in seq_len(3L)) {
      group1 <- comparison_plan$group1[[index]]
      group2 <- comparison_plan$group2[[index]]
      pair_estimate[[index]] <- group_means[[group1]] - group_means[[group2]]
      pair_se[[index]] <- sqrt(
        mse * (1 / group_n[[group1]] + 1 / group_n[[group2]])
      )
      pair_statistic[[index]] <- pair_estimate[[index]] / pair_se[[index]]
      pair_df[[index]] <- omnibus_df2
      pair_p_raw[[index]] <- 2 * stats::pt(
        abs(pair_statistic[[index]]),
        df = pair_df[[index]],
        lower.tail = FALSE
      )
      pair_p_adjusted[[index]] <- stats::ptukey(
        sqrt(2) * abs(pair_statistic[[index]]),
        nmeans = 3,
        df = pair_df[[index]],
        lower.tail = FALSE
      )
      half_width <- stats::qtukey(
        0.95,
        nmeans = 3,
        df = pair_df[[index]]
      ) * pair_se[[index]] / sqrt(2)
      pair_ci_lower[[index]] <- pair_estimate[[index]] - half_width
      pair_ci_upper[[index]] <- pair_estimate[[index]] + half_width
    }
  } else if (branch == "WELCH_GH") {
    welch <- stats::oneway.test(
      Value ~ Treatment,
      data = metric_data,
      var.equal = FALSE
    )
    omnibus_statistic <- unname(welch$statistic)
    omnibus_df1 <- unname(welch$parameter[["num df"]])
    omnibus_df2 <- unname(welch$parameter[["denom df"]])
    omnibus_p[[metric]] <- unname(welch$p.value)
    for (index in seq_len(3L)) {
      group1 <- comparison_plan$group1[[index]]
      group2 <- comparison_plan$group2[[index]]
      pair_estimate[[index]] <- group_means[[group1]] - group_means[[group2]]
      first_component <- group_variances[[group1]] / group_n[[group1]]
      second_component <- group_variances[[group2]] / group_n[[group2]]
      pair_se[[index]] <- sqrt(first_component + second_component)
      pair_df[[index]] <- (first_component + second_component)^2 /
        (
          first_component^2 / (group_n[[group1]] - 1) +
            second_component^2 / (group_n[[group2]] - 1)
        )
      welch_t <- pair_estimate[[index]] / pair_se[[index]]
      pair_statistic[[index]] <- sqrt(2) * abs(welch_t)
      pair_p_raw[[index]] <- 2 * stats::pt(
        abs(welch_t),
        df = pair_df[[index]],
        lower.tail = FALSE
      )
    }
    scipy_result <- scipy_studentized_range(
      pair_statistic,
      pair_df,
      python_executable
    )
    pair_p_adjusted <- scipy_result$p
    half_width <- scipy_result$q_critical * pair_se / sqrt(2)
    pair_ci_lower <- pair_estimate - half_width
    pair_ci_upper <- pair_estimate + half_width
  } else if (branch == "HC3_HOLM") {
    covariance <- hc3_covariance_manual(model)
    coefficients <- stats::coef(model)
    design <- stats::model.matrix(model)
    restriction <- cbind(0, diag(ncol(design) - 1L))
    restricted_beta <- restriction %*% coefficients
    restricted_covariance <- restriction %*% covariance %*% t(restriction)
    omnibus_df1 <- nrow(restriction)
    omnibus_df2 <- stats::df.residual(model)
    omnibus_statistic <- as.numeric(
      t(restricted_beta) %*%
        solve(restricted_covariance, restricted_beta) /
        omnibus_df1
    )
    omnibus_p[[metric]] <- stats::pf(
      omnibus_statistic,
      df1 = omnibus_df1,
      df2 = omnibus_df2,
      lower.tail = FALSE
    )
    mean_design <- stats::model.matrix(
      ~ Treatment,
      data = data.frame(
        Treatment = factor(treatment_levels, levels = treatment_levels)
      )
    )
    for (index in seq_len(3L)) {
      group1_index <- match(comparison_plan$group1[[index]], treatment_levels)
      group2_index <- match(comparison_plan$group2[[index]], treatment_levels)
      contrast <- mean_design[group1_index, ] - mean_design[group2_index, ]
      pair_estimate[[index]] <- as.numeric(contrast %*% coefficients)
      pair_se[[index]] <- sqrt(as.numeric(contrast %*% covariance %*% contrast))
      pair_statistic[[index]] <- pair_estimate[[index]] / pair_se[[index]]
      pair_df[[index]] <- omnibus_df2
      pair_p_raw[[index]] <- 2 * stats::pt(
        abs(pair_statistic[[index]]),
        df = pair_df[[index]],
        lower.tail = FALSE
      )
      half_width <- stats::qt(0.975, df = pair_df[[index]]) * pair_se[[index]]
      pair_ci_lower[[index]] <- pair_estimate[[index]] - half_width
      pair_ci_upper[[index]] <- pair_estimate[[index]] + half_width
    }
    pair_p_adjusted <- stats::p.adjust(pair_p_raw, method = "holm")
  } else {
    kruskal <- stats::kruskal.test(
      Value ~ Treatment,
      data = metric_data
    )
    omnibus_statistic <- unname(kruskal$statistic)
    omnibus_df1 <- unname(kruskal$parameter)
    omnibus_df2 <- NA_real_
    omnibus_p[[metric]] <- unname(kruskal$p.value)
    epsilon_squared <- max(
      0,
      (omnibus_statistic - length(treatment_levels) + 1) /
        (total_n - length(treatment_levels))
    )
    dunn <- dunn_components(metric_data$Value, metric_data$Treatment)
    pair_estimate <- dunn$estimate
    pair_se <- dunn$se
    pair_statistic <- dunn$statistic
    pair_p_raw <- dunn$p_raw
    pair_p_adjusted <- dunn$p_adjusted
  }

  assert_close(overview_row[["统计量"]], omnibus_statistic, paste(metric, "总体统计量"))
  assert_close(overview_row[["分子自由度或df"]], omnibus_df1, paste(metric, "总体df1"))
  assert_close(overview_row[["分母自由度"]], omnibus_df2, paste(metric, "总体df2"))
  assert_close(overview_row[["P原始"]], omnibus_p[[metric]], paste(metric, "总体P"))
  assert_close(overview_row[["R2_cat_描述性"]], model_summary$r.squared, paste(metric, "R2"))
  assert_close(
    overview_row[["Adjusted_R2_描述性"]],
    model_summary$adj.r.squared,
    paste(metric, "adjusted R2")
  )
  assert_close(overview_row[["Eta_squared"]], eta_squared, paste(metric, "Eta squared"))
  assert_close(overview_row[["Omega_squared"]], omega_squared, paste(metric, "Omega squared"))
  assert_close(
    overview_row[["Epsilon_squared_KW"]],
    epsilon_squared,
    paste(metric, "Epsilon squared")
  )

  assert_close(pair_rows[["估计量"]], pair_estimate, paste(metric, "两两估计量"))
  assert_close(pair_rows[["标准误"]], pair_se, paste(metric, "两两SE"))
  assert_close(pair_rows[["检验统计量"]], pair_statistic, paste(metric, "两两统计量"))
  assert_close(pair_rows[["自由度"]], pair_df, paste(metric, "两两df"))
  assert_close(pair_rows[["95CI下限"]], pair_ci_lower, paste(metric, "两两CI下限"))
  assert_close(pair_rows[["95CI上限"]], pair_ci_upper, paste(metric, "两两CI上限"))
  assert_close(pair_rows[["P原始"]], pair_p_raw, paste(metric, "两两原始P"))
  assert_close(pair_rows[["P调整后"]], pair_p_adjusted, paste(metric, "两两调整P"))

  expected_delta <- mapply(
    function(group1, group2) cliffs_delta(group_values[[group1]], group_values[[group2]]),
    comparison_plan$group1,
    comparison_plan$group2
  )
  assert_close(pair_rows[["Cliffs_delta"]], expected_delta, paste(metric, "Cliff delta"))

  q1 <- tapply(metric_data$Value, treatment, stats::quantile, probs = 0.25)
  q3 <- tapply(metric_data$Value, treatment, stats::quantile, probs = 0.75)
  iqr <- q3 - q1
  expected_outlier_flag <- metric_data$Value <
    as.numeric((q1 - 1.5 * iqr)[as.character(treatment)]) |
    metric_data$Value >
      as.numeric((q3 + 1.5 * iqr)[as.character(treatment)])
  if (!identical(as.logical(outlier_rows[["IQR异常值标记"]]), expected_outlier_flag)) {
    stop(metric, " 的 IQR 异常值标记独立复核不一致。")
  }

  for (index in seq_len(3L)) {
    group1 <- comparison_plan$group1[[index]]
    group2 <- comparison_plan$group2[[index]]
    letters1 <- descriptive_rows[["多重比较字母"]][
      descriptive_rows[["处理"]] == group1
    ]
    letters2 <- descriptive_rows[["多重比较字母"]][
      descriptive_rows[["处理"]] == group2
    ]
    shared <- length(intersect(
      strsplit(letters1, "", fixed = TRUE)[[1]],
      strsplit(letters2, "", fixed = TRUE)[[1]]
    )) > 0L
    if (!identical(shared, pair_p_adjusted[[index]] >= alpha_level)) {
      stop(metric, "/", comparison_plan$direction[[index]], " 的字母与P不一致。")
    }
  }
  priority <- if (branch == "KW_DUNN_BH") {
    tapply(rank(metric_data$Value, ties.method = "average"), treatment, mean)
  } else {
    group_means
  }
  highest_group <- names(priority)[which.max(priority)]
  highest_letter <- descriptive_rows[["多重比较字母"]][
    descriptive_rows[["处理"]] == highest_group
  ]
  if (!grepl("A", highest_letter, fixed = TRUE)) {
    stop(metric, " 的最高中心组未优先包含字母A。")
  }

  audit_rows[[metric]] <- data.frame(
    指标 = metric,
    Shapiro_P = shapiro_p,
    Levene_P = levene_p,
    分支 = branch,
    总体_P = omnibus_p[[metric]],
    字母 = paste(
      descriptive_rows[["处理"]],
      descriptive_rows[["多重比较字母"]],
      sep = ":",
      collapse = "|"
    ),
    stringsAsFactors = FALSE
  )
}

expected_bh <- stats::p.adjust(omnibus_p, method = "BH")
assert_close(
  overview[["P_BH_同类指标"]],
  unname(expected_bh[overview[["指标"]]]),
  "同类指标总体BH"
)

source_sha256_after <- toupper(digest::digest(
  file = source_file,
  algo = "sha256",
  serialize = FALSE
))
if (!identical(source_sha256, source_sha256_after)) {
  stop("独立复核前后源文件 SHA256 不一致。")
}

report <- list(
  status = "PASS",
  branch_lock_status = if (is.null(expected_branches)) {
    "UNLOCKED_INDEPENDENT_BRANCHES_READY"
  } else {
    "LOCKED_AND_MATCHED"
  },
  source_sha256 = source_sha256_after,
  rows = total_n,
  group_n = stats::setNames(as.list(as.integer(table(treatment))), treatment_levels),
  metrics = do.call(rbind, audit_rows)
)
dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report,
  path = report_path,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 17,
  na = "null"
)
cat(jsonlite::toJSON(report, dataframe = "rows", auto_unbox = TRUE))
cat("\n")
