# ============================================================================
# 通用模板：Meadow、Steppe、Desert 三个独立生态系统处理组的箱线图，
# 以及以线性模型残差为诊断对象的四分流统计分析。
#
# 用法：
#   Rscript run_three_ecosystem_boxplots.R analysis_config.json
#
# 数据文件、工作表、分组列、样本 ID、指标、标题、输出目录、源文件
# SHA256 和预期样本量均由 JSON 配置提供。源数据只读；Desert 仅在内存中
# 显示为 desert。源表已有比值必须直接使用，派生指标仅在用户明确要求时
# 才可在内存中生成，且不得回写源文件。
#
# 固定统计分流（alpha = 0.05）：
# 1. 先拟合 lm(Value ~ Treatment)，对模型合并残差做诊断。
# 2. 残差 Shapiro-Wilk P >= 0.05 且均值中心 Levene P >= 0.05：
#      单因素 ANOVA + Tukey HSD。
# 3. 残差 Shapiro-Wilk P >= 0.05 且 Levene P < 0.05：
#      Welch ANOVA + Games-Howell。
# 4. 残差 Shapiro-Wilk P < 0.05，但满足预设稳健保护条件：
#      HC3 稳健总体 F + HC3 两两均值差，Holm 校正。
# 5. 残差 Shapiro-Wilk P < 0.05，且不满足稳健保护条件：
#      Kruskal-Wallis + Dunn，两两 P 值使用 Benjamini-Hochberg 校正。
#
# 其它固定规则：
# - 所有请求指标的总体检验 P 值作为同一指标家族使用 BH 校正。
# - 所有原始观测全部保留；IQR 仅作异常值质控标记。
# - 红色菱形和虚线始终表示原尺度算术均值。
# - R²(cat)仅为普通分类线性模型的描述量，不是相关系数。
# - Eta²、Omega²仅在普通 ANOVA 分支报告。
# - 两两比较统一报告 Cliff's delta。
# - 主图每个坐标面板严格锁定为 263.645 × 222.34 PDF pt。
# ============================================================================

options(encoding = "UTF-8", warn = 2, stringsAsFactors = FALSE)

# ------------------------------ 1. 路径与配置 -------------------------------

config_arguments <- commandArgs(trailingOnly = TRUE)
if (length(config_arguments) != 1L) {
  stop("必须且只能提供一个 analysis_config.json 路径。")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("读取配置需要 R 包 jsonlite。")
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("输出保护与源文件核验需要 R 包 digest。")
}
config_path <- normalizePath(
  config_arguments[[1]],
  winslash = "/",
  mustWork = TRUE
)
config_file_sha256 <- toupper(digest::digest(
  file = config_path,
  algo = "sha256",
  serialize = FALSE
))
user_config <- jsonlite::fromJSON(
  config_path,
  simplifyVector = TRUE
)
user_config_raw <- jsonlite::read_json(
  config_path,
  simplifyVector = FALSE
)
required_config_fields <- c(
  "analysis_label",
  "input_file",
  "input_sheet",
  "group_column",
  "output_dir",
  "temp_dir",
  "output_stem",
  "metric_columns",
  "plot_titles",
  "axis_labels",
  "expected_sha256",
  "expected_group_counts",
  "rscript_path",
  "python_executable"
)
missing_config_fields <- setdiff(
  required_config_fields,
  names(user_config)
)
if (length(missing_config_fields) > 0L) {
  stop(
    "配置缺少字段：",
    paste(missing_config_fields, collapse = "、")
  )
}
if (!grepl(
  "^[A-Fa-f0-9]{64}$",
  as.character(user_config$expected_sha256)
)) {
  stop("expected_sha256 必须是源文件的 64 位 SHA256。")
}
if (
  !nzchar(as.character(user_config$input_sheet)) ||
    !nzchar(as.character(user_config$group_column)) ||
    !nzchar(as.character(user_config$output_stem))
) {
  stop("input_sheet、group_column 和 output_stem 不得为空。")
}
input_skip_value <- if (is.null(user_config$input_skip)) {
  0
} else {
  as.numeric(user_config$input_skip)
}
input_range <- if (
  is.null(user_config$input_range) ||
    !nzchar(trimws(as.character(user_config$input_range)))
) NULL else trimws(as.character(user_config$input_range))
if (
  length(input_skip_value) != 1L ||
    !is.finite(input_skip_value) ||
    input_skip_value < 0 ||
    input_skip_value != floor(input_skip_value)
) {
  stop("input_skip 必须是非负整数。")
}
input_skip <- as.integer(input_skip_value)
if (!is.null(input_range) && input_skip != 0L) {
  stop("input_range 与非零 input_skip 不能同时使用。")
}
if (is.null(input_range)) {
  source_header_row <- input_skip + 1L
} else {
  if (grepl("!", input_range, fixed = TRUE)) {
    range_sheet <- sub("!.*$", "", input_range)
    range_sheet <- sub("^'(.*)'$", "\\1", range_sheet)
    range_sheet <- gsub("''", "'", range_sheet, fixed = TRUE)
    if (!identical(range_sheet, as.character(user_config$input_sheet))) {
      stop("input_range 的工作表前缀必须与 input_sheet 完全一致。")
    }
  }
  cell_range <- sub("^.*!", "", input_range)
  range_match <- regexec(
    "^\\$?[A-Za-z]+\\$?([0-9]+):",
    cell_range
  )
  range_parts <- regmatches(cell_range, range_match)[[1]]
  if (length(range_parts) != 2L) {
    stop("input_range 必须是含标题行的 Excel 范围，如 A3:F33。")
  }
  source_header_row <- as.integer(range_parts[[2]])
}

output_root <- normalizePath(
  user_config$output_dir,
  winslash = "/",
  mustWork = FALSE
)
temp_dir <- normalizePath(
  user_config$temp_dir,
  winslash = "/",
  mustWork = FALSE
)

# 输出保护同时存在于启动器和本 R 脚本，避免用户直接运行交付 R 时
# 绕过“不得覆盖历史结果”的边界。只有空目录、由启动器已验证的当前批次，
# 或五个既有文件可由除 expected_branches 外相同的配置证明属于当前批次
# 时才允许继续。
output_stem_for_guard <- as.character(user_config$output_stem)
expected_output_names_for_guard <- c(
  paste0(output_stem_for_guard, "_箱线图总图.pdf"),
  paste0(output_stem_for_guard, "_残差Q-Q诊断.pdf"),
  paste0(output_stem_for_guard, "_统计分析汇总.xlsx"),
  paste0(
    output_stem_for_guard,
    "_绘图与统计分析_残差诊断四分流.R"
  ),
  paste0(output_stem_for_guard, "_分析配置.json")
)
normalize_config_identity <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  if (is.null(names(value))) {
    return(lapply(value, normalize_config_identity))
  }
  ordered_names <- sort(names(value))
  stats::setNames(
    lapply(value[ordered_names], normalize_config_identity),
    ordered_names
  )
}
config_identity <- function(value) {
  value$expected_branches <- NULL
  normalize_config_identity(value)
}
existing_entries <- if (dir.exists(output_root)) {
  list.files(
    output_root,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}
existing_directories <- existing_entries[file.info(existing_entries)$isdir %in% TRUE]
if (length(existing_directories) > 0L) {
  stop("输出目录含子目录，拒绝覆盖：", output_root)
}
existing_files <- existing_entries[file.info(existing_entries)$isdir %in% FALSE]
unexpected_files <- setdiff(basename(existing_files), expected_output_names_for_guard)
if (length(unexpected_files) > 0L) {
  stop(
    "输出目录含非本流程文件，拒绝覆盖：",
    paste(unexpected_files, collapse = "、")
  )
}
if (length(existing_files) > 0L) {
  ownership_marker <- file.path(temp_dir, "current_output_owner.sha256")
  launcher_token <- Sys.getenv("THREE_ECOSYSTEM_OUTPUT_GUARD", unset = "")
  marker_lines <- if (file.exists(ownership_marker)) {
    readLines(ownership_marker, warn = FALSE)
  } else {
    character(0)
  }
  marker_token <- if (length(marker_lines) == 1L) {
    toupper(trimws(marker_lines[[1]]))
  } else {
    ""
  }
  launcher_authorized <- (
    nzchar(launcher_token) &&
      identical(toupper(launcher_token), config_file_sha256) &&
      identical(marker_token, config_file_sha256)
  )
  delivered_config_for_guard <- file.path(
    output_root,
    paste0(output_stem_for_guard, "_分析配置.json")
  )
  same_config_authorized <- FALSE
  if (file.exists(delivered_config_for_guard)) {
    existing_config_raw <- jsonlite::read_json(
      delivered_config_for_guard,
      simplifyVector = FALSE
    )
    same_config_authorized <- identical(
      config_identity(existing_config_raw),
      config_identity(user_config_raw)
    )
  }
  if (!launcher_authorized && !same_config_authorized) {
    stop("既有输出无法证明属于同一配置，拒绝覆盖：", output_root)
  }
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

rscript_path <- normalizePath(
  user_config$rscript_path,
  winslash = "/",
  mustWork = TRUE
)
python_executable <- normalizePath(
  user_config$python_executable,
  winslash = "/",
  mustWork = TRUE
)
expected_python_version <- if (
  is.null(user_config$expected_python_version)
) NULL else as.character(user_config$expected_python_version)
expected_scipy_version <- if (
  is.null(user_config$expected_scipy_version)
) NULL else as.character(user_config$expected_scipy_version)

alpha_level <- 0.05
treatment_levels <- c("Meadow", "Steppe", "desert")
treatment_x <- c(Meadow = 1, Steppe = 2, desert = 3)
treatment_colors <- c(
  Meadow = "#D97A73",
  Steppe = "#D8B74F",
  desert = "#65AFA2"
)
metric_levels <- unname(as.character(user_config$metric_columns))
if (
  length(metric_levels) < 1L ||
    anyNA(metric_levels) ||
    any(trimws(metric_levels) == "") ||
    anyDuplicated(metric_levels)
) {
  stop("metric_columns 必须是非空且不重复的指标列名。")
}
metric_plot_titles <- unlist(user_config$plot_titles, use.names = TRUE)
metric_axis_labels <- unlist(user_config$axis_labels, use.names = TRUE)
if (
  !setequal(names(metric_plot_titles), metric_levels) ||
    !setequal(names(metric_axis_labels), metric_levels)
) {
  stop("plot_titles 和 axis_labels 必须逐一覆盖 metric_columns。")
}
metric_plot_titles <- metric_plot_titles[metric_levels]
metric_axis_labels <- metric_axis_labels[metric_levels]

# 可选的内存派生指标。只允许四种可审计的逐行运算：sum、difference、
# ratio、product。若源表已经有同名列，必须直接使用源列并移除对应配置，
# 从而避免擅自重算源表已有比值。
derived_metric_specs <- user_config_raw$derived_metrics
if (is.null(derived_metric_specs)) {
  derived_metric_specs <- list()
}
if (!is.list(derived_metric_specs) || is.null(names(derived_metric_specs))) {
  if (length(derived_metric_specs) > 0L) {
    stop("derived_metrics 必须是以派生指标名命名的 JSON 对象。")
  }
}
derived_metric_names <- names(derived_metric_specs)
if (length(setdiff(derived_metric_names, metric_levels)) > 0L) {
  stop("derived_metrics 只能定义 metric_columns 中的指标。")
}
allowed_derived_operations <- c(
  "sum",
  "difference",
  "ratio",
  "product"
)
metric_source_notes <- stats::setNames(metric_levels, metric_levels)
for (derived_name in derived_metric_names) {
  specification <- derived_metric_specs[[derived_name]]
  operation <- as.character(specification$operation)
  source_columns <- unlist(specification$columns, use.names = FALSE)
  if (
    length(operation) != 1L ||
      !operation %in% allowed_derived_operations ||
      length(source_columns) < 2L ||
      anyNA(source_columns) ||
      any(trimws(source_columns) == "")
  ) {
    stop("派生指标 ", derived_name, " 的 operation 或 columns 不合法。")
  }
  if (
    operation %in% c("difference", "ratio") &&
      length(source_columns) != 2L
  ) {
    stop(operation, " 派生指标必须且只能提供两列。")
  }
  metric_source_notes[[derived_name]] <- paste0(
    "内存派生:",
    operation,
    "(",
    paste(source_columns, collapse = ","),
    ")"
  )
}

expected_group_counts <- unlist(
  user_config$expected_group_counts,
  use.names = TRUE
)
if (!setequal(names(expected_group_counts), treatment_levels)) {
  stop(
    "expected_group_counts 必须命名为 Meadow、Steppe、desert。"
  )
}
expected_group_counts <- as.integer(
  expected_group_counts[treatment_levels]
)
names(expected_group_counts) <- treatment_levels
if (anyNA(expected_group_counts) || any(expected_group_counts < 2L)) {
  stop("三个预期组样本量均须为至少 2 的整数。")
}

expected_branches <- NULL
if (!is.null(user_config$expected_branches)) {
  expected_branches <- unlist(
    user_config$expected_branches,
    use.names = TRUE
  )
  if (!setequal(names(expected_branches), metric_levels)) {
    stop("expected_branches 必须逐一覆盖 metric_columns。")
  }
  expected_branches <- expected_branches[metric_levels]
}

organism_config <- list(
  list(
    key = gsub("[^A-Za-z0-9_-]", "_", user_config$output_stem),
    organism = as.character(user_config$analysis_label),
    input_file = normalizePath(
      user_config$input_file,
      winslash = "/",
      mustWork = TRUE
    ),
    input_sheet = as.character(user_config$input_sheet),
    input_skip = input_skip,
    input_range = input_range,
    source_header_row = source_header_row,
    group_column = as.character(user_config$group_column),
    sample_id_column = if (
      is.null(user_config$sample_id_column) ||
        !nzchar(as.character(user_config$sample_id_column))
    ) NULL else as.character(user_config$sample_id_column),
    output_stem = as.character(user_config$output_stem),
    expected_sha256 = toupper(as.character(user_config$expected_sha256)),
    expected_group_counts = expected_group_counts,
    expected_branches = expected_branches
  )
)

command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", command_line, value = TRUE)
if (length(script_argument) != 1L) {
  stop("无法唯一确定当前 R 脚本路径。")
}
script_source <- normalizePath(
  sub("^--file=", "", script_argument),
  winslash = "/",
  mustWork = TRUE
)

# ------------------------------ 2. 依赖检查 ---------------------------------

required_packages <- c(
  "car",
  "digest",
  "dplyr",
  "emmeans",
  "FSA",
  "ggplot2",
  "ggpubr",
  "gridExtra",
  "jsonlite",
  "multcompView",
  "readxl",
  "sandwich",
  "tibble",
  "tidyr"
)
missing_packages <- required_packages[
  !vapply(
    required_packages,
    function(package_name) {
      suppressPackageStartupMessages(
        requireNamespace(package_name, quietly = TRUE)
      )
    },
    logical(1)
  )
]
if (length(missing_packages) > 0L) {
  stop("缺少 R 包：", paste(missing_packages, collapse = "、"))
}
if (!file.exists(rscript_path)) {
  stop("找不到锁定的 Rscript：", rscript_path)
}
if (!file.exists(python_executable)) {
  stop("找不到锁定的 Python：", python_executable)
}

# ------------------------------ 3. 通用函数 ---------------------------------

format_p_value <- function(p_value) {
  if (!is.finite(p_value)) {
    return("NA")
  }
  if (p_value < 0.001) {
    return(formatC(p_value, format = "e", digits = 2))
  }
  formatC(p_value, format = "f", digits = 3)
}

format_p_value_ascii <- function(p_value) {
  if (!is.finite(p_value)) {
    return("NA")
  }
  if (p_value < 0.0001) {
    return(formatC(p_value, format = "e", digits = 2))
  }
  formatC(p_value, format = "f", digits = 4)
}

significance_label <- function(p_value) {
  dplyr::case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

cliffs_delta <- function(x, y) {
  comparison_matrix <- outer(x, y, FUN = "-")
  (
    sum(comparison_matrix > 0) -
      sum(comparison_matrix < 0)
  ) / (length(x) * length(y))
}

bias_corrected_skewness <- function(x) {
  n <- length(x)
  centered <- x - mean(x)
  m2 <- mean(centered^2)
  m3 <- mean(centered^3)
  if (n < 3L || !is.finite(m2) || m2 <= 0) {
    return(NA_real_)
  }
  sqrt(n * (n - 1)) / (n - 2) * m3 / (m2^(3 / 2))
}

unbiased_excess_kurtosis <- function(x) {
  n <- length(x)
  centered <- x - mean(x)
  m2 <- mean(centered^2)
  m4 <- mean(centered^4)
  if (n < 4L || !is.finite(m2) || m2 <= 0) {
    return(NA_real_)
  }
  biased_excess <- m4 / (m2^2) - 3
  (n - 1) / ((n - 2) * (n - 3)) *
    ((n + 1) * biased_excess + 6)
}

run_python_strict <- function(code, arguments = character()) {
  result <- system2(
    python_executable,
    args = c("-c", shQuote(code), arguments),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(result, "status")
  if (!is.null(exit_status) && as.integer(exit_status) != 0L) {
    stop(
      "SciPy studentized-range 调用失败（status=", exit_status,
      "）：\n", paste(result, collapse = "\n")
    )
  }
  result
}

python_probe <- run_python_strict(paste(
  "import sys, scipy",
  "from scipy.stats import studentized_range",
  "print(sys.version.split()[0] + '\\t' + scipy.__version__)",
  sep = "\n"
))
if (length(python_probe) != 1L) {
  stop("Python/SciPy 探测输出异常。")
}
python_parts <- strsplit(python_probe, "\t", fixed = TRUE)[[1]]
python_version <- python_parts[[1]]
scipy_version <- python_parts[[2]]
if (is.null(expected_python_version)) {
  expected_python_version <- python_version
}
if (is.null(expected_scipy_version)) {
  expected_scipy_version <- scipy_version
}
if (
  !identical(python_version, expected_python_version) ||
    !identical(scipy_version, expected_scipy_version)
) {
  stop(
    "Python/SciPy 版本不匹配：当前 ", python_version, " / ",
    scipy_version, "；要求 ", expected_python_version, " / ",
    expected_scipy_version
  )
}

scipy_studentized_range <- function(q_values, df_values) {
  if (
    length(q_values) == 0L ||
      length(q_values) != length(df_values) ||
      any(!is.finite(c(q_values, df_values))) ||
      any(q_values < 0) ||
      any(df_values <= 0)
  ) {
    stop("SciPy studentized-range 输入不合法。")
  }
  python_code <- paste(
    "import sys, math, scipy",
    "from scipy.stats import studentized_range",
    paste0("expected = '", expected_scipy_version, "'"),
    "if scipy.__version__ != expected:",
    "    raise SystemExit('SciPy version mismatch')",
    "values = [float(value) for value in sys.argv[1:]]",
    "for index in range(0, len(values), 2):",
    "    q_value = values[index]",
    "    df_value = values[index + 1]",
    "    p_value = float(studentized_range.sf(q_value, 3, df_value))",
    "    q_critical = float(studentized_range.ppf(0.95, 3, df_value))",
    "    if not (math.isfinite(p_value) and math.isfinite(q_critical)):",
    "        raise SystemExit('Non-finite result')",
    "    print(f'{p_value:.17g}\\t{q_critical:.17g}')",
    sep = "\n"
  )
  arguments <- as.vector(rbind(
    format(q_values, digits = 17, scientific = TRUE, trim = TRUE),
    format(df_values, digits = 17, scientific = TRUE, trim = TRUE)
  ))
  output <- run_python_strict(python_code, arguments)
  parsed <- strsplit(output, "\t", fixed = TRUE)
  p_values <- as.numeric(vapply(parsed, `[[`, character(1), 1L))
  q_critical <- as.numeric(vapply(parsed, `[[`, character(1), 2L))
  if (
    length(p_values) != length(q_values) ||
      any(!is.finite(c(p_values, q_critical))) ||
      any(p_values < 0 | p_values > 1)
  ) {
    stop("SciPy studentized-range 返回结果不合法。")
  }
  tibble::tibble(
    P_SciPy_sf = p_values,
    Q_critical_SciPy_ppf = q_critical
  )
}

make_compact_letters <- function(pairwise_data, center_values) {
  named_p_values <- stats::setNames(
    pairwise_data$P_adjusted,
    paste(pairwise_data$group1, pairwise_data$group2, sep = "-")
  )
  raw_letters <- multcompView::multcompLetters(
    named_p_values,
    threshold = alpha_level,
    compare = "<",
    Letters = LETTERS
  )$Letters

  used_letters <- unique(unlist(strsplit(
    unname(raw_letters), "", fixed = TRUE
  )))
  letter_priority <- vapply(used_letters, function(letter_name) {
    groups_with_letter <- names(raw_letters)[
      grepl(letter_name, unname(raw_letters), fixed = TRUE)
    ]
    max(unname(center_values[groups_with_letter]))
  }, numeric(1))
  ordered_old_letters <- used_letters[
    order(-letter_priority, match(used_letters, LETTERS))
  ]
  letter_map <- stats::setNames(
    LETTERS[seq_along(ordered_old_letters)],
    ordered_old_letters
  )
  normalized <- vapply(raw_letters, function(letter_string) {
    old_characters <- strsplit(letter_string, "", fixed = TRUE)[[1]]
    new_characters <- unname(letter_map[old_characters])
    paste0(
      new_characters[order(match(new_characters, LETTERS))],
      collapse = ""
    )
  }, character(1))
  normalized[treatment_levels]
}

letters_overlap <- function(first_letters, second_letters) {
  first_characters <- strsplit(first_letters, "", fixed = TRUE)[[1]]
  second_characters <- strsplit(second_letters, "", fixed = TRUE)[[1]]
  any(first_characters %in% second_characters)
}

comparison_plan <- tibble::tribble(
  ~comparison_rank, ~group1, ~group2,
  1L, "Meadow", "Steppe",
  2L, "Meadow", "desert",
  3L, "Steppe", "desert"
)

contrast_list <- list(
  "Meadow - Steppe" = c(1, -1, 0),
  "Meadow - desert" = c(1, 0, -1),
  "Steppe - desert" = c(0, 1, -1)
)

# ------------------------------ 4. 单指标分析 -------------------------------

analyze_metric <- function(metric_data, metric_name) {
  ordinary_model <- stats::lm(Value ~ Treatment, data = metric_data)
  ordinary_summary <- summary(ordinary_model)
  model_residuals <- stats::residuals(ordinary_model)

  shapiro_result <- stats::shapiro.test(model_residuals)
  residual_shapiro_p <- unname(shapiro_result$p.value)

  levene_result <- car::leveneTest(
    y = model_residuals,
    group = metric_data$Treatment,
    center = mean
  )
  levene_f <- unname(levene_result[["F value"]][[1]])
  levene_p <- unname(levene_result[["Pr(>F)"]][[1]])

  group_residual_means <- tapply(
    model_residuals,
    metric_data$Treatment,
    mean
  )
  absolute_deviation <- abs(
    model_residuals -
      as.numeric(group_residual_means[as.character(metric_data$Treatment)])
  )
  manual_levene_model <- stats::lm(
    absolute_deviation ~ metric_data$Treatment
  )
  manual_levene_table <- stats::anova(manual_levene_model)
  manual_levene_f <- unname(
    manual_levene_table[["F value"]][[1]]
  )
  manual_levene_p <- unname(
    manual_levene_table[["Pr(>F)"]][[1]]
  )
  if (
    abs(levene_f - manual_levene_f) > 1e-10 ||
      abs(levene_p - manual_levene_p) > 1e-10
  ) {
    stop(metric_name, " 的 Levene 两种实现不一致。")
  }

  residual_skewness <- bias_corrected_skewness(model_residuals)
  residual_kurtosis <- unbiased_excess_kurtosis(model_residuals)
  max_abs_rstudent <- max(abs(stats::rstudent(ordinary_model)))
  group_sizes <- table(metric_data$Treatment)
  mild_non_normal_eligible <- (
    nrow(metric_data) >= 25L &&
      min(group_sizes) >= 5L &&
      abs(residual_skewness) <= 1 &&
      abs(residual_kurtosis) <= 2 &&
      max_abs_rstudent <= 3
  )

  if (residual_shapiro_p >= alpha_level && levene_p >= alpha_level) {
    final_branch <- "ANOVA_TUKEY"
    residual_class <- "未检出残差偏离正态且未检出方差异质"
  } else if (
    residual_shapiro_p >= alpha_level &&
      levene_p < alpha_level
  ) {
    final_branch <- "WELCH_GH"
    residual_class <- "未检出残差偏离正态但检出方差异质"
  } else if (mild_non_normal_eligible) {
    final_branch <- "HC3_HOLM"
    residual_class <- "检出残差偏离正态但满足预设稳健保护条件"
  } else {
    final_branch <- "KW_DUNN_BH"
    residual_class <- "检出残差偏离正态且不满足预设稳健保护条件"
  }

  anova_reference <- stats::anova(ordinary_model)
  p_anova_reference <- unname(anova_reference[["Pr(>F)"]][[1]])
  welch_reference <- stats::oneway.test(
    Value ~ Treatment,
    data = metric_data,
    var.equal = FALSE
  )
  p_welch_reference <- unname(welch_reference$p.value)
  kw_reference <- stats::kruskal.test(
    Value ~ Treatment,
    data = metric_data
  )
  p_kw_reference <- unname(kw_reference$p.value)

  hc3_covariance <- sandwich::vcovHC(ordinary_model, type = "HC3")
  car_anova_default <- utils::getS3method(
    "Anova",
    "default",
    envir = asNamespace("car")
  )
  hc3_reference <- car_anova_default(
    ordinary_model,
    type = 2,
    test.statistic = "F",
    vcov. = hc3_covariance
  )
  p_hc3_reference <- unname(hc3_reference[["Pr(>F)"]][[1]])

  r_squared_cat <- unname(ordinary_summary$r.squared)
  adjusted_r_squared <- unname(ordinary_summary$adj.r.squared)

  if (final_branch == "ANOVA_TUKEY") {
    overall_test <- "One-way ANOVA"
    statistic_name <- "F"
    overall_statistic <- unname(anova_reference[["F value"]][[1]])
    df1 <- unname(anova_reference$Df[[1]])
    df2 <- unname(anova_reference$Df[[2]])
    overall_p_raw <- p_anova_reference
    ss_between <- unname(anova_reference[["Sum Sq"]][[1]])
    ss_within <- unname(anova_reference[["Sum Sq"]][[2]])
    mse <- unname(anova_reference[["Mean Sq"]][[2]])
    eta_squared <- ss_between / (ss_between + ss_within)
    omega_squared <- max(
      0,
      (ss_between - df1 * mse) /
        (ss_between + ss_within + mse)
    )
    epsilon_squared_kw <- NA_real_
    overall_effect_note <- "Eta²与Omega²仅来自普通ANOVA"
  } else if (final_branch == "WELCH_GH") {
    overall_test <- "Welch one-way ANOVA"
    statistic_name <- "Welch F"
    overall_statistic <- unname(welch_reference$statistic)
    df1 <- unname(welch_reference$parameter[["num df"]])
    df2 <- unname(welch_reference$parameter[["denom df"]])
    overall_p_raw <- p_welch_reference
    eta_squared <- NA_real_
    omega_squared <- NA_real_
    epsilon_squared_kw <- NA_real_
    overall_effect_note <- "Welch分支不套用普通ANOVA效应量"
  } else if (final_branch == "HC3_HOLM") {
    overall_test <- "HC3 robust omnibus F"
    statistic_name <- "HC3 F"
    overall_statistic <- unname(hc3_reference[["F"]][[1]])
    df1 <- unname(hc3_reference[["Df"]][[1]])
    df2 <- stats::df.residual(ordinary_model)
    overall_p_raw <- p_hc3_reference
    eta_squared <- NA_real_
    omega_squared <- NA_real_
    epsilon_squared_kw <- NA_real_
    overall_effect_note <- paste0(
      "总体F使用同一lm的HC3协方差；",
      "未套用普通ANOVA效应量"
    )
  } else {
    overall_test <- "Kruskal-Wallis"
    statistic_name <- "H (chi-squared)"
    overall_statistic <- unname(kw_reference$statistic)
    df1 <- unname(kw_reference$parameter)
    df2 <- NA_real_
    overall_p_raw <- p_kw_reference
    eta_squared <- NA_real_
    omega_squared <- NA_real_
    epsilon_squared_kw <- max(
      0,
      (
        overall_statistic -
          length(treatment_levels) + 1
      ) /
        (
          nrow(metric_data) -
            length(treatment_levels)
        )
    )
    overall_effect_note <- paste0(
      "epsilon-squared为KW效应量；结论针对分布或平均秩；",
      "R2(cat)仅为分类模型描述量"
    )
  }

  get_values <- function(group_name) {
    metric_data$Value[metric_data$Treatment == group_name]
  }

  if (final_branch == "ANOVA_TUKEY") {
    emmeans_object <- emmeans::emmeans(
      ordinary_model,
      ~ Treatment
    )
    # 必须使用 emmeans 内置的成套两两比较，才能得到真正的
    # Tukey studentized-range 家族校正。若把自定义对比列表传给
    # contrast()，emmeans 会自动切换校正方式，不符合本流程。
    contrast_object <- pairs(
      emmeans_object,
      reverse = FALSE,
      adjust = "tukey"
    )
    contrast_table <- as.data.frame(summary(
      contrast_object,
      infer = c(TRUE, TRUE),
      level = 0.95,
      adjust = "tukey"
    ))
    expected_contrasts <- paste(
      comparison_plan$group1,
      comparison_plan$group2,
      sep = " - "
    )
    stopifnot(
      nrow(contrast_table) == nrow(comparison_plan),
      identical(as.character(contrast_table$contrast), expected_contrasts)
    )
    pairwise_results <- comparison_plan |>
      dplyr::mutate(
        Pairwise_test = "Tukey HSD after one-way ANOVA",
        P_adjustment = "Tukey familywise adjustment",
        Estimate_type =
          "Arithmetic mean difference (group1 - group2)",
        Estimate = contrast_table$estimate,
        SE = contrast_table$SE,
        Test_statistic = contrast_table$t.ratio,
        Test_df = contrast_table$df,
        CI95_lower = contrast_table$lower.CL,
        CI95_upper = contrast_table$upper.CL,
        CI_type = "95% simultaneous Tukey CI",
        P_raw = 2 * stats::pt(
          abs(contrast_table$t.ratio),
          df = contrast_table$df,
          lower.tail = FALSE
        ),
        P_adjusted = contrast_table$p.value
      )
  } else if (final_branch == "WELCH_GH") {
    components <- lapply(
      seq_len(nrow(comparison_plan)),
      function(index) {
        group1 <- comparison_plan$group1[[index]]
        group2 <- comparison_plan$group2[[index]]
        x <- get_values(group1)
        y <- get_values(group2)
        estimate <- mean(x) - mean(y)
        se <- sqrt(
          stats::var(x) / length(x) +
            stats::var(y) / length(y)
        )
        df <- (
          stats::var(x) / length(x) +
            stats::var(y) / length(y)
        )^2 /
          (
            (stats::var(x) / length(x))^2 / (length(x) - 1) +
              (stats::var(y) / length(y))^2 / (length(y) - 1)
          )
        welch_t <- estimate / se
        list(
          estimate = estimate,
          se = se,
          df = df,
          welch_t = welch_t,
          q = sqrt(2) * abs(welch_t)
        )
      }
    )
    scipy_result <- scipy_studentized_range(
      q_values = vapply(components, `[[`, numeric(1), "q"),
      df_values = vapply(components, `[[`, numeric(1), "df")
    )
    pairwise_results <- comparison_plan |>
      dplyr::mutate(
        Pairwise_test = paste0(
          "Games-Howell after Welch one-way ANOVA ",
          "(SciPy exact)"
        ),
        P_adjustment = "Games-Howell; no extra correction",
        Estimate_type =
          "Arithmetic mean difference (group1 - group2)",
        Estimate = vapply(
          components,
          `[[`,
          numeric(1),
          "estimate"
        ),
        SE = vapply(components, `[[`, numeric(1), "se"),
        Test_statistic = vapply(
          components,
          `[[`,
          numeric(1),
          "q"
        ),
        Test_df = vapply(components, `[[`, numeric(1), "df"),
        CI95_lower = Estimate -
          scipy_result$Q_critical_SciPy_ppf * SE / sqrt(2),
        CI95_upper = Estimate +
          scipy_result$Q_critical_SciPy_ppf * SE / sqrt(2),
        CI_type = paste0(
          "95% simultaneous Games-Howell CI via SciPy"
        ),
        P_raw = 2 * stats::pt(
          abs(vapply(
            components,
            `[[`,
            numeric(1),
            "welch_t"
          )),
          df = Test_df,
          lower.tail = FALSE
        ),
        P_adjusted = scipy_result$P_SciPy_sf
      )
  } else if (final_branch == "HC3_HOLM") {
    emmeans_object <- emmeans::emmeans(
      ordinary_model,
      ~ Treatment,
      vcov. = hc3_covariance
    )
    contrast_object <- emmeans::contrast(
      emmeans_object,
      method = contrast_list,
      adjust = "none"
    )
    contrast_table <- as.data.frame(summary(
      contrast_object,
      infer = c(TRUE, TRUE),
      level = 0.95,
      adjust = "none"
    ))
    adjusted_p <- stats::p.adjust(
      contrast_table$p.value,
      method = "holm"
    )
    pairwise_results <- comparison_plan |>
      dplyr::mutate(
        Pairwise_test = paste0(
          "HC3 covariance contrasts after HC3 robust omnibus F"
        ),
        P_adjustment =
          "Holm within the three-comparison family",
        Estimate_type =
          "Arithmetic mean difference (group1 - group2)",
        Estimate = contrast_table$estimate,
        SE = contrast_table$SE,
        Test_statistic = contrast_table$t.ratio,
        Test_df = contrast_table$df,
        CI95_lower = contrast_table$lower.CL,
        CI95_upper = contrast_table$upper.CL,
        CI_type = "95% pointwise HC3 t CI; not Holm-adjusted",
        P_raw = contrast_table$p.value,
        P_adjusted = adjusted_p
      )
  } else {
    dunn_raw <- FSA::dunnTest(
      Value ~ Treatment,
      data = metric_data,
      method = "none"
    )$res
    mean_ranks <- tapply(
      rank(metric_data$Value, ties.method = "average"),
      metric_data$Treatment,
      mean
    )
    pairwise_results <- comparison_plan |>
      dplyr::rowwise() |>
      dplyr::mutate(
        Pairwise_test = paste0(
          "Dunn pairwise mean-rank comparison after ",
          "Kruskal-Wallis"
        ),
        P_adjustment = paste0(
          "Benjamini-Hochberg within the ",
          "three-comparison family"
        ),
        Estimate_type =
          "Mean-rank difference (group1 - group2)",
        Estimate = unname(
          mean_ranks[group1] -
            mean_ranks[group2]
        ),
        SE = {
          matching_row <- which(vapply(
            strsplit(
              dunn_raw$Comparison,
              " - ",
              fixed = TRUE
            ),
            function(parts) {
              setequal(parts, c(group1, group2))
            },
            logical(1)
          ))
          abs(Estimate / dunn_raw$Z[[matching_row]])
        },
        Test_statistic = Estimate / SE,
        Test_df = NA_real_,
        CI95_lower = NA_real_,
        CI95_upper = NA_real_,
        CI_type = paste0(
          "Dunn does not provide a confidence interval ",
          "in this workflow"
        ),
        P_raw = {
          matching_row <- which(vapply(
            strsplit(
              dunn_raw$Comparison,
              " - ",
              fixed = TRUE
            ),
            function(parts) {
              setequal(parts, c(group1, group2))
            },
            logical(1)
          ))
          dunn_raw$P.unadj[[matching_row]]
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        P_adjusted = stats::p.adjust(P_raw, method = "BH")
      )
  }

  pairwise_results <- pairwise_results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Cliffs_delta = cliffs_delta(
        get_values(group1),
        get_values(group2)
      ),
      Significance = significance_label(P_adjusted),
      P_label = paste0(
        dplyr::case_when(
          final_branch == "ANOVA_TUKEY" ~ "Tukey p = ",
          final_branch == "WELCH_GH" ~ "GH p = ",
          final_branch == "HC3_HOLM" ~ "HC3-Holm p = ",
          TRUE ~ "Dunn-BH p = "
        ),
        format_p_value(P_adjusted)
      ),
      xmin = unname(treatment_x[group1]),
      xmax = unname(treatment_x[group2])
    ) |>
    dplyr::ungroup()

  if (
    nrow(pairwise_results) != 3L ||
      any(!is.finite(pairwise_results$P_adjusted)) ||
      any(
        pairwise_results$P_adjusted < 0 |
          pairwise_results$P_adjusted > 1
      )
  ) {
    stop(metric_name, " 的两两比较未通过完整性检查。")
  }

  if (final_branch == "KW_DUNN_BH") {
    center_for_letters <- tapply(
      rank(metric_data$Value, ties.method = "average"),
      metric_data$Treatment,
      mean
    )
  } else {
    center_for_letters <- tapply(
      metric_data$Value,
      metric_data$Treatment,
      mean
    )
  }
  compact_letters <- make_compact_letters(
    pairwise_results,
    center_for_letters
  )
  if (any(is.na(compact_letters))) {
    stop(metric_name, " 的多重比较字母生成失败。")
  }
  for (index in seq_len(nrow(pairwise_results))) {
    row <- pairwise_results[index, ]
    overlap <- letters_overlap(
      compact_letters[[row$group1]],
      compact_letters[[row$group2]]
    )
    expected_overlap <- row$P_adjusted >= alpha_level
    if (!identical(overlap, expected_overlap)) {
      stop(metric_name, " 的字母与调整后 P 值不一致。")
    }
  }

  group_q1 <- tapply(
    metric_data$Value,
    metric_data$Treatment,
    stats::quantile,
    probs = 0.25,
    names = FALSE
  )
  group_q3 <- tapply(
    metric_data$Value,
    metric_data$Treatment,
    stats::quantile,
    probs = 0.75,
    names = FALSE
  )
  group_iqr <- group_q3 - group_q1
  group_lower <- group_q1 - 1.5 * group_iqr
  group_upper <- group_q3 + 1.5 * group_iqr

  outlier_data <- metric_data |>
    dplyr::mutate(
      Q1 = as.numeric(group_q1[as.character(Treatment)]),
      Q3 = as.numeric(group_q3[as.character(Treatment)]),
      IQR_value = as.numeric(group_iqr[as.character(Treatment)]),
      IQR_lower = as.numeric(group_lower[as.character(Treatment)]),
      IQR_upper = as.numeric(group_upper[as.character(Treatment)]),
      IQR_outlier = Value < IQR_lower | Value > IQR_upper
    )

  mean_ranks_all <- tapply(
    rank(metric_data$Value, ties.method = "average"),
    metric_data$Treatment,
    mean
  )
  descriptive_statistics <- metric_data |>
    dplyr::group_by(Treatment) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = mean(Value),
      SD = stats::sd(Value),
      SE = stats::sd(Value) / sqrt(dplyr::n()),
      Median = stats::median(Value),
      Q1 = stats::quantile(Value, 0.25, names = FALSE),
      Q3 = stats::quantile(Value, 0.75, names = FALSE),
      Minimum = min(Value),
      Maximum = max(Value),
      IQR_outlier_count = sum(
        Value <
          (
            stats::quantile(Value, 0.25, names = FALSE) -
              1.5 * stats::IQR(Value)
          ) |
          Value >
            (
              stats::quantile(Value, 0.75, names = FALSE) +
                1.5 * stats::IQR(Value)
            )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Mean_rank = as.numeric(
        mean_ranks_all[as.character(Treatment)]
      ),
      Letter = unname(
        compact_letters[as.character(Treatment)]
      )
    )

  group_variances <- tapply(
    metric_data$Value,
    metric_data$Treatment,
    stats::var
  )
  design_full_rank <- ordinary_model$rank ==
    ncol(stats::model.matrix(ordinary_model))

  list(
    metric = metric_name,
    data = metric_data,
    model = ordinary_model,
    residuals = model_residuals,
    final_branch = final_branch,
    residual_class = residual_class,
    shapiro_result = shapiro_result,
    shapiro_p = residual_shapiro_p,
    levene_f = levene_f,
    levene_p = levene_p,
    manual_levene_f = manual_levene_f,
    manual_levene_p = manual_levene_p,
    skewness = residual_skewness,
    kurtosis = residual_kurtosis,
    max_abs_rstudent = max_abs_rstudent,
    mild_non_normal_eligible = mild_non_normal_eligible,
    overall_test = overall_test,
    statistic_name = statistic_name,
    overall_statistic = overall_statistic,
    df1 = df1,
    df2 = df2,
    overall_p_raw = overall_p_raw,
    eta_squared = eta_squared,
    omega_squared = omega_squared,
    epsilon_squared_kw = epsilon_squared_kw,
    overall_effect_note = overall_effect_note,
    r_squared_cat = r_squared_cat,
    adjusted_r_squared = adjusted_r_squared,
    p_anova_reference = p_anova_reference,
    p_welch_reference = p_welch_reference,
    p_hc3_reference = p_hc3_reference,
    p_kw_reference = p_kw_reference,
    group_variances = group_variances,
    design_full_rank = design_full_rank,
    pairwise = pairwise_results,
    letters = compact_letters,
    descriptive = descriptive_statistics,
    outliers = outlier_data
  )
}

# ------------------------------ 5. 主图函数 ---------------------------------

make_metric_plot <- function(result, overall_p_bh, seed_offset) {
  metric_data <- result$data |>
    dplyr::mutate(
      Order = unname(treatment_x[as.character(Treatment)])
    )
  metric_centers <- result$descriptive |>
    dplyr::mutate(
      Order = unname(treatment_x[as.character(Treatment)]),
      Center = Mean
    )

  data_min <- min(metric_data$Value)
  data_max <- max(metric_data$Value)
  data_span <- data_max - data_min
  if (!is.finite(data_span) || data_span <= 0) {
    data_span <- max(abs(data_max), 1)
  }

  pairwise_plot <- result$pairwise |>
    dplyr::mutate(
      y.position = data_max +
        data_span *
          (0.10 + 0.14 * (comparison_rank - 1L))
    )
  letter_plot <- result$descriptive |>
    dplyr::mutate(
      Order = unname(treatment_x[as.character(Treatment)]),
      y.position = data_max + data_span * 0.035
    )

  omnibus_short_name <- dplyr::case_when(
    result$overall_test == "One-way ANOVA" ~ "ANOVA",
    result$overall_test == "Welch one-way ANOVA" ~ "Welch",
    result$overall_test == "HC3 robust omnibus F" ~ "HC3 F",
    TRUE ~ "Kruskal-Wallis"
  )
  method_subtitle <- dplyr::case_when(
    result$final_branch == "ANOVA_TUKEY" ~
      "ANOVA + Tukey HSD",
    result$final_branch == "WELCH_GH" ~
      "Welch ANOVA + Games-Howell",
    result$final_branch == "HC3_HOLM" ~
      "HC3 robust F + HC3 contrasts (Holm)",
    TRUE ~ "Kruskal-Wallis + Dunn (BH)"
  )
  model_label <- paste0(
    "R²(cat) = ",
    formatC(result$r_squared_cat, format = "f", digits = 2),
    "; ", omnibus_short_name,
    "; q(BH) = ", format_p_value(overall_p_bh)
  )

  ggplot2::ggplot(
    metric_data,
    ggplot2::aes(x = Order, y = Value, group = Treatment)
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = Treatment),
      width = 0.36,
      alpha = 0.25,
      outlier.shape = NA,
      linewidth = 0.65,
      colour = "#343434"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = Treatment),
      position = ggplot2::position_jitter(
        width = 0.055,
        height = 0,
        seed = 20260804 + seed_offset
      ),
      shape = 21,
      size = 2.35,
      alpha = 0.82,
      colour = "#595959",
      stroke = 0.45
    ) +
    ggplot2::geom_line(
      data = metric_centers,
      ggplot2::aes(x = Order, y = Center, group = 1),
      inherit.aes = FALSE,
      colour = "#D75452",
      linewidth = 0.72,
      linetype = "22"
    ) +
    ggplot2::geom_point(
      data = metric_centers,
      ggplot2::aes(x = Order, y = Center),
      inherit.aes = FALSE,
      shape = 23,
      size = 2.75,
      fill = "#D75452",
      colour = "white",
      stroke = 0.60
    ) +
    ggpubr::stat_pvalue_manual(
      pairwise_plot,
      label = "P_label",
      y.position = "y.position",
      xmin = "xmin",
      xmax = "xmax",
      tip.length = 0.012,
      bracket.size = 0.40,
      label.size = 3.0,
      family = "DejaVu Sans",
      hide.ns = FALSE,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = letter_plot,
      ggplot2::aes(
        x = Order,
        y = y.position,
        label = Letter
      ),
      inherit.aes = FALSE,
      family = "DejaVu Sans",
      fontface = "bold",
      size = 4.0,
      colour = "#222222"
    ) +
    ggplot2::annotate(
      "text",
      x = 1,
      y = data_min - data_span * 0.13,
      label = model_label,
      hjust = 0,
      vjust = 1,
      size = 3.0,
      colour = "#333333",
      family = "DejaVu Sans"
    ) +
    ggplot2::scale_x_continuous(
      breaks = unname(treatment_x),
      labels = treatment_levels,
      limits = c(0.66, 3.34)
    ) +
    ggplot2::scale_fill_manual(
      values = treatment_colors,
      guide = "none"
    ) +
    ggplot2::labs(
      title = unname(metric_plot_titles[[result$metric]]),
      subtitle = method_subtitle,
      x = "Ecosystem",
      y = unname(metric_axis_labels[[result$metric]])
    ) +
    ggplot2::coord_cartesian(
      ylim = c(
        data_min - data_span * 0.22,
        data_max + data_span * 0.50
      ),
      clip = "off"
    ) +
    ggplot2::theme_classic(
      base_size = 11.5,
      base_family = "DejaVu Sans"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13.2,
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.1,
        hjust = 0.5,
        colour = "#4D4D4D"
      ),
      axis.title = ggplot2::element_text(size = 10.8),
      axis.text = ggplot2::element_text(
        size = 9.8,
        colour = "#222222"
      ),
      axis.line = ggplot2::element_line(linewidth = 0.65),
      axis.ticks = ggplot2::element_line(linewidth = 0.50),
      panel.grid.major.y = ggplot2::element_line(
        colour = "#E8E8E8",
        linewidth = 0.42
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 10,
        r = 12,
        b = 7,
        l = 8
      )
    )
}

set_fixed_panel_size <- function(plot_object) {
  target_panel_width_pt <- 263.645
  target_panel_height_pt <- 222.34
  plot_grob <- ggplot2::ggplotGrob(plot_object)
  panel_layout <- which(plot_grob$layout$name == "panel")
  if (length(panel_layout) != 1L) {
    stop("每个指标图必须且只能包含一个坐标面板。")
  }
  panel_column <- plot_grob$layout$l[[panel_layout]]
  panel_row <- plot_grob$layout$t[[panel_layout]]
  plot_grob$widths[[panel_column]] <- grid::unit(
    target_panel_width_pt / 72,
    "in"
  )
  plot_grob$heights[[panel_row]] <- grid::unit(
    target_panel_height_pt / 72,
    "in"
  )
  measured_width_pt <- 72 * grid::convertWidth(
    plot_grob$widths[[panel_column]],
    "in",
    valueOnly = TRUE
  )
  measured_height_pt <- 72 * grid::convertHeight(
    plot_grob$heights[[panel_row]],
    "in",
    valueOnly = TRUE
  )
  if (
    abs(measured_width_pt - target_panel_width_pt) > 1e-8 ||
      abs(measured_height_pt - target_panel_height_pt) > 1e-8
  ) {
    stop("坐标面板绝对尺寸锁定失败。")
  }
  plot_grob
}

# ------------------------------ 6. 全指标流程 -------------------------------

analyze_organism <- function(config, organism_index) {
  output_dir <- output_root
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  output_figure_pdf <- file.path(
    output_dir,
    paste0(config$output_stem, "_箱线图总图.pdf")
  )
  output_diagnostic_pdf <- file.path(
    output_dir,
    paste0(config$output_stem, "_残差Q-Q诊断.pdf")
  )
  output_excel <- file.path(
    output_dir,
    paste0(config$output_stem, "_统计分析汇总.xlsx")
  )
  output_script <- file.path(
    output_dir,
    paste0(
      config$output_stem,
      "_绘图与统计分析_残差诊断四分流.R"
    )
  )
  output_config <- file.path(
    output_dir,
    paste0(config$output_stem, "_分析配置.json")
  )
  summary_json <- file.path(
    temp_dir,
    paste0(config$output_stem, "_七表数据.json")
  )

  if (!file.exists(config$input_file)) {
    stop("找不到源文件：", config$input_file)
  }
  input_sha256_before <- toupper(digest::digest(
    file = config$input_file,
    algo = "sha256",
    serialize = FALSE
  ))
  if (!identical(input_sha256_before, config$expected_sha256)) {
    stop(
      config$organism,
      "源文件 SHA256 与锁定值不一致。\n当前：",
      input_sha256_before,
      "\n锁定：",
      config$expected_sha256
    )
  }

  if (!config$input_sheet %in% readxl::excel_sheets(config$input_file)) {
    stop("源工作簿缺少工作表：", config$input_sheet)
  }
  source_data <- if (is.null(config$input_range)) {
    readxl::read_excel(
      config$input_file,
      sheet = config$input_sheet,
      skip = config$input_skip,
      .name_repair = "minimal"
    )
  } else {
    readxl::read_excel(
      config$input_file,
      sheet = config$input_sheet,
      range = config$input_range,
      .name_repair = "minimal"
    )
  }
  if (anyDuplicated(names(source_data))) {
    stop("所选数据范围含重复表头，无法安全映射指标列。")
  }
  source_rows <- config$source_header_row + seq_len(nrow(source_data))
  direct_metric_levels <- setdiff(
    metric_levels,
    derived_metric_names
  )
  derived_source_columns <- unique(unlist(lapply(
    derived_metric_specs,
    function(specification) {
      unlist(specification$columns, use.names = FALSE)
    }
  ), use.names = FALSE))
  if (length(intersect(derived_metric_names, names(source_data))) > 0L) {
    stop(
      "源表已有以下指标列，必须直接使用而不得在内存中重算：",
      paste(
        intersect(derived_metric_names, names(source_data)),
        collapse = "、"
      )
    )
  }
  required_columns <- unique(c(
    config$group_column,
    config$sample_id_column,
    direct_metric_levels,
    derived_source_columns
  ))
  missing_columns <- setdiff(required_columns, names(source_data))
  if (length(missing_columns) > 0L) {
    stop(
      config$organism,
      "源工作表缺少列：",
      paste(missing_columns, collapse = "、")
    )
  }
  numeric_source_columns <- unique(c(
    direct_metric_levels,
    derived_source_columns
  ))
  for (column_name in numeric_source_columns) {
    if (!is.numeric(source_data[[column_name]])) {
      stop("源指标或派生基础列不是数值型：", column_name)
    }
    if (
      anyNA(source_data[[column_name]]) ||
        any(!is.finite(source_data[[column_name]]))
    ) {
      stop("源指标或派生基础列含缺失值或非有限值：", column_name)
    }
  }
  for (derived_name in derived_metric_names) {
    specification <- derived_metric_specs[[derived_name]]
    operation <- as.character(specification$operation)
    source_columns <- unlist(
      specification$columns,
      use.names = FALSE
    )
    source_values <- lapply(
      source_columns,
      function(column_name) source_data[[column_name]]
    )
    derived_values <- switch(
      operation,
      sum = Reduce(`+`, source_values),
      difference = source_values[[1]] - source_values[[2]],
      ratio = {
        if (any(source_values[[2]] == 0)) {
          stop("派生比值 ", derived_name, " 的分母含 0。")
        }
        source_values[[1]] / source_values[[2]]
      },
      product = Reduce(`*`, source_values),
      stop("不支持的派生操作：", operation)
    )
    if (anyNA(derived_values) || any(!is.finite(derived_values))) {
      stop("派生指标含缺失值或非有限值：", derived_name)
    }
    source_data[[derived_name]] <- as.numeric(derived_values)
  }
  if (
    nrow(source_data) != sum(config$expected_group_counts) ||
      any(is.na(source_data[[config$group_column]])) ||
      any(trimws(source_data[[config$group_column]]) == "")
  ) {
    stop(config$organism, "样本行数或分组标签异常。")
  }

  source_ecosystem <- trimws(as.character(
    source_data[[config$group_column]]
  ))
  mapped_treatment <- dplyr::case_when(
    source_ecosystem == "Meadow" ~ "Meadow",
    source_ecosystem == "Steppe" ~ "Steppe",
    source_ecosystem %in% c("Desert", "desert") ~ "desert",
    TRUE ~ NA_character_
  )
  if (any(is.na(mapped_treatment))) {
    stop(
      config$organism,
      "存在无法映射的 ecosystem：",
      paste(
        unique(source_ecosystem[is.na(mapped_treatment)]),
        collapse = "、"
      )
    )
  }
  treatment_factor <- factor(
    mapped_treatment,
    levels = treatment_levels
  )
  observed_group_counts <- as.integer(table(treatment_factor))
  names(observed_group_counts) <- treatment_levels
  if (!identical(observed_group_counts, config$expected_group_counts)) {
    stop(
      config$organism,
      "三个生态系统样本量与配置不一致；当前 ",
      paste(
        names(observed_group_counts),
        observed_group_counts,
        sep = "=",
        collapse = "、"
      )
    )
  }

  numeric_data <- as.data.frame(
    source_data[metric_levels],
    check.names = FALSE
  )
  for (column_name in metric_levels) {
    if (!is.numeric(numeric_data[[column_name]])) {
      stop("指标列不是数值型：", column_name)
    }
  }
  if (
    anyNA(numeric_data) ||
      any(!is.finite(as.matrix(numeric_data)))
  ) {
    stop(config$organism, "请求指标存在缺失值或非有限值。")
  }

  if (is.null(config$sample_id_column)) {
    sample_ids <- paste0("SourceRow_", source_rows)
    sample_id_note <- paste0(
      "源表无样本ID；以内存生成的SourceRow追踪源行，",
      "无法据此验证统计独立性"
    )
  } else {
    sample_ids <- trimws(as.character(
      source_data[[config$sample_id_column]]
    ))
    if (
      anyNA(sample_ids) ||
        any(sample_ids == "") ||
        anyDuplicated(sample_ids)
    ) {
      stop("样本 ID 列含缺失、空值或重复值。")
    }
    sample_id_note <- paste0(
      "使用源列 ", config$sample_id_column,
      " 追踪样本；独立性仍须由试验设计确认"
    )
  }

  analysis_wide <- tibble::as_tibble(numeric_data) |>
    dplyr::mutate(
      Source_row = source_rows,
      Sample_ID = sample_ids,
      Treatment_original = source_ecosystem,
      Treatment = treatment_factor,
      .before = 1
    )
  analysis_long <- analysis_wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_levels),
      names_to = "Metric",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Metric = factor(Metric, levels = metric_levels)
    )

  metric_results <- lapply(metric_levels, function(metric_name) {
    metric_data <- analysis_long |>
      dplyr::filter(Metric == metric_name) |>
      dplyr::select(
        Source_row,
        Sample_ID,
        Treatment_original,
        Treatment,
        Value
      )
    analyze_metric(metric_data, metric_name)
  })
  names(metric_results) <- metric_levels

  observed_branches <- vapply(
    metric_results,
    `[[`,
    character(1),
    "final_branch"
  )
  if (
    !is.null(config$expected_branches) &&
      !identical(observed_branches, config$expected_branches)
  ) {
    stop(
      config$organism,
      "分支与锁定结果不一致：",
      paste(
        names(observed_branches),
        observed_branches,
        sep = "=",
        collapse = "；"
      )
    )
  }

  overall_p_raw <- vapply(
    metric_results,
    `[[`,
    numeric(1),
    "overall_p_raw"
  )
  overall_p_bh <- stats::p.adjust(
    overall_p_raw,
    method = "BH"
  )

  # ---------------------------- 6.1 全指标总图 -----------------------------

  plot_objects <- lapply(seq_along(metric_levels), function(index) {
    metric_name <- metric_levels[[index]]
    make_metric_plot(
      metric_results[[metric_name]],
      overall_p_bh[[metric_name]],
      seed_offset = organism_index * 100L + index
    )
  })

  measurement_pdf <- tempfile(
    pattern = paste0(config$key, "_panel_measure_"),
    tmpdir = temp_dir,
    fileext = ".pdf"
  )
  grDevices::cairo_pdf(
    filename = measurement_pdf,
    width = 20,
    height = 20,
    family = "DejaVu Sans"
  )
  fixed_grobs <- lapply(plot_objects, set_fixed_panel_size)
  grob_widths_in <- vapply(fixed_grobs, function(plot_grob) {
    grid::convertWidth(
      sum(plot_grob$widths),
      "in",
      valueOnly = TRUE
    )
  }, numeric(1))
  grob_heights_in <- vapply(fixed_grobs, function(plot_grob) {
    grid::convertHeight(
      sum(plot_grob$heights),
      "in",
      valueOnly = TRUE
    )
  }, numeric(1))
  common_width_in <- max(grob_widths_in)
  common_height_in <- max(grob_heights_in)
  plot_columns <- min(3L, length(metric_levels))
  plot_rows <- ceiling(length(metric_levels) / plot_columns)

  caption_text <- paste0(
    "Brackets: branch-specific adjusted pairwise p values. ",
    "Letters: shared letters indicate adjusted p >= 0.05. ",
    "Red diamonds/line: arithmetic means; all ",
    sum(config$expected_group_counts),
    " observations per indicator retained. ",
    "Residual diagnostics used pooled lm residuals."
  )
  wrapped_caption <- paste(
    strwrap(
      caption_text,
      width = 52L * plot_columns
    ),
    collapse = "\n"
  )
  global_caption <- grid::textGrob(
    wrapped_caption,
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 8.0,
      col = "#555555",
      fontfamily = "DejaVu Sans",
      lineheight = 1.05
    )
  )
  combined_grob <- gridExtra::arrangeGrob(
    grobs = fixed_grobs,
    ncol = plot_columns,
    widths = grid::unit(
      rep(common_width_in, plot_columns),
      "in"
    ),
    heights = grid::unit(
      rep(common_height_in, plot_rows),
      "in"
    ),
    bottom = global_caption,
    padding = grid::unit(6, "pt")
  )
  output_width_in <- grid::convertWidth(
    sum(combined_grob$widths),
    "in",
    valueOnly = TRUE
  )
  output_height_in <- grid::convertHeight(
    sum(combined_grob$heights),
    "in",
    valueOnly = TRUE
  )
  grDevices::dev.off()
  unlink(measurement_pdf)

  grDevices::cairo_pdf(
    filename = output_figure_pdf,
    width = output_width_in,
    height = output_height_in,
    family = "DejaVu Sans"
  )
  grid::grid.newpage()
  grid::grid.draw(combined_grob)
  grDevices::dev.off()

  # ---------------------------- 6.2 残差 Q-Q 图 ----------------------------

  grDevices::cairo_pdf(
    filename = output_diagnostic_pdf,
    width = 5 * plot_columns,
    height = 4.25 * plot_rows,
    family = "Microsoft YaHei"
  )
  graphics::par(
    mfrow = c(plot_rows, plot_columns),
    mar = c(4.0, 4.1, 4.6, 1.1),
    oma = c(0.5, 0.5, 0.8, 0.5),
    cex = 0.88
  )
  for (metric_name in metric_levels) {
    result <- metric_results[[metric_name]]
    stats::qqnorm(
      result$residuals,
      main = paste0(
        unname(metric_plot_titles[[metric_name]]),
        " pooled lm residuals\n",
        "Shapiro-Wilk p = ",
        format_p_value_ascii(result$shapiro_p),
        "\n",
        result$residual_class
      ),
      pch = 21,
      bg = "#4F81BD",
      col = "#3E3E3E",
      cex = 0.90,
      xlab = "Theoretical quantiles",
      ylab = "Model residual quantiles",
      cex.main = 0.82
    )
    stats::qqline(
      result$residuals,
      col = "#D75452",
      lwd = 1.3
    )
    graphics::grid(
      col = "#E8E8E8",
      lty = 1,
      lwd = 0.55
    )
  }
  grDevices::dev.off()

  # ---------------------------- 6.3 七张统计表 -----------------------------

  analysis_overview <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      tibble::tibble(
        指标 = metric_name,
        源文件 = basename(config$input_file),
        源列 = unname(metric_source_notes[[metric_name]]),
        残差类别 = result$residual_class,
        最终分支 = result$final_branch,
        总体检验 = result$overall_test,
        统计量名称 = result$statistic_name,
        统计量 = result$overall_statistic,
        分子自由度或df = result$df1,
        分母自由度 = result$df2,
        P原始 = result$overall_p_raw,
        P_BH_同类指标 = overall_p_bh[[metric_name]],
        `R2_cat_描述性` = result$r_squared_cat,
        `Adjusted_R2_描述性` = result$adjusted_r_squared,
        Eta_squared = result$eta_squared,
        Omega_squared = result$omega_squared,
        Epsilon_squared_KW = result$epsilon_squared_kw,
        总体效应量说明 = result$overall_effect_note,
        残差ShapiroP = result$shapiro_p,
        均值中心LeveneP = result$levene_p,
        IQR异常点数 = sum(result$outliers$IQR_outlier)
      )
    }
  ))

  pairwise_export <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      result$pairwise |>
        dplyr::transmute(
          指标 = metric_name,
          组1 = group1,
          组2 = group2,
          比较方向 = paste(group1, group2, sep = " - "),
          两两检验 = Pairwise_test,
          P值校正 = P_adjustment,
          估计量类型 = Estimate_type,
          估计量 = Estimate,
          标准误 = SE,
          检验统计量 = Test_statistic,
          自由度 = Test_df,
          `95CI下限` = CI95_lower,
          `95CI上限` = CI95_upper,
          置信区间类型 = CI_type,
          P原始 = P_raw,
          P调整后 = P_adjusted,
          显著性 = Significance,
          `Cliffs_delta` = Cliffs_delta
        )
    }
  ))

  descriptive_export <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      result$descriptive |>
        dplyr::transmute(
          指标 = metric_name,
          处理 = as.character(Treatment),
          N = N,
          算术均值 = Mean,
          标准差 = SD,
          标准误 = SE,
          中位数 = Median,
          Q1 = Q1,
          Q3 = Q3,
          最小值 = Minimum,
          最大值 = Maximum,
          平均秩 = Mean_rank,
          IQR异常点数 = IQR_outlier_count,
          多重比较字母 = Letter
        )
    }
  ))

  model_diagnostics <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      variances <- result$group_variances
      tibble::tibble(
        指标 = metric_name,
        模型 = "lm(Value ~ Treatment)",
        观测总数 = nrow(result$data),
        模型秩 = result$model$rank,
        设计矩阵列数 = ncol(
          stats::model.matrix(result$model)
        ),
        满秩 = result$design_full_rank,
        残差自由度 = stats::df.residual(result$model),
        Meadow方差 = unname(variances[["Meadow"]]),
        Steppe方差 = unname(variances[["Steppe"]]),
        desert方差 = unname(variances[["desert"]]),
        最大最小方差比 = max(variances) / min(variances),
        `R2_cat_描述性` = result$r_squared_cat,
        `Adjusted_R2_描述性` = result$adjusted_r_squared,
        P_ANOVA参考 = result$p_anova_reference,
        P_Welch参考 = result$p_welch_reference,
        P_HC3参考 = result$p_hc3_reference,
        P_KW参考 = result$p_kw_reference,
        参考P用途 = paste0(
          "仅供方法对照；不参与正式分支以外的结论、",
          "主图或字母"
        )
      )
    }
  ))

  residual_diagnostics <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      tibble::tibble(
        指标 = metric_name,
        诊断对象 = paste0(
          "lm(Value ~ Treatment)的",
          nrow(result$data),
          "个合并残差"
        ),
        Shapiro_W = unname(result$shapiro_result$statistic),
        Shapiro_P = result$shapiro_p,
        均值中心Levene_F = result$levene_f,
        均值中心Levene_P = result$levene_p,
        手工绝对离差_F = result$manual_levene_f,
        手工绝对离差_P = result$manual_levene_p,
        两种Levene_F绝对差 = abs(
          result$levene_f - result$manual_levene_f
        ),
        两种Levene_P绝对差 = abs(
          result$levene_p - result$manual_levene_p
        ),
        偏差校正偏度 = result$skewness,
        无偏超额峰度 = result$kurtosis,
        最大绝对外部学生化残差 = result$max_abs_rstudent,
        `N>=25` = nrow(result$data) >= 25L,
        每组n至少5 = min(table(result$data$Treatment)) >= 5L,
        偏度绝对值不大于1 = abs(result$skewness) <= 1,
        峰度绝对值不大于2 = abs(result$kurtosis) <= 2,
        最大绝对rstudent不大于3 =
          result$max_abs_rstudent <= 3,
        满足HC3稳健保护条件 =
          result$mild_non_normal_eligible,
        残差类别 = result$residual_class,
        最终分支 = result$final_branch
      )
    }
  ))

  outlier_export <- dplyr::bind_rows(lapply(
    metric_levels,
    function(metric_name) {
      result <- metric_results[[metric_name]]
      result$outliers |>
        dplyr::transmute(
          源文件 = basename(config$input_file),
          源行号 = Source_row,
          样本ID = Sample_ID,
          原始样本前缀 = Treatment_original,
          显示处理 = as.character(Treatment),
          指标 = metric_name,
          源值 = Value,
          Q1 = Q1,
          Q3 = Q3,
          IQR = IQR_value,
          IQR下限 = IQR_lower,
          IQR上限 = IQR_upper,
          IQR异常值标记 = IQR_outlier,
          是否保留在分析中 = TRUE
        )
    }
  ))

  branch_summary <- paste(
    paste(
      metric_levels,
      observed_branches,
      sep = "="
    ),
    collapse = "；"
  )
  derived_summary <- if (length(derived_metric_names) == 0L) {
    "全部请求指标直接读取源列；源表已有比值不重算"
  } else {
    paste0(
      "直接读取源列；仅按用户明确要求在内存中派生：",
      paste(
        unname(metric_source_notes[derived_metric_names]),
        collapse = "；"
      ),
      "；不回写源文件"
    )
  }
  read_scope_note <- if (is.null(config$input_range)) {
    paste0(
      "工作表=", config$input_sheet,
      "；跳过顶部行数=", config$input_skip,
      "；标题所在Excel行=", config$source_header_row
    )
  } else {
    paste0(
      "工作表=", config$input_sheet,
      "；范围=", config$input_range,
      "；标题所在Excel行=", config$source_header_row
    )
  }
  method_quality <- tibble::tibble(
    项目 = c(
      "源数据原则",
      "源文件",
      "源文件SHA256_运行前",
      "读取范围",
      "样本映射",
      "样本量",
      "分析指标",
      "比值使用原则",
      "残差正态性",
      "方差齐性",
      "分支规则",
      "当前正式分支",
      "总体P同类校正",
      "多重比较P",
      "多重比较字母",
      "效应量",
      "异常值策略",
      "主图中心",
      "主图版式",
      "独立性风险",
      "小样本风险",
      "Rscript",
      "Python与SciPy",
      "输出Excel",
      "源文件未回写"
    ),
    说明 = c(
      "只读读取；不插补、不四舍五入、不重排、不回写",
      basename(config$input_file),
      input_sha256_before,
      read_scope_note,
      paste0(
        "分组源列=", config$group_column,
        "；Meadow/Steppe保持；Desert仅在内存中显示为desert"
      ),
      paste(
        paste0(names(config$expected_group_counts), " n=",
          config$expected_group_counts),
        collapse = "；"
      ),
      paste(metric_levels, collapse = "、"),
      derived_summary,
      paste0(
        "仅对lm(Value ~ Treatment)的合并残差执行一次",
        "Shapiro-Wilk"
      ),
      paste0(
        "对同一模型残差执行均值中心Levene，",
        "并用手工绝对离差模型复核"
      ),
      paste0(
        "Shapiro与Levene决定ANOVA/Welch；",
        "Shapiro<0.05时按预设保护条件分HC3或KW"
      ),
      branch_summary,
      paste0(
        length(metric_levels),
        "个总体原始P作为同一检验族采用Benjamini-Hochberg校正；",
        "主图显示q(BH)"
      ),
      paste0(
        "Tukey/Games-Howell为各自家族校正；HC3用Holm；",
        "Dunn用BH"
      ),
      paste0(
        "由正式分支调整后P生成；参数/HC3按算术均值，",
        "KW按平均秩优先A"
      ),
      paste0(
        "Eta²和Omega²仅用于普通ANOVA；",
        "KW用epsilon-squared；两两均报告Cliff's delta"
      ),
      paste0(
        "IQR仅标记；",
        sum(config$expected_group_counts),
        "个观测全部进入每个指标分析和绘图"
      ),
      "红色菱形与虚线始终表示原尺度算术均值",
      paste0(
        length(metric_levels),
        "面板",
        plot_rows,
        "×",
        plot_columns,
        "单页Cairo矢量PDF；每个坐标面板",
        "263.645×222.34 PDF pt；线性尺度"
      ),
      sample_id_note,
      paste0(
        "当前组样本量为",
        paste(config$expected_group_counts, collapse = "/"),
        "；小样本时Shapiro和Levene检验力有限；",
        "结果需结合原始点和效应量解释"
      ),
      rscript_path,
      paste0(
        python_executable,
        " | Python ",
        python_version,
        " | SciPy ",
        scipy_version
      ),
      output_excel,
      paste0(
        "脚本不调用任何写入源工作簿的函数；",
        "运行后再次核对SHA256"
      )
    )
  )

  jsonlite::write_json(
    list(
      "分析总览" = analysis_overview,
      "两两比较" = pairwise_export,
      "描述统计" = descriptive_export,
      "模型诊断" = model_diagnostics,
      "残差诊断" = residual_diagnostics,
      "异常值标记" = outlier_export,
      "方法与质控" = method_quality
    ),
    path = summary_json,
    dataframe = "rows",
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 17,
    na = "null",
    null = "null"
  )

  same_script_path <- file.exists(output_script) && identical(
    normalizePath(script_source, winslash = "/", mustWork = TRUE),
    normalizePath(output_script, winslash = "/", mustWork = TRUE)
  )
  copied_script <- if (same_script_path) {
    TRUE
  } else {
    file.copy(
      script_source,
      output_script,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }
  if (!isTRUE(copied_script)) {
    stop("无法复制完整 R 脚本到 ", output_dir)
  }
  same_config_path <- identical(
    normalizePath(config_path, winslash = "/", mustWork = TRUE),
    normalizePath(output_config, winslash = "/", mustWork = FALSE)
  )
  copied_config <- if (same_config_path) {
    TRUE
  } else {
    file.copy(
      config_path,
      output_config,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }
  if (!isTRUE(copied_config)) {
    stop("无法复制分析配置到 ", output_dir)
  }

  input_sha256_after <- toupper(digest::digest(
    file = config$input_file,
    algo = "sha256",
    serialize = FALSE
  ))
  if (!identical(input_sha256_before, input_sha256_after)) {
    stop(config$organism, "源工作簿运行前后 SHA256 不一致。")
  }

  formal_without_excel <- c(
    output_figure_pdf,
    output_diagnostic_pdf,
    output_script,
    output_config,
    summary_json
  )
  if (!all(file.exists(formal_without_excel))) {
    stop(config$organism, "PDF、R脚本或七表JSON未完整生成。")
  }

  message("ORGANISM_COMPLETE=", config$organism)
  message("SOURCE_SHA256=", input_sha256_after)
  message(
    "BRANCHES=",
    paste(
      metric_levels,
      observed_branches,
      sep = ":",
      collapse = "|"
    )
  )
  message(
    "LETTERS=",
    paste(vapply(metric_levels, function(metric_name) {
      paste0(
        metric_name,
        "[",
        paste(
          treatment_levels,
          metric_results[[metric_name]]$letters,
          sep = ":",
          collapse = ","
        ),
        "]"
      )
    }, character(1)), collapse = "|")
  )
  message("FIGURE_PDF=", output_figure_pdf)
  message("QQ_PDF=", output_diagnostic_pdf)
  message("SUMMARY_JSON=", summary_json)
  message("ANALYSIS_CONFIG=", output_config)

  invisible(list(
    output_dir = output_dir,
    summary_json = summary_json,
    output_excel = output_excel,
    branches = observed_branches,
    overall_p_raw = overall_p_raw,
    overall_p_bh = overall_p_bh
  ))
}

# ------------------------------ 7. 执行全指标分析 ---------------------------

analysis_outputs <- lapply(
  seq_along(organism_config),
  function(index) {
    analyze_organism(organism_config[[index]], index)
  }
)

message("THREE_ECOSYSTEM_ANALYSIS_COMPLETE")
