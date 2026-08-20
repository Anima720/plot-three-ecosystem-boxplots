# 模板适配指南

## 1. 环境

按 `use-local-r` Skill 优先使用：

```text
D:\Program Files\R\R-4.5.0\bin\Rscript.exe
```

工作流启动器会把本机不可用的 `C.UTF-8` 改为已验证的 `English_United States.utf8`，从而正确解析中文 R 标识符和列名。

使用工作区依赖加载器取得当前 Node.js、Python 包和 `@oai/artifact-tool/dist/artifact_tool.mjs` 的路径。不要从互联网另装包来绕开本机已提供运行时。

R 主模板需要：`car`、`digest`、`dplyr`、`emmeans`、`FSA`、`ggplot2`、`ggpubr`、`gridExtra`、`jsonlite`、`multcompView`、`readxl`、`sandwich`、`tibble`、`tidyr`。Games–Howell 需要 Python 的 SciPy `studentized_range`。

## 2. 配置

复制 [analysis_config.example.json](../scripts/analysis_config.example.json)，不要直接编辑 Skill 内的模板。路径全部写绝对路径并使用 `/` 或转义后的 `\\`。

- `analysis_label`：本批类别说明。
- `input_file` / `input_sheet`：只读源 Excel。
- `input_skip`：标题行上方需要跳过的行数，标题在第 1 行时填 `0`。
- `input_range`：需要精确限定数据区时填写包含标题行的范围，如 `A3:F33`；否则填 `null`。不得与非零 `input_skip` 同时使用；如写成 `Sheet1!A3:F33`，工作表前缀必须与 `input_sheet` 完全一致。
- `group_column`：含 Meadow、Steppe、Desert 的列。
- `sample_id_column`：存在时填列名；没有时用 `null`。
- `output_dir`：新的最终目录。
- `temp_dir`：单独临时目录，不放最终交付。
- `output_stem`：全部输出文件的共同前缀。
- `metric_columns`：按最终面板顺序列出。用户已给列名时精确照录；用户只说“全部指标”时按数据契约排除结构字段与协变量后保留源列顺序，存在歧义则先确认候选/排除清单。
- `derived_metrics`：默认 `{}`；只为用户明确要求且源表不存在的派生指标填写。
- `plot_titles` / `axis_labels`：键必须与指标完全一致。源表或用户提供单位时写明单位；没有可靠单位时只写指标名并记录“单位未提供”，不得猜测。
- `expected_sha256`：源文件 64 位 SHA256。
- `expected_group_counts`：逐组精确样本量，键固定为 Meadow、Steppe、desert。
- `expected_branches`：首轮 `null`；独立复核后写成每指标的 `ANOVA_TUKEY`、`WELCH_GH`、`HC3_HOLM` 或 `KW_DUNN_BH`。
- 运行时路径和版本从本机探测结果填写。

## 3. 运行顺序

1. 只读检查 Excel、真实标题行和数据范围，生成首轮配置。
2. 在新的空输出目录运行 `run_workflow.ps1`。它会完成主分析、Excel 重开审计和独立统计复核，并返回 `FIRST_PASS_COMPLETE_INDEPENDENT_BRANCHES_READY_TO_LOCK`。
3. 打开临时目录中的 `statistics_audit_report.json`，确认 `PASS`，将其中逐指标 `分支` 原样写入 `expected_branches`。
4. 用同一输出目录再次运行启动器。除 `expected_branches` 外配置必须完全相同；启动器会自动执行独立统计、Excel、PDF 与交付目录审计。
5. 实际查看 `pdf_previews` 和 `excel_previews` 中的全部预览，确认无裁切、重叠和乱码。
6. 从最终交付 R 脚本与锁定配置再重跑一次，确认可重复。交付 R 自身也会执行同批次身份保护；不同配置或含无关文件的既有目录必须拒绝。

独立统计复核固定使用 [audit_statistics.R](../scripts/audit_statistics.R)。它不调用主脚本的中间对象，而是重新读取源 Excel 并独立复算诊断、四分支统计、字母和异常值追踪；不得用主结果自我比较替代这一步。

## 4. 不应通过改代码解决的失败

- SHA256 不一致：重新确认用户指定文件，不能更新哈希来掩盖来源变化。
- 列缺失或名称近似：回看真实表头，不能模糊匹配到相似指标。
- 缺失/非有限值：报告给用户，不能默认删行。
- 分组数或样本量不符：检查设计和数据范围，不能放宽为任意组。
- 分支与锁定值不一致：重新独立计算，判断是数据变化、配置错误还是代码错误。
- PDF 裁切或面板尺寸不符：调整外边距和总画布，不改变内部坐标面板锁定尺寸。
