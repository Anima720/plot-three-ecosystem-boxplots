---
name: plot-three-ecosystem-boxplots
description: Use when Codex receives an Excel workbook from the established Meadow, Steppe, and Desert research system and the user wants the same three-independent-group boxplots or statistical deliverables as prior batches, including requests that add indicators or reproduce the current workflow.
---

# 三生态系统箱线图工作流

## 适用边界

仅用于当前研究体系中的三个独立生态系统组：`Meadow`、`Steppe`、`Desert`。图表中只把 `Desert` 显示为 `desert`，不回写源文件。指标数量可以增加。

不要用于配对、重复测量、区组、嵌套、时间序列或超过/少于三个处理组的数据。在本研究体系已明确三个生态系统组为独立处理的前提下可以继续；缺少样本 ID 本身不等于已证明不独立，但必须记录这一限制。若数据或设计记录明确显示站点内子样、同一样方重复测量或其他层级依赖，应停止把简单单因素检验当作最终推断，并说明需要能表达依赖结构的模型。

## 开始前必读

执行任务前完整读取：

- [数据契约](references/data-contract.md)
- [统计契约](references/statistical-contract.md)
- [图形与交付契约](references/figure-output-contract.md)
- [复核清单](references/verification-checklist.md)
- [模板适配指南](references/adaptation-guide.md)

这些文件共同构成本工作流的锁定规格，不得只凭常见统计习惯临时替换方法。

## 可复用代码

- [一键流程启动器](scripts/run_workflow.ps1)：设置本机 R 的 UTF-8 环境；首轮运行统计绘图、Excel 与独立统计审计并输出待锁定分支，锁定轮再运行全部可程序化交付审计。
- [R 主模板](scripts/run_three_ecosystem_boxplots.R)：数据校验、残差诊断、四分流统计、字母、PDF 和七表 JSON。
- [配置模板](scripts/analysis_config.example.json)：每批数据只需填写路径、列名、标题、样本量和哈希。
- [独立统计审计器](scripts/audit_statistics.R)：重新读取源 Excel，独立复算残差诊断、四分流、总体检验、三次两两比较、效应量、异常值标记与字母。
- [Excel 构建器](scripts/build_summary_workbook.mjs)：生成、渲染并重开核查七工作表 Excel。
- [通用 Excel 审计器](scripts/audit_workbook.mjs)：逐单元格比较七表 JSON 与重开工作簿，并扫描公式错误。
- [通用 PDF 审计器](scripts/audit_pdfs.py)：核对单页、指标文字、分支标签、三括号和固定面板尺寸，并输出预览。
- [通用交付审计器](scripts/audit_delivery.py)：核对源哈希、最终五文件集合、脚本/配置一致性和旧方法残留。

## 固定工作流

### 1. 只读盘点数据

定位用户指定的源工作簿，计算 SHA256，检查工作表、原始表头、组标签、样本 ID、指标类型、单位、缺失值、非有限值和每组样本量。不得修改、另存、排序或清洗源工作簿。

结合当前研究设计和可用样本标识确认三个组按独立观测分析；无样本 ID 时记录无法逐行核验独立性的限制，但不把缺失 ID 单独作为否决理由。源表已有指标或比值直接使用。只有用户明确要求的派生指标才可写入配置的 `derived_metrics`，并仅在内存中逐行计算。

指标选择按以下顺序锁定：用户给出列名时严格按列名和顺序；用户只说“全部指标”且所选工作表显然是专用指标表时，自动采用除分组列、样本/站点/样方/重复编号及其他结构字段外的全部有限数值响应列，并保持源列顺序。经纬度、深度、时间、处理剂量、测序深度、质控量或其他可能是协变量/元数据的数值列不得自动纳入。若盘点后仍无法唯一判断某列是响应指标还是结构字段，只询问一次具体的候选列/排除列确认，不得靠模糊匹配或把所有数值列盲目纳入多重检验。

### 2. 建立本批配置

复制 `analysis_config.example.json` 到项目临时目录并填写：

- 输入文件、工作表、标题行读取方式、分组列和可选样本 ID 列；
- 全部指标源列、英文图题和纵轴标题；单位由源表或用户提供时必须保留，未提供时不得猜测，轴标题只写指标名并在交付说明中注明单位未提供；
- 新建且不覆盖旧结果的输出目录；
- 源文件 SHA256 和精确组样本量；
- 本机 R、Python 路径与可选版本锁；
- 首轮将 `expected_branches` 设为 `null`。

必须使用 `use-local-r` 的定位规则调用本机 R。生成 Excel 前加载工作区依赖，取得 Node.js 和 `artifact_tool.mjs` 的当前路径；不要假定缓存版本永远不变。

### 3. 首轮运行

在 PowerShell 中运行：

```powershell
& 'C:\Users\86532\.codex\skills\plot-three-ecosystem-boxplots\scripts\run_workflow.ps1' `
  -ConfigPath 'C:\absolute\path\analysis_config.json' `
  -NodePath 'C:\path\from\workspace-dependencies\node.exe' `
  -ArtifactToolPath 'C:\path\from\workspace-dependencies\artifact_tool.mjs'
```

所有 R 警告均按错误处理。若缺列、缺失值、非有限值、标签异常、样本量不符、哈希变化、版本不符或校验失败，停止并查明原因，不得静默删除观测或放宽检查。

启动器与交付 R 脚本都只允许写入空的新目录，或由除 `expected_branches` 外完全相同的配置证明属于当前批次的重跑目录；这样首轮配置可在锁定分支后原位确定性重跑，同时不会覆盖历史结果。

### 4. 独立复核并锁定分支

首轮启动器会自动调用独立统计审计器，重新读取源表并复核每个指标的 Shapiro–Wilk、均值中心 Levene、分支、总体检验、三次两两比较、调整后 P 值、置信区间、效应量、异常值标记和字母。Games–Howell 使用完整精度 studentized-range 结果。读取临时目录中的 `statistics_audit_report.json`，确认状态为 `PASS`，再把其中逐指标分支写回配置。

将已复核的每个指标分支写回 `expected_branches`，然后再次执行完整流程。最终交付配置不得保留 `null`。

### 5. 完整验收

锁定分支后的第二轮启动器会自动执行独立统计、Excel、PDF 和最终目录审计。随后必须实际查看临时目录中的 PDF 与七张 Excel 预览，确认无裁切、重叠或乱码；程序化渲染不能代替目视检查。再按 [复核清单](references/verification-checklist.md) 完成从交付脚本重跑。任何一项未通过都不能宣称完成。

### 6. 交付

交付新文件夹，至少包含：

- 全指标箱线图总图矢量 PDF；
- 合并模型残差 Q–Q 诊断 PDF；
- 七工作表统计汇总 Excel；
- 带完整中文说明的 R 脚本；
- 本批分析配置 JSON。

向用户简要报告输出位置、源 SHA256 是否保持、各指标正式分支和字母，并明确任何独立性、伪重复或小样本限制。

## 不得偏离的事项

- 不对原始数据逐组做正态性分流；诊断对象是 `lm(Value ~ Treatment)` 的合并残差。
- 不自动转换、截尾、插补、删异常值或改用完整案例。
- 不以 Brown–Forsythe、原始值 Levene、Bartlett 或视觉判断替代锁定的残差均值中心 Levene。
- 不把 Games–Howell 再做 Holm；不把 Dunn 改成 Holm；不把 HC3 改成 Welch。
- 不把 `R²(cat)` 称为相关系数或 Welch 效应量。
- 不隐藏不显著比较；三个固定组对均须显示括号并进入表格。
- 不覆盖旧结果目录，不把临时 JSON、预览图或核查 sidecar 放入最终交付目录。
