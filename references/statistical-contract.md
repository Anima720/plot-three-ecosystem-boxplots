# 统计契约

## 共同设置

- 显著性阈值 `alpha = 0.05`。
- 每个指标独立拟合 `lm(Value ~ Treatment)`，`Treatment` 顺序固定为 `Meadow / Steppe / desert`。
- Shapiro–Wilk 的对象是该模型全部合并残差，不是三组原始值分别检验。
- 方差齐性采用同一模型残差的均值中心 Levene 检验；再用 `abs(residual - group residual mean)` 的单因素模型手工复核 F 与 P，二者必须一致。

## 四分流规则

按以下顺序判定，不能交换：

1. `Shapiro P >= 0.05` 且 `Levene P >= 0.05`
   - 普通单因素 ANOVA；
   - Tukey HSD 三组全比较；
   - Tukey 同时置信区间和家族校正 P 值。
2. `Shapiro P >= 0.05` 且 `Levene P < 0.05`
   - Welch 单因素 ANOVA；
   - Games–Howell 三组全比较；
   - 使用完整精度 studentized-range 生存函数与分位数计算校正 P 值和同时置信区间；不再叠加 Holm。
3. `Shapiro P < 0.05`，且下列五个稳健保护条件全部满足
   - 总样本量 `N >= 25`；
   - 每组 `n >= 5`；
   - 合并残差偏差校正偏度绝对值 `<= 1`；
   - 合并残差无偏超额峰度绝对值 `<= 2`；
   - 最大绝对外部学生化残差 `<= 3`；
   - 使用同一线性模型的 HC3 协方差修正总体 F；
   - HC3 两两算术均值差，三次比较用 Holm 校正。
4. `Shapiro P < 0.05` 且任一稳健保护条件不满足
   - Kruskal–Wallis 总体检验；
   - Dunn 三组全比较；
   - 固定三次原始 P 值用 R 的 Benjamini–Hochberg 方法校正。

Levene 的结果在 `Shapiro P < 0.05` 的两个分支中仍须报告，但不改变上述 HC3/Kruskal–Wallis 守门规则。

## 多重性

- 每个指标恰有三次比较，固定顺序与方向：
  1. `Meadow - Steppe`
  2. `Meadow - desert`
  3. `Steppe - desert`
- Tukey 与 Games–Howell 使用各自方法内置的家族控制。
- HC3 三比较使用 Holm。
- Dunn 三比较使用 BH。
- 同一批请求指标的全部总体原始 P 值再作为一个检验族做 BH；图中显示总体 `q(BH)`，Excel 同时保留总体原始 P。

## 字母

- 字母由正式分支的调整后两两 P 值产生，不由未校正 P 或其他参考分支产生。
- 两组共享至少一个字母，当且仅当其调整后 `P >= 0.05`。
- ANOVA、Welch 和 HC3 分支按算术均值从高到低优先赋 `A`；Kruskal–Wallis 分支按平均秩从高到低优先赋 `A`。
- 逐对自动检查字母重叠关系与调整后 P 值完全一致。

## 描述量和效应量

- 红色中心菱形和虚线始终表示原尺度算术均值，所有分支一致。
- `R²(cat)` 和 adjusted `R²` 仅是普通分类线性模型的描述量，不是相关系数、Welch 效应量或非参数效应量。
- Eta²、Omega²只在普通 ANOVA 分支报告。
- Kruskal–Wallis 分支报告 epsilon²。
- Welch 和 HC3 分支不套用普通 ANOVA 的 Eta²/Omega²。
- 每次两两比较均报告 Cliff's delta，方向与“组1−组2”一致。

## 数据保留

所有原始观测都进入统计和作图。IQR 规则只生成质控标记，不删除或降权。不得自动对数、平方根、Yeo–Johnson 转换，不做 20% 截尾均值、Yuen、WRS2、lincon 或其他未锁定流程。
