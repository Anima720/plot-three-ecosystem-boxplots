from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path


FORBIDDEN_SCRIPT_TERMS = (
    "WRS2",
    "t1way",
    "lincon",
    "Yuen",
    "20%截尾",
    "20% trimmed",
    "trimmed mean",
    "Sidak",
)


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", required=True, type=Path)
    parser.add_argument("--template-r", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    return parser.parse_args()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest().upper()


def assert_no_misleading_method_text(text: str, label: str) -> None:
    import re

    folded = text.casefold()
    for term in FORBIDDEN_SCRIPT_TERMS:
        if term.casefold() in folded:
            raise AssertionError(f"{label} 残留旧方法术语：{term}")
    if re.search(
        r"Dunn[\s\-+_/()：:]*Holm|Holm[\s\-+_/()：:]*Dunn",
        text,
        flags=re.IGNORECASE,
    ):
        raise AssertionError(f"{label} 出现 Dunn 使用 Holm 的误导表述。")
    contexts = []
    contexts.extend(
        match.group(0)
        for match in re.finditer(
            r"R(?:²|\^?2)\s*\(cat\).{0,100}(?:相关系数|correlation coefficient|correlation)",
            text,
            flags=re.IGNORECASE | re.DOTALL,
        )
    )
    contexts.extend(
        match.group(0)
        for match in re.finditer(
            r"(?:相关系数|correlation coefficient|correlation).{0,100}R(?:²|\^?2)\s*\(cat\)",
            text,
            flags=re.IGNORECASE | re.DOTALL,
        )
    )
    for context in contexts:
        if not re.search(
            r"不是|并非|不属于|不能称为|非相关系数|\bnot\b|isn't|is not|does not",
            context,
            flags=re.IGNORECASE,
        ):
            raise AssertionError(f"{label} 将 R²(cat) 误称为相关系数。")


arguments = parse_arguments()
config_path = arguments.config.resolve(strict=True)
template_r = arguments.template_r.resolve(strict=True)
config = json.loads(config_path.read_text(encoding="utf-8"))
metrics = list(config["metric_columns"])
branches = config.get("expected_branches")
if not isinstance(branches, dict) or set(branches) != set(metrics):
    raise AssertionError("最终配置必须锁定每个指标的 expected_branches。")

source_path = Path(config["input_file"]).resolve(strict=True)
source_hash = sha256(source_path)
if source_hash != str(config["expected_sha256"]).upper():
    raise AssertionError("交付审计时源文件 SHA256 与配置不一致。")

output_dir = Path(config["output_dir"]).resolve(strict=True)
output_stem = str(config["output_stem"])
expected_files = {
    f"{output_stem}_箱线图总图.pdf",
    f"{output_stem}_残差Q-Q诊断.pdf",
    f"{output_stem}_统计分析汇总.xlsx",
    f"{output_stem}_绘图与统计分析_残差诊断四分流.R",
    f"{output_stem}_分析配置.json",
}
actual_files = {path.name for path in output_dir.iterdir() if path.is_file()}
if actual_files != expected_files:
    raise AssertionError(
        "最终目录文件集合不一致；"
        f"missing={sorted(expected_files - actual_files)}; "
        f"extra={sorted(actual_files - expected_files)}"
    )

file_rows = []
for file_name in sorted(expected_files):
    file_path = output_dir / file_name
    if file_path.stat().st_size <= 0:
        raise AssertionError(f"空交付文件：{file_path}")
    file_rows.append(
        {
            "name": file_name,
            "bytes": file_path.stat().st_size,
            "sha256": sha256(file_path),
        }
    )

delivered_r = output_dir / f"{output_stem}_绘图与统计分析_残差诊断四分流.R"
if sha256(delivered_r) != sha256(template_r):
    raise AssertionError("交付 R 脚本与本次实际运行模板不一致。")
script_text = delivered_r.read_text(encoding="utf-8")
assert_no_misleading_method_text(script_text, "交付 R 脚本")

delivered_config = output_dir / f"{output_stem}_分析配置.json"
if json.loads(delivered_config.read_text(encoding="utf-8")) != config:
    raise AssertionError("交付配置与本次运行配置的 JSON 内容不一致。")
assert_no_misleading_method_text(
    delivered_config.read_text(encoding="utf-8"),
    "交付配置",
)

summary_json = (
    Path(config["temp_dir"]) / f"{output_stem}_七表数据.json"
).resolve(strict=True)
assert_no_misleading_method_text(
    summary_json.read_text(encoding="utf-8"),
    "七表 JSON",
)

report = {
    "status": "PASS",
    "source": str(source_path),
    "source_sha256": source_hash,
    "output_dir": str(output_dir),
    "metric_count": len(metrics),
    "branches": branches,
    "method_text_scans": {
        "R": "PASS",
        "config_JSON": "PASS",
        "seven_table_JSON": "PASS",
    },
    "files": file_rows,
}
arguments.report.parent.mkdir(parents=True, exist_ok=True)
arguments.report.write_text(
    json.dumps(report, ensure_ascii=False, indent=2),
    encoding="utf-8",
)
print(json.dumps(report, ensure_ascii=False))
