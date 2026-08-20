from __future__ import annotations

import argparse
import json
from collections import Counter
from pathlib import Path

import fitz


PANEL_WIDTH_PT = 263.645
PANEL_HEIGHT_PT = 222.34
PANEL_TOLERANCE_PT = 0.02

METHOD_LABELS = {
    "ANOVA_TUKEY": "ANOVA + Tukey HSD",
    "WELCH_GH": "Welch ANOVA + Games-Howell",
    "HC3_HOLM": "HC3 robust F + HC3 contrasts (Holm)",
    "KW_DUNN_BH": "Kruskal-Wallis + Dunn (BH)",
}

BRACKET_LABELS = {
    "ANOVA_TUKEY": "Tukey p =",
    "WELCH_GH": "GH p =",
    "HC3_HOLM": "HC3-Holm p =",
    "KW_DUNN_BH": "Dunn-BH p =",
}

FORBIDDEN_PDF_TERMS = (
    "WRS2",
    "t1way",
    "lincon",
    "Yuen",
    "20%截尾",
    "20% trimmed",
    "trimmed mean",
    "Sidak",
)


def assert_no_misleading_method_text(text: str, label: str) -> None:
    import re

    folded = text.casefold()
    for term in FORBIDDEN_PDF_TERMS:
        if term.casefold() in folded:
            raise AssertionError(
                f"{label}: forbidden old method term {term!r}"
            )
    compact = "".join(text.split())
    if (
        "Dunn-Holm".casefold() in compact.casefold()
        or "Holm-Dunn".casefold() in compact.casefold()
    ):
        raise AssertionError(f"{label}: Dunn must not use Holm adjustment")

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
            raise AssertionError(
                f"{label}: R²(cat) is mislabeled as correlation"
            )


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", required=True, type=Path)
    parser.add_argument("--preview-dir", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    return parser.parse_args()


def panel_rectangles(page: fitz.Page) -> list[dict[str, float]]:
    matches: list[dict[str, float]] = []
    for drawing in page.get_drawings():
        for item in drawing["items"]:
            if item[0] != "re":
                continue
            rectangle = item[1]
            width = float(rectangle.width)
            height = float(rectangle.height)
            if (
                abs(width - PANEL_WIDTH_PT) <= PANEL_TOLERANCE_PT
                and abs(height - PANEL_HEIGHT_PT) <= PANEL_TOLERANCE_PT
            ):
                matches.append(
                    {
                        "x0": float(rectangle.x0),
                        "y0": float(rectangle.y0),
                        "width": width,
                        "height": height,
                    }
                )
    return matches


arguments = parse_arguments()
config = json.loads(arguments.config.read_text(encoding="utf-8"))
metrics = list(config["metric_columns"])
plot_titles = [config["plot_titles"][metric] for metric in metrics]
expected_branches = config.get("expected_branches")
if not isinstance(expected_branches, dict):
    raise AssertionError(
        "最终 PDF 审计前必须把 expected_branches 锁定为逐指标对象。"
    )
if set(expected_branches) != set(metrics):
    raise AssertionError("expected_branches 未逐一覆盖 metric_columns。")

output_dir = Path(config["output_dir"])
output_stem = config["output_stem"]
pdf_jobs = (
    (output_dir / f"{output_stem}_箱线图总图.pdf", "main"),
    (output_dir / f"{output_stem}_残差Q-Q诊断.pdf", "qq"),
)

arguments.preview_dir.mkdir(parents=True, exist_ok=True)
report: dict[str, object] = {"status": "PASS", "pdfs": []}

for pdf_path, kind in pdf_jobs:
    if not pdf_path.is_file():
        raise FileNotFoundError(pdf_path)
    document = fitz.open(pdf_path)
    if len(document) != 1:
        raise AssertionError(
            f"{pdf_path.name}: expected one page, found {len(document)}"
        )

    page = document[0]
    page_text = page.get_text()
    for plot_title in plot_titles:
        if plot_title not in page_text:
            raise AssertionError(
                f"{pdf_path.name}: missing metric title {plot_title!r}"
            )
    assert_no_misleading_method_text(page_text, pdf_path.name)

    dimensions = None
    if kind == "main":
        dimensions = panel_rectangles(page)
        if len(dimensions) != len(metrics):
            raise AssertionError(
                f"{pdf_path.name}: expected {len(metrics)} locked panels, "
                f"found {len(dimensions)}"
            )
        for treatment in ("Meadow", "Steppe", "desert"):
            if treatment not in page_text:
                raise AssertionError(
                    f"{pdf_path.name}: missing treatment {treatment}"
                )
        branch_counts = Counter(expected_branches.values())
        for branch, expected_count in branch_counts.items():
            method_label = METHOD_LABELS[branch]
            observed_count = page_text.count(method_label)
            if observed_count != expected_count:
                raise AssertionError(
                    f"{pdf_path.name}: {method_label!r} expected "
                    f"{expected_count}, found {observed_count}"
                )
            bracket_label = BRACKET_LABELS[branch]
            expected_brackets = expected_count * 3
            observed_brackets = page_text.count(bracket_label)
            if observed_brackets != expected_brackets:
                raise AssertionError(
                    f"{pdf_path.name}: {bracket_label!r} expected "
                    f"{expected_brackets}, found {observed_brackets}"
                )
        if page_text.count("q(BH) =") != len(metrics):
            raise AssertionError(
                f"{pdf_path.name}: q(BH) label count is not {len(metrics)}"
            )
        for caption_fragment in (
            "Brackets: branch-specific adjusted pairwise p values.",
            "Red diamonds/line: arithmetic means;",
            "Residual diagnostics used pooled lm residuals.",
        ):
            if caption_fragment not in page_text.replace("\n", " "):
                raise AssertionError(
                    f"{pdf_path.name}: incomplete caption fragment "
                    f"{caption_fragment!r}"
                )

    pixmap = page.get_pixmap(matrix=fitz.Matrix(2.2, 2.2), alpha=False)
    preview_path = arguments.preview_dir / f"{output_stem}_{kind}.png"
    pixmap.save(preview_path)
    report["pdfs"].append(
        {
            "kind": kind,
            "path": str(pdf_path),
            "pages": len(document),
            "page_width_pt": float(page.rect.width),
            "page_height_pt": float(page.rect.height),
            "text_chars": len(page_text),
            "panel_dimensions": dimensions,
            "preview": str(preview_path),
        }
    )
    document.close()

arguments.report.parent.mkdir(parents=True, exist_ok=True)
arguments.report.write_text(
    json.dumps(report, ensure_ascii=False, indent=2),
    encoding="utf-8",
)
print(json.dumps(report, ensure_ascii=False))
