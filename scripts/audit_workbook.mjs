import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const sheetOrder = [
  "分析总览",
  "两两比较",
  "描述统计",
  "模型诊断",
  "残差诊断",
  "异常值标记",
  "方法与质控",
];

const forbiddenOldTerms = [
  "WRS2",
  "t1way",
  "lincon",
  "Yuen",
  "20%截尾",
  "20% trimmed",
  "trimmed mean",
  "Sidak",
];

function assertNoMisleadingMethodText(text, label) {
  const folded = text.toLocaleLowerCase("en-US");
  for (const term of forbiddenOldTerms) {
    if (folded.includes(term.toLocaleLowerCase("en-US"))) {
      throw new Error(`${label}: 残留旧方法术语 ${term}`);
    }
  }
  if (/dunn[\s\-+_/()：:]*holm|holm[\s\-+_/()：:]*dunn/iu.test(text)) {
    throw new Error(`${label}: 出现 Dunn 使用 Holm 的误导表述`);
  }
  const r2Contexts = [
    ...text.matchAll(/R(?:²|\^?2)\s*\(cat\).{0,100}(?:相关系数|correlation coefficient|correlation)/giu),
    ...text.matchAll(/(?:相关系数|correlation coefficient|correlation).{0,100}R(?:²|\^?2)\s*\(cat\)/giu),
  ];
  for (const match of r2Contexts) {
    if (!/(?:不是|并非|不属于|不能称为|非相关系数|not|isn't|is not|does not)/iu.test(match[0])) {
      throw new Error(`${label}: R²(cat) 被误称为相关系数`);
    }
  }
}

function parseArguments(argv) {
  const values = {};
  for (let index = 0; index < argv.length; index += 2) {
    const key = argv[index];
    const value = argv[index + 1];
    if (!key?.startsWith("--") || value === undefined) {
      throw new Error(
        "参数格式：--json <path> --workbook <path> " +
          "--artifact-tool <artifact_tool.mjs> --report <path> " +
          "[--label <text>]",
      );
    }
    values[key.slice(2)] = value;
  }
  for (const required of ["json", "workbook", "artifact-tool", "report"]) {
    if (!values[required]) {
      throw new Error(`缺少参数 --${required}`);
    }
  }
  return values;
}

function normalize(value) {
  if (value === undefined || value === null || value === "") {
    return null;
  }
  return value;
}

function equalCell(actual, expected) {
  const normalizedActual = normalize(actual);
  const normalizedExpected = normalize(expected);
  if (normalizedActual === null || normalizedExpected === null) {
    return normalizedActual === normalizedExpected;
  }
  if (
    typeof normalizedActual === "number" &&
    typeof normalizedExpected === "number"
  ) {
    const scale = Math.max(1, Math.abs(normalizedExpected));
    return Math.abs(normalizedActual - normalizedExpected) <= 1e-12 * scale;
  }
  return normalizedActual === normalizedExpected;
}

const cli = parseArguments(process.argv.slice(2));
const artifactToolPath = path.resolve(cli["artifact-tool"]);
const { FileBlob, SpreadsheetFile } = await import(
  pathToFileURL(artifactToolPath).href
);
const jsonPath = path.resolve(cli.json);
const workbookPath = path.resolve(cli.workbook);
const reportPath = path.resolve(cli.report);
const label = cli.label || "三生态系统指标";

const expected = JSON.parse(await fs.readFile(jsonPath, "utf8"));
const workbook = await SpreadsheetFile.importXlsx(
  await FileBlob.load(workbookPath),
);
const actualSheetNames = workbook.worksheets.items.map((sheet) => sheet.name);
if (JSON.stringify(actualSheetNames) !== JSON.stringify(sheetOrder)) {
  throw new Error(
    `${label}: 工作表顺序不一致 ${JSON.stringify(actualSheetNames)}`,
  );
}

const sheetReports = [];
const workbookText = [];
for (const sheetName of sheetOrder) {
  const expectedRows = expected[sheetName];
  if (!Array.isArray(expectedRows) || expectedRows.length === 0) {
    throw new Error(`${label}/${sheetName}: JSON 为空`);
  }
  const headers = Object.keys(expectedRows[0]);
  const expectedMatrix = [
    headers,
    ...expectedRows.map((row) =>
      headers.map((header) => normalize(row[header])),
    ),
  ];
  const actualMatrix = workbook.worksheets
    .getItem(sheetName)
    .getUsedRange(true).values;
  if (actualMatrix.length !== expectedMatrix.length) {
    throw new Error(
      `${label}/${sheetName}: 行数不一致 ` +
        `${actualMatrix.length} != ${expectedMatrix.length}`,
    );
  }
  for (let rowIndex = 0; rowIndex < expectedMatrix.length; rowIndex += 1) {
    if (actualMatrix[rowIndex].length !== expectedMatrix[rowIndex].length) {
      throw new Error(`${label}/${sheetName}: 第${rowIndex + 1}行列数不一致`);
    }
    for (
      let columnIndex = 0;
      columnIndex < expectedMatrix[rowIndex].length;
      columnIndex += 1
    ) {
      if (actualMatrix[rowIndex][columnIndex] !== null) {
        workbookText.push(String(actualMatrix[rowIndex][columnIndex]));
      }
      if (
        !equalCell(
          actualMatrix[rowIndex][columnIndex],
          expectedMatrix[rowIndex][columnIndex],
        )
      ) {
        throw new Error(
          `${label}/${sheetName}: 单元格` +
            `(${rowIndex + 1},${columnIndex + 1})不一致；` +
            `actual=${JSON.stringify(actualMatrix[rowIndex][columnIndex])};` +
            `expected=${JSON.stringify(expectedMatrix[rowIndex][columnIndex])}`,
        );
      }
    }
  }
  sheetReports.push({
    name: sheetName,
    rows: expectedRows.length,
    columns: headers.length,
    valuesMatchJson: "PASS",
  });
}

assertNoMisleadingMethodText(workbookText.join("\n"), `${label}/Excel`);

const errorInspection = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 300 },
});
if (!errorInspection.ndjson.includes("matched 0")) {
  throw new Error(`${label}: 公式错误扫描未通过`);
}
await fs.rm(`${workbookPath}.inspect.ndjson`, { force: true });

const report = {
  status: "PASS",
  label,
  workbookPath,
  sheets: sheetReports,
  formulaErrorScan: "PASS",
  methodTextScan: "PASS",
};
await fs.mkdir(path.dirname(reportPath), { recursive: true });
await fs.writeFile(reportPath, JSON.stringify(report, null, 2), "utf8");
console.log(JSON.stringify(report));
