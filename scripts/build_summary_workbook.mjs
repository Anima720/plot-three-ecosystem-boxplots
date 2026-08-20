import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

function parseArguments(argv) {
  const values = {};
  for (let index = 0; index < argv.length; index += 2) {
    const key = argv[index];
    const value = argv[index + 1];
    if (!key?.startsWith("--") || value === undefined) {
      throw new Error(
        "参数格式应为 --json <path> --output <path> " +
          "--preview-dir <path> --report <path> " +
          "--artifact-tool <artifact_tool.mjs> [--label <text>]",
      );
    }
    values[key.slice(2)] = value;
  }
  for (const required of [
    "json",
    "output",
    "preview-dir",
    "report",
    "artifact-tool",
  ]) {
    if (!values[required]) {
      throw new Error(`缺少参数 --${required}`);
    }
  }
  return values;
}

const cli = parseArguments(process.argv.slice(2));
const artifactToolPath = path.resolve(cli["artifact-tool"]);
const {
  FileBlob,
  SpreadsheetFile,
  Workbook,
} = await import(pathToFileURL(artifactToolPath).href);
const previewRoot = path.resolve(cli["preview-dir"]);
const reportPath = path.resolve(cli.report);
const jobs = [
  {
    label: cli.label || "三生态系统指标",
    jsonPath: path.resolve(cli.json),
    outputPath: path.resolve(cli.output),
  },
];

const sheetOrder = [
  "分析总览",
  "两两比较",
  "描述统计",
  "模型诊断",
  "残差诊断",
  "异常值标记",
  "方法与质控",
];

function displayLength(value) {
  const text = value == null ? "" : String(value);
  let width = 0;
  for (const character of text) {
    width += /[\u0000-\u00ff]/.test(character) ? 1 : 2;
  }
  return width;
}

function columnName(index) {
  let number = index + 1;
  let name = "";
  while (number > 0) {
    const remainder = (number - 1) % 26;
    name = String.fromCharCode(65 + remainder) + name;
    number = Math.floor((number - 1) / 26);
  }
  return name;
}

function populateSheet(workbook, sheetName, rows) {
  if (!Array.isArray(rows) || rows.length === 0) {
    throw new Error(`工作表 ${sheetName} 没有数据。`);
  }

  const headers = Object.keys(rows[0]);
  const matrix = [
    headers,
    ...rows.map((row) =>
      headers.map((header) => {
        const value = row[header];
        return value === undefined ? null : value;
      }),
    ),
  ];

  const sheet = workbook.worksheets.add(sheetName);
  sheet.showGridLines = false;
  sheet.freezePanes.freezeRows(1);

  const lastColumn = columnName(headers.length - 1);
  const usedAddress = `A1:${lastColumn}${matrix.length}`;
  const usedRange = sheet.getRange(usedAddress);
  usedRange.values = matrix;
  usedRange.format.font = {
    name: "Microsoft YaHei",
    size: 10,
    color: "#1F2937",
  };
  usedRange.format.verticalAlignment = "center";
  usedRange.format.borders = {
    preset: "all",
    style: "thin",
    color: "#D7E3E1",
  };

  const headerRange = sheet.getRange(`A1:${lastColumn}1`);
  headerRange.format.fill = "#0F766E";
  headerRange.format.font = {
    name: "Microsoft YaHei",
    size: 10,
    bold: true,
    color: "#FFFFFF",
  };
  headerRange.format.horizontalAlignment = "center";
  headerRange.format.verticalAlignment = "center";
  headerRange.format.wrapText = true;
  headerRange.format.rowHeight = 32;

  const columnWidths = [];
  for (let columnIndex = 0; columnIndex < headers.length; columnIndex += 1) {
    const columnLetter = columnName(columnIndex);
    const values = rows.map((row) => row[headers[columnIndex]]);
    const nonNull = values.filter(
      (value) => value !== null && value !== undefined,
    );
    const isNumeric =
      nonNull.length > 0 &&
      nonNull.every(
        (value) => typeof value === "number" && Number.isFinite(value),
      );
    const isInteger =
      isNumeric && nonNull.every((value) => Number.isInteger(value));

    if (isNumeric) {
      const dataRange = sheet.getRange(
        `${columnLetter}2:${columnLetter}${matrix.length}`,
      );
      dataRange.format.horizontalAlignment = "right";
      dataRange.setNumberFormat(isInteger ? "0" : "0.000000000000000");
    }

    const contentLengths = values.map((value) => {
      if (
        isNumeric &&
        typeof value === "number" &&
        Number.isFinite(value)
      ) {
        return isInteger
          ? String(value).length
          : value.toFixed(15).length;
      }
      return displayLength(value);
    });
    const maximumContent = Math.max(
      displayLength(headers[columnIndex]),
      ...contentLengths,
    );
    const longTextColumn =
      (!isNumeric && maximumContent > 24) ||
      /方法|说明|判定|备注|风险|口径|数据源|检验对象|诊断对象|类别|结论|分支|源列|校正|估计量类型|置信区间类型|版本/.test(
        headers[columnIndex],
      );
    const minimumWidth = isNumeric && !isInteger ? 22 : 10;
    const maximumWidth = longTextColumn ? 44 : isNumeric ? 32 : 28;
    const width = Math.max(
      minimumWidth,
      Math.min(maximumWidth, maximumContent + 3),
    );
    columnWidths.push(width);
    const columnRange = sheet.getRange(
      `${columnLetter}1:${columnLetter}${matrix.length}`,
    );
    columnRange.format.columnWidth = width;
    if (longTextColumn) {
      columnRange.format.wrapText = true;
    }
  }

  for (let rowIndex = 0; rowIndex < rows.length; rowIndex += 1) {
    const estimatedLines = Math.max(
      1,
      ...headers.map((header, columnIndex) =>
        Math.ceil(
          displayLength(rows[rowIndex][header]) /
            Math.max(columnWidths[columnIndex] - 2, 8),
        ),
      ),
    );
    sheet.getRange(
      `A${rowIndex + 2}:${lastColumn}${rowIndex + 2}`,
    ).format.rowHeight = Math.min(
      94,
      18 * Math.min(estimatedLines, 5) + 4,
    );
  }

  return {
    name: sheetName,
    rows: rows.length,
    columns: headers.length,
    range: usedAddress,
  };
}

await fs.mkdir(previewRoot, { recursive: true });
const report = { workbooks: [] };

for (const job of jobs) {
  const raw = JSON.parse(await fs.readFile(job.jsonPath, "utf8"));
  const workbook = Workbook.create();
  const workbookReport = {
    label: job.label,
    outputPath: job.outputPath,
    sheets: [],
  };

  for (const sheetName of sheetOrder) {
    workbookReport.sheets.push(
      populateSheet(workbook, sheetName, raw[sheetName]),
    );
  }

  await fs.mkdir(path.dirname(job.outputPath), { recursive: true });
  const output = await SpreadsheetFile.exportXlsx(workbook);
  await output.save(job.outputPath);

  const organismPreviewDir = path.join(previewRoot, job.label);
  await fs.mkdir(organismPreviewDir, { recursive: true });
  for (const sheetName of sheetOrder) {
    const preview = await workbook.render({
      sheetName,
      autoCrop: "all",
      scale: 0.82,
      format: "png",
    });
    const bytes = new Uint8Array(await preview.arrayBuffer());
    await fs.writeFile(
      path.join(organismPreviewDir, `${sheetName}.png`),
      bytes,
    );
  }

  const reopened = await SpreadsheetFile.importXlsx(
    await FileBlob.load(job.outputPath),
  );
  const reopenedSheetNames = reopened.worksheets.items.map(
    (sheet) => sheet.name,
  );
  if (JSON.stringify(reopenedSheetNames) !== JSON.stringify(sheetOrder)) {
    throw new Error(
      `${job.label}工作簿重开后的工作表顺序不正确：` +
        JSON.stringify(reopenedSheetNames),
    );
  }
  for (const sheetReport of workbookReport.sheets) {
    const reopenedSheet = reopened.worksheets.getItem(sheetReport.name);
    const reopenedRows = reopenedSheet.getUsedRange(true).values.length - 1;
    if (reopenedRows !== sheetReport.rows) {
      throw new Error(
        `${job.label}/${sheetReport.name}重开后行数不符：` +
          `${reopenedRows} != ${sheetReport.rows}`,
      );
    }
  }

  const errorInspection = await reopened.inspect({
    kind: "match",
    searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
    options: { useRegex: true, maxResults: 300 },
  });
  if (!errorInspection.ndjson.includes("matched 0")) {
    throw new Error(
      `${job.label}工作簿公式错误扫描未通过：${errorInspection.ndjson}`,
    );
  }
  // artifact-tool 的 inspect 会在工作簿旁生成只用于核查的 sidecar；
  // 它不属于最终交付物，核查完成后只删除这个精确文件。
  await fs.rm(`${job.outputPath}.inspect.ndjson`, { force: true });
  workbookReport.reopenCheck = "PASS";
  workbookReport.formulaErrorScan = "PASS";
  report.workbooks.push(workbookReport);
}

await fs.writeFile(reportPath, JSON.stringify(report, null, 2), "utf8");
console.log(JSON.stringify(report));
