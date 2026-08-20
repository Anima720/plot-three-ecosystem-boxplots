param(
  [Parameter(Mandatory = $true)]
  [string]$ConfigPath,

  [string]$RscriptPath = "D:\Program Files\R\R-4.5.0\bin\Rscript.exe",

  [string]$NodePath = "C:\Users\86532\.cache\codex-runtimes\codex-primary-runtime\dependencies\node\bin\node.exe",

  [string]$ArtifactToolPath = "C:\Users\86532\.cache\codex-runtimes\codex-primary-runtime\dependencies\node\node_modules\@oai\artifact-tool\dist\artifact_tool.mjs"
)

$ErrorActionPreference = "Stop"

function Get-ConfigIdentityHash {
  param([Parameter(Mandatory = $true)][object]$ConfigObject)

  # expected_branches 是同一批次从首轮到锁定轮唯一允许改变的字段。
  # 其余配置任一变化都会得到不同身份哈希，从而拒绝覆盖既有目录。
  $identityObject = $ConfigObject |
    ConvertTo-Json -Depth 100 |
    ConvertFrom-Json
  if ($identityObject.PSObject.Properties.Name -contains "expected_branches") {
    $identityObject.expected_branches = $null
  }
  $identityJson = $identityObject | ConvertTo-Json -Depth 100 -Compress
  $identityBytes = [System.Text.UTF8Encoding]::new($false).GetBytes($identityJson)
  $sha = [System.Security.Cryptography.SHA256]::Create()
  try {
    return -join ($sha.ComputeHash($identityBytes) | ForEach-Object {
      $_.ToString("X2")
    })
  } finally {
    $sha.Dispose()
  }
}

$resolvedConfig = (Resolve-Path -LiteralPath $ConfigPath).Path
$resolvedRscript = (Resolve-Path -LiteralPath $RscriptPath).Path
$resolvedNode = (Resolve-Path -LiteralPath $NodePath).Path
$resolvedArtifactTool = (Resolve-Path -LiteralPath $ArtifactToolPath).Path
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$analysisScript = Join-Path $scriptRoot "run_three_ecosystem_boxplots.R"
$workbookScript = Join-Path $scriptRoot "build_summary_workbook.mjs"
$statisticsAuditScript = Join-Path $scriptRoot "audit_statistics.R"
$workbookAuditScript = Join-Path $scriptRoot "audit_workbook.mjs"
$pdfAuditScript = Join-Path $scriptRoot "audit_pdfs.py"
$deliveryAuditScript = Join-Path $scriptRoot "audit_delivery.py"

$config = Get-Content -LiteralPath $resolvedConfig -Encoding UTF8 -Raw |
  ConvertFrom-Json
$outputDir = [System.IO.Path]::GetFullPath([string]$config.output_dir)
$temporaryDir = [System.IO.Path]::GetFullPath([string]$config.temp_dir)
$outputStem = [string]$config.output_stem
$summaryJson = Join-Path $temporaryDir ($outputStem + "_七表数据.json")
$outputWorkbook = Join-Path $outputDir ($outputStem + "_统计分析汇总.xlsx")
$previewDir = Join-Path $temporaryDir "excel_previews"
$buildReport = Join-Path $temporaryDir "excel_build_report.json"
$statisticsAuditReport = Join-Path $temporaryDir "statistics_audit_report.json"
$workbookAuditReport = Join-Path $temporaryDir "workbook_audit_report.json"
$pdfPreviewDir = Join-Path $temporaryDir "pdf_previews"
$pdfAuditReport = Join-Path $temporaryDir "pdf_audit_report.json"
$deliveryAuditReport = Join-Path $temporaryDir "delivery_audit_report.json"
$deliveredConfig = Join-Path $outputDir ($outputStem + "_分析配置.json")
$requiredOutputs = @(
  (Join-Path $outputDir ($outputStem + "_箱线图总图.pdf")),
  (Join-Path $outputDir ($outputStem + "_残差Q-Q诊断.pdf")),
  $outputWorkbook,
  (Join-Path $outputDir ($outputStem + "_绘图与统计分析_残差诊断四分流.R")),
  $deliveredConfig
)
$expectedOutputNames = @($requiredOutputs | ForEach-Object {
  [System.IO.Path]::GetFileName($_)
})

# 不允许把历史结果目录当作新批次输出。只有空目录，或能由同一配置
# 证明属于当前批次的确定性重跑目录，才允许替换本流程自己的五个文件。
New-Item -ItemType Directory -Path $temporaryDir -Force | Out-Null
$configIdentityHash = Get-ConfigIdentityHash -ConfigObject $config
$configFileHash = (
  Get-FileHash -LiteralPath $resolvedConfig -Algorithm SHA256
).Hash.ToUpperInvariant()
$ownershipMarker = Join-Path $temporaryDir "current_output_owner.sha256"
$existingEntries = if (Test-Path -LiteralPath $outputDir) {
  @(Get-ChildItem -LiteralPath $outputDir -Force)
} else {
  @()
}
$existingDirectories = @($existingEntries | Where-Object { $_.PSIsContainer })
if ($existingDirectories.Count -gt 0) {
  throw "输出目录含子目录，拒绝覆盖：$outputDir"
}
$existingFiles = @($existingEntries | Where-Object { -not $_.PSIsContainer })
$unexpectedFiles = @($existingFiles | Where-Object {
  $_.Name -notin $expectedOutputNames
})
if ($unexpectedFiles.Count -gt 0) {
  throw "输出目录含非本流程文件，拒绝覆盖：$($unexpectedFiles.Name -join '；')"
}
if ($existingFiles.Count -gt 0) {
  $ownedByMarker = (
    (Test-Path -LiteralPath $ownershipMarker -PathType Leaf) -and
    ((Get-Content -LiteralPath $ownershipMarker -Raw).Trim().ToUpperInvariant() -eq $configFileHash)
  )
  $ownedByDeliveredConfig = $false
  if (Test-Path -LiteralPath $deliveredConfig -PathType Leaf) {
    $existingConfig = Get-Content -LiteralPath $deliveredConfig -Encoding UTF8 -Raw |
      ConvertFrom-Json
    $existingIdentityHash = Get-ConfigIdentityHash -ConfigObject $existingConfig
    $ownedByDeliveredConfig = $existingIdentityHash -eq $configIdentityHash
  }
  if (-not ($ownedByMarker -or $ownedByDeliveredConfig)) {
    throw "既有输出无法证明属于同一配置，拒绝覆盖：$outputDir"
  }
}
[System.IO.File]::WriteAllText(
  $ownershipMarker,
  $configFileHash + [Environment]::NewLine,
  [System.Text.UTF8Encoding]::new($false)
)

# 当前 Windows 会话可能把 C.UTF-8 传给 R；该名称在本机 R 中不可用。
# 使用 R 4.5 已验证的 UTF-8 区域设置，保证中文列名和注释可解析。
$env:LC_ALL = "English_United States.utf8"
$env:LC_CTYPE = "English_United States.utf8"
$env:LANG = "English_United States.utf8"
$previousOutputGuard = $env:THREE_ECOSYSTEM_OUTPUT_GUARD
$analysisExitCode = 1
try {
  $env:THREE_ECOSYSTEM_OUTPUT_GUARD = $configFileHash
  & $resolvedRscript --vanilla $analysisScript $resolvedConfig
  $analysisExitCode = $LASTEXITCODE
} finally {
  if ($null -eq $previousOutputGuard) {
    Remove-Item Env:THREE_ECOSYSTEM_OUTPUT_GUARD -ErrorAction SilentlyContinue
  } else {
    $env:THREE_ECOSYSTEM_OUTPUT_GUARD = $previousOutputGuard
  }
}
if ($analysisExitCode -ne 0) {
  throw "R 分析流程失败，退出码：$analysisExitCode"
}

& $resolvedNode $workbookScript `
  --json $summaryJson `
  --output $outputWorkbook `
  --preview-dir $previewDir `
  --report $buildReport `
  --artifact-tool $resolvedArtifactTool `
  --label ([string]$config.analysis_label)
if ($LASTEXITCODE -ne 0) {
  throw "Excel 构建与重开核查失败，退出码：$LASTEXITCODE"
}

& $resolvedNode $workbookAuditScript `
  --json $summaryJson `
  --workbook $outputWorkbook `
  --artifact-tool $resolvedArtifactTool `
  --report $workbookAuditReport `
  --label ([string]$config.analysis_label)
if ($LASTEXITCODE -ne 0) {
  throw "Excel 逐单元格独立审计失败，退出码：$LASTEXITCODE"
}

$sourceHashAfter = (
  Get-FileHash -LiteralPath ([string]$config.input_file) -Algorithm SHA256
).Hash.ToUpperInvariant()
$expectedHash = ([string]$config.expected_sha256).ToUpperInvariant()
if ($sourceHashAfter -ne $expectedHash) {
  throw "完整流程结束后源文件 SHA256 与配置不一致。"
}

$missingOutputs = @($requiredOutputs | Where-Object {
  -not (Test-Path -LiteralPath $_ -PathType Leaf)
})
if ($missingOutputs.Count -gt 0) {
  throw "最终交付文件不完整：$($missingOutputs -join '；')"
}

& $resolvedRscript --vanilla $statisticsAuditScript `
  $resolvedConfig `
  $summaryJson `
  $statisticsAuditReport
if ($LASTEXITCODE -ne 0) {
  throw "独立统计复核失败，退出码：$LASTEXITCODE"
}

$branchProperties = if ($null -eq $config.expected_branches) {
  @()
} else {
  @($config.expected_branches.PSObject.Properties)
}
$branchesLocked = (
  $branchProperties.Count -eq @($config.metric_columns).Count -and
  $branchProperties.Count -gt 0
)
if (-not $branchesLocked) {
  Write-Output "FIRST_PASS_COMPLETE_INDEPENDENT_BRANCHES_READY_TO_LOCK"
  Write-Output "STATISTICS_AUDIT_REPORT=$statisticsAuditReport"
  Write-Output "SOURCE_SHA256=$sourceHashAfter"
  Write-Output "OUTPUT_DIR=$outputDir"
  exit 0
}

$resolvedPython = (Resolve-Path -LiteralPath ([string]$config.python_executable)).Path
& $resolvedPython -X utf8 $pdfAuditScript `
  --config $resolvedConfig `
  --preview-dir $pdfPreviewDir `
  --report $pdfAuditReport
if ($LASTEXITCODE -ne 0) {
  throw "PDF 结构与渲染审计失败，退出码：$LASTEXITCODE"
}

& $resolvedPython -X utf8 $deliveryAuditScript `
  --config $resolvedConfig `
  --template-r $analysisScript `
  --report $deliveryAuditReport
if ($LASTEXITCODE -ne 0) {
  throw "最终交付目录审计失败，退出码：$LASTEXITCODE"
}

Write-Output "AUTOMATED_AUDITS_COMPLETE_VISUAL_REVIEW_REQUIRED"
Write-Output "SOURCE_SHA256=$sourceHashAfter"
Write-Output "OUTPUT_DIR=$outputDir"
