param([string] $DevelopmentRoot = 'D:\DEV')

$ErrorActionPreference = 'Stop'
$repositories = Get-ChildItem -LiteralPath $DevelopmentRoot -Directory | Where-Object {
    Test-Path -LiteralPath (Join-Path $_.FullName '.git')
}
$failures = New-Object System.Collections.Generic.List[string]

foreach ($repository in $repositories) {
    $policyPath = Join-Path $repository.FullName '.codex\policy.json'
    if (-not (Test-Path -LiteralPath $policyPath)) {
        $failures.Add("Missing policy: $($repository.FullName)")
        continue
    }
    Write-Output "Testing $($repository.Name)"
    & (Join-Path $PSScriptRoot 'Test-Policy.ps1') -PolicyPath $policyPath
    if ($LASTEXITCODE -ne 0) {
        $failures.Add("Policy tests failed: $($repository.FullName)")
    }
}

if ($failures.Count -gt 0) {
    $failures | ForEach-Object { Write-Error $_ }
    exit 1
}
Write-Output "All repository policies passed: $($repositories.Count)"
