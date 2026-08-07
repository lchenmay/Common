param([switch] $Full, [switch] $Json, [switch] $PolicyOnly)

$ErrorActionPreference = 'Stop'
$repoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..')).Path
. (Join-Path $repoRoot 'CodexPolicy\VerificationCommon.ps1')
Reset-CodexVerificationResults
$quiet = [bool] $Json
$statusLines = @(git -C $repoRoot status --porcelain=v1 --untracked-files=all)

Invoke-CodexVerificationCheck -Name 'policy-tests' -WorkingDirectory $repoRoot -Executable 'powershell.exe' -Arguments @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', (Join-Path $repoRoot 'CodexPolicy\Test-Policy.ps1'), '-PolicyPath', (Join-Path $repoRoot '.codex\policy.json')) -Quiet:$quiet
if (-not $PolicyOnly) {
    Invoke-CodexVerificationCheck -Name 'dotnet-core-release-build' -WorkingDirectory $repoRoot -Executable 'dotnet' -Arguments @('build', (Join-Path $repoRoot 'Common-Codex.slnf'), '-c', 'Release', '--no-restore') -Quiet:$quiet
    $mobileChanged = $Full -or @($statusLines | Where-Object { $_ -match '(?i)^.. (UtilMaui|MauiFs|MauiFsLogics)[\\/]' }).Count -gt 0
    if ($mobileChanged) {
        Invoke-CodexVerificationCheck -Name 'dotnet-mobile-release-build' -WorkingDirectory $repoRoot -Executable 'dotnet' -Arguments @('build', (Join-Path $repoRoot 'Common-All.sln'), '-c', 'Release', '--no-restore') -Quiet:$quiet
    }
    $kestrelChanged = $Full -or @($statusLines | Where-Object { $_ -match '(?i)^.. UtilKestrel[\\/]' }).Count -gt 0
    if ($kestrelChanged -and (Test-Path -LiteralPath (Join-Path $repoRoot 'UtilKestrel\Test\UtilKestrel.Test.fsproj'))) {
        Invoke-CodexVerificationCheck -Name 'util-kestrel-tests' -WorkingDirectory $repoRoot -Executable 'dotnet' -Arguments @('test', (Join-Path $repoRoot 'UtilKestrel\Test\UtilKestrel.Test.fsproj'), '-c', 'Release', '--no-restore') -Quiet:$quiet
    }
    $typescriptChanged = $Full -or @($statusLines | Where-Object { $_ -match '(?i)^.. TypeScriptVue[\\/]' }).Count -gt 0
    if ($typescriptChanged) {
        Invoke-CodexVerificationCheck -Name 'typescript-vue-build' -WorkingDirectory (Join-Path $repoRoot 'TypeScriptVue') -Executable 'npm' -Arguments @('run', 'build') -Quiet:$quiet
    }
    if ($kestrelChanged) {
        Invoke-CodexVerificationCheck -Name 'aiarwa-compatibility-build' -WorkingDirectory 'D:\DEV\Aiarwa' -Executable 'dotnet' -Arguments @('build', 'D:\DEV\Aiarwa\Aiarwa.sln', '-c', 'Release', '--no-restore') -Quiet:$quiet
    }
}

Write-CodexVerificationReport -Repository $repoRoot -ChangedFileCount $statusLines.Count -Full:$Full -PolicyOnly:$PolicyOnly -Json:$Json
if (-not $script:CodexVerificationSuccess) { exit 1 }
exit 0
