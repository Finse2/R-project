param([switch]$WithOllama)

$ErrorActionPreference = 'Stop'
$launcher = Join-Path $env:LOCALAPPDATA 'Programs\R\R-4.5.3\bin\Rscript.exe'
if (-not (Test-Path -LiteralPath $launcher)) {
    $launcher = (Get-Command Rscript.exe -ErrorAction Stop).Source
}

$ollamaServer = $null
$previousOllamaHost = $env:OLLAMA_HOST
Push-Location -LiteralPath (Split-Path -Parent $PSScriptRoot)
try {
    if ($WithOllama) {
        $ollamaExe = (Get-Command ollama.exe -ErrorAction Stop).Source
        $env:OLLAMA_HOST = '127.0.0.1:11434'
        $ollamaUrl = 'http://127.0.0.1:11434/api/version'
        try {
            $null = Invoke-RestMethod -Uri $ollamaUrl -TimeoutSec 2
        } catch {
            $ollamaServer = Start-Process -FilePath $ollamaExe -ArgumentList 'serve' -WindowStyle Hidden -PassThru
        }
    }
    & $launcher --vanilla -e "shiny::runApp('.', host = '127.0.0.1', launch.browser = TRUE)"
    if ($LASTEXITCODE -ne 0) { throw "R exited with status $LASTEXITCODE." }
} finally {
    if ($ollamaServer -and -not $ollamaServer.HasExited) {
        Stop-Process -Id $ollamaServer.Id -ErrorAction SilentlyContinue
    }
    if ($WithOllama) { $env:OLLAMA_HOST = $previousOllamaHost }
    Pop-Location
}
