<#
.SYNOPSIS
    One-shot installer for anvil.el MCP server on Windows.

.DESCRIPTION
    Installs Emacs (via winget, unless -SkipEmacs), clones anvil.el,
    writes a minimal ~/.emacs.d/anvil-init.el, starts `runemacs --daemon`,
    drops anvil-stdio.sh + anvil-stdio.cmd wrapper into ~/.emacs.d/,
    and registers the MCP server in %USERPROFILE%\.claude.json.

    anvil-stdio.sh is a bash script, so Git for Windows (or MSYS2) is
    required.  The installer detects either.

.PARAMETER SkipEmacs
    Do not try to install Emacs.  The script will still fail if Emacs
    is not reachable on PATH.

.PARAMETER Prefix
    Where to clone anvil.el.
    Default: $env:USERPROFILE\.emacs.d\external-packages\anvil.el

.PARAMETER Branch
    Branch or tag to check out.  Default: v0.3.1

.PARAMETER ServerId
    MCP server id registered in ~/.claude.json.  Default: anvil

.PARAMETER DryRun
    Print every action but do not execute.

.PARAMETER Yes
    Non-interactive: answer yes to confirmations.

.EXAMPLE
    # Simplest — will install Emacs via winget if missing
    iwr -useb https://raw.githubusercontent.com/zawatton/anvil.el/master/install.ps1 | iex

    # If you already have Emacs:
    .\install.ps1 -SkipEmacs
#>

[CmdletBinding()]
param(
    [switch]$SkipEmacs,
    [string]$Prefix   = "$env:USERPROFILE\.emacs.d\external-packages\anvil.el",
    [string]$Branch   = 'v0.3.1',
    [string]$Repo     = 'https://github.com/zawatton/anvil.el.git',
    [string]$ServerId = 'anvil',
    [switch]$DryRun,
    [switch]$Yes
)

$ErrorActionPreference = 'Stop'

$EmacsDir    = Join-Path $env:USERPROFILE '.emacs.d'
$ClaudeConf  = Join-Path $env:USERPROFILE '.claude.json'
$StdioSrc    = Join-Path $Prefix          'anvil-stdio.sh'
$StdioDst    = Join-Path $EmacsDir        'anvil-stdio.sh'
$CmdWrapper  = Join-Path $EmacsDir        'anvil-stdio.cmd'
$AnvilInit   = Join-Path $EmacsDir        'anvil-init.el'
$InitEl      = Join-Path $EmacsDir        'init.el'

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

function Log  { param($Msg) Write-Host "[anvil] $Msg" -ForegroundColor Cyan }
function Warn { param($Msg) Write-Host "[warn]  $Msg" -ForegroundColor Yellow }
function Die  { param($Msg) Write-Host "[error] $Msg" -ForegroundColor Red; exit 1 }

function Invoke-Step {
    param([string]$Desc, [scriptblock]$Block)
    if ($DryRun) {
        Write-Host "    + $Desc" -ForegroundColor DarkGray
    } else {
        & $Block
    }
}

function Confirm-Or-Die {
    param([string]$Prompt)
    if ($Yes) { return }
    $reply = Read-Host "$Prompt [y/N]"
    if ($reply -notmatch '^(y|Y|yes|YES)$') { Die 'Aborted by user.' }
}

function Get-BashPath {
    # Prefer Git for Windows (most users have it), fall back to MSYS2.
    $candidates = @(
        "$env:ProgramFiles\Git\bin\bash.exe",
        "${env:ProgramFiles(x86)}\Git\bin\bash.exe",
        'C:\msys64\usr\bin\bash.exe',
        'C:\msys64\mingw64\bin\bash.exe'
    )
    foreach ($p in $candidates) {
        if (Test-Path $p) { return $p }
    }
    $cmd = Get-Command bash.exe -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    return $null
}

# ---------------------------------------------------------------------------
# Banner
# ---------------------------------------------------------------------------

Log 'anvil.el installer (Windows)'
Log "  prefix:    $Prefix"
Log "  repo:      $Repo"
Log "  branch:    $Branch"
Log "  server-id: $ServerId"
Log "  skipEmacs: $SkipEmacs"
Log "  dryRun:    $DryRun"
Write-Host ''

# ---------------------------------------------------------------------------
# 1. Prerequisites
# ---------------------------------------------------------------------------

Log 'step 1/7: prerequisites'
if (-not (Get-Command git.exe -ErrorAction SilentlyContinue)) {
    Die 'git not found on PATH. Install Git for Windows: https://git-scm.com/download/win'
}

$bash = Get-BashPath
if (-not $bash) {
    Die 'bash not found. Install Git for Windows (includes bash): https://git-scm.com/download/win'
}
Log "  bash:  $bash"

if (-not (Get-Command python.exe -ErrorAction SilentlyContinue) -and
    -not (Get-Command py.exe     -ErrorAction SilentlyContinue)) {
    Warn 'python not found — will use PowerShell JSON handling instead (still fine)'
}

# ---------------------------------------------------------------------------
# 2. Emacs
# ---------------------------------------------------------------------------

Log 'step 2/7: emacs'
$emacs = Get-Command emacs.exe -ErrorAction SilentlyContinue
if ($emacs) {
    $ver = (& $emacs.Source --version 2>$null | Select-Object -First 1)
    Log "  detected: $ver"
} elseif ($SkipEmacs) {
    Die 'emacs not found on PATH and -SkipEmacs was given. Install Emacs 28.2+ first.'
} else {
    $wg = Get-Command winget.exe -ErrorAction SilentlyContinue
    if (-not $wg) {
        Die 'winget not found. Install Emacs manually (https://www.gnu.org/software/emacs/download.html) and re-run with -SkipEmacs.'
    }
    Confirm-Or-Die 'Install Emacs via winget (GNU.Emacs) now?'
    Invoke-Step 'winget install --id GNU.Emacs --silent --accept-package-agreements --accept-source-agreements' {
        winget install --id GNU.Emacs --silent --accept-package-agreements --accept-source-agreements
    }
    # winget installs to a path that may not be on PATH until next shell.
    $possible = @(
        "$env:ProgramFiles\Emacs\emacs-*\bin\emacs.exe",
        "$env:LOCALAPPDATA\Programs\Emacs\emacs-*\bin\emacs.exe"
    )
    foreach ($glob in $possible) {
        $hit = Get-ChildItem -Path $glob -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($hit) {
            $env:PATH = "$($hit.Directory.FullName);$env:PATH"
            Log "  added to PATH for this session: $($hit.Directory.FullName)"
            break
        }
    }
    if (-not (Get-Command emacs.exe -ErrorAction SilentlyContinue)) {
        Die 'winget install reported success but emacs.exe is still not reachable. Open a new shell and re-run with -SkipEmacs.'
    }
}

# ---------------------------------------------------------------------------
# 3. Clone anvil.el
# ---------------------------------------------------------------------------

Log "step 3/7: clone anvil.el → $Prefix"
if (Test-Path (Join-Path $Prefix '.git')) {
    Log '  repo present, updating'
    Invoke-Step "git -C $Prefix fetch --tags origin" {
        git -C "$Prefix" fetch --tags origin
        git -C "$Prefix" checkout "$Branch"
        git -C "$Prefix" pull --ff-only origin "$Branch" 2>$null | Out-Null
    }
} else {
    Invoke-Step "git clone --branch $Branch $Repo $Prefix" {
        $parent = Split-Path -Parent $Prefix
        if (-not (Test-Path $parent)) { New-Item -ItemType Directory -Path $parent -Force | Out-Null }
        git clone --branch "$Branch" --depth 1 "$Repo" "$Prefix"
    }
}

# ---------------------------------------------------------------------------
# 4. Minimal init file
# ---------------------------------------------------------------------------

Log "step 4/7: bootstrap $AnvilInit"
Invoke-Step "mkdir $EmacsDir" {
    if (-not (Test-Path $EmacsDir)) {
        New-Item -ItemType Directory -Path $EmacsDir -Force | Out-Null
    }
}

# Emacs parses load-path strings as platform paths, but forward slashes are
# safest (backslashes need doubling inside a Lisp string).
$loadPath = $Prefix.Replace('\', '/')

$initContent = @"
;;; anvil-init.el --- minimal anvil bootstrap -*- lexical-binding: t; -*-
;; Generated by install.ps1 on $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')
;; Regenerating is safe — this file carries no user state.

(add-to-list 'load-path "$loadPath")
(require 'anvil)
;; anvil-server-commands holds anvil-server-start / -stop etc.  It is
;; autoload-cookied, but manual-install (plain add-to-list + require) does
;; not generate loaddefs, so we require it explicitly.
(require 'anvil-server-commands)
(anvil-enable)
(anvil-server-start)

(provide 'anvil-init)
;;; anvil-init.el ends here
"@

Invoke-Step "write $AnvilInit" {
    Set-Content -Path $AnvilInit -Value $initContent -Encoding UTF8
    Log "  wrote $AnvilInit"
}

$hookLine = '(load (expand-file-name "anvil-init.el" user-emacs-directory))'
$DotEmacs = Join-Path $env:USERPROFILE '.emacs'

function Ensure-Hook {
    param([string]$Target, [bool]$CreateIfMissing)
    if (-not (Test-Path $Target)) {
        if ($CreateIfMissing) {
            ";;; $(Split-Path -Leaf $Target) --- generated by anvil installer`r`n$hookLine`r`n" |
                Set-Content -Path $Target -Encoding UTF8
            Log "  created $Target"
        }
        return
    }
    if (Select-String -Path $Target -SimpleMatch 'anvil-init.el' -Quiet) {
        Log "  $Target already loads anvil-init (ok)"
    } else {
        Add-Content -Path $Target -Value "`r`n;; added by anvil installer`r`n$hookLine"
        Log "  appended anvil-init hook to $Target"
    }
}

Invoke-Step "ensure init files load anvil-init" {
    # Some distros / ports pre-seed ~/.emacs; if it exists we must write
    # the hook there too (Emacs prefers ~/.emacs over ~/.emacs.d/init.el
    # once both exist).  We never create ~/.emacs ourselves.
    Ensure-Hook -Target $InitEl   -CreateIfMissing $true
    Ensure-Hook -Target $DotEmacs -CreateIfMissing $false
}

# ---------------------------------------------------------------------------
# 5. Start / restart daemon
# ---------------------------------------------------------------------------

Log 'step 5/7: emacs daemon'

$alive = $false
try {
    $probe = & emacsclient.exe -e 't' 2>$null
    if ($probe -match 't') { $alive = $true }
} catch { $alive = $false }

if ($alive) {
    # Non-destructive: require/enable/server-start are all idempotent,
    # so loading anvil-init.el into a live daemon preserves any unrelated
    # state the user already has there.
    Log '  daemon already running — loading anvil-init into existing daemon'
    Invoke-Step "emacsclient -e (load `"$AnvilInit`")" {
        $loadExpr = "(load `"$($AnvilInit.Replace('\','/'))`")"
        & emacsclient.exe -e $loadExpr 2>$null | Out-Null
    }
} else {
    Log '  starting fresh daemon'
    Invoke-Step 'runemacs --daemon' {
        $runemacs = (Get-Command runemacs.exe -ErrorAction SilentlyContinue)
        if ($runemacs) {
            Start-Process -FilePath $runemacs.Source -ArgumentList '--daemon' -WindowStyle Hidden
        } else {
            Start-Process -FilePath 'emacs.exe' -ArgumentList '--daemon' -WindowStyle Hidden
        }
        Start-Sleep -Seconds 3
    }
}

if (-not $DryRun) {
    try {
        $probe = & emacsclient.exe -e '(featurep (quote anvil))' 2>$null
        if ($probe -match 't') {
            Log '  anvil feature loaded in daemon'
        } else {
            Warn 'anvil did not load in daemon.'
            Warn "  inspect: emacsclient -e '(load `"$($AnvilInit.Replace('\','/'))`")'"
        }
    } catch {
        Warn "daemon probe failed: $_"
    }
}

# ---------------------------------------------------------------------------
# 6. stdio bridge + .cmd wrapper
# ---------------------------------------------------------------------------

Log 'step 6/7: install stdio bridge'
if (-not (Test-Path $StdioSrc)) {
    Die "Missing $StdioSrc (repo checkout looks incomplete)"
}
Invoke-Step "copy $StdioSrc → $StdioDst" {
    Copy-Item -Path $StdioSrc -Destination $StdioDst -Force
    Log "  installed $StdioDst"
}

# Claude Code calls the .cmd wrapper; the wrapper translates native Windows
# paths and forwards them to the bash script.  This is cleaner than putting
# bash + script args directly in ~/.claude.json (which requires argument
# escaping of a quoted bash path).
$cmdContent = @"
@echo off
rem Generated by anvil install.ps1 — regenerating is safe.
"$bash" "$($StdioDst.Replace('\','/'))" --server-id=$ServerId
"@

Invoke-Step "write $CmdWrapper" {
    Set-Content -Path $CmdWrapper -Value $cmdContent -Encoding ASCII
    Log "  wrote $CmdWrapper"
}

# ---------------------------------------------------------------------------
# 7. Register with Claude Code
# ---------------------------------------------------------------------------

Log "step 7/7: register MCP server in $ClaudeConf"

Invoke-Step "update $ClaudeConf" {
    if (Test-Path $ClaudeConf) {
        Copy-Item -Path $ClaudeConf -Destination "$ClaudeConf.anvil-backup" -Force
        $cfg = Get-Content -Raw -Path $ClaudeConf | ConvertFrom-Json
    } else {
        $cfg = [pscustomobject]@{}
    }

    if (-not $cfg.PSObject.Properties['mcpServers']) {
        $cfg | Add-Member -NotePropertyName mcpServers -NotePropertyValue ([pscustomobject]@{})
    }

    $entry = [pscustomobject]@{
        command = $CmdWrapper
        args    = @()
    }

    # Replace (or add) the entry keyed by $ServerId.
    if ($cfg.mcpServers.PSObject.Properties[$ServerId]) {
        $cfg.mcpServers.$ServerId = $entry
    } else {
        $cfg.mcpServers | Add-Member -NotePropertyName $ServerId -NotePropertyValue $entry
    }

    $cfg | ConvertTo-Json -Depth 32 |
        Set-Content -Path $ClaudeConf -Encoding UTF8
    Log "  registered mcpServers.$ServerId → $CmdWrapper"
}

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------

Write-Host ''
Log 'install complete'
Log '  restart Claude Code (or run: claude mcp list) to pick up the new server.'
Log "  verify:  emacsclient -e '(anvil-server-list-tools)'"
Log '  persist daemon across reboots: create a Task Scheduler entry running'
Log "           `"$($emacs.Source)`" --daemon  at logon (optional)"

if ($DryRun) {
    Write-Host ''
    Warn 'DRY RUN — nothing was actually executed. Re-run without -DryRun.'
}
