param(
    [string] $Platform = $null
)

$ErrorActionPreference = "Stop";

function Invoke-Exe {
    param(
        [ScriptBlock] $Cmd,
        [int[]] $AllowedExitCodes = @(0)
    )

    $backupErrorActionPreference = $script:ErrorActionPreference
    $script:ErrorActionPreference = 'Continue'

    try {
        & $Cmd
        if ($AllowedExitCodes -notcontains $LastExitCode) {
            throw "External command failed with exit code ${LastExitCode}: $Cmd"
        }
    } finally {
        $script:ErrorActionPreference = $backupErrorActionPreference
    }
}

function Test-AppVeyor {
    return Test-Path env:APPVEYOR
}

function Get-StackUrl {
    param(
        [Parameter(Mandatory=$true)]
        [string] $Platform
    )

    if ($Platform -eq 'x86_64') {
        return 'https://get.haskellstack.org/stable/windows-x86_64.zip'
    } else {
        return 'https://get.haskellstack.org/stable/windows-i386.zip'
    }
}

function Install-Stack {
    param(
        [Parameter(Mandatory=$true)]
        [string] $Platform
    )

    Invoke-Exe { curl.exe --silent --show-error --output C:\stack.zip --location --insecure -- $(Get-StackUrl -Platform $Platform) }
    Invoke-Exe { 7z.exe x -oC:\sr C:\stack.zip }
    $env:STACK_ROOT = 'C:\sr'
}

function Build-Project {
    param(
        [Parameter(Mandatory=$true)]
        [string] $Platform
    )

    Invoke-Exe { C:\sr\stack.exe build --install-ghc --arch $Platform }
}

if (Test-AppVeyor) {
    $cwd = pwd
    $Platform = $env:PLATFORM
}

Install-Stack -Platform $Platform
Build-Project -Platform $Platform

if (Test-AppVeyor) {
    cd $cwd
}
