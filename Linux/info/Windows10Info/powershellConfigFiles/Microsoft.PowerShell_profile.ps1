## Put this file in the Documents/WindowsPowerShell folder.
#
#  Powershell will start in your "home" directory, which is
#  usually C:\Users\your_user_name, the one above Documents.
#

## Print original Path
"Original Path: " + $env:Path

## Modify the Path environmental variable
#  Put the JDK I installed before any native Java
$env:Path = "C:\Program Files\Java\jdk1.8.0_131\bin;$env:Path"
#  Set path to locally installed scripts and programs.
$env:Path += "$home\Documents\WindowsPowerShell\psbin"
$env:Path += ";$home\opt\bin"

## Define some useful functions
# Change the PowerShell prompt
function prompt {
    "`n$pwd`n>  "
}

# Define something that behaves more like Posix ls -a
# gci cmdlet lies to you if file attributes are hidden
function la {
    if ($args.count -eq 0) {
        Get-ChildItem -Force
    } else {
        foreach ($arg in $args) {
            Get-ChildItem -Force $arg
        }
    }
}

# Print path in a list format (can be assigned to arrays)
function path {
    $SystemRoot = $env:SystemRoot
    ($env:Path).split(";") | foreach {
          $_ = $_.Replace('%SystemRoot%', $SystemRoot)
          $_ -replace '\\$', ''
    }
}

# Drill down through path and find files that match patterns
function whence {
    $patterns = $args[0..$args.count]
    foreach ($pattern in $patterns) {
        foreach ($dir in path) {
            $file = "${dir}\${pattern}"
            if (Test-Path $file) {
                gci $dir | ForEach-Object {
                    if ($_.Name -like $pattern) {
                        Write-Output("${dir}\$_")
                    }
                }
            }
        }
    }
}

# Function wrapper for gnu diff
function gdiff {
   C:\Users\schelleg\opt\vim72\diff.exe -a -w $args
}

# Need to easily grep thru files and directories 
# note: I am not using the right powershell paradigms
function pmgrep {
    $pattern = $args[0]
    $files = $args[1..($args.count-1)]
    foreach ($file in $files) {
       if (Test-Path $file) {
           if (Test-Path $file -pathType leaf) {
               $count = 0
               $contents = (gc $file)
               $contents -split "`n" | Select-String "$pattern" | ForEach-Object { 
                   Write-Output("${file}: " + $_ + "`n")
                   $count++
               }
               if ( $count -ne 0 ) {
                   Write-Output(("_"*40) + "`n" )
               }
           } else {
               $subfiles = gci $file
               foreach ( $subfile in $subfiles ) {
                   pmgrep $pattern "$file\$subfile"
               }
           }
       }
    }
}

## Setup useful aliases
# For executables
ni -path alias:np -value "C:\Windows\notepad.exe"
ni -path alias:wp -value "C:\Windows\write.exe"
ni -path alias:fm -value "C:\Windows\explorer.exe"

## Show what version of PowerShell we are using
"`nPowerShell Version = " +  $PSVersionTable.PSVersion.toString()

## Turn off all context colors since these don't play well with me reversing
#  my colors with the Magnifier accessibility tool.
#
#     In the shortcut for Powershell on my shortcut bar I 
#        rt-click title bar -> Properties -> Colors
#     Set Screen and Popup Background to 255, 255, 255 (This gets reversed to black)
#     Set Screen and Popup Text color to   1,  36,  86 (Reverses to a pleasant yellow)
#
#  The following lines are from info I found on stack overflow.
#
$fgColor = "DarkMagenta"
$bgColor = "White"
Set-PSReadlineOption -TokenKind Parameter -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind String -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Operator -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Type -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Variable -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Number -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Member -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Command -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Comment -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -TokenKind Keyword -ForegroundColor $fgColor -BackgroundColor $bgColor
Set-PSReadlineOption -ContinuationPromptForegroundColor $fgColor -ContinuationPromptBackgroundColor $bgColor
Set-PSReadlineOption -EmphasisForegroundColor $fgColor -EmphasisBackgroundColor $bgColor
Set-PSReadlineOption -ErrorForegroundColor $fgColor -ErrorBackgroundColor $bgColor
(Get-Host).PrivateData.ErrorForegroundColor=$fgColor
(Get-Host).PrivateData.ErrorBackgroundColor=$bgColor
(Get-Host).PrivateData.WarningForegroundColor=$fgColor
(Get-Host).PrivateData.WarningBackgroundColor=$bgColor
(Get-Host).PrivateData.DebugForegroundColor=$fgColor
(Get-Host).PrivateData.DebugBackgroundColor=$bgColor
(Get-Host).PrivateData.VerboseForegroundColor=$fgColor
(Get-Host).PrivateData.VerboseBackgroundColor=$bgColor
(Get-Host).PrivateData.ProgressForegroundColor=$fgColor
(Get-Host).PrivateData.ProgressBackgroundColor=$bgColor

## To be able to run this script, run powershell as the administrator
#  and run the command:
#      Set-ExecutionPolicy Unrestricted
#  Then, as your regular user, run the commands:
#      cd .\Documents\WindowsPowerShell\
#      Unblock-File Microsoft.PowerShell_profile.ps1
#  Now when powershell is launched, this file will configure your
#  powershell environment.
#
#  You have made Windows more useful, and your regular user more powerful,
#  but remember: "With great power comes great responsibility."
