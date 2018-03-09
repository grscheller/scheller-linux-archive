## Put this file in the Documents/WindowsPowerShell folder.
#
#  Powershell will start in your "home" directory, which is
#  usually C:\Users\your_user_name, the one above Documents.
#

## Modify the Path environmental variable
"Virgin Path: " + $env:Path
$env:Path += "$home\psbin"      # Powershell scripts/programs I write.
$env:Path += ";$home\opt\bin"   # For stuff I install locally.

## Define some useful functions
# Change the PowerShell prompt
function prompt {
    "`n$pwd`n>  "
}

# Define something that behaves more like Posix ls -a
#    gci lies to you if file attributes are hidden
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

## Set Execution Policy - Can't change this without admin priviledges.
# Since I only run local scripts, RemoteSigned would be the more secure choice.
if ( (Get-ExecutionPolicy).ToString() -ne "Unrestricted" )
{
     Set-ExecutionPolicy Unrestricted
}

## Turn of all context colors since these don't play well with my vision problems.
#  Works well with Settings -> Ease of Access -> High contrast -> High Constrast Black,
#  keywords are just slightly highlighted.
#
#     In the shortcut for Powershell on my shortcut bar I 
#        rt-click title bar -> Properties -> Colors
#     Set Screen and Popup Background to 1, 36, 86
#     Set Screen and Popup Text color to  238, 237, 240
#
#  The following lines I "script-kiddied" off of stack overflow.
$fgColor = "White"
$bgColor = "DarkBlue"
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
