## Put this file in your Documents\WindowsPowerShell folder.
#
#  Powershell will start in your "home" directory, which is
#  usually C:\Users\your_user_name, the one above Documents.
#
#  This file hopelessly out of date cica. 2021.
#

## Modify the Path environmental variable
"Virgin Path: " + $env:Path
$env:Path += ";$home\opt\bin"

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

