## Start in my Documents directory

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

## To be able to run this script, run powershell as the administrator
#  and run the command:
#      Set-ExecutionPolicy Unrestricted
#  Then as your regular user run the commands:
#      cd,, .\Documents\WindowsPowerShell\
#      Unblock-File Microsoft.PowerShell_profile.ps1
#  Now when powershell is launched, this file will configure your
#  powershell environment.
#
#  You have made Windows more useful, and your regular user more powerful,
#  but remember: "With great power comes great responsibilities."
