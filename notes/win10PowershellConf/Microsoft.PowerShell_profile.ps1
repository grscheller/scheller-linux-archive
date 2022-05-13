## Location: $HOME\WindowsPowerShell\Microsoft.PowerShell_profile.ps1

## Modify the Path environmental variable
"Virgin Path: " + $env:Path
$env:Path += "$home\opt\bin;"

## Define some useful functions
# Change the PowerShell prompt
function prompt {
    "`n$pwd`n>  "
}

# Define something that behaves more like Posix ls -a
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
function dp {
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

function to-euler7 {
    scp -P 22555 $args[0..$args.count] grs@euler7:./catch
}

function to-gauss17 {
    scp -P 31502 $args[0..$args.count] grs@gauss17:./catch
}

function from-euler7 {
    scp -P 22555 "grs@euler7:./$($args[0])" .
}

function from-gauss17 {
    scp -P 31502 "grs@gauss17:$($args[0])" .
}

function euler7 {
    ssh -p 22555 grs@euler7
}

function gauss17 {
    ssh -p 31502 grs@gauss17
}

## Setup useful aliases
# For executables
ni -path alias:np -value "C:\Windows\notepad.exe"
ni -path alias:wp -value "C:\Windows\write.exe"
ni -path alias:fm -value "C:\Windows\explorer.exe"

## Show what version of PowerShell we are using
"`nPowerShell Version = " +  $PSVersionTable.PSVersion.toString()
