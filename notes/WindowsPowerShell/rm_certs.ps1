## Remove other people's certs from your NIPRNet computer
#
function rm_certs {
$Certs = Get-ChildItem -Path "Cert:\CurrentUser\My" | Where-Object DNSNameList -Match "\d\d\d\d\d\d\d\d\d\d"
    $Certs | ForEach-Object{
        if ($_.DNSNameList -NotMatch $($env:USERNAME.substring(0,10))) {
            Write-Host "REMOVED - NOT $env:USERNAME"
            Get-ChildItem -Path "Cert:\CurrentUser\My" | Where-Object Thumbprint -eq $_.Thumbprint | Remove-Item -Force -Verbose
        } else {
            Write-Host "RETAINED - $env:USERNAME"
        }
    }
}

rm_certs