Set-PSReadLineOption -HistoryNoDuplicates -ShowToolTips
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward
Set-PSReadlineOption -HistorySavePath C:\Users\Abiola\.pshistory.txt
