# Runs the specified test number from the specified part. 
# Should be run from within the /src folder.

[CmdletBinding()]
param(
    [Parameter(Mandatory=$True)]
    [int]$partNum,

    [Parameter(Mandatory=$True)]
    [int]$testNum
)

# Error handling
$errorCode # value should be set by callee
trap {
    Write-Host $_
    exit $errorCode
}

$loadString = "(load `"tetris.lisp`")`n"

$testsPath = "..\tests"
$partPath = "\part_"
$testPath = "\test"
$inputFileName = "\input"

if ($partNum -eq 1) {
    $partPath += "1";
} elseif ($partNum -eq 2) {
    $partPath += "2"; 
} else {
    $errorCode = -1
    throw "Test number '$partNum' does not exist."
}

$testPath += ($testNum -as [string]) + "\"
$inputFilePath = $testsPath + $partPath + $testPath + $inputFileName
$fileContents = Get-Content $inputFilePath | Out-String
$loadString + $fileContents | clisp