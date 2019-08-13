biggerthantarget = which(outputs > target)
toobig = min(eightprobs["T",]-MTDtoxicity)
smallestAboveTarget = 
  min(log10doseValues[(eightprobs["T",]-MTDtoxicity)>=0])
largestBelowMTDtoxicity = 
  max(log10doseValues[(eightprobs["T",]-MTDtoxicity)<=0])

smallestAboveMTDtoxicity = 
  min(log10doseValues[(eightprobs["T",]-MTDtoxicity)>=0])
largestBelowMTDtoxicity = 
  max(log10doseValues[(eightprobs["T",]-MTDtoxicity)<=0])

MTDdose = 10^mean(smallestAboveMTDtoxicity, largestBelowMTDtoxicity)
EUatMTDdose_L = eightprobs["EU", which(log10doseValues==smallestAboveMTDtoxicity)]
EUatMTDdose_U = eightprobs["EU", which(log10doseValues==largestBelowMTDtoxicity)]
