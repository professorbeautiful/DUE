target = DUEsaving$MTDtoxicity   ## 0.33

smallestAboveTarget = 
  min(log10doseValues[(eightprobs["T",]-target)>=0])
largestBelowTarget = 
  max(log10doseValues[(eightprobs["T",]-target)<=0])

MTDdose = 10^mean(smallestAboveTarget, largestBelowTarget)
EUatMTDdose_L = eightprobs["EU", which(log10doseValues==smallestAboveTarget)]
EUatMTDdose_U = eightprobs["EU", which(log10doseValues==largestBelowTarget)]
