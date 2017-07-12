#entrybox.nPops.f <-
#function(tryval) {
#cat("entrybox.nPops.f: Callback call: ", sys.call(), "\n")
#DUEcopy("nPops")
#DUEcopy("proportions")
#DUEcopy("thisPop")
nPopsTemp = as.integer(DUEenv$nPops)

if(nPopsTemp < DUEenv$nPops) {
  DUEenv$proportions = DUEenv$proportions[1:nPopsTemp]/sum(DUEenv$proportions[1:nPopsTemp])
  if(DUEenv$thisPop > nPopsTemp) 
    DUEenv$thisPop <- nPopsTemp
}
if(nPopsTemp > DUEenv$nPops) {
  newPopIndices <- (DUEenv$nPops+1):nPopsTemp
  DUEenv$proportions[newPopIndices] <- 0 
  for(i in newPopIndices) {
    DUEenv$the.means.pop[[i]] <- DUEenv$the.means.pop[[DUEenv$nPops]]
    DUEenv$the.variances.pop[[i]] <- DUEenv$the.variances.pop[[DUEenv$nPops]]
    DUEenv$the.correlations.pop[[i]] <- DUEenv$the.correlations.pop[[DUEenv$nPops]]
  }
}
DUEenv$nPops <- nPopsTemp




