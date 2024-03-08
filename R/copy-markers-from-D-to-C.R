##   copy-markers-from-D-to-C

#  Step 1:  export transcript from D as markdown.
#  Step 2: run this script to extract the marker info.
transcript = readLines('DUE tour video/DUE tour intro transcript.md')
markers = grep('^##', transcript, v=T)
head(markers)
markerStrings = gsub('.*] ', '', markers)
markerTimes = gsub('] .*', '', markers)
markerTimes = gsub('##..', '', markerTimes)
data.frame(markerTimes, markerStrings)

markerTimesForCamtasia = sapply(
  strsplit(split = ':', markerTimes),
  function(tim) { #ignoring hours for now.
    paste0(tim[2], tim[3],'00')}
)
markerMinutes = as.numeric(sapply(
  strsplit(split = ':', markerTimes),
  function(tim) { tim[2]}
))
markerSeconds = as.numeric(sapply(
  strsplit(split = ':', markerTimes),
  function(tim) { tim[3]}
))
data.frame(markerMinutes,markerSeconds,markerTimes)
DtoC_toc_time = function(minutes, seconds, frames=0)
  as.character((minutes*60*30 + seconds*30 + frames)* 2352)
### really 23520000  but need to avoid conversion to scientific notation

markerTemplate = # 10 spaces, 12 spaces...
'          {
            "endTime" : toc_time,
            "time" : toc_time,
            "value" : "markerString",
            "duration" : 0
          }'
DtoC_toc_times = DtoC_toc_time(markerMinutes, markerSeconds)
markerTemplateSubstituted = sapply(
  1:length(markerMinutes),
  function(ind)
    gsub(x=gsub('toc_time', 
                paste0(DtoC_toc_time(markerMinutes[ind], 
                              markerSeconds[ind]), '0000'), 
                markerTemplate
    ),
    'markerString', markerStrings[ind])
)
data.frame(DtoC_toc_times,markerTimes, markerMinutes, markerSeconds)
markerTemplateSubstituted = paste(markerTemplateSubstituted, collapse=',\n')
# cat(markerTemplateSubstituted, '\n')
# writeLines(markerTemplateSubstituted,
#            'DUE tour video/markers-from-D-to-C.txt')

# make a new txt file here in Rstudio.
# --- or better, set it to JSON!  Collapse sections at will.
tscprojPath = 'DUE tour video/DUE tour video- IN PROGRESS.cmproj/project.tscproj'
system(paste0('cat "', tscprojPath, '" | pbcopy'))
# paste into the new txt file.
#'  save as  for example
#'  project-with-many-markers.tscproj
writeLines(markerTemplateSubstituted,
           pipe('pbcopy'))
# Search for toc , and paste the copied markerTemplateSubstituted
# CLOSE the C project.
system(paste('cp project-with-many-markers.tscproj', tscprojPath))
# Now you can reopen the C project.
#'  
#'  OK this works. I pasted the tscproj into a new txt file here, 
#'  Then used writeLines to put markerTemplateSubstituted into pbcopy
#'  Then pasted into appropriate place (search "toc")
#'  Checked the boundaries.
#'  And the project opened with all the markers.
#'  Using vi was not successful.
#'   

