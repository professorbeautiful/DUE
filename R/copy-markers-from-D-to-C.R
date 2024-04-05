#'   copy-markers-from-D-to-C
#'   For copying Markers from Descript to Camtasia.

#'  Step 1:  export transcript from D as markdown.
#'      Publish->Export->Transcript
#'      Toggle on Include markers
#'      Under Timecodes, toggle on Markers.
#'  Step 2: Make sure that the cmproj project is closed.
#'  Step 3: run  copy_markers_from_D_to_C() to extract the marker info.
#'      This will save a BACKUP inside cmproj, AND copy newFile over the
#'      previous tscproj
#'  Step 4:  reopen the project.
#'  
#'  This should work whether there were previously markers or not.
#'  If there were, they will be overwritten, which is what we want.

#      

cmProjLocation = '/Users/Roger/Google Drive/_HOME/DUE/DUE tour video/'
cmProjName = 'DUE tour video, in progress'
cmProjPath = paste0(cmProjLocation, cmProjName, '.cmproj/')
transcriptPath = paste0(cmProjLocation,
                        'DUE tour video, in progress.md')
file.exists(transcriptPath)
hasAnyMarkers = function(tscprojPath=tscprojPath){
  0 == (system(intern = F, paste0( "grep '\"toc\"' '", tscprojPath, "'")))
} 
tscprojPath = paste0(cmProjPath,  'project.tscproj')
file.exists(tscprojPath)
hasAnyMarkers(tscprojPath)

copy_markers_from_D_to_C =  function()  {
  # make a BACKUP just in case
  system(paste0('cp "', tscprojPath, '" "', tscprojPath, '".BACKUP'))
  # Does the tscprojPath already have any markers?
  tscprojFile = readLines(tscprojPath)
  if(! identical(length(grep('"toc"', tscprojFile))>0,  
                 hasAnyMarkers(tscprojPath)  ) )
    warning('failed toc test')
  transcript = readLines(transcriptPath)
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
  #data.frame(DtoC_toc_times,markerTimes, markerMinutes, markerSeconds)
  markerTemplateSubstituted = paste(markerTemplateSubstituted, collapse=',\n')
  # cat(markerTemplateSubstituted, '\n')
  # writeLines(markerTemplateSubstituted,
  #            'DUE tour video/markers-from-D-to-C.txt')
  
  postMarkerSection = tscprojFile[
    grep('"captionAttributes"', tscprojFile):length(tscprojFile)
  ]
  if(hasAnyMarkers(tscprojPath)) {  ##
    preMarkerSection = tscprojFile[
      1:(grep('"toc"', tscprojFile) - 2)
    ] 
  } else {
    preMarkerSection = tscprojFile[
      1: (grep('"captionAttributes"', tscprojFile) - 1)
    ]
  }
  newFile <<- c(preMarkerSection, 
'    "parameters" : {
      "toc" : {
        "type" : "string",
        "keyframes" : [',
              markerTemplateSubstituted,
'       ]
      }
    },', postMarkerSection
)
  writeLines(newFile, 'newFile.tscproj')

  system(paste0('cp newFile.tscproj "', tscprojPath, '"'))
  # Now you can reopen the C project.
  #'  
  #'  OK this works. I pasted the tscproj into a new txt file here, 
  #'  Then used writeLines to put markerTemplateSubstituted into pbcopy
  #'  Then pasted into appropriate place (search "toc")
  #'  Checked the boundaries.
  #'  And the project opened with all the markers.
  #'  Using vi was not successful.
  #'   
}
copy_markers_from_D_to_C()
