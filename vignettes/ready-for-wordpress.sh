## Upload the files from vignettes/figures  to the blog media page.
##  No equal signs "=" or "," in figure file names!
##  Check the year/month setting that the files were uploaded.
yearmonth=2019/11/
## Unfortunately, if I add more figures next month, this whole system fails!

## Next, run DUE_vignette-no-base64.html

## Next, run this file.
tempfile=`ls -t /private/var/folders/*/*/T/*/*/DUE_vignette-no-base64.html |head -1`
echo $tempfile
##  Check $tempfile against the R Markdown tab

#For example tempfile=/private/var/folders/6v/y_cp6z4d22ggbgrqdk0g73v80000gv/T/RtmpxbKzxS/preview-112e64d8af0c1.dir/DUE_vignette-no-base64.html

localfolderstring=/Users/Roger/Google%20Drive/_HOME/DUE/vignettes/figures/
localfolder=/Users/Roger/GoogleDrive/_HOME/DUE/vignettes/figures/
## Gee, thanks, Google! Sheesh.   Computer science 101:  no spaces in file names.

blogfolder=http://www.professorbeautiful.org/IveBeenThinkin/wp-content/uploads/$yearmonth 

cat $tempfile | sed "s,$localfolderstring,$blogfolder," | tee "$localfolder/../DUE_vignette-no-base64.html" | pbcopy

## Finally, paste into the html editor at IveBeenThinkin