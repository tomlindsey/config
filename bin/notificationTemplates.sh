#!/bin/bash
#
# before running this script execute the following sql and generate a text file
# with one line per url.  if you export the file as delimited (csv, etc) remove
# be sure to remove any seperators
#
# select distinct url from filerep where id in (select templateid from notify_def where status != 86)

#
# the first argument should tell us which region
if [ "$1" == "" ]; then
    echo "give me a udb1 server: { main | release | stage | prod, etc. }"
    exit 1
fi

#
# variables
theFileList=~/scratch/notificationTemplates
theUser=db2inst1
theServer=udb1.$1
theTempDir=./temp

theWorkDir=work_cc

#
# create a temp dir and any required the working directories
if [ -d $theTempDir ]; then
    echo "temp directory already exists:" $theTempDir
    exit 2
fi
mkdir $theTempDir
mkdir $theTempDir/$theWorkDir

#
# get the files from the production server
sftp $theUser@$theServer <<EOF
cd /Volumes/Lockbox
lcd $theTempDir
`sed 's#\(file:///lockbox/\)\(.*\)#get \2 .#g' $theFileList`
bye
EOF

#
# cp files from temp to work that have a CC address
find $theTempDir -type f -name "*.zip" -depth 1 -exec sh -c 'unzip -c {} | egrep -i "^CC:[ ]*[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}"' \; -exec cp {} $theTempDir/$theWorkDir \;
#
# create an sftp upload script
pushd $theTempDir/$theWorkDir
ls -1 *.zip > p
cat > upload <<EOF
#!/bin/bash
sftp root@$theServer <<XXX
`grep -f p ~/scratch/notificationTemplates | sed 's#\(file:///lockbox/\)\(.*\)\(FSDID.*zip\)#put \3 /Volumes/Lockbox/\2 #g'`
bye
XXX
EOF
chmod +x upload
rm p
popd
