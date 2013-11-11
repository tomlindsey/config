#!/bin/bash
#
# assuming that there is a CSV file and 4th field populated with filerep.url
# 1. extract the url and replace it with a directory mount for release
# 2. copy the files to a local directory
#
# select corr_template.id,category,type,url from corr_template join filerep on corr_template.id=filerep.id where status != 86 order by category asc, url asc
mkdir tFiles
pushd tFiles

#
# create the category directories
# copy the files to the directories
awk -F, '{gsub(/ /,"", $2); printf("mkdir %s\n", $2);}' < ../$1 | sort | uniq | bash
awk -F, '{gsub(/ /,"", $2); gsub(/file:\/\/\/lockbox\//,"~/lockbox/release/", $4); printf("cp %s ./%s\n", $4, $2);}' < ../$1 | bash

#
# find all of the files that contain the string ".organization"
rm tFilesWithOrganization
find . -type f -name "*.zip" -exec sh -c 'unzip -p {} | egrep -qi "{[a-zA-Z.]*organization[ ]*}" ' \; -print > tFilesWithOrganization
wc -l tFilesWithOrganization

#
# generate a list of commitment, grant, and payment letter tokens
# generate a list of all tokens
rm tokensForCGPLetters
find GlobalCommitmentLetter CommitmentLetter GrantLetter GlobalGrantLetter PaymentLetter GlobalPaymentLetter -type f -name "*.zip" -exec sh -c 'unzip -p {} | egrep -ho "{[a-zA-Z.]*}" ' \; | sort | uniq > tokensForCGPLetters
wc -l tokensForCGPLetters

rm tokensAll
find . -type f -name "*.zip" -exec sh -c 'unzip -p {} | egrep -ho "{[a-zA-Z.]*}" ' \; | sort | uniq > tokensAll
wc -l tokensAll

#
# for grins calculate the diff between the global grant letter and all others
mkdir glDiff
rm glDiff/*
unzip -p ./GlobalGrantLetter/FSDID-51157818152.zip > global
find GrantLetter -type f -name "*.zip" -execdir sh -c 'unzip -p {} > foo; diff ../global foo > ../glDiff/{}.diff; rm foo' \;
rm global

popd
