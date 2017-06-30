library(RCurl)

url <- "ftp://ftp-cmsaf.dwd.de/ORD23934/"
host<-"ftp-cmsaf.dwd.de"
usename<-"routcm"
userpwd <- "Sheosch4"
destfile<-"/nobackup/users/dirksen/SARAH/"

#via bash: ftp host
#enter username
#enter password
#mget ORD23934/*
#gaat naar huidige dir
# http://www.tldp.org/HOWTO/FTP-3.html
# to unzip all the files in one directory:
# http://linux-journal.blogspot.nl/2005/04/unzip-all-zip-files-in-directory.html
#https://askubuntu.com/questions/518370/extract-several-zip-files-each-in-a-new-folder-with-the-same-name-via-ubuntu-t 
# unzip \*.zip -d /nobackup/users/dirksen/SARAH/


#WERKT NIET
# url<-paste( url, usename, '&form_pass=', userpwd, sep = '') 
# 
# filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE,crlf = TRUE)
# filenames <- strsplit(filenames, "\r\n")
# filenames = unlist(filenames)
# filenames
# 
# #location<-"ftp://ftp-cmsaf.dwd.de/ORD23934/"
# 
# library(utils)
# download.file(url=url,destfile = destfile)
# 
# ## Url for downloading - Does not contain login credentials.
# url <- "http://statcounter.com/p7447608/csv/download_log_file?ufrom=1323783441&uto=1323860282" 
# 
# USERNAME = 'your username'
# PASSWORD = 'your password'
# 
# ## Url for downloading - Does contain login credentials. Use this one!! 
# url <- paste( 'http://statcounter.com/p7447608/csv/download_log_file?ufrom=1323783441&uto=1323860282&form_user=', USERNAME, '&form_pass=', PASSWORD, sep = '') 
