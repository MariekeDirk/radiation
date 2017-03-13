library(data.table)

output.sum<-list.files("/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily/",pattern = ".txt",full.names = T)

out.sum<-fread(output.sum)

