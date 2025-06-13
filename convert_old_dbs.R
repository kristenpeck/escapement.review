# Fetch old versions of the database
# Note need to be in a 32-bit version of R, then install RODBC 
#pull out the SILs and SENs and save into csvs, then exit and load into current project


library(RODBC)


#MISSING 2012 Area 4:

#set file path to old db
path = file.path("C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/escapement.review/old_dbs/Access97 Stream Esc_ DB_2012.mdb")
channel <- RODBC::odbcConnectAccess(path)

#retrieve list of tables
sqlTables(channel)

#pull out SILs
BC16.area4.2012SIL <- RODBC::sqlFetch(channel,"StreamInspection")

#pull out SENs
BC16.area4.2012SEN <- RODBC::sqlFetch(channel,"tblSEN")

#pull out Streams
BC16.area4.2012stream <- RODBC::sqlFetch(channel,"Streams")

#check if Babine streams are in here (470-490): 
unique(BC16.area4.2012SIL$StreamID)

#EXPORT
write.csv(BC16.area4.2012SIL,file = "./old_dbs/StreamInspection2012.csv", row.names = F)
write.csv(BC16.area4.2012SEN,file = "./old_dbs/tblSEN2012.csv", row.names = F)





#MISSING 2013 Area 4:

#set file path to old db
path = file.path("C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/escapement.review/old_dbs/Access00 Stream Esc_DB_2013.mdb")
channel <- RODBC::odbcConnectAccess(path)

#retrieve list of tables
sqlTables(channel)

#pull out SILs
BC16.area4.2013SIL <- RODBC::sqlFetch(channel,"StreamInspection")

#pull out SENs
BC16.area4.2013SEN <- RODBC::sqlFetch(channel,"tblSEN")

#pull out Streams
BC16.area4.2013stream <- RODBC::sqlFetch(channel,"Streams")

#check if Babine streams are in here (470-490): 
unique(BC16.area4.2013SIL$StreamID)

#EXPORT
write.csv(BC16.area4.2013SIL,file = "./old_dbs/StreamInspection2013.csv", row.names = F)
write.csv(BC16.area4.2013SEN,file = "./old_dbs/tblSEN2013.csv", row.names = F)



