library(excel.link)

#Load relevant files. Warning: this is a lot of information. The file size is listed after each sheet.
disp1 <- xl.read.file(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_FY2002_2010.xlsx"
                    , password = "Djs01242020") #39MB
disp2 <- xl.read.file(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_FY2011_2019.xlsx"
                      , password = "Djs01242020") #23MB
off1 <- xl.read.file(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_FY2002_2010.xlsx"
                     , password = "Djs01242020") #80MB
off2 <- xl.read.file(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_FY2011_2019.xlsx"
                     , password = "Djs01242020") #52MB
demo <- xl.read.file(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Complaint_Demographics_FY2002_2019.xlsx"
                     , password = "Djs01242020") #42MB

#Merge the two large tables. This may also take a little while.

disp <- rbind(disp1,disp2)
off  <- rbind(off1,off2)

#Join the tables to create a master table using the keys

offdemo <- merge(off,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
offdisp <- merge(off,disp,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- merge(offdisp,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))