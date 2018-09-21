#Course/Section Analysis                                                                                             
                                                                                                                    rm(list=ls())
                                                                                                                      responses<-data.frame(

# 1
#ENTER DATE OF ASSESSMENT (OPTIONAL:Leave blank if no date)

"17 September 2018"
                                                                                                                ,

# 2
#ENTER COURSE LETTERS
"G"
                                                                                                                ,

# 3
#ENTER CLASS NUMBER(s)
#Put the section number(s) and separate with a comma. For example, '1,3,5'.
                                                                                                                    c(
3
                                                                                                                      ),
# 4
#WOULD YOU LIKE TO ADD THIS GRADE TO THE LOG?
#This means if you want to add this score to track individual students.
"YES"
                                                                                                                ,
# 5
#WOULD YOU LIKE THE REPORT TO DISPLAY THE TIME YOU GENERATED IT?
#Type "YES" or "NO".
                                                                                                                
"yes"


# 56
# Press "run". You're all set!                                                                     
                                                                                                      )




test_date<-as.character(responses[1,1])
Course<-as.character(responses[1,2])
sections<-as.numeric(responses[,3])
lg<-as.character(responses[1,4])
report<-as.character(responses[1,5])




### INITIAL SETUP INFORMATION ###
usePackage <- function(p) 
{ #CREDIT TO SALEM MARAFI#
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE) 
  require(p, character.only = TRUE)
}
usePackage("rstudioapi")



##ENTER FILE LOCATION OF 'Reports' FOLDER
file_location<-dirname(rstudioapi::getSourceEditorContext()$path)


























###################### DO NOT EDIT BELOW ######################
# DO NOT EDIT BELOW ## DO NOT EDIT BELOW ## DO NOT EDIT BELOW #
# DO NOT EDIT BELOW ## DO NOT EDIT BELOW ## DO NOT EDIT BELOW #
###############################################################


###Folder Creation

for(i in sections){

if(dir.exists(paste(file_location,"/Student_Folders/",Course,i,sep=""))){
  
}else{
  dir.create(paste(file_location,"/Student_Folders/",Course,i,sep=""))
}
}



##Install or add readxl


usePackage("readxl")

##IMPORT DATA##

for(i in sections){
assign(paste("test",i,sep=""), read_excel(path=paste(file_location,"/ScoreCards/Grade_sheet.xlsx",sep=""),sheet=paste(Course,i,sep="")))
}

#Save mem file
for(i in sections){
assign(paste("mem.na",i,sep=""),eval(parse(text=paste("test",i,sep=""))))
}
##Calculate students per section

for(i in sections){
assign(paste("stu",i,sep=""), length(eval(parse(text=paste("test",i,"$Score",sep=""))))-1)
}

##Calculate missing per section

for(i in sections){
assign(paste("miss",i,sep=""), sum(is.na(eval(as.symbol(paste("test",i,sep=""))))))
}

##Calculate total missing values

sum_missing<-0
for(i in sections){
	sum_missing<-sum(
	sum_missing,sum(is.na(eval(as.symbol(paste("test",i,sep="")))))
	)
}

##Calculate total students

sum_students<-0
for(i in sections){
	sum_students<-sum(
	sum_students,length(eval(parse(text=paste("test",i,"$Score",sep=""))))-1)
}

##Removing Missing Values

for(i in sections){
	assign(paste("test",i,sep=""),na.omit(eval(as.symbol(paste("test",i,sep="")))))
}


##Labeling Classes from Sections	

class_value<-eval(as.symbol(paste("test",sections[1],sep="")))
class<-sub(paste(" Class ",sections[1],sep=""),"",class_value$LastName[1])
rm(class_value)



##Calculating Total Scores##

total_scores<-NULL
for(i in sections){
	total_scores<-c(total_scores,eval(parse(text=paste("test",i,"$Score[2:length(test",i,"$Score)]",sep=""))))
}


##Finding Assessment Name##

z<-eval(parse(text=paste("test",sections[1],"$FirstName[1]",sep="")))


##Finding max points##

max<-eval(parse(text=paste("test",sections[1],"$Score[1]",sep="")))






######################################################################
##Write Tracking File##

#install library join
usePackage("plyr")
library(plyr)




if(toupper(lg)=="YES"){

  for(i in sections){
    
    
    if(file.exists(paste(file_location,"/ScoreCards/Memory/track",i,".csv",sep=""))){
      assign(paste("track",i,sep=""),read.csv(paste(file_location,"/ScoreCards/Memory/track",i,".csv",sep="")))
      assign(paste("track",i,sep=""),eval(parse(text=paste("track",i,"[,-1]",sep=""))))
      
    }else{
      
        assign(paste("track",i,sep=""), data.frame(eval(parse(text=paste("test",i,"$LastName[1:length(test",i,"$LastName)]",sep=""))),eval(parse(text=paste("test",i,"$FirstName[1:length(test",i,"$FirstName)]",sep=""))),fix.empty.names=FALSE,stringsAsFactors=FALSE))
      
      eval(parse(text=paste(
        "track",i,"[1,1]<-'Total'",sep=""
      )))
      
      eval(parse(text=paste(
        "track",i,"[1,2]<-'Points'",sep=""
      )))
      
      eval(parse(text=paste("colnames(track",i,")<-c('LastName','FirstName')",sep="")))
    }
    

#Make new testfiles
assign(paste("mem",i,sep=""),eval(parse(text=paste("test",i,sep=""))))

#Merge Track File
zee<-gsub(" ", "_", z)
zee<-gsub("-","90215",zee)

eval(parse(text=paste("colnames(mem",i,")<-c('LastName','FirstName',zee)",sep="")))






if(is.na(match(zee,colnames(eval(parse(text=paste("track",i,sep=""))))))){

  
  
  eval(parse(text=paste(
    "track",i,"[,zee]<-mem.na",i,"[(1:length(mem.na",i,"$Score)),3]"
    ,sep=""
  )))
  
  
  
  #eval(parse(text=paste("track",i,"<-track",i,"[,1:ncol(track",i,")-1]",sep="")))
  
  #assign(paste("track",i,sep=""),eval(parse(text=paste("join(track",i,",mem",i,"[1:length(mem",i,"$LastName),],type='full')",sep=""))))
  
  #assign(paste("track",i,sep=""),eval(parse(text=paste("track",i,"[order(track",i,"$LastName),]",sep=""))))
  
  #Calculate percentage
  
  #eval(parse(text=paste("track",i,"[,ncol(track",i,")]<-100*track",i,"[,ncol(track",i,")]/max",sep="")))
  
  eval(parse(text=paste("track",i,"[2:length(track",i,"$LastName),colnames(track",i,")==zee]<-100*track",i,"[2:length(track",i,"$LastName),colnames(track",i,")==zee]/max",sep="")))
  
  
  
  
  
  
 

#Write Track File

write.csv(eval(parse(text=paste("track",i,sep=""))),eval(parse(text=paste("'",file_location,"/ScoreCards/Memory/track",i,".csv'",sep=""))))

}else{
 
  
  eval(parse(text=paste(
    "track",i,"[,colnames(track",i,")==zee]<-mem.na",i,"[(1:length(mem.na",i,"$Score)),3]"
    ,sep=""
  )))
  
  
  #assign(paste("track",i,sep=""),eval(parse(text=paste("track",i,"[order(track",i,"$LastName),]",sep=""))))
  
  #Calculate percentage
  
  eval(parse(text=paste("track",i,"[2:length(track",i,"$LastName),colnames(track",i,")==zee]<-100*track",i,"[2:length(track",i,"$LastName),colnames(track",i,")==zee]/max",sep="")))
  
  
  #eval(parse(text=paste("track",i,"[,ncol(track",i,")]<-100*track",i,"[,ncol(track",i,")]/max",sep="")))
  
  
  #Write Track File
  
  write.csv(eval(parse(text=paste("track",i,sep=""))),eval(parse(text=paste("'",file_location,"/ScoreCards/Memory/track",i,".csv'",sep=""))))
  
}
}
  
}else{
  for(i in sections){
    
    
    if(file.exists(paste(file_location,"/ScoreCards/Memory/track",i,".csv",sep=""))){
      assign(paste("track",i,sep=""),read.csv(paste(file_location,"/ScoreCards/Memory/track",i,".csv",sep="")))
      assign(paste("track",i,sep=""),eval(parse(text=paste("track",i,"[,-1]",sep=""))))
      
    }else{
      eval(parse(text=paste("track",i,"<-data.frame(0,1)",sep="")))
    }}
}

######
##Create new track files with NA values removed

#for(i in sections){eval(parse(text=paste("track",i,"<-track",i,"[complete.cases(track",i,"[,ncol(track",i,")]),]",sep=""))) }
######################################################################

#track7[complete.cases(track7[,ncol(track7)]),]



##Full File PDF Export##


if(length(sections)>1){
pdf(file=paste(file_location,"/Completed/",Course,"_",z,"_Classes_",paste(sections,collapse="_"),".pdf",sep=""),width=8.2,height=11)
}else{
	pdf(file=paste(file_location,"/Completed/",Course,"_",z,"_Class_",paste(sections,collapse="_"),".pdf",sep=""),width=8.2,height=11)
	}


##COMBINATION PLOTS OF ALL SECTIONS SELECTED##



##Creating the title page##
report<-toupper(report)

if(report=="YES"){
plot(0:8.5, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(4.75, 6, class)
text(4.75, 7, paste(z,"Results"))
text(4.75, 5, test_date)
text(4.75, 4, paste("Report Generated:",format(Sys.time(), "%a %b %d %X %Y")))

}else{
	plot(0:8.5, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(4.75, 6, class)
text(4.75, 7, paste(z,"Results"))
text(4.75, 5, test_date)

}


##Computing mean and median values

a<-round(mean(total_scores),digits=2)
b<-round(median(total_scores),digits=2)


##Sections Density Plot
par(mfrow=c(3,1)) 
dens_tot<-density(total_scores)
dens<-dens_tot
if(length(sections)>1){
plot(dens_tot,xlim=c(min(total_scores),max(total_scores)),main=paste(class,"Classes",paste(sections,collapse=","),"Statistics for",z),xlab=paste("Mean=",a,"    ","Median=",b),axes=FALSE,ylab="")
}else{plot(dens_tot,xlim=c(min(total_scores),max(total_scores)),main=paste(class,"Class",paste(sections,collapse=","),"Statistics for",z),xlab=paste("Mean=",a,"    ","Median=",b),axes=FALSE,ylab="")
	}

axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))

abline(v=.70*max,col="orange",lty=3)
abline(v=.72*max,col="orange",lty=3)
abline(v=.76*max,col="orange",lty=3)
abline(v=.78*max,col="orange",lty=3)
abline(v=.80*max,col="orange",lty=3)
abline(v=.84*max,col="orange",lty=3)
abline(v=.86*max,col="orange",lty=3)
abline(v=.88*max,col="orange",lty=3)
abline(v=.92*max,col="orange",lty=3)
abline(v=.94*max,col="orange",lty=3)
abline(v=.96*max,col="orange",lty=3)
abline(v=.99*max,col="orange",lty=3)
abline(v=mean(total_scores),col="blue")
abline(v=median(total_scores),col="green")

text(x= .7*max, y=0, labels= "F",col="black",pos=2)
text(y= 0, x= .695*max, labels= "D-",col="black",pos=4)
text(y= 0, x= .715*max, labels= "D",col="black",pos=4)
text(y= 0, x= .755*max, labels= "D+",col="black",pos=4)
text(y= 0, x= .775*max, labels= "C-",col="black",pos=4)
text(y= 0, x= .795*max, labels= "C",col="black",pos=4)
text(y= 0, x= .835*max, labels= "C+",col="black",pos=4)
text(y= 0, x= .855*max, labels= "B-",col="black",pos=4)
text(y= 0, x= .875*max, labels= "B",col="black",pos=4)
text(y= 0, x= .915*max, labels= "B+",col="black",pos=4)
text(y= 0, x= .935*max, labels= "A-",col="black",pos=4)
text(y= 0, x= .955*max, labels= "A",col="black",pos=4)
text(y= 0, x= .985*max, labels= "A+",col="black",pos=4)

legend("topleft", legend=c("Mean", "Median"),col=c("blue","green"),lty=1:1, cex=0.8)


##Sections Boxplot

if(length(sections)>1){
boxplot(total_scores,horizontal=TRUE,main=paste("Grade Distribution of", class,"Classes",paste(sections,collapse=","),z),xlab="Score",axes=FALSE)
}else{
	boxplot(total_scores,horizontal=TRUE,main=paste("Grade Distribution of", class,"Class",paste(sections,collapse=","),z),xlab="Score",axes=FALSE)
	}
axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))

legend("topleft", legend=c("Mean"),col=c("blue"),lty=1, cex=0.8)

abline(v=.70*max,col="orange",lty=3)
abline(v=.72*max,col="orange",lty=3)
abline(v=.76*max,col="orange",lty=3)
abline(v=.78*max,col="orange",lty=3)
abline(v=.80*max,col="orange",lty=3)
abline(v=.84*max,col="orange",lty=3)
abline(v=.86*max,col="orange",lty=3)
abline(v=.88*max,col="orange",lty=3)
abline(v=.92*max,col="orange",lty=3)
abline(v=.94*max,col="orange",lty=3)
abline(v=.96*max,col="orange",lty=3)
abline(v=.99*max,col="orange",lty=3)
abline(v=mean(total_scores),col="blue")

text(x= .7*max, y=.5, labels= "F",col="black",pos=2)
text(y= .5, x= .695*max, labels= "D-",col="black",pos=4)
text(y= .5, x= .715*max, labels= "D",col="black",pos=4)
text(y= .5, x= .755*max, labels= "D+",col="black",pos=4)
text(y= .5, x= .775*max, labels= "C-",col="black",pos=4)
text(y= .5, x= .795*max, labels= "C",col="black",pos=4)
text(y= .5, x= .835*max, labels= "C+",col="black",pos=4)
text(y= .5, x= .855*max, labels= "B-",col="black",pos=4)
text(y= .5, x= .875*max, labels= "B",col="black",pos=4)
text(y= .5, x= .915*max, labels= "B+",col="black",pos=4)
text(y= .5, x= .935*max, labels= "A-",col="black",pos=4)
text(y= .5, x= .955*max, labels= "A",col="black",pos=4)
text(y= .5, x= .985*max, labels= "A+",col="black",pos=4)


##Sections Barplot

F<-length(total_scores[round(total_scores/max,digits=2)<.70])
Dminus<-length(total_scores[round(total_scores/max,digits=2) >= .70 & round(total_scores/max,digits=2) < .72])
D<-length(total_scores[round(total_scores/max,digits=2) >= .72 & round(total_scores/max,digits=2) < .76])
Dplus<-length(total_scores[round(total_scores/max,digits=2) >= .76 & round(total_scores/max,digits=2) < .78])
Cminus<-length(total_scores[round(total_scores/max,digits=2) >= .78 & round(total_scores/max,digits=2) < .80])
C<-length(total_scores[round(total_scores/max,digits=2) >= .80 & round(total_scores/max,digits=2) < .84])
Cplus<-length(total_scores[round(total_scores/max,digits=2) >= .84 & round(total_scores/max,digits=2) < .86])
Bminus<-length(total_scores[round(total_scores/max,digits=2) >= .86 & round(total_scores/max,digits=2) < .88])
B<-length(total_scores[round(total_scores/max,digits=2) >= .88 & round(total_scores/max,digits=2) < .92])
Bplus<-length(total_scores[round(total_scores/max,digits=2) >= .92 & round(total_scores/max,digits=2) < .94])
Aminus<-length(total_scores[round(total_scores/max,digits=2) >= .94 & round(total_scores/max,digits=2) < .96])
A<-length(total_scores[round(total_scores/max,digits=2) >= .96 & round(total_scores/max,digits=2) < .99])
Aplus<-(length(total_scores[round(total_scores/max,digits=2) >= .99]))

grades<-c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+")
grade_val<-c(F,Dminus,D,Dplus,Cminus,C,Cplus,Bminus,B,Bplus,Aminus,A,Aplus)
breakdown<-rbind(grades,grade_val)

if(length(sections)>1){
xx<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Classes",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))

}else{xx<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Class",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))
	}
text(x = xx, y = grade_val, label = grade_val, pos = 3, cex = 0.8, col = "red")
text(x=xx,y=grade_val-1,label=paste(round(100*grade_val/sum(grade_val),digits=1),"%",sep=""),pos=3,cex=0.7,col="black")

################# COMPLETION OF COMBINED PLOTS #################


##Comparison of Sections
#NOTE TO SELF: FOR COMPARISON PLOTS,TRY TO DO "IF length(sections)>1..."
if(length(sections)>1){

par(mfrow=c(length(sections),1)) 

##Barplot Comparison of Sections

for(i in sections){
	test<-eval(as.symbol(paste("test",i,sep="")))


F<-length(test$Score[round(test$Score/max,digits=2)<.70])
Dminus<-length(test$Score[round(test$Score/max,digits=2) >= .70 & round(test$Score/max,digits=2) < .72])
D<-length(test$Score[round(test$Score/max,digits=2) >= .72 & round(test$Score/max,digits=2) < .76])
Dplus<-length(test$Score[round(test$Score/max,digits=2) >= .76 & round(test$Score/max,digits=2) < .78])
Cminus<-length(test$Score[round(test$Score/max,digits=2) >= .78 & round(test$Score/max,digits=2) < .80])
C<-length(test$Score[round(test$Score/max,digits=2) >= .80 & round(test$Score/max,digits=2) < .84])
Cplus<-length(test$Score[round(test$Score/max,digits=2) >= .84 & round(test$Score/max,digits=2) < .86])
Bminus<-length(test$Score[round(test$Score/max,digits=2) >= .86 & round(test$Score/max,digits=2) < .88])
B<-length(test$Score[round(test$Score/max,digits=2) >= .88 & round(test$Score/max,digits=2) < .92])
Bplus<-length(test$Score[round(test$Score/max,digits=2) >= .92 & round(test$Score/max,digits=2) < .94])
Aminus<-length(test$Score[round(test$Score/max,digits=2) >= .94 & round(test$Score/max,digits=2) < .96])
A<-length(test$Score[round(test$Score/max,digits=2) >= .96 & round(test$Score/max,digits=2) < .99])
Aplus<-(length(test$Score[round(test$Score/max,digits=2) >= .99])-1)

grades<-c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+")
grade_val1<-c(F,Dminus,D,Dplus,Cminus,C,Cplus,Bminus,B,Bplus,Aminus,A,Aplus)
breakdown<-rbind(grades,grade_val)

xx<-barplot(grade_val1,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Class",i,"\nTotal Students:",eval(as.symbol(paste("stu",i,sep=""))),"\nNumber Missing:",eval(as.symbol(paste("miss",i,sep="")))),ylab="Frequency",ylim=c(0,max(grade_val)+1))
text(x = xx, y = grade_val1, label = grade_val1, pos = 3, cex = 0.8, col = "red")
text(x=xx,y=grade_val1-1,label=paste(round(100*grade_val1/sum(grade_val1),digits=1),"%",sep=""),pos=3,cex=0.7,col="black")
}

##Density Plot Comparison of Sections

par(mfrow=c(length(sections),1)) 
for(i in sections){
	test<-eval(as.symbol(paste("test",i,sep="")))

a<-round(mean(test$Score[2:length(test$Score)]),digits=2)
b<-round(median(test$Score[2:length(test$Score)]),digits=2)

plot(density(test$Score[2:length(test$Score)]),xlim=c(min(total_scores),max(total_scores)),main=paste(test$LastName[1],"Statistics for",z),xlab=paste("Class Mean=",a,"    ","Class Median=",b),axes=FALSE,ylab="")
axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))

abline(v=.70*max,col="orange",lty=3)
abline(v=.72*max,col="orange",lty=3)
abline(v=.76*max,col="orange",lty=3)
abline(v=.78*max,col="orange",lty=3)
abline(v=.80*max,col="orange",lty=3)
abline(v=.84*max,col="orange",lty=3)
abline(v=.86*max,col="orange",lty=3)
abline(v=.88*max,col="orange",lty=3)
abline(v=.92*max,col="orange",lty=3)
abline(v=.94*max,col="orange",lty=3)
abline(v=.96*max,col="orange",lty=3)
abline(v=.99*max,col="orange",lty=3)
abline(v=mean(test$Score[2:length(test$Score)]),col="blue")
abline(v=median(test$Score[2:length(test$Score)]),col="green")

text(x= .7*max, y=0, labels= "F",col="black",pos=2)
text(y= 0, x= .695*max, labels= "D-",col="black",pos=4)
text(y= 0, x= .715*max, labels= "D",col="black",pos=4)
text(y= 0, x= .755*max, labels= "D+",col="black",pos=4)
text(y= 0, x= .775*max, labels= "C-",col="black",pos=4)
text(y= 0, x= .795*max, labels= "C",col="black",pos=4)
text(y= 0, x= .835*max, labels= "C+",col="black",pos=4)
text(y= 0, x= .855*max, labels= "B-",col="black",pos=4)
text(y= 0, x= .875*max, labels= "B",col="black",pos=4)
text(y= 0, x= .915*max, labels= "B+",col="black",pos=4)
text(y= 0, x= .935*max, labels= "A-",col="black",pos=4)
text(y= 0, x= .955*max, labels= "A",col="black",pos=4)
text(y= 0, x= .985*max, labels= "A+",col="black",pos=4)

legend("topleft", legend=c("Mean", "Median"),col=c("blue","green"),lty=1:1, cex=0.8)
}

##Boxplot Comparison of Sections

par(mfrow=c(length(sections),1)) 
for(i in sections){
	test<-eval(as.symbol(paste("test",i,sep="")))

boxplot(test$Score[2:length(test$Score)],horizontal=TRUE,main=paste("Grade Distribution of", test$LastName[1], z),xlab="Score",ylim=c(min(total_scores),max(total_scores)),axes=FALSE)
axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))

legend("topleft", legend=c("Mean"),col=c("blue"),lty=1, cex=0.8)

abline(v=.70*max,col="orange",lty=3)
abline(v=.72*max,col="orange",lty=3)
abline(v=.76*max,col="orange",lty=3)
abline(v=.78*max,col="orange",lty=3)
abline(v=.80*max,col="orange",lty=3)
abline(v=.84*max,col="orange",lty=3)
abline(v=.86*max,col="orange",lty=3)
abline(v=.88*max,col="orange",lty=3)
abline(v=.92*max,col="orange",lty=3)
abline(v=.94*max,col="orange",lty=3)
abline(v=.96*max,col="orange",lty=3)
abline(v=.99*max,col="orange",lty=3)
abline(v=mean(test$Score[2:length(test$Score)]),col="blue")

text(x= .7*max, y=.5, labels= "F",col="black",pos=2)
text(y= .5, x= .695*max, labels= "D-",col="black",pos=4)
text(y= .5, x= .715*max, labels= "D",col="black",pos=4)
text(y= .5, x= .755*max, labels= "D+",col="black",pos=4)
text(y= .5, x= .775*max, labels= "C-",col="black",pos=4)
text(y= .5, x= .795*max, labels= "C",col="black",pos=4)
text(y= .5, x= .835*max, labels= "C+",col="black",pos=4)
text(y= .5, x= .855*max, labels= "B-",col="black",pos=4)
text(y= .5, x= .875*max, labels= "B",col="black",pos=4)
text(y= .5, x= .915*max, labels= "B+",col="black",pos=4)
text(y= .5, x= .935*max, labels= "A-",col="black",pos=4)
text(y= .5, x= .955*max, labels= "A",col="black",pos=4)
text(y= .5, x= .985*max, labels= "A+",col="black",pos=4)
}

##Remove test##
rm(test)

}else{grade_val1<-grade_val}



#################ATTACHING OF INDIVIDUAL PLOTS#################


layout(matrix(c(1,2,3,3), 2, 2, byrow = FALSE), 
       widths=c(1.4,0.6), heights=c(1,1))

for(h in sections){
	for (i in 2:length(eval(parse(text=paste("test",h,"$Score",sep=""))))){
  test<-eval(as.symbol(paste("test",h,sep="")))
  
  if (round(test$Score[i]/max,digits=2)<(.70)) {
    grade="F"
  } else if (round(test$Score[i]/max,digits=2) >= .70 & round(test$Score[i]/max,digits=2) < .72) {
    grade="D-"
  } else if (round(test$Score[i]/max,digits=2) >= .72 & round(test$Score[i]/max,digits=2) < .76) {
    grade="D"
  } else if (round(test$Score[i]/max,digits=2) >= .76 & round(test$Score[i]/max,digits=2) < .78) {
    grade="D+"
  } else if (round(test$Score[i]/max,digits=2) >= .78 & round(test$Score[i]/max,digits=2) < .80) {
    grade="C-"
  } else if (round(test$Score[i]/max,digits=2) >= .80 & round(test$Score[i]/max,digits=2) < .84) {
    grade="C"
  } else if (round(test$Score[i]/max,digits=2) >= .84 & round(test$Score[i]/max,digits=2) < .86) {
    grade="C+"
  } else if (round(test$Score[i]/max,digits=2) >= .86 & round(test$Score[i]/max,digits=2) < .88) {
    grade="B-"
  } else if (round(test$Score[i]/max,digits=2) >= .88 & round(test$Score[i]/max,digits=2) < .92) {
    grade="B"
  } else if (round(test$Score[i]/max,digits=2) >= .92 & round(test$Score[i]/max,digits=2) < .94) {
    grade="B+"
  } else if (round(test$Score[i]/max,digits=2) >= .94 & round(test$Score[i]/max,digits=2) < .96) {
    grade="A-"
  } else if (round(test$Score[i]/max,digits=2) >= .96 & round(test$Score[i]/max,digits=2) < .99) {
    grade="A"
  } else  
    grade="A+"
  
  
  
  set.seed(90215)
  
  a<-round(mean(total_scores),digits=2)
  b<-round(median(total_scores),digits=2)
  
  plot(dens_tot,xlim=c(min(total_scores),max(total_scores)),main=paste(test$FirstName[i],test$LastName[i],z),xlab=paste("Course Mean=",a,"    ","Course Median=",b),axes=FALSE,ylab="") #Rush - Roundabout#
  axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))
  axis(2, seq(0, max(dens$y), 0.01))
  
  
  
  # Add the shaded area.
  dens<-dens_tot
  polygon(c(0, dens$x[dens$x>0 & dens$x < test$Score[i]], test$Score[i]), c(0, dens$y[dens$x>=0 & dens$x <= test$Score[i]], 0),col="powderblue",border="powderblue")
  
  abline(v=.70*max,col="orange",lty=3)
  abline(v=.72*max,col="orange",lty=3)
  abline(v=.76*max,col="orange",lty=3)
  abline(v=.78*max,col="orange",lty=3)
  abline(v=.80*max,col="orange",lty=3)
  abline(v=.84*max,col="orange",lty=3)
  abline(v=.86*max,col="orange",lty=3)
  abline(v=.88*max,col="orange",lty=3)
  abline(v=.92*max,col="orange",lty=3)
  abline(v=.94*max,col="orange",lty=3)
  abline(v=.96*max,col="orange",lty=3)
  abline(v=.99*max,col="orange",lty=3)
  abline(v=mean(total_scores),col="blue")
  abline(v=median(total_scores),col="green")
  
  text(x= .7*max, y=0, labels= "F",col="black",pos=2,cex=.7)
  text(y= 0, x= .69*max, labels= "D-",col="black",pos=4,cex=.7)
  text(y= 0, x= .71*max, labels= "D",col="black",pos=4,cex=.7)
  text(y= 0, x= .75*max, labels= "D+",col="black",pos=4,cex=.7)
  text(y= 0, x= .77*max, labels= "C-",col="black",pos=4,cex=.7)
  text(y= 0, x= .79*max, labels= "C",col="black",pos=4,cex=.7)
  text(y= 0, x= .83*max, labels= "C+",col="black",pos=4,cex=.7)
  text(y= 0, x= .85*max, labels= "B-",col="black",pos=4,cex=.7)
  text(y= 0, x= .87*max, labels= "B",col="black",pos=4,cex=.7)
  text(y= 0, x= .91*max, labels= "B+",col="black",pos=4,cex=.7)
  text(y= 0, x= .93*max, labels= "A-",col="black",pos=4,cex=.7)
  text(y= 0, x= .95*max, labels= "A",col="black",pos=4,cex=.7)
  text(y= 0, x= .98*max, labels= "A+",col="black",pos=4,cex=.7)
  
  ##Calculate percentile
  if(round(100*mean(total_scores<=test$Score[i]),digits=0)==11){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)==12){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)==13){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==1){
  	suff<-"st"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==3){
  	suff<-"rd"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==2){
  	suff<-"nd"
  }else{
  	suff<-"th"
  	}
  
  legend("topleft", legend=c("Mean", "Median",paste(test$FirstName[i])),col=c("blue","green","powderblue"),lty=1:1, cex=0.8)
  
  mtext(paste("Score:",test$Score[i],"out of",max," | ","Letter Grade:",grade," | ","Percentile:",paste(round(100*mean(total_scores<=test$Score[i]),digits=0),suff,sep="")),3)
  
  
 if(length(sections)>1){
  inplot<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Classes",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))
 }else{
 	inplot<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Class",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))
 }
 
  text(x = inplot, y = grade_val, label = grade_val, pos = 3, cex = 0.8, col = "red")
  text(x=xx,y=grade_val-1,label=paste(round(100*grade_val/sum(grade_val),digits=1),"%",sep=""),pos=3,cex=0.7,col="black")
  
  boxplot(total_scores,main="Grade Distribution of",ylab="Score",ylim=c(min(total_scores),max(total_scores)),axes=FALSE)
  axis(2,c(0,min(total_scores),0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))
  
  mtext(paste(z),3)
  abline(h=.70*max,col="orange",lty=3)
  abline(h=.72*max,col="orange",lty=3)
  abline(h=.76*max,col="orange",lty=3)
  abline(h=.78*max,col="orange",lty=3)
  abline(h=.80*max,col="orange",lty=3)
  abline(h=.84*max,col="orange",lty=3)
  abline(h=.86*max,col="orange",lty=3)
  abline(h=.88*max,col="orange",lty=3)
  abline(h=.92*max,col="orange",lty=3)
  abline(h=.94*max,col="orange",lty=3)
  abline(h=.96*max,col="orange",lty=3)
  abline(h=.99*max,col="orange",lty=3)
  abline(h=mean(total_scores),col="blue")
  
  points(round(test$Score[i]), type="p", cex=1.5, pch=8,col="red")
  text(x= 0.5, y= .69*max, labels= "F",col="black",pos=4)
  text(x= 0.5, y= .71*max, labels= "D-",col="black",pos=4)
  text(x= 0.5, y= .75*max, labels= "D",col="black",pos=4)
  text(x= 0.5, y= .77*max, labels= "D+",col="black",pos=4)
  text(x= 0.5, y= .79*max, labels= "C-",col="black",pos=4)
  text(x= 0.5, y= .83*max, labels= "C",col="black",pos=4)
  text(x= 0.5, y= .85*max, labels= "C+",col="black",pos=4)
  text(x= 0.5, y= .87*max, labels= "B-",col="black",pos=4)
  text(x= 0.5, y= .91*max, labels= "B",col="black",pos=4)
  text(x= 0.5, y= .93*max, labels= "B+",col="black",pos=4)
  text(x= 0.5, y= .95*max, labels= "A-",col="black",pos=4)
  text(x= 0.5, y= .98*max, labels= "A",col="black",pos=4)
  text(x= 0.5, y= 1*max, labels= "A+",col="black",pos=4)
  text(x= 1.4, y= mean(total_scores), labels= "Mean",col="blue",pos=3)
  legend("bottomright", legend=test$FirstName[i],col="red",pch=8, cex=0.8)
  
  
}
}

for(h in sections){
  #####PLOT TRACKING FILES
  
  if(eval(parse(text=paste("ncol(track",h,")>3",sep="")))){
    
    

    

    
   for(i in 2:length(eval(parse(text=paste("test",h,"$LastName",sep=""))))){
     par(mfrow=c(1,1),xpd=TRUE)
     
  
     #####Create individual data frame    
     
     
     
     ind_track<-eval(parse(text=paste("track",h,"[track",h,"[,1]==as.character(test",h,"[",i,",1]),1:ncol(track",h,")]"
                                      ,sep="")))
     ind_name<-ind_track[1,c(1,2)]  
     ind_track<-ind_track[3:ncol(ind_track)]
     
     
     #######  
     
     
     
     
     par(mfrow=c(1,1),xpd=TRUE)
     
     
     #Old code  
     #eval(parse(text=paste("plot(1:(ncol(track",h,")-2),track",h,"[",i,",3:ncol(track",h,")],xlim=c(0,ncol(track",h,")-1),ylim=c(30,110),xaxt='n',pch=19,ylab='Grade (%)',axes=FALSE,xlab='Assessment',main='Grades of Major Assessments')",sep="")))  
     #
     
     eval(parse(text=paste("plot(as.numeric(ind_track),xlim=c(0,ncol(track",h,")-1),ylim=c(30,110),xaxt='n',pch=19,ylab='Grade (%)',axes=FALSE,xlab='Assessment',main='Grades of Major Assessments')",sep="")))  
     
     
     ##Calculate Mean Score
     
     
     eval(parse(text=paste("y_mean_value<-rbind(track",h,"[1,","3:ncol(track",h,")],ind_track)",sep="")))
     
     
     y_mean_val<-as.matrix(y_mean_value[, colSums(is.na(y_mean_value))==0],nrow=2)
     
     total_point_num<-sum(y_mean_value[1,]*y_mean_value[2,]/100)
     
     total_point_den<-sum(y_mean_value[1,])
     
     ##
     
     
     axis(2,c(40,50,60,70,72,76,78,80,84,86,88,92,94,96,99),las=1)
     
     
     
     eval(parse(text=paste(
       "lines(c(1:(ncol(ind_track))),ind_track)"
       ,sep=""
     )))
     
     listzee<-eval(parse(text=paste("gsub('90215','-',colnames(ind_track))",sep="")))
     listzee<-eval(parse(text=paste("gsub('_',' ',listzee)",sep="")))
     
     
     ###Putting x labels#################
     
     eval(parse(text=paste(
       "text(x=c(1:(length(colnames(ind_track)))),y=40,labels=listzee,cex=.5,srt=90)"
       ,sep=""
     )))
     
     ####################
     
     #eval(parse(text=paste("mtext(colnames(track",h,")[3:length(colnames(track",h,"))],at=c(1:(length(colnames(track",h,"))-2)),side=1,cex=.5)",sep="")))
     
     for(k in c(70,72,76,78,80,84,86,88,92,94,96,99)){
       eval(parse(text=paste(
         "lines(c(0:(ncol(ind_track))),rep(",k,",(ncol(ind_track)+1)),col='orange',lty=3)"
         ,sep=""
       )))
     }
     
     
     
     eval(parse(text=paste("abline(h=100*total_point_num/total_point_den,col='red',lty=3)",sep="")))
     
     legend("topright","Average",lty=3,col="red")
     
     #abline(h=70,col="orange",lty=3)
     #abline(h=72,col="orange",lty=3)
     #abline(h=76,col="orange",lty=3)
     #abline(h=78,col="orange",lty=3)
     #abline(h=80,col="orange",lty=3)
     #abline(h=84,col="orange",lty=3)
     #abline(h=86,col="orange",lty=3)
     #abline(h=88,col="orange",lty=3)
     #abline(h=92,col="orange",lty=3)
     #abline(h=94,col="orange",lty=3)
     #abline(h=96,col="orange",lty=3)
     #abline(h=99,col="orange",lty=3)
     
     
     #eval(parse(text=paste("abline(lm(as.matrix(track",h,"[",i,",3:ncol(track",h,")])~c(1:(length(colnames(track",h,"))-2))))",sep="")))
     
     mtext(paste(eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep="")))," ",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),sep=""),3)
     
     mtext(paste(as.character(ind_name[1,2])," ",as.character(ind_name[1,1]),sep=""),3)
     
     text(x= 0, y= 68, labels= "F",col="black",pos=4)
     text(x= 0, y= 70, labels= "D-",col="black",pos=4)
     text(x= 0, y= 72, labels= "D",col="black",pos=4)
     text(x= 0, y= 76, labels= "D+",col="black",pos=4)
     text(x= 0, y= 78, labels= "C-",col="black",pos=4)
     text(x= 0, y= 80, labels= "C",col="black",pos=4)
     text(x= 0, y= 84, labels= "C+",col="black",pos=4)
     text(x= 0, y= 86, labels= "B-",col="black",pos=4)
     text(x= 0, y= 88, labels= "B",col="black",pos=4)
     text(x= 0, y= 92, labels= "B+",col="black",pos=4)
     text(x= 0, y= 94, labels= "A-",col="black",pos=4)
     text(x= 0, y= 96, labels= "A",col="black",pos=4)
     text(x= 0, y= 99, labels= "A+",col="black",pos=4)
   }
    
  }else{
    eagles<-"win"
  }
  
}



dev.off()



################Export Individual Files################
#######################################################

##Student Folder Creation

for(h in sections){
  for(i in 2:length(eval(parse(text=paste("test",h,"$Score",sep=""))))){
    if(dir.exists(paste(file_location,"/Student_Folders/",Course,h,"/",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),"_",eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep=""))),sep=""))){
      
    }else{
      dir.create(paste(file_location,"/Student_Folders/",Course,h,"/",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),"_",eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep=""))),sep=""))
 
    }
    
  }
}

#Individual Reports




for(h in sections){
	for (i in 2:length(eval(parse(text=paste("test",h,"$Score",sep=""))))){
		
  test<-eval(as.symbol(paste("test",h,sep="")))
 
 
 ##PDF EXPORT##
 

 if(length(sections)>1){
    pdf(file=paste(file_location,"/Student_Folders/",Course,h,"/",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),"_",eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep=""))),"/",eval(parse(text=paste("test",h,"$FirstName[1]",sep=""))),"_Classes_",paste(sections,collapse="_"),".pdf",sep=""),width=8.2,height=11)
    }else{
    	    pdf(file=paste(file_location,"/Student_Folders/",Course,h,"/",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),"_",eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep=""))),"/",eval(parse(text=paste("test",h,"$FirstName[1]",sep=""))),"_Class_",paste(sections,collapse="_"),".pdf",sep=""),width=8.2,height=11)
    	    }

    	    
layout(matrix(c(1,2,3,3), 2, 2, byrow = FALSE), 
       widths=c(1.4,0.6), heights=c(1,1))
  
  if (round(test$Score[i]/max,digits=2)<(.70)) {
    grade="F"
  } else if (round(test$Score[i]/max,digits=2) >= .70 & round(test$Score[i]/max,digits=2) < .72) {
    grade="D-"
  } else if (round(test$Score[i]/max,digits=2) >= .72 & round(test$Score[i]/max,digits=2) < .76) {
    grade="D"
  } else if (round(test$Score[i]/max,digits=2) >= .76 & round(test$Score[i]/max,digits=2) < .78) {
    grade="D+"
  } else if (round(test$Score[i]/max,digits=2) >= .78 & round(test$Score[i]/max,digits=2) < .80) {
    grade="C-"
  } else if (round(test$Score[i]/max,digits=2) >= .80 & round(test$Score[i]/max,digits=2) < .84) {
    grade="C"
  } else if (round(test$Score[i]/max,digits=2) >= .84 & round(test$Score[i]/max,digits=2) < .86) {
    grade="C+"
  } else if (round(test$Score[i]/max,digits=2) >= .86 & round(test$Score[i]/max,digits=2) < .88) {
    grade="B-"
  } else if (round(test$Score[i]/max,digits=2) >= .88 & round(test$Score[i]/max,digits=2) < .92) {
    grade="B"
  } else if (round(test$Score[i]/max,digits=2) >= .92 & round(test$Score[i]/max,digits=2) < .94) {
    grade="B+"
  } else if (round(test$Score[i]/max,digits=2) >= .94 & round(test$Score[i]/max,digits=2) < .96) {
    grade="A-"
  } else if (round(test$Score[i]/max,digits=2) >= .96 & round(test$Score[i]/max,digits=2) < .99) {
    grade="A"
  } else  
    grade="A+"
  
 
  
  a<-round(mean(total_scores),digits=2)
  b<-round(median(total_scores),digits=2)
  
  plot(dens_tot,xlim=c(min(total_scores),max(total_scores)),main=paste(test$FirstName[i],test$LastName[i],z),xlab=paste("Course Mean=",a,"    ","Course Median=",b),axes=FALSE,ylab="") #FLY EAGLES FLY#
  axis(1,c(0,0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))
  axis(2, seq(0, max(dens$y), 0.01))
  
  
  
  # Add the shaded area.
  dens<-dens_tot
  polygon(c(0, dens$x[dens$x>0 & dens$x < test$Score[i]], test$Score[i]), c(0, dens$y[dens$x>=0 & dens$x <= test$Score[i]], 0),col="powderblue",border="powderblue")
  
  abline(v=.70*max,col="orange",lty=3)
  abline(v=.72*max,col="orange",lty=3)
  abline(v=.76*max,col="orange",lty=3)
  abline(v=.78*max,col="orange",lty=3)
  abline(v=.80*max,col="orange",lty=3)
  abline(v=.84*max,col="orange",lty=3)
  abline(v=.86*max,col="orange",lty=3)
  abline(v=.88*max,col="orange",lty=3)
  abline(v=.92*max,col="orange",lty=3)
  abline(v=.94*max,col="orange",lty=3)
  abline(v=.96*max,col="orange",lty=3)
  abline(v=.99*max,col="orange",lty=3)
  abline(v=mean(total_scores),col="blue")
  abline(v=median(total_scores),col="green")
  
  text(x= .7*max, y=0, labels= "F",col="black",pos=2,cex=.7)
  text(y= 0, x= .69*max, labels= "D-",col="black",pos=4,cex=.7)
  text(y= 0, x= .71*max, labels= "D",col="black",pos=4,cex=.7)
  text(y= 0, x= .75*max, labels= "D+",col="black",pos=4,cex=.7)
  text(y= 0, x= .77*max, labels= "C-",col="black",pos=4,cex=.7)
  text(y= 0, x= .79*max, labels= "C",col="black",pos=4,cex=.7)
  text(y= 0, x= .83*max, labels= "C+",col="black",pos=4,cex=.7)
  text(y= 0, x= .85*max, labels= "B-",col="black",pos=4,cex=.7)
  text(y= 0, x= .87*max, labels= "B",col="black",pos=4,cex=.7)
  text(y= 0, x= .91*max, labels= "B+",col="black",pos=4,cex=.7)
  text(y= 0, x= .93*max, labels= "A-",col="black",pos=4,cex=.7)
  text(y= 0, x= .95*max, labels= "A",col="black",pos=4,cex=.7)
  text(y= 0, x= .98*max, labels= "A+",col="black",pos=4,cex=.7)
  
  
    ##Calculate percentile
  if(round(100*mean(total_scores<=test$Score[i]),digits=0)==11){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)==12){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)==13){
  	suff<-"th"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==1){
  	suff<-"st"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==3){
  	suff<-"rd"
  }else if(round(100*mean(total_scores<=test$Score[i]),digits=0)%%10==2){
  	suff<-"nd"
  }else{
  	suff<-"th"
  	}
  
  
  legend("topleft", legend=c("Mean", "Median",paste(test$FirstName[i])),col=c("blue","green","powderblue"),lty=1:1, cex=0.8)
  
  mtext(paste("Score:",test$Score[i],"out of",max," | ","Letter Grade:",grade," | ","Percentile:",paste(round(100*mean(total_scores<=test$Score[i]),digits=0),suff,sep="")),3)
  
 if(length(sections)>1){
  inplot<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Classes",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))
 }else{
 	inplot<-barplot(grade_val,col="powderblue",names.arg=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),main=paste(class,"Class",paste(sections,collapse=","),"\nTotal Students:",sum_students,"\nNumber Missing:",sum_missing),ylab="Frequency",ylim=c(0,max(grade_val)+1))
  
 }

  
  
 
  
  text(x = inplot, y = grade_val, label = grade_val, pos = 3, cex = 0.8, col = "red")
    text(x=xx,y=grade_val-1,label=paste(round(100*grade_val/sum(grade_val),digits=1),"%",sep=""),pos=3,cex=0.7,col="black")
    
    
  boxplot(total_scores,main="Grade Distribution of",ylab="Score",ylim=c(min(total_scores),max(total_scores)),axes=FALSE)
  axis(2,c(0,min(total_scores),0.1*max,0.2*max,0.3*max,0.4*max,0.5*max,0.6*max,0.7*max,0.72*max,0.76*max,0.78*max,0.8*max,0.84*max,0.86*max,0.88*max,0.92*max,0.94*max,0.96*max,0.99*max,max(total_scores)))
  
  mtext(paste(z),3)
  abline(h=.70*max,col="orange",lty=3)
  abline(h=.72*max,col="orange",lty=3)
  abline(h=.76*max,col="orange",lty=3)
  abline(h=.78*max,col="orange",lty=3)
  abline(h=.80*max,col="orange",lty=3)
  abline(h=.84*max,col="orange",lty=3)
  abline(h=.86*max,col="orange",lty=3)
  abline(h=.88*max,col="orange",lty=3)
  abline(h=.92*max,col="orange",lty=3)
  abline(h=.94*max,col="orange",lty=3)
  abline(h=.96*max,col="orange",lty=3)
  abline(h=.99*max,col="orange",lty=3)
  abline(h=mean(total_scores),col="blue")
  
  points(round(test$Score[i]), type="p", cex=1.5, pch=8,col="red")
  text(x= 0.5, y= .69*max, labels= "F",col="black",pos=4)
  text(x= 0.5, y= .71*max, labels= "D-",col="black",pos=4)
  text(x= 0.5, y= .75*max, labels= "D",col="black",pos=4)
  text(x= 0.5, y= .77*max, labels= "D+",col="black",pos=4)
  text(x= 0.5, y= .79*max, labels= "C-",col="black",pos=4)
  text(x= 0.5, y= .83*max, labels= "C",col="black",pos=4)
  text(x= 0.5, y= .85*max, labels= "C+",col="black",pos=4)
  text(x= 0.5, y= .87*max, labels= "B-",col="black",pos=4)
  text(x= 0.5, y= .91*max, labels= "B",col="black",pos=4)
  text(x= 0.5, y= .93*max, labels= "B+",col="black",pos=4)
  text(x= 0.5, y= .95*max, labels= "A-",col="black",pos=4)
  text(x= 0.5, y= .98*max, labels= "A",col="black",pos=4)
  text(x= 0.5, y= 1*max, labels= "A+",col="black",pos=4)
  text(x= 1.4, y= mean(total_scores), labels= "Mean",col="blue",pos=3)
  legend("bottomright", legend=test$FirstName[i],col="red",pch=8, cex=0.8)
 
  
#####PLOT TRACKING FILES
  
if(eval(parse(text=paste("ncol(track",h,")>3",sep="")))){

    
#####Create individual data frame    
  

  
ind_track<-eval(parse(text=paste("track",h,"[track",h,"[,1]==as.character(test",h,"[",i,",1]),1:ncol(track",h,")]"
                      ,sep="")))
ind_name<-ind_track[1,c(1,2)]  
ind_track<-ind_track[3:ncol(ind_track)]
 
  
#######  

  
  

par(mfrow=c(1,1),xpd=TRUE)


#Old code  
#eval(parse(text=paste("plot(1:(ncol(track",h,")-2),track",h,"[",i,",3:ncol(track",h,")],xlim=c(0,ncol(track",h,")-1),ylim=c(30,110),xaxt='n',pch=19,ylab='Grade (%)',axes=FALSE,xlab='Assessment',main='Grades of Major Assessments')",sep="")))  
#

eval(parse(text=paste("plot(as.numeric(ind_track),xlim=c(0,ncol(track",h,")-1),ylim=c(30,110),xaxt='n',pch=19,ylab='Grade (%)',axes=FALSE,xlab='Assessment',main='Grades of Major Assessments')",sep="")))  


##Calculate Mean Score


eval(parse(text=paste("y_mean_value<-rbind(track",h,"[1,","3:ncol(track",h,")],ind_track)",sep="")))


y_mean_val<-as.matrix(y_mean_value[, colSums(is.na(y_mean_value))==0],nrow=2)

total_point_num<-sum(y_mean_value[1,]*y_mean_value[2,]/100)

total_point_den<-sum(y_mean_value[1,])

##


axis(2,c(40,50,60,70,72,76,78,80,84,86,88,92,94,96,99),las=1)



eval(parse(text=paste(
  "lines(c(1:(ncol(ind_track))),ind_track)"
  ,sep=""
)))

listzee<-eval(parse(text=paste("gsub('90215','-',colnames(ind_track))",sep="")))
listzee<-eval(parse(text=paste("gsub('_',' ',listzee)",sep="")))


###Putting x labels#################

eval(parse(text=paste(
  "text(x=c(1:(length(colnames(ind_track)))),y=40,labels=listzee,cex=.5,srt=90)"
  ,sep=""
)))

####################

#eval(parse(text=paste("mtext(colnames(track",h,")[3:length(colnames(track",h,"))],at=c(1:(length(colnames(track",h,"))-2)),side=1,cex=.5)",sep="")))

for(k in c(70,72,76,78,80,84,86,88,92,94,96,99)){
eval(parse(text=paste(
  "lines(c(0:(ncol(ind_track))),rep(",k,",(ncol(ind_track)+1)),col='orange',lty=3)"
  ,sep=""
)))
}



eval(parse(text=paste("abline(h=100*total_point_num/total_point_den,col='red',lty=3)",sep="")))

legend("topright","Average",lty=3,col="red")

#abline(h=70,col="orange",lty=3)
#abline(h=72,col="orange",lty=3)
#abline(h=76,col="orange",lty=3)
#abline(h=78,col="orange",lty=3)
#abline(h=80,col="orange",lty=3)
#abline(h=84,col="orange",lty=3)
#abline(h=86,col="orange",lty=3)
#abline(h=88,col="orange",lty=3)
#abline(h=92,col="orange",lty=3)
#abline(h=94,col="orange",lty=3)
#abline(h=96,col="orange",lty=3)
#abline(h=99,col="orange",lty=3)


#eval(parse(text=paste("abline(lm(as.matrix(track",h,"[",i,",3:ncol(track",h,")])~c(1:(length(colnames(track",h,"))-2))))",sep="")))

mtext(paste(eval(parse(text=paste("test",h,"$FirstName[",i,"]",sep="")))," ",eval(parse(text=paste("test",h,"$LastName[",i,"]",sep=""))),sep=""),3)

#mtext(paste(as.character(ind_name[1,2])," ",as.character(ind_name[1,1]),sep=""),3)

text(x= 0, y= 68, labels= "F",col="black",pos=4)
text(x= 0, y= 70, labels= "D-",col="black",pos=4)
text(x= 0, y= 72, labels= "D",col="black",pos=4)
text(x= 0, y= 76, labels= "D+",col="black",pos=4)
text(x= 0, y= 78, labels= "C-",col="black",pos=4)
text(x= 0, y= 80, labels= "C",col="black",pos=4)
text(x= 0, y= 84, labels= "C+",col="black",pos=4)
text(x= 0, y= 86, labels= "B-",col="black",pos=4)
text(x= 0, y= 88, labels= "B",col="black",pos=4)
text(x= 0, y= 92, labels= "B+",col="black",pos=4)
text(x= 0, y= 94, labels= "A-",col="black",pos=4)
text(x= 0, y= 96, labels= "A",col="black",pos=4)
text(x= 0, y= 99, labels= "A+",col="black",pos=4)

}else{
 eagles<-"win"
}


 dev.off() 
}
}



rm(list=ls())

######################## Version 1.7.2 ######################
######################## Last Updated #######################
########################  2018-09-07  #######################










######################## FINISHED!!! ########################
# FINISHED!!! ## FINISHED!!! ### FINISHED!!! ## FINISHED!!! #
# FINISHED!!! ## FINISHED!!! ### FINISHED!!! ## FINISHED!!! #
#############################################################

