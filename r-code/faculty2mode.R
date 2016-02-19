########################################
## Date: 02/17/2016
## Zack W. Almquist
## Assistant Professor
## Department of Sociology 
## School of Statistics
## University of Minnesota
########################################`

library(rvest)
facultyPage<- html("https://cla.umn.edu/sociology/people/faculty")
facultyFI<-facultyPage %>% html_nodes("tr")%>% html_nodes("td") %>% html_text()

faculty<-facultyFI[grep("Professor",facultyFI)]
assistant<-grep("Assistant",faculty)
associate<-grep("Associate",faculty)
Emeritus<-grep("Emeritus",faculty)
full<-which(!(1:length(faculty))%in%c(assistant,associate,Emeritus))

faculty<-as.vector(sapply(sapply(sapply(faculty,function(x){strsplit(x,"Professor")[[1]][1]}),function(x){strsplit(x,"Associate")[[1]][1]}),function(x){strsplit(x,"Assistant")[[1]][1]}))

topics32<-facultyFI[seq(3,length(facultyFI),3)][1:32]
uTopics<-unique(gsub("and ","",unlist(strsplit(unlist(strsplit(topics32,", ")),"; "))))

### Recode a few things
uTopics[grep("demography",tolower(uTopics))]<-"Demography"
uTopics[grep("crim",tolower(uTopics))]<-"Criminology"
uTopics[grep("life course",tolower(uTopics))]<-"Life Course"
uTopics[grep("health",tolower(uTopics))]<-"Health"
uTopics[grep("fam",tolower(uTopics))]<-"Family"
uTopics[grep("adol",tolower(uTopics))]<-"Family"
uTopics[grep("Work",uTopics)]<-"Organizations"
uTopics[grep("org",tolower(uTopics))]<-"Organizations"
uTopics[grep("survey",tolower(uTopics))]<-"Survey Methods"
uTopics[grep("child",tolower(uTopics))]<-"Family"
uTopics[grep("qualita",tolower(uTopics))]<-"Qualitative Methods"
uTopics[grep("ethnography",tolower(uTopics))]<-"Qualitative Methods"
uTopics[grep("punish",tolower(uTopics))]<-"Criminology"
uTopics[grep("deviance",tolower(uTopics))]<-"Criminology"
uTopics[grep("hom",tolower(uTopics))]<-"Criminology"
uTopics[grep("race",tolower(uTopics))]<-"Race/Thnicity"
uTopics[grep("black",tolower(uTopics))]<-"Race/Thnicity"
uTopics[grep("afro",tolower(uTopics))]<-"Race/Thnicity"
uTopics[grep("juvenile",tolower(uTopics))]<-"Criminology"
uTopics[grep("law",tolower(uTopics))]<-"Criminology"
uTopics[grep("global",tolower(uTopics))]<-"Globalization"
uTopics[grep("trans",tolower(uTopics))]<-"Globalization"
uTopics[grep("econ",tolower(uTopics))]<-"Economic Sociology"
uTopics[grep("compa",tolower(uTopics))]<-"Qualitative Methods"
uTopics[grep("quant",tolower(uTopics))]<-"Quantitative Methods"
uTopics[grep("field",tolower(uTopics))]<-"Quantitative Methods"
uTopics[grep("math",tolower(uTopics))]<-"Quantitative Methods"
uTopics[grep("discourse",tolower(uTopics))]<-"Qualitative Methods"
uTopics[grep("mixed",tolower(uTopics))]<-"Qualitative Methods"
uTopics[grep("islam",tolower(uTopics))]<-"Religion"
uTopics[grep("antisemitism",tolower(uTopics))]<-"Religion"
uTopics[grep("judaism",tolower(uTopics))]<-"Religion"
uTopics[grep("holocaust",tolower(uTopics))]<-"Religion"
uTopics[grep("japan",tolower(uTopics))]<-"Asia"
uTopics[grep("asia",tolower(uTopics))]<-"Asia"
uTopics[grep("chin",tolower(uTopics))]<-"Asia"
uTopics[grep("culture",tolower(uTopics))]<-"Culture"
uTopics[grep("pol",tolower(uTopics))]<-"Politics"
uTopics[grep("inequality",tolower(uTopics))]<-"Inequality"
uTopics[grep("dev",tolower(uTopics))]<-"Development"
uTopics[grep("net",tolower(uTopics))]<-"Network Analysis"
uTopics[grep("mov",tolower(uTopics))]<-"Movements"
uTopics[grep("mig",tolower(uTopics))]<-"Migration"
uTopics[grep("integ",tolower(uTopics))]<-"Migration"
uTopics[grep("amer",tolower(uTopics))]<-"America"
uTopics[grep("gend",tolower(uTopics))]<-"Gender"
uTopics[grep("intergen",tolower(uTopics))]<-"Family"
uTopics[grep("clim",tolower(uTopics))]<-"Enviromental Sociology"
uTopics[grep("vis",tolower(uTopics))]<-"Visual Sociology"
uTopics[grep("youth",tolower(uTopics))]<-"Family"
uTopics[grep("judge",tolower(uTopics))]<-"Decision Making"
uTopics[grep("labor",tolower(uTopics))]<-"Unionization"
uTopics[grep("media",tolower(uTopics))]<-"Communication"

uTopics<-unique(uTopics)
uTopics<-uTopics[order(uTopics)]
uTpoics<-gsub("//.","",uTopics)

recode<-list(list("demography", "Demography"),
list("crim", "Criminology"),
list("life course", "Life Course"),
list("health", "Health"),
list("fam", "Family"),
list("adol", "Family"),
list("Work", "Organizations"),
list("orgs", "Organizations"),
list("survey", "Survey Methods"),
list("child", "Family"),
list("qualita", "Qualitative Methods"),
list("ethnography", "Qualitative Methods"),
list("punish", "Criminology"),
list("deviance", "Criminology"),
list("hom", "Criminology"),
list("race", "Race/Thnicity"),
list("black", "Race/Thnicity"),
list("afro", "Race/Thnicity"),
list("juvenile", "Criminology"),
list("law", "Criminology"),
list("global", "Globalization"),
list("trans", "Globalization"),
list("econ", "Economic Sociology"),
list("compa", "Qualitative Methods"),
list("quant", "Quantitative Methods"),
list("field", "Quantitative Methods"),
list("math", "Quantitative Methods"),
list("discourse", "Qualitative Methods"),
list("mixed", "Qualitative Methods"),
list("islam", "Religion"),
list("antisemitism", "Religion"),
list("judaism", "Religion"),
list("holocaust", "Religion"),
list("japan", "Asia"),
list("asia", "Asia"),
list("chin", "Asia"),
list("culture", "Culture"),
list("pol", "Politics"),
list("inequality", "Inequality"),
list("dev", "Development"),
list("net", "Network Analysis"),
list("mov", "Movements"),
list("mig", "Migration"),
list("integ", "Migration"),
list("amer", "America"),
list("gend", "Gender"),
list("intergen", "Family"),
list("clim", "Enviromental Sociology"))

topics32_recode<-topics32

for(i in 1:length(recode)){
print(i)
topics32_recode<-gsub(recode[[i]][[1]],recode[[i]][[2]],topics32_recode,ignore.case =TRUE)
}


role<-rep(NA,length(faculty))
role[assistant]<-"Assistant"
role[associate]<-"Associate"
role[Emeritus]<-"Emeritus"
role[full]<-"Professor"


faculty.df<-data.frame(faculty[1:32],role[1:32],matrix(0,nr=32,nc=length(uTopics)),stringsAsFactors =FALSE)
colnames(faculty.df)<-c("Faculty","Role",uTopics)
who.code<-lapply(topics32_recode,function(x){
which(colnames(faculty.df)%in%names(unlist(sapply(colnames(faculty.df),function(y){grep(y,x)}))))
	})

for(i in 1:NROW(faculty.df)){
	faculty.df[i,who.code[[i]]]<-1
}

topic<-t(faculty.df[,c(3:NCOL(faculty.df))])
colnames(topic)<-faculty.df[,1]
write.csv(topic,file="topicByFaculty.csv")
save(topic,file="topic.Rda")
library(sna)
library(network)
net<-network(topic,bipartite=TRUE)
col<-rep(NA,network.size(net))
col[1:NROW(topic)]<-"blue"
col[(NROW(topic)+1):length(col)]<-"red"

## Plot
plot(net,label="vertex.names",
label.cex=.4, edge.col=rgb(0,0,0,.2), 
vertex.col=col)




