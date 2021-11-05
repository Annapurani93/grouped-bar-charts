library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(ggtext)
library(patchwork)


read_excel("depression.xlsx")->data
data.frame(data)->data
colnames(data)<-c("Group","2020","2021")

data%>%
  slice_head(n=3)->age
data[c(4,5),]->gender
data[c(6,7),]->disablility
data[c(8,9),]->expense
data[c(10,11),]->household
data[c(12,13),]->children


melt(age,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->age1
melt(gender,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->gender1
melt(disablility,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->disability1
melt(expense,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->expense1
melt(household,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->household1
melt(children,id.vars = "Group",measure.vars = c("2020","2021"),value.name = "value")->children1

age1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->age1
gender1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->gender1
disability1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->disability1
expense1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->expense1
household1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->household1
children1%>%
  mutate(variable=fct_relevel(variable,levels="2021","2020"))->children1


ggplot(age1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  geom_text(colour="white",position=position_dodge(width=0.8),vjust=0.75,hjust=-0.55,fontface="bold")+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=21, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="BY AGE")->agegraph


ggplot(gender1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  geom_text(colour="white",position=position_dodge(width=0.8), vjust=0.75,hjust=-0.55,fontface="bold")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=21, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="BY GENDER")->gendergraph

ggplot(disability1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  geom_text(colour="white",position=position_dodge(width=0.8),vjust=0.75,hjust=-0.55,fontface="bold")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=21, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="IN THE DIFFERENTLY ABLED")->disabilitygraph

ggplot(expense1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  geom_text(colour="white",position=position_dodge(width=0.8), vjust=0.75,hjust=-0.55,fontface="bold")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=18, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="BY INCOME LEVELS")->expensegraph

ggplot(household1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  geom_text(colour="white",position=position_dodge(width=0.8),vjust=0.75,hjust=-0.55,fontface="bold")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=21, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="BY HOUSEHOLD SIZE")->householdgraph


ggplot(children1, aes(fill=variable,x=value,y=reorder(Group,value,decreasing=TRUE),label=paste0(value,"%")))+
  geom_bar(colour="white",position=position_dodge(width = 0.8),width=0.6,stat="identity")+
  geom_text(colour="white",position=position_dodge(width=0.8), vjust=0.75,hjust=-0.55,fontface="bold")+
  scale_fill_manual(values=c("#fe4a49","#f4f4f8"),labels=c("January to March 2021 (in %)","July 2019 to March 2020 (in %)"))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.y=element_text(color="white", size=21, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=18, colour = "white", face="bold"))+
  labs(title="IN FAMILIES WITH CHILDREN")->childrengraph

gendergraph+agegraph+disabilitygraph+expensegraph+householdgraph+childrengraph+
  plot_layout(ncol=2,widths=c(9,9))+
  plot_annotation(
    title = "DEPRESSION SYMPTOMS SURGE POST PANDEMIC",
    subtitle="<span style='color:white'>Rates of depression symptoms across all groups of people climbed in <span style='color:#fe4a49'>early 2021 (January to March)</span>,<br>when compared to the period before the pandemic (July 2019 to March 2020)</span>",
    caption = "Data: @DiversityinData | Design: @annapurani93"
  )&
  theme(
    legend.position = "none",
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black"),
    plot.title = element_text(size = rel(4.5),hjust = .5, margin = margin(t = 15, b = 15), color = "white",  face = "bold"),
    plot.subtitle = element_markdown(hjust = .5, size = rel(2.5),margin = margin(t = 15, b = 10),face="bold"), 
    plot.caption = element_text(size = rel(2), color="white",margin = margin(t = 15, b = 10, r = 25)),
    plot.margin = margin(t = 15,r = 20, b = 15, l = 20)
  )->finalplot


ggsave("depressionsymptoms.png", finalplot, width = 30, height = 27)
ggsave("depressionsymptoms.pdf", finalplot, width = 30, height = 27,device=cairo_pdf)


