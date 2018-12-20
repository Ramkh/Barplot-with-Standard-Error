SRI1 <- read.csv("C:/Users/vijay/Desktop/SRI trichoderma/SRI1.csv")
attach(SRI1)
names(SRI1)
as.factor(Rep)->rep
as.factor(Var)->var
as.factor(var)
as.factor(Subplot)->bio
as.factor(Var)->var

library(ggplot2)
library(Rmisc)
library(plyr)
SRI11 <- summarySE(SRI1, measurevar= ("GY"), groupvars=c("var", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI11

library(ggExtra)
library(grid)
library(reshape2)
o <- ggplot(SRI11, aes(bio, GY, fill=bio)) + geom_bar(stat="identity", color="black", 
                                                      position=position_dodge())+labs(x=" ", y="Grain Yield (mt/ha)")
p<-o + geom_errorbar(aes(ymin = GY, ymax = GY+se), width=.2, position=position_dodge(.9)) +facet_grid(.~var) + theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
p
q<-p+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
q

SRI112 <- summarySE(SRI1, measurevar= ("paniclelength"), groupvars=c("var", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI112


r <- ggplot(SRI112, aes(bio, paniclelength, fill= bio)) + geom_bar(stat="identity", color="black", 
                                                                   position=position_dodge())+labs(x=" ", y="Panicle Length (cm)")
r
s <-r +geom_errorbar(aes(ymin=paniclelength, ymax= paniclelength+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~var)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
s


SRI13 <- summarySE(SRI1, measurevar= ("tillerpersqm"), groupvars=c("var", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI13
u <- ggplot(SRI13 , aes(bio, tillerpersqm, fill= bio)) + geom_bar(stat="identity", color="black", 
                                                                  position=position_dodge())+labs(x=" ", y=" Panicle per sq. m")
u

v <-u + geom_errorbar(aes(ymin = tillerpersqm, ymax = tillerpersqm +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~var) + theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
v


SRI14 <- summarySE(SRI1, measurevar= ("TGW"), groupvars=c("var", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI14
k <- ggplot(SRI14 , aes(bio, TGW, fill= bio)) + geom_bar(stat="identity", color="black", 
                                                         position=position_dodge())+labs(x=" ", y=" Test Weight (gm)")
k
m <-k + geom_errorbar(aes(ymin= TGW, ymax= TGW+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~var)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
m


library(gridExtra)

grid.arrange(q, s, v, m, ncol=2)








SRI16 <- summarySE(SRI1, measurevar= ("TGW"), groupvars=c("var","Mainplot", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI16

model1<-aov(TGW~rep+Mainplot)
model1
LSD.test(model1, "Mainplot",console=TRUE)
model2<-aov(TGW~rep+bio)
comp4<-LSD.test(model2,"bio")
comp4
model3<-aov(TGW~rep+var)
comp5<-LSD.test(model3,"var")
comp5
model9 <- lm(TGW ~ bio*var, data = SRI1)
anova(model9)

SRI1$bio.var<-with(SRI1,interaction (bio.var))
modle8<-aov(TGW~bio.var,data=SRI1)
comp6<-LSD.test(modle8,"bio.var",alpha=0.001,console=TRUE) 
comp6
SRI1$bio.Mainplot<-with(SRI1,interaction(bio,Mainplot))
modle10<-aov(TGW~bio.Mainplot,data=SRI1)
comp10<-LSD.test(modle10,"bio.Mainplot",alpha=0.001,console=TRUE) 
comp10

SRI1$bio.var.Mainplot<-with(SRI1,interaction(bio,var,Mainplot))
modle10<-aov(TGW~bio.var.Mainplot,data=SRI1)
comp10<-LSD.test(modle10,"bio.var.Mainplot",alpha=0.001,console=TRUE) 
comp10
model <- lm(TGW ~ Mainplot * bio*var, data = SRI1)
anova(model)
