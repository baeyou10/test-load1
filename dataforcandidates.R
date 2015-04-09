setwd("C:/Users/jbae/Downloads")
install.packages('ggplot2')
install.packages('ggthemes', dependencies = TRUE)
install.packages("glm2")
install.packages("gam")
install.packages("foreign")
install.packages('nnet')
install.packages("sjPlot")
library(sjPlot)
library(gam)
library(ggplot2)
library("ggthemes")
library(vcd)

#### data
df<-read.csv("Data for Candidates.csv");
df1<-data.frame(df$id, df$star,df$rank,df$click,df$cost,df$order)
df1<-na.omit(df1)
colnames(df)<-c('dev','id','city','st','ctry','star','rank','click','cost','order')
df$ctry<-as.character(df$ctry)
df.us<-subset(df,df$ctry=='US')
dev<-df.us[,1];id<-df.us[,2];city<-df.us[,3];st<-df.us[,4];ctry<-df.us[,5];
star<-df.us[,6];rank<-df.us[,7];click<-df.us[,8];cost<-df.us[,9];order<-df.us[,10]
df.us$star<-as.factor(df.us$star);
df.us$dev<-as.factor(df.us$dev)
df.us$rank<-as.factor(df.us$rank);rank<-df.us$rank
star<-df.us$star
dev<-df.us$dev
glm1<-lm(star~rank+click+cost+order)
summary(df.us)
#recategorize
region<-as.character(st)
region[st=='CT'|st=='ME'|st=='MA'|st=='NH'|st=='RI'|st=='VT']=1
region[st=='NJ'|st=='NY'|st=='PA']=2
region[st=='IL'|st=='IN'|st=='MI'|st=='OH'|st=='WI']=3
region[st=='IA'|st=='KS'|st=='MN'|st=='MO'|st=='NE'|st=='ND'|st=='SD']=4
region[st=='DE'|st=='FL'|st=='GA'|st=='MD'|st=='NC'|st=='SC'|st=='VA'|st=='DC'|st=='WV']=5
region[st=='AL'|st=='KY'|st=='MS'|st=='TN']=6
region[st=='AR'|st=='LA'|st=='OK'|st=='TX']=7
region[st=='AZ'|st=='CO'|st=='ID'|st=='MT'|st=='NV'|st=='NM'|st=='UT'|st=='WY']=8
region[st=='AK'|st=='CA'|st=='HI'|st=='OR'|st=='WA'|st=='VI'|st=='PR']=9
region<-factor(region)
summary(region)
barplot(table(region))
df.us$region<-region

#eda
summary(df)
qplot(star)+theme_economist()+ggtitle('Star Rating Distribution')
qplot(rank)+theme_economist()+ggtitle('Rank Rating Distribution')
qplot(click,binwidth=20,xlim=c(0,150))+theme_economist()+ggtitle('Click Distribution')
qplot(cost,xlim=c(0,150))+theme_economist()+ggtitle('Cost Distribution')
qplot(order,xlim=c(0,12))+theme_economist()+ggtitle('Order Distribution')
#plot(km1,col=km1$cluster)
qplot(region)+theme_economist()+ggtitle("Region Distribution")

#not my code below; function from stackoverflow
pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE) 
{if (smooth ){
  if (scale) {
    pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
  }
  else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
  } #else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
}

else      #smooth is not true
{ if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale)
} else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor) }
} #end of else (smooth)

}   #end of function

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

pars(mfrow=c(1,1))
vars<-cbind(dev,region,star,rank,click,cost,order)
pairs(vars, lower.panel=panel.cor)






#modeling
lm1<-lm(formula=rank~dev+star+cost+click+order+region,data=df.us,family='gaussian')
summary(lm1)
lcost<-log(cost)
glm1<-glm(formula =lcost~click+star+cost+order+region,data=df.us,family="gaussian"(link="log"))
summary(glm1)
glm2<-glm(formula=cost~click+star+order+region,data=df.us,family="gaussian"(link="identity"))
summary(glm2)
glm3<-glm(formula=region~dev+star+cost+click+order+rank,data=df.us,family='binomial'(link='probit'))
anova(glm3)
summary(glm3)
glm4<-glm(formula=region~dev+star+cost+click+order+rank,data=df.us,family='binomial'(link='logit'))
summary(glm4)

glm5<-glm(formula=cost~click+star+cost+order+region+star*click+star*cost+star*order+region*click+region*cost+region*order,data=df.us,family="gaussian"(link="identity"))
summary(glm5)


mlm1<-multinom(region~star+cost+click+order+rank)
summary(mlm1)



#main model
mmodel<-glm2
summary(mmodel)
anova(mmodel)
pred<-predict(model,type='response',se.fit=T)
mdf<-data.frame(pred$fit,pred$se.fit)
plot(pred~cost)

qplot(click,cost)+theme_economist()+ggtitle("Clicks to Cost")
qplot(click,cost,xlim=c(200,1000))+theme_economist()+ggtitle('Possible Pursuits')


#sjp.glm(mmodel,
 #       axisLabels.y=c('Intercept','Click','star2','star2.5','star 3','star 3.5','star 4','star 4.5','star 5'
  #                     ,'order','Region2','Region3','Region4','Region5','Region6','Region7','Region8','Region9'),
   #     gridBreaksAt=0.4)

