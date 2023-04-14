#Replication script for Altindag & Kaushal 2019
## Packages
# To install and open the R packages that you need for this code. 
need <- c('foreign','stargazer','readr','dplyr','dummies','readxl','haven', 'zoo',
          'ggplot2','maptools','googleVis','glue','readxl','data.table','lfe')
have <- need %in% rownames(installed.packages()) 
if(any(!have)) install.packages(need[!have]) 
invisible(lapply(need, library, character.only=T)) 

# Change path to whereever your script is 
# To set up the working directory and place data within the same folder as the script
script_folder = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(glue('{script_folder}'))
rm(list = ls())
#Table III: Effect of Refugees on Voting Behavior


df1 <- readRDS('../data/SurveyKonda.Rda')
df2 <- readRDS('../data/ImpRefInf.RDS')
#area 
df1$Area <- df1$s25 
#df1$Area[is.na(df1$Area==TRUE)] <- 999  
#religion 
df1$Religious[df1$Religion==1 | df1$Religion==2]  <- 0 
df1$Religious[df1$Religion==3 | df1$Religion==4]  <- 1 
#df1$Religious[is.na(df1$Religion)==TRUE] <- 999 
#Missing Age  
df1$missing.Age <- 0 
df1$missing.Age[is.na(df1$Age==TRUE)] <- 1 
#Handle other missings 

df1$ym <- as.yearmon(paste(df1$Year, df1$Month, sep = "-"))

var.set <- c('Eth','Educ','Sunni','Religious','Female','Area','income','Age','Province',
             'p1','p2','p3','p4','p5','Arab1965Pct','TotNbRefIn','ym','idP','YM','missing.Age')

df1 <- df1 %>%
  select(var.set)

df3 <- merge(df1,df2,by=c('ym','Province'))

x.list1 <- c('Eth','Educ','Sunni','Religious','Female','Area','income')
x.list2 <- c('Age', 'missing.Age')
y.list <- c('p1','p2','p3','p4','p5')


missing.val <- function(x){
  out1 <- x 
  out1[is.na(x)==TRUE] <- 999 
  return(out1)
}
df3 <- df3 %>% 
  mutate_at(c(x.list1,'Age'), missing.val) %>%
  filter(is.na(p1)==FALSE)
#create the instrument 
df3$iv <- (df3$Arab1965Pct*df3$TotNbRefIn)/1000000  


t3.func <- function(x) {
  v.name <- y.list[x]
  v.num <- match(c(v.name, x.list1,x.list2,'ym','tRef1','idP','iv'), names(df3))
  df.reg <- df3[,v.num]
  colnames(df.reg) <- c('y',x.list1,x.list2,'ym','tRef1','idP','iv')
  out1 <- felm(data=df.reg, formula= y ~   tRef1
               | idP + ym  | 0 | idP)
  out2 <- felm(data=df.reg, formula= y ~  1
               | idP + ym  | (tRef1~iv) | idP)
  s0 <-  round(out1$N,0)
  s1 <-  round(out1$coefficients[1,1],2)
  s2 <-  round(out1$cse[1],2) 
  s3 <-  round(out1$cpval[1],3)
  s4 <-  round(out2$coefficients[1,1],2)
  s5 <-  round(out2$cse[1],2)
  s6 <-  round(out2$cpval[1],3)
  s7 <-  round(mean(df.reg$y, na.rm=TRUE),2) 
  s8 <-  round(out2$stage1$coefficients[1,1],2)
  s9 <-  round(out2$stage1$cse[1],2)
  s10 <- round(out2$stage1$cpval[1],3)
  s11 <- round(out2$stage1$iv1fstat$tRef[5],2)
  s.comb <- rbind(s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
  return(s.comb)
}
out <- lapply(1:(length(y.list)), t3.func) 
NoCovarPanelA <- as.data.frame(do.call(cbind, out))

rm(list=c("df1","df2","df3", "var.set","x.list1","x.list2","y.list","t3.func", "missing.val", "out"))


df1 <- readRDS("../data/ElectionProvince.Rda")
df1$Month <- 6 
df1$Month[df1$YM==670] <- 11

df1$Year <- 2015 
df1$Year[df1$YM==617] <- 2011
df1$ym <- as.yearmon(paste(df1$Year, df1$Month, sep = "-"))

var.set <- c('Under40Pct' ,'FemalePct','PrimaryPct','ym','Arab1965Pct','TotNbRefIn',
             'AKPPct','CHPPct','MHPPct','HDPPct','Prate','Province','idP')

df1 <- df1 %>% 
  select(var.set)

x.list1 <- c('Under40Pct' ,'FemalePct','PrimaryPct')
y.list <- c('AKPPct','CHPPct','MHPPct','HDPPct','Prate')
df1$iv <- (df1$Arab1965Pct*df1$TotNbRefIn)/1000000  

df2 <- readRDS('../data/ImpRefInf.RDS')
df3 <- merge(df1,df2,by=c('ym','Province'))

t3.func <- function(x) {
  v.name <- y.list[x]
  v.num <- match(c(v.name, x.list1,'tRef1', 'ym','idP','iv'), names(df3))
  df.reg <- df3[,v.num]
  colnames(df.reg) <- c('y',x.list1,'tRef1','ym','idP','iv')
  out1 <- felm(data=df.reg, formula= y ~ tRef1
               | idP + ym| 0 | idP)
  out2 <- felm(data=df.reg, formula= y ~ 1
               | idP + ym|  (tRef1~iv) | idP)
  s0 <-  round(out1$N,0)
  s1 <-  round(out1$coefficients[1,1],2)
  s2 <-  round(out1$cse[1],2) 
  s3 <-  round(out1$cpval[1],3)
  s4 <-  round(out2$coefficients[1,1],2)
  s5 <-  round(out2$cse[1],2)
  s6 <-  round(out2$cpval[1],4)
  s7 <-  round(mean(df.reg$y, na.rm=TRUE),2) 
  s8 <-  round(out2$stage1$coefficients[1,1],2)
  s9 <-  round(out2$stage1$cse[1],2)
  s10 <- round(out2$stage1$cpval[1],4)
  s11 <- round(out2$stage1$iv1fstat$tRef[5],2)
  s.comb <- rbind(s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
  return(s.comb)
}
out <- lapply(1:(length(y.list)), t3.func) 
NoCoVarPanelB <- as.data.frame(do.call(cbind, out))
 


rm(list=c("df1","df2","df3", "x.list1","y.list","var.set","t3.func","out"))

NoCovarPanelA
NoCoVarPanelB
