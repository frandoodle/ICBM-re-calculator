
library(tidyverse)
######################code working perfectly like Sarah's################
# iag=39+43.1346
# ibg=102.9+113.6694
# re=1.1
# kY=0.8
# Yag=(iag)*exp(re*-kY)
# Ybg= (ibg)*exp(re*-kY)
# hY=0.125
# kO=0.00605
# hbg=0.3
# init_Carbon=38162.9212
# O=((init_Carbon-(hY*kY*(Yag+iag)/(kO-kY))-(hbg*kY*(Ybg+ibg)/(kO-kY)))*exp(-kO*re))+
#   ((hY*kY*(Yag+iag)/(kO-kY))*exp(-kY*re))+
#   ((hbg*kY*(Ybg+ibg)/(kO-kY))*exp(-kY*re))
# SOC=Yag+Ybg+O
# print(SOC)
###########################################################################






icbm_holos4_classic=function(times, iag, ibg, re, yopool){
  # times=1983:2009
  # the number of years for the run
  # yopool=c(0.04, 34, 0.028)
  # re=0.98
  n=length(times)

  #Vectoring the inputs
  kO=rep(0.00605,n)  # decomposition rate of old carbon pool 
  kY=rep(0.8,n)
  hY=rep(0.125,n)
  hM=rep(0.31, n)
  hbg=rep(0.3, n)
  Yag=list()
  Ybg=list()
  O=list()
  re=rep(re, n)
  
  ##TEST CODE###
  # iag=rep(977,n)
  # ibg=rep(288,n)
  # yag_ini=109
  # ybg_ini=288

  
  # This is a transparent implementation of ICBM classic version suggested by Roland and Sarah (Lethbridge AAFC)
  # The spreadsheet version has been ported to R using the same formulae
  
  
  for(run in 1:n){
    if(run==1)
    {
      Yag[[run]]=(yopool[[1]]+iag[[run]])*exp(re[[run]]*-kY[[run]])
      Ybg[[run]]=(yopool[[3]]+ibg[[run]])*exp(re[[run]]*-kY[[run]])
      O[[run]]=yopool[[2]]
    }
    else{
       
      fa=re[run]*-kY[run]
      Yag[[run]]=(Yag[[run-1]]+iag[[run-1]])*exp(fa)
      Ybg[[run]]=(Ybg[[run-1]]+ibg[[run-1]])*exp(fa)
      O[[run]]=((O[[run-1]]-(hY[[run]]*kY[[run]]*(Yag[[run-1]]+
                                                    iag[[run-1]])/(kO[[run]]-kY[[run]]))-(hbg[[run]]*kY[[run]]*(Ybg[[run-1]]+ibg[[run-1]])/(kO[[run]]-kY[[run]])))*exp(-kO[[run]]*re[[run]]))+
        ((hY[[run]]*kY[[run]]*(Yag[[run-1]]+iag[[run-1]])/(kO[[run]]-kY[[run]]))*exp(-kY[[run]]*re[[run]]))+
        ((hbg[[run]]*kY[[run]]*(Ybg[[run-1]]+ibg[[run-1]])/(kO[[run]]-kY[[run]]))*exp(-kY[[run]]*re[[run]]))
      
      
    }
    
  }
  #print(length(time))
  #print(length(Yag))
  #print(length(Ybg))
  #print(length(O))
  out=data.frame("time"=times, "Yag"=unlist(Yag), "Ybg"=unlist(Ybg), "Old"=unlist(O))  
  out$Tot=out$Yag+out$Ybg+out$Old
  #View(out)
  
  return(out%>%arrange(time))
}




kY=0.8
hY=0.125
kO=0.00605
hbg=0.3
hm=0.31
hag=0.125

# steady state 
Yss=function(Cinput, re)
{
  #Cinput=880
  #re=0.8
  Cinput*(exp(-kY*re))/(1-exp(-kY*re))
}


Oss=function(Cag,Cbg,Cm,re){
  yag=Yss(Cag,re)
  ybg=Yss(Cbg,re)
  ym=Yss(Cm,re)
  p1=(exp(-kY*re)-exp(kO*re))/(1-exp(-kO*re))
  p2=((hag*kY)*(yag+Cag)+(hbg*kY)*(ybg+Cbg)+(hm*kY)*(ym+Cm))/(kO-kY)
  return(data.frame(yagss=yag, yabss=ybg, ymss=ym, oldss=p1*p2))
}


#Testing code
# Oss(0.567, 0.166, 0,1)
# Oss(567, 166, 0,1)
