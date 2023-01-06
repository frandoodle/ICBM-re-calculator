
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




# times - the number of years to run
# iag - annual carbon input to aboveground young C pool (?).
# 			Should be a vector of the same length as "times"
# ibg - annual carbon input to belowground young C pool (?).
# 			Should be a vector of the same length as "times"
# iman - annual carbon input to manure young C pool (?).
# 			 Should be a vector of the same length as "times"
# re - a soil climate-and management parameter that aggregates
#			 the external influences on soil biological activity
#			 Should be a single, constant numerical value (?).
#			 OR MAYBE: Should be a vector of the same length as "times"
# yopool - A vector of length 3 representing the 
#					 Initial C pools. 
#						yopool = c(initial young aboveground C,
#											 initial young belowground C,
#											 initial old C)

icbm_holos4_classic_manure=function(times, iag, ibg,iman,re, yopool,
kO = 0.00605,
kY = 0.8,
hag = 0.125,
hm = 0.31,
hbg = 0.3,
... # ellipsis is added to allow for passing unused arguments using do.call when doing parameter overrides
){
  # times=1983:2009
  # the number of years for the run
  #yopool=c(0, 38643.15,0 )
  #re=0.98
  #testdata=read.csv("~/ICBM/TestData/FW2_N45P20_VarYield_12-17.csv")
  #times=min(testdata$Year):max(testdata$Year)
  
  n=length(times)

  #Vectoring the inputs
  kO=rep(kO,n)  # decomposition rate of old carbon pool 
  kY=rep(kY,n)
  hag=rep(hag,n)
  hm=rep(hm, n)
  hbg=rep(hbg, n)
  
  
  
  Yag=list()
  Ybg=list()
  Yman=list() # Code for Manure
  O=list()
  #re=rep(re, n)
  
  ##TEST CODE###
  # iag=rep(977,n)
  # ibg=rep(288,n)
  # iman=rep(88,n) # Code for Manure
  # yag_ini=109
  # ybg_ini=288

  
  # iag=testdata$iag
  # ibg=testdata$ibg
  # iman=testdata$m
  # re=testdata$re_crop
  
  
  
  # This is a transparent implementation of ICBM classic version suggested by Roland and Sarah (Lethbridge AAFC)
  # The spreadsheet version has been ported to R using the same formulae
  
  
  for(run in 1:n){
    if(run==1)
    {
      #run=1
      Yag[[run]]=(yopool[[1]]+iag[[run]])*exp(re[[run]]*-kY[[run]])
      Ybg[[run]]=(yopool[[3]]+ibg[[run]])*exp(re[[run]]*-kY[[run]])
      
      #Yman[[run]]=(yopool[[1]]+iman[[run]])*exp(re[[run]]*-kY[[run]]) # Code for Manure
      Yman[[run]]=(0+0)*exp(re[[run]]*-kY[[run]]) # Code for Manure
      O[[run]]=yopool[[2]]
    }
    else{
       #run=2
      fa=re[run]*-kY[run]
      
      Yag[[run]]=(Yag[[run-1]]+
                    
                    iag[[run-1]])*exp(fa)
      
      
      Ybg[[run]]=(Ybg[[run-1]]+
                    ibg[[run-1]])*exp(fa)
      
      
      Yman[[run]]=(Yman[[run-1]]+
                    iman[[run-1]])*exp(fa)
      
      
      
      O[[run]]=((O[[run-1]]-(hag[[run]]*kY[[run]]*(Yag[[run-1]]+iag[[run-1]])/(kO[[run]]-kY[[run]]))-
                   (hbg[[run]]*kY[[run]]*(Ybg[[run-1]]+ibg[[run-1]])/(kO[[run]]-kY[[run]]))-
                   (hm[[run]]*kY[[run]]*(Yman[[run-1]]+iman[[run-1]])/(kO[[run]]-kY[[run]])))*exp(-kO[[run]]*re[[run]]))+
        ((hag[[run]]*kY[[run]]*(Yag[[run-1]]+iag[[run-1]])/(kO[[run]]-kY[[run]]))*exp(-kY[[run]]*re[[run]]))+
        ((hbg[[run]]*kY[[run]]*(Ybg[[run-1]]+ibg[[run-1]])/(kO[[run]]-kY[[run]]))*exp(-kY[[run]]*re[[run]]))+
        ((hm[[run]]*kY[[run]]*(Yman[[run-1]]+iman[[run-1]])/(kO[[run]]-kY[[run]]))*exp(-kY[[run]]*re[[run]])) #Code for Manure

      
      # hag_par=hag[[run]]*(kY[[run]]*(Yag[[run-1]]+iag[[run-1]]))/(kO[[run]]-kY[[run]])
      # hbg_par=hbg[[run]]*(kY[[run]]*(Ybg[[run-1]]+ibg[[run-1]]))/(kO[[run]]-kY[[run]])
      # hman_par=hm[[run]]*kY[[run]]*(Yman[[run-1]]+iman[[run-1]])/(kO[[run]]-kY[[run]])
      # 
      # print(run)
      # print(hag_par)
      # print(hbg_par)
      # print(hman_par)
      
      
      
      
      # term1= (O[[run-1]]-hag_par-hbg_par-hman_par)*exp(-kO[run]*re[run])
      # 
      # expterm=exp(-kY[run]*re[run])
      # 
      # 
      # O[[run]]=term1+hag_par*expterm+hbg_par*expterm+hman_par*expterm
      
    }
    
  }
  #print(length(time))
  # print(length(Yag))
  # print(length(Ybg))
  # print(length(O))
  # print(length(Yman))
  out=data.frame("time"=times, "Yag"=unlist(Yag), "Ybg"=unlist(Ybg), "Old"=unlist(O), "YManure"=unlist(Yman))  
  out$Tot=out$Yag+out$Ybg+out$Old+out$YManure
  out$re=re
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
