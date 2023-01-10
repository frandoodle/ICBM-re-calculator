
# IPCC function that returns the predictions 


#' Function to simulate the SOC using IPCC Carbon model:
#' @author Arumugam Thiagarajan, \email{arumugam.thiagarajan@canada.ca}
#' @param Site_Name unique name of the site


########### LOAD THE LIBRARIES##################
#library(parallel)
library(reshape2)
library(dplyr)
library(rlang)
library(purrr)
#library(doParallel)
#library(parallel)
library(foreach)
library(gdata)
library(FME)
######################### END OF LIBRARY SECTION##################





source("./RCodes/loglike.R")

SITES=list()
SITES[[1]]=c("Breton", "Breton", 26, 12,1979) # orginal supplied by Chang
SITES[[2]]=c("SwiftCurrent_oldRot","SwiftCurrent", 27.6, 33, 1976)
SITES[[3]]=c("SwiftCurrent_NewRot","SwiftCurrent", 27.6, 33,1987)
SITES[[4]]=c("IndianHead", "IndianHead",63.1, 16.3,  1960)
SITES[[5]]=c("Ottawa","Ottawa", 23.3, 52.1, 1992)
SITES[[6]]=c("Ellerslie_Solberg", "Ellerslie", 39, 17,  1983)
SITES[[7]]=c("Lethbridge_E001", "Lethbridge", 31, 46,  1912)
SITES[[8]]=c("Lethbridge_E008", "Lethbridge", 25, 45,  1960)
SITES[[9]]=c("Harrow_CC", "Ottawa", 37,28, 1990)
SITES[[10]]=c("Harrow_CS", "Ottawa", 37,28, 2004)

data.frame(t(sapply(SITES,c)))->SitesDF
names(SitesDF)<-c("SiteName", "WeatherSite", "clay", "sand", "syear")

# function that runs simulations as an Experiment

RIPCC=function(p,X, S)
{
 RunCarbon=function(input_pars,X)
  {
  #X=X
    print(input_pars)  
    eFlag=0
    
    Site_Name=input_pars[1,1]
    Weather_Site=input_pars[1,2]
    CLAY_PX=as.double(input_pars$clay)
    SAND_PX=as.double(input_pars$sand)
    
    init.year=as.double(input_pars$syear)
    
    #print(Weather_Site)
    
    
    eFlag=0
    exFlag=FALSE
    init_treatments=read.csv("~/CMods/Input_Data/Init_SOC_Treatments.csv")
    params <- read.csv(paste0("~/IPCC_2_Eval/Data/default_parameters_V1.csv"),header=TRUE,sep=",",dec=".")[,1:2]
    source("~/IPCC_2_Eval/R/SOCTier2Model_V2.r")
    source("~/IPCC_2_Eval/R/IPCC_Tier2_SteadyState.R")
    
    ##################### COMMON FILES SECTIONS and weather files ##############
    #Get the monthly weather data
    indirpath="~/CMods/Input_Data/"
    weather_all=read.csv(paste0(indirpath, "Weather/All_Sites_Monthly_weather.csv"))
    #print("After weather sites fetched")
    
    ################ init soc treatments########################
    CLAY=CLAY_PX
    SAND=SAND_PX/100
    
    #Prepare weather data for rothc
    weather_all%>%filter(Site==Weather_Site)->weather
    print("After weather site fetched")
    
    cinput_icbm=read.csv(paste0(indirpath,"Sites/", Site_Name,"/cinput_soc_abge.csv"), header = T)
    
    
    
    syear_ori=min(cinput_icbm$year)
    
    
    #Grab the min and max year from cinput
    max_year=as.numeric(max(cinput_icbm$year))
    min_year=as.numeric(min(cinput_icbm$year))
    nyear=max_year-min_year
    
    #remove extra rows after max_year
    cinput_icbm%>%filter(year<=max_year)->cinput
    
    #Code that gets the list of unique treatment names from the cinput file
    Allcols=colnames(cinput_icbm)
    Allcols[grep("_cin_", Allcols, ignore.case = T)]->trts_names
    
    
    trts=list()
    count=1
    for(trt in trts_names){
      trts[count]=strsplit(trt,"_")[[1]][1]
      count=count+1
    }
    trts=unique(trts)
    ########################## END OF COMMON FILE SECTION##############
    
    
    
    
    
    ####################### tillage and fert and init soc block###############################
    till_fert=read.csv(paste0(indirpath,"/Sites/", Site_Name,"/trt_till_fert.csv"),stringsAsFactors = F)
    max_init_soc=max(till_fert$init_soc)
    ######################## end of tillage block########################
    
    
    
    
    
    
    
    #########Calculate the mean cinput and assign them dynamically to all treatments
    for(el in trts){
      #el=trts[1]
      #View(el)
      cinput_icbm%>%select(paste0(el, "_cin_ag"))->carbon_input_ag
      cinput_icbm%>%select(paste0(el, "_cin_bg"))->carbon_input_bg
      cinput_icbm%>%select(paste0(el, "_cin_bge"))->carbon_input_bge
      if(eFlag)
        carbon_inp=as.double(carbon_input_ag[,1])+
        as.double(carbon_input_bge[,1])
      if(!eFlag)
        carbon_inp=as.double(carbon_input_ag[,1])+
        as.double(carbon_input_bg[,1])
      #print(class(carbon_inp))
      carbon_inp=carbon_inp#*p[1]
      coln=paste0(el,"_cin")
      cinput_icbm%>%mutate(!!coln:=carbon_inp)->cinput_icbm
      #View(cinput_icbm)
      assign(paste0(el,"_cin"),carbon_inp)
    }
    
    for(i in 1:length(trts)){
      cinput%>%select(year,paste0(trts[i],"_act_soc"))%>%
        filter(year<=max_year)->tem
      assign(paste0("Actual_",trts[i]),tem)
      
    }
    
    
    
    #######################################################################################
    
    
    
    ################################## IPCC Tier 2 section###################
    
    
    
    ################ Set the parameter values
    
    ipcc_results=NULL
    #p=1
    pars=X[X$SampleID==p, -1]
    cfac=pars[ncol(pars)]
    pars=pars[1:ncol(pars)-1]
    
    
    params[]=unlist(pars)
    
    
    Get_Weather_IPCC=function(siteName,ys,ye){
      tempor=NULL
      weather_all%>%filter(Site==siteName)%>%mutate(tavg=Tavg,
                                                    irrig=0,
                                                    mappet=Prec/PET)%>%
        rename(month=Month)%>%select(month, tavg, mappet,irrig)%>%
        mutate(month=as.numeric(month),mappet=ifelse(mappet>1.25, 1.25, mappet))->ippc_base
      ippc_box=list()
      counter=1
      
      for(y in ys:ye){
        ippc_ghost=ippc_base
        ippc_ghost$year=as.numeric(y)
        ippc_box[[counter]]=ippc_ghost
        ippc_ghost=NULL
        counter=counter+1
      }
      
      tempo=do.call("rbind", ippc_box)
      tempo%>%select(year,month,tavg,mappet,irrig)->tempor
      
      return(tempor)
      
    }
    
    
    
    #Subset to get the actual SOC for all treatments
    cinput_icbm%>%select(ends_with("_soc"))%>%na.omit()->Actual_SOC
    
    
    
    set_till="FT"
    
    
    ############## spin up section##################################
    
    # carbon input files for 3 treatments
    for(el in trts){
      
      INIT_SOC_EL=init_treatments%>%filter(SiteName==SiteName, Treatment==el)%>%pull(soc)  
      
      # Code for selecting specific tillage, ligfrac and nfrac for each treatment
      set_till<<-as.character(till_fert$till[till_fert$treatments==el])
      
      
      
      
      cinput_icbm%>%select(year,paste0(el,"_cin"))%>%
        mutate(sand=SAND,site=1, ligfrac=0.0069, nfrac=0.053, till=set_till)%>%
        rename(cinput=paste0(el,"_cin"))%>%
        select(site,year,sand,cinput,ligfrac,nfrac,till)%>%
        mutate(till=ifelse(year<syear_ori, "RT", till))%>%
        mutate(cinput=ifelse(year<syear_ori, cinput, cinput))%>%
        mutate(cinput=cinput*cfac[1,1])->df ### cinput articulated here...
      assign(paste0(el,"_trt_ipcc"),df)
      
      
      
      
      
      
    }
    
    #View(cinput_icbm)
    
    #Observed SOC for trts
    for(el in trts){
      eval(as.name(paste0("Actual_",el)))%>%select(year, paste0(el,"_act_soc"))%>%
        mutate(SiteName=1, Treatment=el)%>%na.omit()-> dff
      #View(dff)
      assign(paste0("Actual_SOC_",el),dff)
    }
    
    
    
    #Function to Prepare weather data for ippc tier 2
    weather_ipcc=Get_Weather_IPCC(Weather_Site, min_year,max_year)
    
    
    #Call IPCC Tier 2 method for treatments
    
    counter=1
    ipcc_results=NULL
    for(el in trts){
      print("Starting")
      print(el)
      #el=trts[1]
      INIT_SOC_EL=init_treatments%>%
        filter(SiteName==Site_Name, Treatment==el)%>%pull(soc)
      
      cinvec=1
      spinyear=1:1
      ipcc_start=NULL
      init_cin=mean(eval(as.symbol(paste0(el,"_trt_ipcc")))$cinput[1:10])
      
      ipcc_start=data.frame(site=1, 
                            year=1,
                            sand=SAND, 
                            cinput=init_cin,
                            ligfrac=0.0069,
                            nfrac=0.053, till="NT")
      
      print("spin file for ipcc ready")
      
      ipcc_weather_spin=Get_Weather_IPCC(Weather_Site, 1,1)
      
      print("weather file for ipcc spin ready")
      
      IPCCT2_SS(ipcc_start,
                ipcc_weather_spin)->ipcc_sim1
      
      total_soc=as.numeric(INIT_SOC_EL)
      init_active=round((ipcc_sim1$activess/ipcc_sim1$total)*total_soc,2)
      init_slow=round((ipcc_sim1$slowss/ipcc_sim1$total)*total_soc,2)
      init_passive=round((ipcc_sim1$passivess/ipcc_sim1$total)*total_soc,2)
      
      print(paste0("ipcc pools are: active", init_active, " passive, ", init_passive, " slow, ", init_slow))
      IPCCTier2SOMmodel(eval(as.symbol(paste0(el,"_trt_ipcc"))),
                        weather_ipcc,
                        params,
                        init_active, init_slow, init_passive)->ipcc_res
      assign(paste0(el,"_IPCC_Res"),ipcc_res)
      print("after ipcc run")
      
      Actual=eval(as.symbol(paste0("Actual_",el)))%>%rename(Actuals=2)
      ipcc_res%>%select(1,5)%>%mutate(Treatment=el, SiteName=Site_Name, SampleID=p)%>%left_join(Actual)->ips
      #View(ips)
      if(counter==1)
        ipcc_results=ips
      if(counter>1)
      ipcc_results%>%rbind(ips)->ipcc_results
      #View(ipcc_results)
      counter=counter+1
      #print("Finishing")
      
    }
     
    return(ipcc_results)
    }
    print("Simulation called and finished")
    #  ################################## END OF IPCC######################
    # 
    # 
    # 
    # 
    

  

  
  
  
      # ncores=11
      # cl=makeCluster(ncores)
      # registerDoParallel(cl)
      # result<-foreach(i=1:length(SITES), .packages = c("tidyverse", "reshape2"), .combine = rbind)%dopar%
      #    RunCarbon(SITES[[i]], X)
      # stopCluster(cl)

  result=NULL
  for(i in 1:nrow(S)){
    ghost=RunCarbon(S[i,], X)
    #View(S[i,])
    #print("After ghost before binding")
    #View(ghost)
    #ghost=RunCarbon(SitesToRun[1,], X)
    
    result=rbind(result, ghost)
    #print(paste0("after binding", " running ", i))
  }
      
      
  # rm(weather_ipcc)
  # rm(weather_all)
  # rm(tem)
  # rm(trts)
  
  
  #result=RunCarbon(SITES[[1]], X)
  return(result)

}



RunIPCC=function(p, X, drv, val){
  #drv="Region"
  #val="East"
  #p=1
  SitesToRun=NULL
  d=as.symbol(drv)
  sitecat=read.csv("~/CMods/Input_Data/DriverCats.csv")
  sitecat%>%filter(!!d==val)%>%select(SiteName)->SelSites
  #View(SelSites)
  SitesDF%>%filter(SiteName %in% SelSites$SiteName)->SitesToRun
  IPCCPred=RIPCC(p, X, SitesToRun) # Predicted results from the IPCC model 
  #View(SitesToRun)
  m=IPCCPred%>%filter(!is.na(Actuals))%>%pull(TotSOC) # modeled
  o=IPCCPred%>%filter(!is.na(Actuals))%>%pull(Actuals) # observed
  return(data.frame("SampleID"=p,"loglik"= loglik(m,o)))
  
}

#RunIPCC(1, X, "Region","East")














