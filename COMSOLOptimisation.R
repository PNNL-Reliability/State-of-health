library(tidyverse)
library(reshape2)
library(lubridate)
library(feather)

z<-c(9.7e+09,197,6.09e-20,0.319,0.25*3.21e-08,0.338,3.88e-12,55.6,2)

COMSOLfiles<-c("'COMSOL Models/Li Porous SOH 2019 NMC2 Reliability PS.mph'",
               "'COMSOL Models/Li Porous SOH 2019 LFP Reliability PS.mph'",
               "'COMSOL Models/Li Porous SOH 2019 NMC1 Reliability PS.mph'")

COMSOLOutputDirectory<-"COMSOL Output\\"
logz<-log(z)

LossFnReliability<-function(x){
  
  y<-exp(x)
  print(y)
  if(TRUE){
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[1],
                      " -pname K_SEI,k_kinder,D_solv,iex_SEI,delx,i_0_cathdis,U_SEI,k_neg,k_cath_dis,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.0104,
                      ",",
                      y[7-2]*y[9],
                      "[A/m^2],",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      y[10-2]*.855,
                      "[m^2/mol],",
                      "0.2"),
               intern=TRUE)
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    NMC2Data20<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\NMC220DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.0104,3,format="e"),
                        "cathdis",formatC(y[7-2]*y[9],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        "knickel",formatC(y[10-2]*.855,3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    file.remove(file)
    
    
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[1],
                      " -pname K_SEI,k_kinder,D_solv,iex_SEI,delx,i_0_cathdis,U_SEI,k_neg,k_cath_dis,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.0104,
                      ",",
                      y[7-2]*y[9],
                      "[A/m^2],",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      y[10-2]*.855,
                      "[m^2/mol],",
                      "0.6"),
               intern=TRUE)
    
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    NMC2Data60<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\NMC260DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.0104,3,format="e"),
                        "cathdis",formatC(y[7-2]*y[9],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        "knickel",formatC(y[10-2]*.855,3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    
    file.remove(file)
    
  }
  
  if(TRUE){
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[2],
                      "-pname K_SEI,k_kinder,D_solv,iex_SEI,delx,U_SEI,k_neg,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.000164,
                      ",",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      "0.2"),
               intern=TRUE)
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    LFPData20<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\LFP20DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.000164,3,format="e"),
                        #"cathdis",formatC(y[7],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        #"knickel",formatC(y[10],3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    
    file.remove(file)
    
    
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[2],
                      "-pname K_SEI,k_kinder,D_solv,iex_SEI,delx,U_SEI,k_neg,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.000164,
                      ",",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      "0.6"),
               intern=TRUE)
    
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    LFPData60<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\LFP60DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.000164,3,format="e"),
                        #"cathdis",formatC(y[7],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        #"knickel",formatC(y[10],3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    
    file.remove(file)
    
  }
  
  if(TRUE){
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[3],
                      " -pname K_SEI,k_kinder,D_solv,iex_SEI,delx,i_0_cathdis,U_SEI,k_neg,k_cath_dis,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.0104,
                      ",",
                      y[7-2],
                      "[A/m^2],",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      y[10-2]*.738,
                      "[m^2/mol],",
                      "0.2"),
               intern=TRUE)
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    NMC1Data20<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\NMC20DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.0104,3,format="e"),
                        "cathdis",formatC(y[7-2],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        "knickel",formatC(y[10-2]*.738,3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    file.remove(file)
    
    
    oldfiles<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    
    msg<-shell(paste0("comsolbatch -inputfile ",
                      COMSOLfiles[3],
                      " -pname K_SEI,k_kinder,D_solv,iex_SEI,delx,i_0_cathdis,U_SEI,k_neg,k_cath_dis,DOD -plist ",
                      y[4],
                      "[S/m],",
                      y[2],
                      "[h],",
                      y[3],
                      "[m^2/s],",
                      y[1],
                      "[A/m^2],",
                      0.0104,
                      ",",
                      y[7-2],
                      "[A/m^2],",
                      y[8-2],
                      "[V],",
                      y[9-2],
                      "[m/s],",
                      y[10-2]*.738,
                      "[m^2/mol],",
                      "0.6"),
               intern=TRUE)
    
    files<-data.frame(File=list.files(COMSOLOutputDirectory,full.names = TRUE,pattern="*.csv"),stringsAsFactors = FALSE)
    
    file<-files%>%
      filter(!(File %in% oldfiles$File))%>%
      .$File
    
    if(is_empty(file)){return(NA)}
    
    NMC1Data60<<-file%>%
      read.csv(skip=7)%>%
      mutate(Time=X..X,
             Current=Height,
             Aging=Color,
             dt=c(0,diff(Time)),
             Ah=Current*dt,
             State=sign(Current),
             Index=cumsum(State!=lag(State,default=0)))%>%
      group_by(Index)%>%
      summarise(Capacity=sum(Ah),
                Aging=median(Aging))%>%
      ungroup%>%
      mutate(CumAh=cumsum(Capacity))%>%
      filter(Aging==0,
             Capacity>0)%>%
      mutate(CumAh=Capacity-max(Capacity),
             CapLoss=1-Capacity/max(Capacity),
             Cycle=200*seq_along(Index)-200,
             Tot=CumAh-Capacity)
    
    file.copy(from=file,
              to=paste0(COMSOLOutputDirectory,
                        "\\NMC60DOD",
                        "KSEI",formatC(y[4],3,format="e"),
                        "kkinder",formatC(y[2],3,format="e"),
                        "DSolv",formatC(y[3],3,format="e"),
                        "iExSEI",formatC(y[1],3,format="e"),
                        "delX",formatC(0.0104,3,format="e"),
                        "cathdis",formatC(y[7-2],3,format="e"),
                        "USEI",formatC(y[8-2],3,format="e"),
                        "kneg",formatC(y[9-2],3,format="e"),
                        "knickel",formatC(y[10-2]*.738,3,format="e"),
                        now()%>%as.character(format="%Y%m%dT%H%M"),
                        ".csv"),
              overwrite=TRUE)
    
    file.remove(file)
    
  }
  
  
  bind_rows(
    ReliabilityData%>%
      filter(grepl("d20",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="NMC2")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(NMC2Data20$Cycle,NMC2Data20$CapLoss,Cycle)$y),
    ReliabilityData%>%
      filter(grepl("d60",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="NMC2")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(NMC2Data60$Cycle,NMC2Data60$CapLoss,Cycle)$y),
    ReliabilityData%>%
      filter(grepl("d20",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="LFP")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(LFPData20$Cycle,LFPData20$CapLoss,Cycle)$y),
    ReliabilityData%>%
      filter(grepl("d60",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="LFP")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(LFPData60$Cycle,LFPData60$CapLoss,Cycle)$y),
    ReliabilityData%>%
      filter(grepl("d20",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="NMC1")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(NMC1Data20$Cycle,NMC1Data20$CapLoss,Cycle)$y),
    ReliabilityData%>%
      filter(grepl("d60",
                   Scenario2))%>%
      group_by(Chemistry)%>%
      filter(Chemistry=="NMC1")%>%
      select(Cycle,CapLoss,Chemistry)%>%
      mutate(Model=approx(NMC1Data60$Cycle,NMC1Data60$CapLoss,Cycle)$y)
  )%>%
    mutate(Error=CapLoss-Model)%>%
    .$Error%>%
    na.omit%>%
    .^2%>%
    mean->MSE
  
  return(MSE)
  
  
}

a<-optim(logz,LossFnReliability)
