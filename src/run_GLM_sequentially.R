
rm(list = ls())

# devtools::install_github("aemon-j/GLMr")
setwd('C:/Users/au740615/Documents/Projects/glm4-optimization')

library(glmtools)
library(tidyverse)
library(rLakeAnalyzer)
library(GLMr)
library(lubridate)
library(ggplot2)
library(reshape2)

path = "testcase/"


# read in observed data for calibration
obs_wtemp <- read.csv(paste0(path,"/observed.txt")) 


nml_file <- file.path(paste0(path,"/glm4.nml"))
eg_nml <- read_nml(nml_file = file.path(nml_file))
out_file <- file.path(paste0(path,"/output/output.nc"))



# run GLM
GLMr::run_glm(path)
sim_vars(out_file)



var = c('temp', 'OXY_oxy' ,'NIT_nit','NIT_amm', "PHS_frp", 'SIL_rsi', "OGM_doc", 'PHY_tchla')         # variable to which we apply the calibration procedure
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','coef_mix_hyp',"inflow_factor",'Kw','outflow_factor',
                                                  'Fsed_oxy', 'Fsed_oxy', 'Ksed_oxy', 'Kdom_minerl', 'Rdom_minerl',
                                                  'Fsed_nit', 'Fsed_nit', 'Ksed_nit', 'Knitrif', 'Fsed_amm', 'Fsed_amm',
                                                  'Fsed_frp', 'Fsed_frp',
                                                  'Fsed_rsi',
                                                  'R_growth', 'R_growth', 'R_growth',
                                                  "Fsed_dic", 'Rnitrif', 'Kdenit',  'Ksed_frp',
                                                  'Kpom_hydrol',
                                                  "R_resp", "R_resp", "R_resp",
                                                  "R_nuptake","R_nuptake", "R_nuptake",
                                                  "R_puptake",   "R_puptake",   "R_puptake",
                                                  'sed_temp_mean', 'sed_temp_mean', 
                                                  'sed_temp_amplitude', 'sed_temp_amplitude',
                                                  'sed_temp_peak_doy', 'sed_temp_peak_doy'
)),
'lb' = NA,
'ub' = NA,
'x0' = c(1,1,0.4,1,0.8,1, -150, -150, 50, 60, 0.05, -10, -1, 10, 80, 30, 1, 0.5, 0.1, 18, 4, 1, 3,
         250, 0.5, 3,  200, 60,
         0.085,0.085,0.085,
         0.068, 0.068, 0.068,
         0.009, 0.0039, 0.0039,
         9, 15, 0.8, 15.3, 253, 205),
'type' = c('glm', 'glm', 'glm','glm', 'glm','glm','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed',
           "phyto", 'phyto', 'phyto',
           'aed','aed','aed','aed','aed','phyto', 'phyto', 'phyto',
           'phyto', 'phyto', 'phyto','phyto', 'phyto', 'phyto',
           'glm', 'glm', 'glm', 'glm', 'glm', 'glm'),
'file' = c('glm4.nml', 'glm4.nml', 'glm4.nml','glm4.nml','glm4.nml','glm4.nml', 'aed2.nml','aed2.nml','aed2.nml',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           "aed2_phyto_pars.csv", 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'glm4.nml','glm4.nml', 'glm4.nml','glm4.nml', 'glm4.nml','glm4.nml'))
calib_setup$lb[ calib_setup$x0 >= 0] = calib_setup$x0[ calib_setup$x0 >= 0] * 0.3
calib_setup$ub[ calib_setup$x0 >= 0] = calib_setup$x0[ calib_setup$x0 >= 0] * 1.7
calib_setup$lb[ calib_setup$x0 < 0] = calib_setup$x0[ calib_setup$x0 < 0] * 1.7
calib_setup$ub[ calib_setup$x0 < 0] = calib_setup$x0[ calib_setup$x0 < 0] * 0.3
calib_setup$lb[5] = 0.01
calib_setup$ub[5] = 2.0
calib_setup$lb[42] = 150
calib_setup$ub[42] = 280
calib_setup$lb[43] = 150
calib_setup$ub[43] = 280
print(calib_setup)


calib_setup = calib_setup %>% filter(pars %in% c("wind_factor", "lw_factor", "coef_mix_hyp", "Kw"))


any(calib_setup$lb > calib_setup$ub)



if (file.exists(paste0(path,'/calib_results_nse.csv'))){
  file.remove(paste0(path,'/calib_results_nse.csv'))
}
if (file.exists(paste0(path,'/calib_results_nrmse.csv'))){
  file.remove(paste0(path,'/calib_results_nrmse.csv'))
}
if (file.exists(paste0(path,'/calib_results_loglike.csv'))){
  file.remove(paste0(path,'/calib_results_loglike.csv'))
}
if (file.exists(paste0(path,'/calib_par.csv'))){
  file.remove(paste0(path,'/calib_par.csv'))
}

glmcmd = NULL        # command to be used, default applies the glm4r function

#outcome of previous calibration runs
period = list('calibration' = list('start' = get_nml_value(eg_nml, 'start'),
                                   'end' = '1999-12-31 23:00:00'),
              validation = list('start' = '2000-01-01 00:00:00',
                                'end' = get_nml_value(eg_nml, 'stop'))) # define a period for the calibration, 
# this supports a split-sample calibration (e.g. calibration and validation period)
# the ratio value is the ratio of calibration period to validation period
print(period)
      # simulation path/folder
aed_file = 'aed'
glm_file = 'glm4'
phyto_file = 'aed_phyto_pars'
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
metric = 'NRMSE'      # objective function to be minimized, here the normalized root-mean square error
target.fit = 1e-5     # refers to a target fit (stops when metric is below that)
target.iter = 5    # refers to a maximum run of calibration iterations (stops after that many runs)
pop_members = 10
output = out_file    # path of the output file
field_file = obs_wtemp # path of the field data

source('src/calibration_script.R')
library(adagio)
library(DEoptim)

# check 
get_results <- function(var){
  
  save(result, file = paste0(path,var,'.RData'))
  df = read.csv(paste0(path,"/calib_results_nse.csv"))
  parms = read.csv(paste0(path,"/calib_par.csv"))
  
  write.csv(parms,paste0(path,'/',var,'_par.csv'), row.names = F, quote = F)
  write.csv(df,paste0(path,'/',var,'_results.csv'), row.names = F, quote = F)
  
  best_run = df[which.min(df$NRMSE),]
  
  best_parms = parms %>% filter(NRMSE == best_run$NRMSE)
  best_parms = best_parms[1,]
  
  p = as.numeric(best_parms[-c(1, length(best_parms))])
  
  # run model with best parameter set
  for (nml_file in unique(calib_setup$file)){
    if (tools::file_ext(nml_file) == 'csv'){
      
      eg_nml = read_csv(paste0(path, nml_file))
      
      eg_nml = as.data.frame(eg_nml)
      
      idx = which(calib_setup$file == nml_file)
      
      use_p = p[idx]
      
      nml_setup = calib_setup %>% filter(file == nml_file)
      
      for(i in 1:length(nml_setup$pars[!duplicated(nml_setup$pars)])){
        if (any(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars[duplicated(nml_setup$pars)])){
          
          id = which(eg_nml$p_name == nml_setup$pars[!duplicated(nml_setup$pars)][i])
          eg_nml[id,2:ncol(eg_nml)] = use_p[which(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars)]
          
          # eg_nml <- set_nml(eg_nml, nml_setup$pars[!duplicated(nml_setup$pars)][i], 
          # use_p[which(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars)])
        } else {
          
          id = which(eg_nml$p_name == nml_setup$pars[!duplicated(nml_setup$pars)][i])
          eg_nml[id,2:ncol(eg_nml)] = use_p[!duplicated(nml_setup$pars)][i]
          
          
          # eg_nml <- set_nml(eg_nml,nml_setup$pars[!duplicated(nml_setup$pars)][i],
          #                   use_p[!duplicated(nml_setup$pars)][i])
        }
      }
      write_csv(eg_nml, file = paste0(path, nml_file))
      
    } else {
      
      eg_nml <- read_nml(paste0(path,nml_file))
      
      idx = which(calib_setup$file == nml_file)
      use_p = p[idx]
      nml_setup = calib_setup %>% filter(file == nml_file)
      
      for(i in 1:length(nml_setup$pars[!duplicated(nml_setup$pars)])){
        if (any(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars[duplicated(nml_setup$pars)])){
          eg_nml <- set_nml(eg_nml, nml_setup$pars[!duplicated(nml_setup$pars)][i], 
                            use_p[which(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars)])
        } else {
          eg_nml <- set_nml(eg_nml,nml_setup$pars[!duplicated(nml_setup$pars)][i],use_p[!duplicated(nml_setup$pars)][i])
        }
      }
      
      write_nml(eg_nml, file = paste0(path, nml_file))
      
    }
    
    
    
  }
}



###
if(file.exists(paste0(path, glm_file,'-template.nml'))){
  file.copy(paste0(path, glm_file,'-template.nml'), paste0(path, glm_file, '.nml'), overwrite = T)
} 

if(file.exists(paste0(path, aed_file,'-template.nml'))){
  file.copy(paste0(path, aed_file,'-template.nml'), paste0(path, aed_file, '.nml'), overwrite = T)
} 

if(file.exists(paste0(path, phyto_file,'-template.nml'))){
  file.copy(paste0(path, phyto_file,'-template.nml'), paste0(path, phyto_file, '.nml'), overwrite = T)
} 



###
glm_version()
#### TEMPERATURE
var = 'temp' 

print(calib_setup)

result = calibrate_glm(var,
                       path,
                       field_file,
                       aed_file = aed_file,
                       glm_file = glm_file,
                       phyto_file = phyto_file,
                       calib_setup,
                       glmcmd = NULL,
                       period,
                       scaling = FALSE,
                       verbose = TRUE,
                       metric = 'NRMSE',
                       target.fit = target.fit,
                       target.iter = target.iter,
                       pop_members = pop_members,
                       plotting = TRUE,
                       output,
                       parallelMode = F,
                       overwrite = T)

get_results(var)
