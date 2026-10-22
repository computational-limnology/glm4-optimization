# remove everything from workspace
rm(list = ls())


library(glmtools)
library(tidyverse)
library(rLakeAnalyzer)
library(GLM3r)
library(lubridate)
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
library(ggplot2)
library(reshape2)

# declare output files
sim_folder= 'GLM-AED2/'
out_file <- file.path(sim_folder, "output","output.nc")

# read observed data
df_obs <- read.csv('trainingData/MendotaData_observedGLM_checked.csv')

# read best simulation results
df_nrmse = read.csv("GLM-AED2/calib_results_nrmse.csv")
df_log = read.csv("GLM-AED2/calib_results_loglike.csv")
df = read.csv("GLM-AED2/calib_results_nse.csv")

df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
                     PHY_tchla) %>%
  dplyr::filter(flag > -1e8)


ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  geom_point(aes(as.POSIXct(time), value, col = NRMSE)) + 
  geom_line(aes(as.POSIXct(time), value, col = NRMSE)) + 
  theme(legend.position = 'bottom') + xlab('time') + ylab('NSE')+
  scale_colour_continuous(type = rev("viridis")) +
  facet_wrap(~variable, scales = 'free')

best_run = df[which.max(df$temp),]
best_run = df[which.min(df$NRMSE),]

df.head=df %>%
  arrange(NRMSE)
head(df.head, n = 10)

all_MSE = unique(df.head$NRMSE)

# get best parameters
parms = read.csv("GLM-AED2/calib_par.csv")

df[which(df$OXY_oxy == max(df$OXY_oxy)),]
df[which(df$PHS_frp == max(df$PHS_frp)),]

ggplot(df) +
  geom_point(aes(OXY_oxy, temp)) +
  xlim(0,1)+ylim(0,1)

idx = na.omit(match(which(df$OXY_oxy > 0.56), which(df$temp >= 0.8)))

best_NRMSE = df %>% filter(temp > 0.8 & OXY_oxy >= 0.56) 
best_parms = parms %>% filter(NRMSE == best_NRMSE$NRMSE)
# 
# df_nrmse %>% filter(NRMSE == best_run$NRMSE)
# df_log %>% filter(NRMSE == best_run$NRMSE)
# 
# best_parms = parms[which(df$OXY_oxy > 0.56) & which(df$temp >= 0.8),]
# 
best_parms = parms %>% filter(NRMSE == best_run$NRMSE)
best_parms = best_parms[1,]
# 
# best_parms = parms[which(df$OXY_oxy == max(df$OXY_oxy)),]
# 
# best_parms = parms[which(df$NRMSE == df.head$NRMSE[2])[1],]
# 
# best_parms = parms[which(df$NRMSE == all_MSE[2])[1],]
# df[which(df$NRMSE == all_MSE[2])[1],]
# 
# best_parms = parms[which(df$PHS_frp == max(df$PHS_frp)),]
# 
# best_parms = best_parms[1,]



# get calibration setup
config_file <- "LakeEnsemblR.yaml"
model <- c("GLM", "GOTM", "Simstrat")

path = "GLM-AED2/"
nml_file <- file.path("GLM-AED2/", 'glm3.nml')
eg_nml <- read_nml(nml_file = file.path(nml_file))

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
'file' = c('glm3.nml', 'glm3.nml', 'glm3.nml','glm3.nml','glm3.nml','glm3.nml', 'aed2.nml','aed2.nml','aed2.nml',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           "aed2_phyto_pars.csv", 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv', 'aed2_phyto_pars.csv',
           'glm3.nml','glm3.nml', 'glm3.nml','glm3.nml', 'glm3.nml','glm3.nml'))
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

idx_phyto = which(calib_setup$type ==  'phyto')
calib_setup = calib_setup[- idx_phyto,]


aed_file = 'aed2'
glm_file = 'glm3'
phyto_file = 'aed2_phyto_pars'

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

# run GLM
GLM3r::run_glm('GLM-AED2/')
sim_vars(out_file)

level = get_var(file = out_file, var_name = 'lake_level')
plot(level)

volumes = get_var(file = out_file, var_name = 'lake_volume')
plot(volumes)

snow = get_var(file = out_file, var_name = 'snow_thickness')
plot(snow)

snow = get_var(file = out_file, var_name = 'white_ice_thickness')
plot(snow)

snow = get_var(file = out_file, var_name = 'blue_ice_thickness')
plot(snow)

meteo = read_csv('GLM-AED2/meteo_file_snowtoPP.csv')


dates_meteo = meteo %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(total_rain = mean(Rain))

dates_meteo %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(total_rain = sum(total_rain))

m.df = reshape2::melt(meteo, id.var = 'Date')
ggplot(m.df %>% filter(Date < '1990-04-01 00:00:00')) + 
  geom_line(aes(Date, value))+
  facet_wrap(~ variable, scales = 'free')

plot_var(nc_file = out_file, var_name = 'ZOO_grz', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'PHY_tchla', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OXY_oxy', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_poc', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_doc', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'PHS_frp', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'NIT_nit', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'temp', legend.position = 'bottom')

plot_var(nc_file = out_file, var_name = 'ZOO_zoo01', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'ZOO_zoo02', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'ZOO_zoo03', legend.position = 'bottom')

plot_var(nc_file = out_file, var_name = 'PHY_green', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'PHY_diatom', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'PHY_cyano', legend.position = 'bottom')

# visualize FITS, important stuff
GLM3r::run_glm('GLM-AED2/')
x_start = as.POSIXct('1995-01-01 00:00:00')
x_end = as.POSIXct('2004-12-31 23:00:00')

# TEMPERATURE
var_name = 'temp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'Temp. (deg C)'

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# OXYGEN
var_name = 'OXY_oxy'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'DO (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# GLM3r::run_glm('GLM-AED2/')
# NITORGEN
var_name = 'NIT_nit'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'NO3 (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# PHOSPHORUS
var_name = 'PHS_frp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'SRP (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# pH
var_name = 'CAR_pH'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'pH (log10 mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# NIT_amm
var_name = 'NIT_amm'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'Amm (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# Silica
var_name = 'SIL_rsi'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'Silica (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')


# PHYTO
var_name = 'PHY_tchla'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
surface_temp$var = surface_temp[,2]
y_label = 'Chl-a (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 5), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (5m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 13)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 13), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (13m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# DOC
# GLM3r::run_glm('GLM-AED2/')
var_name = 'OGM_doc'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'DOC (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (5m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# ZOOPLANKTON
# GLM3r::run_glm('GLM-AED2/')
var_name = 'ZOO_zoo01'
surface_temp_1 <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 0.5)
surface_temp_1$var = surface_temp_1[,2]
var_name = 'ZOO_zoo02'
surface_temp_2 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_2$var = surface_temp_2[,2]
var_name = 'ZOO_zoo03'
surface_temp_3 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_3$var = surface_temp_3[,2]
y_label = 'ZOO (mmol C/m3)'
# plot_df_obs = df_obs %>% select(datetime, depth, var_name)
# plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot()+
  geom_line(data = surface_temp_1, aes(DateTime, var, col = 'sim_1')) +
  geom_line(data = surface_temp_2, aes(DateTime, var, col = 'sim_2')) +
  geom_line(data = surface_temp_3, aes(DateTime, var, col = 'sim_3')) +
  geom_line() +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CAL') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CAL, col = 'obs_CAL')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CLA') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CLA, col = 'obs_CLA')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_COP') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_COP, col = 'obs_COP')) +
  ggtitle(paste0('Average',var_name)) +
  xlim(x_start, x_end) + ylim(0, 500)+
  xlab(label = '') + ylab(label = y_label) +
  theme_bw()

h <- h1; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/',var_name,'.png'), plot = h, dpi = 300, width = 15, height = 9, units = 'in')

# PHYTOPLANKTON-ZOOPLANKTON
# GLM3r::run_glm('GLM-AED2/')
var_name = 'ZOO_zoo01'
surface_temp_1 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_1$var = surface_temp_1[,2]
var_name = 'ZOO_zoo02'
surface_temp_2 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_2$var = surface_temp_2[,2]
var_name = 'ZOO_zoo03'
surface_temp_3 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_3$var = surface_temp_3[,2]
var_name = 'PHY_tchla'
surface_temp_4 <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
surface_temp_4$var = surface_temp_4[,2]

plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

y_label = 'ZOO (mmol C/m3)'


h1 <- ggplot()+
  # geom_line(data = surface_temp_1, aes(DateTime, var, col = 'sim_1')) +
  geom_line(data = surface_temp_2, aes(DateTime, var, col = 'sim_2')) +
  # geom_line(data = surface_temp_3, aes(DateTime, var, col = 'sim_3')) +
  geom_line(data = surface_temp_4, aes(DateTime, var, col = 'sim_phy')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CAL') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CAL, col = 'obs_CAL')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CLA') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CLA, col = 'obs_CLA')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_COP') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_COP, col = 'obs_COP')) +
  geom_point(data = plot_df_obs %>% filter(depth == 5), aes(as.POSIXct(datetime), var, col = 'obs_phy')) +
  ggtitle(paste0('Average',var_name)) +
  xlim(x_start, x_end) + ylim(0, 500)+
  xlab(label = '') + ylab(label = y_label) +
  theme_bw()

h <- h1; h
ggsave(filename = paste0('GLM-AED2/A_08092024/figures/','phyto-zoop','.png'), plot = h, dpi = 300, width = 15, height = 9, units = 'in')

GLM3r::run_glm('GLM-AED2/')
sim_vars('GLM-AED2/output/output.nc')

surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
plot(surface_temp)

## ANALYTICS
plot_var(nc_file = out_file, var_name = 'PHY_tchla', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OXY_oxy', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_poc', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_doc', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'CAR_dic', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_docr', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'temp', legend.position = 'bottom')

plot_var(nc_file = out_file, var_name = 'OGM_doc_anaerobic', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_doc_denit', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_doc_min', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_docr_min', legend.position = 'bottom')
plot_var(nc_file = out_file, var_name = 'OGM_doc_swi', legend.position = 'bottom')
