vast_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
setwd(dir = vast_output_dirname)
load("Save.RData")


# setting -------------------------------------------------------
Sdreport = Save[["Opt"]][["SD"]]
use_biascorr=TRUE
# Which parameters
if( "ln_Index_tl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
  # SpatialDeltaGLMM
  CogName = "mean_Z_tm"
  EffectiveName = "effective_area_tl"
  TmbData[['n_c']] = 1
}
if( "ln_Index_ctl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
  # VAST Version < 2.0.0
  CogName = "mean_Z_ctm"
  EffectiveName = "effective_area_ctl"
}
if( "ln_Index_cyl" %in% rownames(TMB::summary.sdreport(Sdreport)) ){
  # VAST Version >= 2.0.0
  CogName = "mean_Z_cym"
  EffectiveName = "effective_area_cyl"
  Save$TmbData[["n_t"]] = nrow(Save$TmbData[["t_yz"]])
}
Year_Set = 1:Save$TmbData$n_t
Years2Include = 1:Save$TmbData$n_t
strata_names = 1:Save$TmbData$n_l
category_names = 1:Save$TmbData$n_c



# from plot_range_index.R ---------------------------------------
if( !any(c("effective_area_tl","effective_area_ctl","effective_area_cyl") %in% names(Report)) ){
  message( "To plot effective area occupied, please re-run with Options['Calculate_effective_area']=1" )
}else{
  message( "Plotting effective area occupied..." )
  
  # Extract estimates
  SD = TMB::summary.sdreport(Sdreport)
  SD_effective_area_ctl = SD_log_effective_area_ctl = array( NA, dim=c(unlist(Save$TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
  # Effective area
  if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
    SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Est. (bias.correct)','Std. Error')]
  }
  if( !any(is.na(SD_effective_area_ctl)) ){
    message("Using bias-corrected estimates for effective area occupied (natural scale)...")
  }else{
    message("Not using bias-corrected estimates for effective area occupied (natural scale)...")
    SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Estimate','Std. Error')]
  }
  # Log-Effective area
  if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
    SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Est. (bias.correct)','Std. Error')]
  }
  if( !any(is.na(SD_log_effective_area_ctl)) ){
    message("Using bias-corrected estimates for effective area occupied (log scale)...")
  }else{
    message("Not using bias-corrected estimates for effective area occupied (log scale)...")
    SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Estimate','Std. Error')]
  }
  
  # Plot area
  plot_index( Index_ctl=array(SD_log_effective_area_ctl[,,,'Estimate'],dim(SD_log_effective_area_ctl)[1:3]),
              sd_Index_ctl=array(SD_log_effective_area_ctl[,,,'Std. Error'],dim(SD_log_effective_area_ctl)[1:3]),
              Year_Set=Year_Set, Years2Include=Years2Include, strata_names=strata_names, category_names=category_names,
              DirName="", PlotName=FileName_EffArea, scale="uniform",
              interval_width=interval_width, xlab="Year", ylab="Effective area occupied, ln(km^2)", Yrange=c(NA,NA),
              width=ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))*4, height=ceiling(sqrt(TmbData$n_c))*4 )
  #png( file=FileName_EffArea, width=ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))*2.5, height=ceiling(sqrt(TmbData$n_c))*2.5, res=200, units="in")
  #  par( mfrow=c(1,1), mar=c(2,2,1,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(1,1,1,0), mfrow=c(ceiling(sqrt(TmbData$n_c)),ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))))
  #  for( cI in 1:TmbData$n_c ){
  #    Ybounds = SD_log_effective_area_ctl[cI,,1,1]%o%rep(interval_width,2) + SD_log_effective_area_ctl[cI,,1,2]%o%c(-interval_width,interval_width)
  #    plot_lines( x=Year_Set, y=SD_log_effective_area_ctl[cI,,1,1], ybounds=Ybounds, ylim=range(Ybounds), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="", ylab="", type="l", main=category_names[cI])
  #  }
  #  mtext( side=1:3, text=c("Year","ln(km^2)","Effective area occupied"), outer=TRUE, line=c(0,0,0) )
  #dev.off()
  
  # Write to file
  EffectiveArea_Table = NULL
  for( cI in 1:Save$TmbData$n_c ){
    Tmp = cbind("Year"=Year_Set, "EffectiveArea"=SD_log_effective_area_ctl[cI,,1,1], "SE"=SD_log_effective_area_ctl[cI,,1,2])
    if( Save$TmbData$n_c>1 ) Tmp = cbind( "Category"=category_names[cI], Tmp)
    EffectiveArea_Table = rbind(EffectiveArea_Table, Tmp)
  }
  
  # Return stuff
  Return = c(Return, list("SD_effective_area_ctl"=SD_effective_area_ctl, "SD_log_effective_area_ctl"=SD_log_effective_area_ctl, "EffectiveArea_Table"=EffectiveArea_Table))
}
  

write.csv(EffectiveArea_Table, "EffectiveArea_Table.csv")



# figure --------------------------------------------------------
ea = read.csv("EffectiveArea_Table.csv")
ea$Year = 1995:2019

g = ggplot(ea, aes(x = Year, y = EffectiveArea))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
e = geom_errorbar(ymin = ea$EffectiveArea-ea$SE, ymax = ea$EffectiveArea+ea$SE, width = 0.3)
lab = labs(x = "年", y = "ln(km^2)")
EA = g+p+l+e+theme_bw(base_family = "HiraKakuPro-W3")+lab+scale_x_continuous(breaks=seq(1996, 2019, by = 3), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0), limits = c(6.95, 7.45))
setwd(dirname)
ggsave(file = "effectivearea.png", plot = EA, units = "in", width = 11.69, height = 8.27)
