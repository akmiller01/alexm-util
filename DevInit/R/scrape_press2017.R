list.of.packages <- c("scrapeR","jsonlite","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

urls = c(
  "https://tian-y.github.io/PRESS2017Maps/PRESS_2017_AFG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_AGO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ALB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ARE.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ARG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ARM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ATG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_AZE.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BDI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BEN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BFA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BGD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BGR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BHS.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BIH.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BLR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BLZ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BMU.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BOL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BRA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BRB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BRN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BTN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_BWA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CAF.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CHL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CHN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CIV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CMR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_COD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_COG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_COK.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_COL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_COM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CPV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CRI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CUB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_CYP.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_DJI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_DMA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_DOM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_DZA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ECU.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_EGY.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ERI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ETH.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_FJI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_FSM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GAB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GEO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GHA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GIN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GMB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GNB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GNQ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GRC.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GRD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GTM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_GUY.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_HND.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_HRV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_HTI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_HUN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_IDN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_IND.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_IRN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_IRQ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_JAM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_JOR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KAZ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KEN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KGZ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KHM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KIR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KNA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KOR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KSV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_KWT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LAO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LBN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LBR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LBY.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LCA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LKA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LSO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_LTU.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MAR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MDA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MDG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MDV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MEX.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MHL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MKD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MLI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MLT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MMR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MNE.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MNG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MOZ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MRT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MUS.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MWI.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_MYS.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NAM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NCL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NER.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NGA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NIC.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_NPL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_OMN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PAK.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PAN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PER.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PHL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PLW.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PNG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_POL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PRK.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_PRY.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_QAT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ROM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_RUS.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_RWA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SAU.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SDN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SEN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SGP.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SHN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SLB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SLE.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SLV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SOM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SRB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SSD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_STP.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SUR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SWZ.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SYC.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_SYR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TCA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TCD.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TGO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_THA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TJK.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TKL.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TKM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TLS.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TON.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TTO.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TUN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TUR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TUV.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_TZA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_UGA.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_UKR.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_URY.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_UZB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_VCT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_VEN.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_VNM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_VUT.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_WBG.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_WLF.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_WSM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_YEM.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ZAF.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ZMB.htm"
  ,"https://tian-y.github.io/PRESS2017Maps/PRESS_2017_ZWE.htm"
)

c1alllist = list()
c2alllist = list()
c3alllist = list()
t1alllist = list()

for(i in 1:length(urls)){
  url = urls[i]
  iso3 = substr(url,51,53)
  message(iso3)
  source = scrape(url, headers=T,follow=T,parse=T)[[1]]
  script_elems = getNodeSet(source,"//script")
  script_vals = sapply(script_elems,xmlValue)
  script_vals = trimws(script_vals[which(script_vals!="")])
  if(length(script_vals)>0){
    chart_sources = sapply(script_vals[c(3,4,5)],fromJSON)
    chart_1_data = chart_sources[[1]]$data
    chart_2_data = chart_sources[[4]]$data
    chart_3_data = chart_sources[[7]]$data
    
    c1list = list()
    for(rownum in 1:nrow(chart_1_data)){
      c1row = chart_1_data[rownum,]
      x = eval(c1row$x)[[1]]
      y = eval(c1row$y)[[1]]
      name = eval(c1row$name)[[1]]
      label = eval(c1row$text)[[1]]
      rowdf = data.frame(x,y,name,label)
      c1list[[rownum]] = rowdf
    }
    c1dat = rbindlist(c1list)
    
    c2list = list()
    for(rownum in 1:nrow(chart_2_data)){
      c2row = chart_2_data[rownum,]
      x = eval(c2row$x)[[1]]
      y = eval(c2row$y)[[1]]
      name = eval(c2row$name)[[1]]
      label = eval(c2row$text)[[1]]
      rowdf = data.frame(x,y,name,label)
      c2list[[rownum]] = rowdf
    }
    c2dat = rbindlist(c2list)
    
    c3list = list()
    for(rownum in 1:nrow(chart_3_data)){
      c3row = chart_3_data[rownum,]
      labels = eval(c3row$labels)[[1]]
      values = eval(c3row$values)[[1]]
      rowdf = data.frame(labels,values)
      c3list[[rownum]] = rowdf
    }
    c3dat = rbindlist(c3list)
    
    c1dat$iso3 = iso3
    c2dat$iso3 = iso3
    c3dat$iso3 = iso3
    
    c1alllist[[i]] = c1dat
    c2alllist[[i]] = c2dat
    c3alllist[[i]] = c3dat
    
    row_elems = getNodeSet(source,"//table/tbody/tr")
    if(length(row_elems)>0){
      row_vals = sapply(row_elems,xmlValue)
      celllist = list()
      for(j in 1:length(row_vals)){
        row_val = row_vals[j]
        cells = strsplit(row_val,"\n")[[1]]
        celldf = data.frame(t(cells))
        names(celldf) = c("Donor","Program Name","Date","Amount")
        celllist[[j]] = celldf
      }
      t1_tmp = rbindlist(celllist)
      t1_tmp$iso3 = iso3
      t1alllist[[i]] = t1_tmp
    }
  }
}

c1 = rbindlist(c1alllist)
c2 = rbindlist(c2alllist)
c3 = rbindlist(c3alllist)
t1 = rbindlist(t1alllist)

setwd("~")
save(c1,c2,c3,t1,file="press2017_charts.RData")