library(stringr)
library(raster)
library(dplyr)
library(readr)
library(magrittr)

# debug
options(warn=0) # 2)
options(error=stop) # recover)

wd = getwd()
v = list(
  EC = list(
    dir_in  = 'data/CetMap_2015-09_public-release/Duke_CetMap_Models_EC_20150602/Model_Predictions'),
  GOM = list(
    dir_in  = 'data/CetMap_2015-09_public-release/Duke_CetMap_Models_GOM_20150713/Model_Predictions'))
spp_csv     = 'data/species/spp.csv'
spp_wts_csv = 'data/species/spp_weights.csv'

# read in species conservation status and associated weights
# create weights: read_csv(spp_wts_csv) %>% mutate(w = seq(100, 1, length.out = 9)) %>% select(status, w) %>% write_csv(spp_wts_csv)
spp = read_csv(spp_csv) %>%
  left_join(read_csv(spp_wts_csv), by='natureserve') #%>%
  #filter(sp_code %in% c('Atlantic_spotted_dolphin','Atlantic_white_sided_dolphin','North_Atlantic_right_whale')) # DEBUG
w_spp = spp %>% 
  group_by(sp_code) %>%
  arrange(sp_code, sp_guild, sp_name, sp_scientific) %>%
  mutate(sp_code_w = mean(w)) %T>%
  write_csv('data/species/spp_code_w.csv') %>%
  summarize(w = first(sp_code_w))



S = list() # big stack of species rasters by region
for (rgn in names(v)){ # rgn = 'EC'
  
  # set outputs
  grd_aea = sprintf('data/species/spp_%s_nzw_aea.grd', rgn)
  grd_mer = sprintf('data/species/spp_%s_nzw_mer.grd', rgn)
  
#   # skip if already done
#   if (file.exists(grd_aea)){
#     cat(sprintf('%s: SKIPPING b/c grd_mer exists\n', rgn)) # Seals only in EC
#     next()
#   } else {
#     cat(sprintf('%s\n', rgn))
#   }
  
  # get list of images
  wd = v[[rgn]][['dir_in']]
  imgs = list.files(path=wd, pattern='._abundance\\.img$')
  
  # extract species and month from filename
  m = str_match(imgs, '([A-z]+)(_month([0-9]+))?(_months_([0-9]+)_to_([0-9]+))?_abundance.img') %>% 
    as.data.frame(stringsAsFactors=F) %>%
    mutate(
      fname   = V1,
      sp_code = V2,
      month   = as.integer(V4),
      month_beg = as.integer(V6),
      month_end = as.integer(V7)) %>%
    select(sp_code, month, month_beg, month_end, fname) %>%
    arrange(sp_code, month, month_beg)
    
  # output regional species csv for manipulating into spp.csv
  # data_frame(
  #    sp_code = unique(m$sp_code)) %>% 
  #    write_csv(sprintf('data/species/spp_%s.csv', rgn))

  # iterate over months (1:12) and annual average (0)
  for (mo in 0:12){ # mo = 1
    
    # get suffix for annual avg ('') or monthly ('_##')
    cat(sprintf('  %02d\n', mo))
    sfx = ifelse(mo==0, '', sprintf('_%02d', mo))
    
    # iterate over species
    m_spp = unique(m$sp_code)
    for (s_c in m_spp){ # s_c = 'Atlantic spotted dolphin' # (1) # s_c = 'Atlantic_white_sided_dolphin' # (12) # s_c = 'North_Atlantic_right_whale'
      
      # skip seals
      if (s_c %in% spp$sp_code){
        cat(sprintf('    %s\n', s_c))
      } else {
        cat(sprintf('    %s: SKIPPING b/c not in spp.csv\n', s_c))
        next()
      }

      # read raster, avg if annual (mo==0)
      s_imgs = sprintf('%s/%s', wd, filter(m, sp_code == s_c) %>% .$fname)
      if (mo==0 & length(s_imgs) > 1){
        r_n = mean(stack(s_imgs))
      } else if (mo!=0 & length(s_imgs) > 1){ # mo=6
        f_n = file.path(
          wd,
          m %>%
            filter(sp_code == s_c & month==mo) %>%
            .$fname)
        r_n = raster(f_n) # ; plot(r_n)
      } else {
        r_n = raster(s_imgs) # plot(r_n)
      }
    
      # get quantiles of cumulative abundance
      d_q = r_n %>%
        getValues() %>%
        data_frame(x=.) %>% 
        mutate(i=row_number()) %>%
        filter(!is.na(x)) %>%
        arrange(desc(x)) %>% 
        mutate(n_pct = cumsum(x) / sum(x))
      r_q = r_n
      r_q[d_q$i] = d_q$n_pct
      
      # plot with contours
      # plot(r_q)
      # contour(r_q, levels=c(0.5, 0.99), add=T, drawlabels=F, col=c('blue','red'))
      
      # filter by pixels making up 99% of abundance
      r_nf = mask(r_n, r_q <= 0.99, maskvalue=0) # plot(r_nf)
      
      # get normalized score (scaled by overall abundance) and weighted by NatureServe conservation status
      r_nfz = calc(r_nf, fun=function(x){ (x - mean(x, na.rm=T)) / sd(x, na.rm=T) }) # 
      w =  w_spp %>% filter(sp_code == s_c) %>% .$w
      r_nfzw = r_nfz * w # plot(r_nfzw)
  
      # TODO: evaluate scales of abundance (n) vs normalization (z) vs extinction species weight (w), 
      #  eg EC Atlantic_white_sided_dolphin: 
      #     r_n = [0, 38.54], r_nz = [-0.53, 6.07], w = 25.75, r_nzw = [-13.53, 156.26]
      #  eg EC Seals: 
      #     r_n = [0, 5507.99], r_nz = [-0.03, 93.22], w = 25.75, r_nzw = [-13.53, 156.26]
      
      # add to stack
      nms = sprintf('%s_%s%s', s_c, c('n','q','nf','nfz','nfzw'), sfx) # indiv per 100km^2
      if (!exists('stk')){
        stk = stack(r_n, r_q, r_nf, r_nfz, r_nfzw)
        names(stk) = nms
      } else {
        stk = stack(stk, r_n, r_q, r_nf, r_nfz, r_nfzw)
        names(stk) = c(names(stk)[1:(nlayers(stk)-length(nms))], nms)
      }
    } # for species

    substk = function(x, v, sfx){ # x=stk; s='_nf'
      ext = sprintf('_%s%s', v, sfx)
      lyrs = names(x)[str_sub(names(x), nchar(ext)*-1, -1) == ext]
      raster::subset(x, lyrs)
    }
    
    # a_i: number of species
    s_i = substk(stk, 'nf', sfx)
    a_i = sum(!is.na(s_i), na.rm=T) # plot(a_i)
    a_i = mask(a_i, a_i, maskvalue=0)
    
    # a_n: average species abundance
    a_nf = substk(stk, 'nf', sfx) %>%
      sum(na.rm=T) / a_i %>%
      mask(a_i) # plot(a_n)
    
    # a_nz: average species normalized density
    a_nfz = substk(stk, 'nfz', sfx) %>%
      sum(na.rm=T) / a_i %>%
      mask(a_i) # plot(a_nz)
    
    # a_nzw: average species normalized density * extinction risk weight
    a_nfzw = substk(stk, 'nfzw', sfx) %>%
      sum(na.rm=T) / a_i %>%
      mask(a_i) # plot(a_nzw)
    
    # add to stack
    stk = stack(stk, a_i, a_nf, a_nfz, a_nfzw)
    names(stk) = c(names(stk)[1:(nlayers(stk)-4)], sprintf('ALL_%s%s', c('i','nf','nfz','nfzw'), sfx))
  
  } # end for mo in months
  
  # write out raster stack per region
  cat('writing grd_aea\n')
  writeRaster(stk, grd_aea, overwrite=T)

  # project stack to Mercator for leaflet
  cat('projecting grd_aea to grd_mer\n')
  stk_mer = leaflet::projectRasterForLeaflet(stk)
  cat('writing grd_mer\n')
  writeRaster(stk_mer, grd_mer, overwrite=T)

  # remove stack for next region
  rm(stk)
}

# combine weighted stacks: see report_cetmpap.R


#   
#   img_ec = '~/Downloads/CetMap_2015-09_public-release/Duke_CetMap_Models_EC_20150602/Model_Predictions/North_Atlantic_right_whale_month07_abundance.img'
#   r_ec_aea = raster(img_ec)
#   r_ec_mer = projectRaster(r_ec_aea, crs=CRS(epsg3857), res=10000, method='bilinear')