options(error=NULL)

iplot = function(x, legend.title=''){
  # x = raster(stk_i, 'depth'); legend.title='depth'
  
  library(stringr)
  library(RColorBrewer)
  library(leaflet)
  library(raster)
  addLegend = leaflet::addLegend
  
  if (class(x) == 'RasterLayer'){
    
    # project if not already in mercator for leaflet
    if (str_sub(proj4string(x), 1, 103) == str_sub(leaflet:::epsg3857, 1, 103)){
      x_mer = x  
    } else {
      x_mer = projectRasterForLeaflet(x)  
    }
    
    pal = colorNumeric(rev(brewer.pal(11, 'Spectral')), values(x_mer), na.color = "transparent")
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% 
      addRasterImage(x_mer, colors=pal, project=F, opacity=0.8) %>%
      addLegend(pal=pal, position='bottomright', values=values(x_mer), title=legend.title)
  }
} 

crop_na = function(r){
  # crop out NA margins of raster
  m <- is.na(as.matrix(r))
  cols = which(colSums(m) != nrow(m))
  rows = which(rowSums(m) != ncol(m))
  crop(
    r,
    extent(
      r, 
      rows[1], rows[length(rows)],
      cols[1], cols[length(cols)]))
}

add_raster = function(x, opacity = 1, colors='Spectral', project = TRUE){
  # for adding rasters to ggmap, pulled from leaflet::addRasterImage

  require(leaflet)
  require(RColorBrewer)
  
  stopifnot(inherits(x, "RasterLayer"))
  
  # project if needed
  if (project) {
    projected <- projectRasterForLeaflet(x)
  } else {
    projected <- x
  }
  
  # get bounds
  bounds <- raster::extent(
    raster::projectExtent(
      raster::projectExtent(
        x, crs = sp::CRS(leaflet:::epsg3857)), crs = sp::CRS(leaflet:::epsg4326)))
  
  # set colors
   if (!is.function(colors)) {
      colors <- colorNumeric(colors, domain = NULL, na.color = "#00000000", alpha = TRUE)
   }
  
  alpha_na = function(x, a){
    ifelse(x=="#00000000", x, scales::alpha(x, alpha=a))
  }
    
  img <- raster::values(projected) %>% colors() %>% alpha_na(opacity) %>% # 
    matrix(nrow=nrow(projected), byrow=T) %>%
    grDevices::as.raster()
  
  inset_raster(img, bounds@xmin, bounds@xmax, bounds@ymin, bounds@ymax)
}

plot_raster = function(
  r,       # raster to plot
  r_depth, # depth raster for contours (extracted via marmap::getNOAA.bathy())
  st,      # states polygons (see ma, ie Mid-Atlantic states prep)
  pdf_path = 'img/birds_mid-atlantic.pdf',
  col = rev(colorRampPalette(brewer.pal(11,'Spectral'))(255)),
  r_extend = c(2,2), bg = 'gray80',
  pdf_width = 7, pdf_height = 7, pdf_open = T,
  graticule_x = seq(-65,-80), graticule_y = seq(34,43),
  awc=F, phylopic='',
  study_area=F,
  par_mar=c(4,4,0,1.5)+0.2, # c(bottom, left, top, right)
  lbl_pts = NULL, lbl_col='gray90',
  depth_do = T,
  wea_do = F,
  quantile_do = F,
  quantile_vals = c(0.4,0.6,0.8),
  quantile_cols = c('darkblue','darkgreen','darkred'),
  quantile_labs = c('60%','40%','20%'),
  quantile_lwd = 2){
  
  # example: 
  #   plot_raster(r_b_c, r_depth, st=ma, pdf_path = 'img/birds_mid-atlantic.pdf')
  # plot_raster(r_i6, r_depth, ma, pdf_path='img/industry_npv6_mid-atlantic.pdf', phylopic='wind turbine')
  # r=r_i6; r_depth=r_depth; st=ma; pdf_path='img/industry_npv6_mid-atlantic.pdf'; phylopic='wind turbine'
  # col = rev(colorRampPalette(brewer.pal(11,'Spectral'))(255));
  # r_extend = c(2,2); bg = 'gray80'
  # pdf_height = 7; pdf_open = T
  # graticule_x = seq(-65,-80); graticule_y = seq(34,43)
  # awc=F
  # study_area=F
  # par_mar=c(4,4,0,1.5)+0.2
  # lbl_pts = NULL; lbl_col='gray90'
  # depth_do = T
  # quantile_do = F
  # quantile_vals = c(0.4,0.6,0.8)
  # quantile_cols = c('darkblue','darkgreen','darkred')
  # quantile_labs = c('60%','40%','20%')
  # quantile_lwd = 2
  
  # plot_raster(r_u, r_depth, ma, pdf_path='img/map_birds_vs_industry8_utility_2019-01-wea.pdf', 
  # awc=F, depth_do = F, quantile_do = T, 
  # wea_do=T, # NEW!
  # lbl_pts = lbl_pts, lbl_col='black')
  #
  # r=r_u; r_depth=r_depth; st=ma; pdf_path='img/map_birds_vs_industry8_utility_2019-01-wea.pdf'
  # awc=T; depth_do = F; quantile_do = T
  # wea_do=T # NEW!
  # lbl_pts = lbl_pts; lbl_col='black'
  # phylopic=''
  # col = rev(colorRampPalette(brewer.pal(11,'Spectral'))(255));
  # r_extend = c(2,2); bg = 'gray80'
  # pdf_height = 7; pdf_open = T
  # graticule_x = seq(-65,-80); graticule_y = seq(34,43)
  # study_area=F
  # par_mar=c(4,4,0,1.5)+0.2
  # quantile_vals = c(0.4,0.6,0.8)
  # quantile_cols = c('darkblue','darkgreen','darkred')
  # quantile_labs = c('60%','40%','20%')
  # quantile_lwd = 2

  # turn on pdf device
  pdf(pdf_path, width=pdf_width, height=pdf_height)
  par('mar' = par_mar) # c(bottom, left, top, right)
  
  r_e = raster::extend(r, r_extend)
  if (study_area){
    r_1 = mask(!is.na(r_e), r_e)
    study = rasterToPolygons(r_1, dissolve=T)
    
    # plot study area polygon
    plot(study, col=NA, border='red', lwd=2)
  } else {
    # plot once to extended raster to get plotting area
    plot(r_e, col=col, xaxs='r', yaxs='r', asp=1, useRaster=F, xaxt='n', yaxt='n')
  }
  
  # get raster extents at different projections for plotting area (par)
  p = par('usr')
  par_aea = raster(xmn=p[1], xmx=p[2], ymn=p[3], ymx=p[4], crs = crs(r))
  par_gcs = projectExtent(par_aea, crs=CRS(leaflet:::epsg4326))
  
  # plot background color
  if (wea_do){
    bg = "white"
  }
  rect(p[1], p[3], p[2], p[4], col=bg)
  
  if (study_area){
    # plot study area polygon
    plot(study, col=NA, border='red', lwd=2, add=T)
  } else {
    if (wea_do){
      # plot raster grays
      col = rev(colorRampPalette(gray.colors(12))(255))
      plot(r, col=col, xaxs='r', yaxs='r', asp=1, useRaster=F, xaxt='n', yaxt='n', add=T)
    } else {
      # plot raster colors
      plot(r, col=col, xaxs='r', yaxs='r', asp=1, useRaster=F, xaxt='n', yaxt='n', add=T)
    }
  }
  
  # plot Atlantic Wind Connection
  if (awc){
    awc_gcs = rgdal::readOGR('../consmap/data/wind_connection','atlantic-wind-connection_gcs', verbose=F)
    awc = spTransform(awc_gcs, crs(r))
    plot(awc, col='purple', add=T, lwd=2)
  }
  
  # plot states
  plot(st, col='white', border='gray60', add=T, lwd=0.5)
  
  # hard-coding DC exception to plot as asterisk
  points(coordinates(subset(st, code=='DC')), pch='*', cex=2, col='gray20')
  
  # plot all other states
  st_notdc = subset(st, code!='DC')
  text(st_notdc$x, st_notdc$y, labels=as.character(st_notdc$code), cex=1, col='gray50') #, font=2)
  
  # get graticules, crop to par, plot
  grat_aea = graticule::graticule(graticule_x, graticule_y, proj=proj4string(r))
  grat_c_aea = crop(grat_aea, par_aea)
  plot(grat_c_aea, lty=2, col='gray50', add=T)
  
  # add reference graphics to lower right
  g = c(p[2] - (p[2] - p[1])/6, p[3]+(p[4] - p[3])/12)
  scalebar(100000, xy=c(g[1],g[2]), type='bar', divs=4, label='100 km', adj=c(-0.1, 1.5)) #, adj=c(0.5, -0.5)) # xy=click()
  GISTools::north.arrow(xb=g[1]+50000, yb=g[2]+50000, len=10000, lab="N")
  
  # get graticule labels
  gc = data_frame(id=integer(0), x_min=numeric(0), x_max=numeric(0), y_min=numeric(0), y_max=numeric(0))
  for (i in 1:length(grat_c_aea)){ # i = 1
    xy = coordinates(grat_c_aea@lines[[i]])[[1]]
    gc = bind_rows(
      gc,
      data_frame(
        id = i,
        x_min = min(xy[,'x']),
        x_max = max(xy[,'x']),
        y_min = min(xy[,'y']),
        y_max = max(xy[,'y'])) %>%
        mutate(
          # get axis
          ax     = ifelse(diff(range(xy[,'x'])) > diff(range(xy[,'y'])), 'y', 'x'), # axis to apply lines
          ch_min = ifelse(ax=='x', as.character(y_min), as.character(x_min)),       # character min, for later filtering non-min lines
          # get axis tick value based on axes
          ax_tic = ifelse(
            ax == 'x', 
            xy[which.min(xy[,'y']),'x'],
            xy[which.min(xy[,'x']),'y'])))
  }
  # remove lines not at min
  gc = gc %>%
    group_by(ch_min) %>%
    mutate(
      n_min = n()) %>%
    group_by(ax) %>%
    filter(n_min == max(n_min)) %>%
    as.data.frame()
  # get points along axes
  x_pts = gc %>% filter(ax=='x') %>% select(x=ax_tic, y=y_min)
  y_pts = gc %>% filter(ax=='y') %>% select(x=x_min, y=ax_tic)
  # get lon/lat labels along axes
  x_lbs = SpatialPoints(
    x_pts, proj4string=crs(r)) %>%
    spTransform(crs(leaflet:::epsg4326)) %>%
    coordinates() %>% .[,'x'] %>% round()
  y_lbs = SpatialPoints(
    y_pts, proj4string=crs(r)) %>%
    spTransform(crs(leaflet:::epsg4326)) %>%
    coordinates() %>% .[,'y'] %>% round()
  
  # add axes ticks and labels
  axis(1, at=x_pts$x, labels=x_lbs, col.ticks='gray50', col.axis='gray50')
  axis(2, at=y_pts$y, labels=y_lbs, col.ticks='gray50', col.axis='gray50', las=2)
  lines(c(p[1], p[1], p[2], p[2], p[1]),
        c(p[3], p[4], p[4], p[3], p[3]), lwd=2, col='black')
  title(xlab='longitude', ylab='latitude', col.lab='gray40')
  
  # add depth contours
  if (depth_do){
    r_d_aea = projectRaster(r_depth, par_aea)
    # see http://www.nees.rpi.edu/research/wind-towers/ for offshore wind dev reasons behind these depths
    #contour_depths = c(-30, -80, -200, -500, -1000, -1500, -3000)
    # see http://www.boem.gov/renewable-energy-program/renewable-energy-guide/offshore-wind-energy.aspx
    contour_depths = c(-30, -60, -200, -900, -2000, -3000)
    contour(
      mask(r_d_aea, r_d_aea >= 0, maskvalue=1), 
      levels = contour_depths, 
      lwd    = seq(0.8, 1.6, length.out=length(contour_depths)),
      labcex = 0.8, font = 2, col='gray20', add=T)
  }
  
  # wind energy areas (wea)
  if (wea_do){
    library(sf)
    boem  <- rbind(
      read_sf("data/boem/BOEM_Renewable_Energy_Areas_Shapefiles_10_24_2018/BOEM_Lease_Areas_10_24_2018.shp") %>% 
        mutate(
          leased = TRUE) %>% 
        select(name = Company, leased),
      read_sf("data/boem/BOEM_Renewable_Energy_Areas_Shapefiles_10_24_2018/BOEM_Wind_Planning_Areas_10_24_2018.shp") %>% 
        filter(
          !CAT1 %in% c("California Call Area","Hawaii Call Area")) %>% 
        mutate(
          leased = TRUE) %>% 
        select(name = INFO, leased)) %>% 
      st_transform(raster::projection(par_aea))

    boem_grouped_shp <- "data/boem/boem_grouped_aea.shp"
    if (!file.exists(boem_grouped_shp)){
      boem_grouped <- boem %>% 
        group_by(name, leased) %>% 
        summarize(n = n()) %>% 
        ungroup() %>% 
        mutate(
          area_km2 = st_area(geometry) %>% units::set_units(km^2))
      write_sf(boem_grouped, boem_grouped_shp)
    }
    
    boem <- boem %>% as("Spatial")
    boem@data$name <- as.factor(boem@data$name)
    
    # plot raster b&w
    #rect(p[1], p[3], p[2], p[4], col=bg) # bg="gray80"
    # rect(p[1], p[3], p[2], p[4], col="white")
    # col = rev(colorRampPalette(gray.colors(12))(255))
    # plot(r, col=col, xaxs='r', yaxs='r', asp=1, useRaster=F, xaxt='n', yaxt='n', add=T)
    
    pal <- leaflet::colorFactor("Spectral", domain=boem@data$name) # boem@data$name)
    #plot(boem["name"], border=NA, col=alpha(pal(boem@data$name), 0.5), add=T)
    plot(boem["name"], border=NA, col=alpha(pal(boem@data$name), 0.9), add=T)
  }
  
  # quantile contours
  if (quantile_do){
    #cat('adding contours\n')
    #contour(r, levels = quantile_vals, col=quantile_cols, drawlabels=F, lwd=quantile_lwd, add=T)
    #plot(r)
    #contour(r, levels = quantile_vals, add=T)
    contour(r, levels = quantile(r, probs=quantile_vals), col=quantile_cols, labels=quantile_labs, drawlabels=T, lwd=quantile_lwd, add=T)
    #filledContour(r, levels = quantile_vals, col=quantile_cols, labels=quantile_labs, drawlabels=T, lwd=quantile_lwd, add=T)
  }

  # add point labels
  if (!is.null(lbl_pts)){
    with(lbl_pts, text(x, y, as.character(label), col=lbl_col, adj=c(0,1)))
    with(lbl_pts, points(x, y, pch='.', cex=2))
  }

  if (phylopic=='herring gull'){
    #http://phylopic.org/image/0871b630-2dcd-4d35-b0a4-6a6551140553/
    library(rphylopic)
    img = image_data('0871b630-2dcd-4d35-b0a4-6a6551140553', size=128)[[1]]
    #add_phylopic_base(img, alpha=1, x=p[1]+(p[2]-p[1])/8, y=p[4]-(p[4]-p[3])/8, ysize=(p[4]-p[3])/6)
    #add_phylopic_base(img, 0.5, 0.5, 0.2)
    add_phylopic_base(img, alpha=1, 0.08, 0.8, 0.12)
  }
  if (phylopic=='balaena'){
    #http://phylopic.org/image/18438875-f6e8-4e46-8fda-5a07bd4c1a85/
    library(rphylopic)
    # plot(r_i6)
    img = image_data('18438875-f6e8-4e46-8fda-5a07bd4c1a85', size=128)[[1]] # grid::grid.raster(img)
    #add_phylopic_base(img, alpha=1, x=p[1]+(p[2]-p[1])/6, y=p[4]-(p[4]-p[3])/6, ysize=(p[4]-p[3])/12) #, col='green')
    add_phylopic_base(img, alpha=1, 0.15, 0.85, 0.25)
  }
  if (phylopic=='wind turbine'){
    #img = httr::content(httr::GET('file:/Users/bbest/github/consmap-prep/img/phylopics/ian-symbol-energy-wind-turbine-1_black.png'))
    img = png::readPNG(here::here('img/phylopics/ian-symbol-energy-wind-turbine-1_black.png'))
    img = array(c(matrix(0, nrow=nrow(img), ncol=ncol(img)), img), dim=c(nrow(img), ncol(img), 4))
    img[,,1:3] = 0
    img[,,4] = ifelse(img[,,4]==1, 0,1)
    #add_phylopic_base(img, alpha=1, x=p[1]+(p[2]-p[1])/10, y=p[4]-(p[4]-p[3])/5, ysize=(p[4]-p[3])/3)
    add_phylopic_base(img, alpha=1, 0.08, 0.8, 0.12)
  }
  
  #http://phylopic.org/image/0871b630-2dcd-4d35-b0a4-6a6551140553/
  #add_phylopic_base(img, 1, posx[i], posy[i], sizey[i], cols[i])
    
  # finish
  dev.off()
  if (pdf_open) system(sprintf('open %s', pdf_path))
}
