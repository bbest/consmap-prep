is_pareto = function(x, y, method='max', rev_x=F){
  # http://stackoverflow.com/questions/21294829/fast-calculations-of-the-pareto-front-in-r
  
  # check inputs
  stopifnot(length(x)==length(y))
  stopifnot(is.numeric(x) & is.numeric(y))
  stopifnot(method %in% c('min','max'))
  
  
  # get max pareto-optimal points
  if (method=='max' & rev_x==F){
    v = data_frame(i=1:length(x), x=x, y=y) %>%
      arrange(desc(x), desc(y)) %>%
      mutate(
        y_cummax = cummax(y),
        pareto = !duplicated(y_cummax)) %>%
      arrange(i) %>%
      .$pareto
  }
  
  # get max pareto-optimal points with x_rev
  if (method=='max' & rev_x==T){
    v = data_frame(i=1:length(x), x=x, y=y) %>%
      arrange(x, desc(y)) %>%
      mutate(
        y_cummax = cummax(y),
        pareto = !duplicated(y_cummax)) %>%
      arrange(i) %>%
      .$pareto
  }
  
  # get min pareto-optimal points
  if (method=='min'){
    v = data_frame(i=1:length(x), x=x, y=y) %>%
      arrange(x, y) %>%
      mutate(
        y_cummin = cummin(y),
        pareto = !duplicated(y_cummin)) %>%
      arrange(i) %>%
      .$pareto
  }
  
  # return value
  return(v)
}

simulate_utility = function(
  d, x, y, 
  fxn=lazyeval::lazy(a * x + (1 - a) * y), 
  fxn_slope=lazyeval::lazy(- a/(1 - a)), 
  fxn_intercept=lazyeval::lazy(u[u_max] / (1 - a)), 
  a_vec = seq(0, 1, 0.1), 
  rescale = T,
  rescale_sfx  = '_rescaled',
  a_out        = 'a_sim',   # alpha
  u_out        = 'u_sim',
  slope   = T,
  u_max        = 'u_max',
  u_slope      = 'u_slope',
  u_yintercept = 'u_yintercept'){  # utility
  
  library(dplyr)
  library(scales)
  
  # check arguments
  stopifnot(x %in% names(d)); stopifnot(y %in% names(d))
  stopifnot(is.numeric(d[[x]])); stopifnot(is.numeric(d[[y]])); stopifnot(is.numeric(a_vec))
  stopifnot(sum(duplicated(d[,c(x,y)])) == 0)
  
  # transform data
  r = d %>%
    tbl_df() %>%
    # rename input columns to x_in, x_in
    select_(., .dots = c(x_in=x, y_in=y)) %>%
    # rescale x and y, if rescale==T
    {if ( rescale)
      mutate(
        ., 
        x = rescale(x_in), 
        y = rescale(y_in))  else .} %>%
        {if (!rescale) 
          mutate(
            ., 
            x = x_in, 
            y = y_in) else .}
  
  # iterate over alpha vector, calculate utility and bind to output
  o = NULL
  for (a in a_vec){ # a = 0.5
    #browser()
    o_i = r %>%
      mutate(a = a) %>%                              # store alpha
      mutate_(.dots = setNames(list(fxn), 'u')) %>% # apply utility function
      {if (slope)
          mutate(.,
            u_max   = u == max(u)) %>%
          mutate_(
            .dots = setNames(list(fxn_slope), 'u_slope')) %>%               # apply slope function,
          mutate_(
            .dots = setNames(list(fxn_intercept), 'u_yintercept')) else .}  # apply yintercept function,
    if (is.null(o)){
      o = o_i
    } else {
      o = o %>%
        bind_rows(o_i)
    }
  }
  
  # get names for x & y rescaled columns
  x_rescaled = sprintf('%s%s', x, rescale_sfx)
  y_rescaled = sprintf('%s%s', y, rescale_sfx)
  
  # return data frame
  d %>%
    tbl_df() %>%
    # drop output columns if in input data frame
    .[,!names(d) %in% c(a_out, u_out)] %>%
    {if (rescale) .[,!names(d) %in% c(x_rescaled, y_rescaled)] else .} %>%
    {if (slope) .[,!names(d) %in% c(u_max, u_slope, u_yintercept)] else .} %>%
    left_join(
      o %>%
        # rename output columns
        rename_(., .dots = setNames(c('x_in','y_in','a','u'), c(x,y,a_out,u_out))) %>%
        {if ( rescale) rename_(., .dots = setNames(c('x','y'), c(x_rescaled,y_rescaled))) else .} %>%
        {if (!rescale) select_(., .dots = c('-x','-y')) else .} %>%
        {if (   slope) rename_(., .dots = setNames(c('u_max','u_slope','u_yintercept'), c(u_max,u_slope,u_yintercept))) else .},
      by = c(x, y))
}