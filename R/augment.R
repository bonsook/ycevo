
#' @importFrom generics augment
#' @export
augment.ycevo <- function(
    x,
    newdata = NULL,
    loess = TRUE){
  df_flat <- unnest(x, data)
  
  cols <- attr(x, "cols")
  qdate_label <- "qdate"
  if(!is.null(cols)) qdate_label <- vapply(as.list(cols)[-1], as.character, character(1))[qdate_label]
  
  if(is.null(newdata)) {
    newdata <- df_flat
  } else {
    if(all(c("xgrid", qdate_label) %in% colnames(newdata)))
      warning("Both xgrid and ", qdate_label, " presented in newdata. Using xgrid.")
    if(!"xgrid" %in% colnames(newdata)){
      newdata <- mutate(newdata, xgrid = ecdf(temp <- getElement(newdata, qdate_label))(temp))
    }
  }
  
  # newdata <- tibble(qdatetime, tupq, tau)
  if(!loess) {
    norow <- anti_join(newdata, df_flat, by = setdiff(c("tau", "xgrid"), colnames(newdata)))
    if(nrow(norow)>0) {
      stop("If loess = FALSE, newdata should be a subset of the xgrid and tau used to fit ycevo (e.g. the default).")
    }
    return(df_flat)
  }
  
  # x <- mutate(x, loess = lapply(
  #   data, function(data)
  #     stats::loess(
  #       .discount ~ tau, 
  #       data = data, 
  #       control = loess.control(surface = "direct"))))
  
  if(any(newdata$tau> max(df_flat$tau)) || any(newdata$tau < min(df_flat$tau)))
    warning("tau in newdata outside of the original range of tau used to fit the model are extrapolated from loess.")
  interpolate(x, newdata, qdate_label)
}


interpolate <- function(object, newdata, qdate_label){
  stopifnot(inherits(object, "ycevo"))
  
  xnames <- setdiff(colnames(object), c("data", "loess", qdate_label))
  ncl <- xnames[!xnames %in% colnames(newdata)]
  
  if(length(ncl)>0)
    stop(paste(ncl, collapse = ", "), " not found in newdata")
  object <- arrange(object, across(all_of(xnames)))
  
  x_sort <- object %>%
    select(all_of(xnames)) 
  
  
  
  newdata_idx <- newdata %>% 
    distinct(across(all_of(xnames))) %>% 
    # slice(46) %>% 
    rowwise() %>% 
    mutate(.idx = list({
      reduce(xnames, function(current_rows, next_x_name){
        if(!is.list(current_rows)) current_rows <- list(current_rows)
        lapply(current_rows, function(current_rows){
          if(length(current_rows) == 1) return(current_rows)
          temp_x_sort <- x_sort[current_rows,]
          next_x <- getElement(temp_x_sort, next_x_name)
          next_row <- findInterval(get(next_x_name), next_x)
          next_rows <- c(next_row, next_row+1)
          next_rows <- next_rows[next_rows>0 & next_rows<=nrow(temp_x_sort)]
          out <- lapply(next_x[next_rows], function(x) current_rows[next_x==x])
          if(next_x_name == xnames[[length(xnames)]]) out <- unlist(out)
          out
        }) %>% 
          list_flatten()
      }, .init = seq_len(nrow(x_sort))) %>% 
        unlist()
    })) %>% 
    unnest(.idx)
  
  df_loess <- newdata_idx %>% 
    group_by(.idx) %>% 
    nest(.key = "xvalue") %>% 
    ungroup() %>% 
    left_join(mutate(object, .idx= row_number()), by = ".idx") %>% 
    mutate(loess = lapply(data, function(data)
      stats::loess(.discount ~ tau, data = data, 
                   control = loess.control(surface = "direct")))) %>% 
    select(.idx, loess, all_of(xnames))
  
  newdata_pred <- newdata %>% 
    # select(all_of(xnames), tau) %>% 
    left_join(newdata_idx,by = xnames) %>% 
    group_by(.idx) %>% 
    mutate(.discount = {
      predict(df_loess$loess[[which(df_loess$.idx == unlist(cur_group()))]], tau)
    }, 
    {
      df_loess[df_loess$.idx == unlist(cur_group()),] %>% 
        select(all_of(xnames)) %>% 
        rename_with(function(x) paste0(x, "_grid."), all_of(xnames))
    }
    ) %>% 
    ungroup()
  
  interp_recu <- function(df, cur_g){
    if(nrow(df)==1 && !is_grouped_df(df)) return(select(df, .discount))
    df <- df %>%
      summarise(.discount = {
        if(n() == 1){
          .discount
        } else {
          current_x_name <- setdiff(colnames(df), c(colnames(cur_group()), ".discount"))
          cg <- getElement(cur_g, current_x_name)
          out <- do.call(\(d1, d2){
            mapply(\(d1, d2){
              approx(x = get(current_x_name), y = c(d1, d2), xout = cg)$y
            }, d1 = d1$.discount, d2=d2$.discount, SIMPLIFY = TRUE)
          }, .discount)
          list(tibble(.discount = out))
        }
      }, .groups = "drop_last")
    interp_recu(df, cur_g)
  }
  
  output <- newdata_pred %>% 
    select(-.idx) %>% 
    nest(.meta = !c(all_of(xnames), ends_with("_grid."), .discount),
         .discount = .discount, 
         .by = c(all_of(xnames), ends_with("_grid."))) %>% 
    # mutate(.discount = lapply(.discount, unlist)) %>% 
    group_by(across(c(all_of(xnames)))) %>% 
    nest() %>% 
    mutate(data = list({
      current_grid <- cur_group()
      meta <- slice(select(data[[1]], .meta), 1)
      data[[1]] %>% 
        select(-.meta) %>% 
        rename_with(function(x) gsub("_grid.","", x), everything()) %>% 
        group_by(across(all_of(xnames))) %>% 
        interp_recu(current_grid) %>% 
        bind_cols(meta)
    }))
  
  output <- output %>% 
    ungroup() %>% 
    unnest(data) %>%
    unnest(everything()) %>% 
    mutate(.yield =  -log(.discount) / tau) %>% 
    relocate(all_of(colnames(newdata))) 
  
  output
}

