# convert_1440
rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(lubridate)
library(here)
library(tidyr)
# set to
bb_id = "2586235"

# need to rerun with correct calculation of n
id_file = here("ids_with_good_qa.txt")

biobank_ids = readLines(id_file)
biobank_ids = as.integer(biobank_ids)

rerun = TRUE
drop_bad = FALSE

for (drop_bad in c(FALSE, TRUE)) {
  
  app = ""
  if (drop_bad) {
    app = "nobad_"
  }
  sum_files = list.files(
    pattern = ".*summary_.*goodQA.*",
    path = here(), recursive = TRUE
  )
  if (drop_bad) {
    keep = grepl("nobad", sum_files)
  } else {
    keep = !grepl("nobad", sum_files)
  }
  sum_files = sum_files[ keep ]
  
  ofiles = sub("summary", "overall", sum_files)
  ofiles = ofiles[ file.exists(ofiles)]
  
  
  full_data = c("summary_1440_goodQA.rds",
                "summary_1368_1440_goodQA.rds")
  full_data = c(paste0("nobad_", full_data), full_data)
  
  odata = sub("summary", "overall", full_data)
  if (drop_bad) {
    keep = grepl("nobad", full_data)
  } else {
    keep = !grepl("nobad", full_data)
  }
  full_data = full_data[ keep ]
  
  if (drop_bad) {
    keep = grepl("nobad", odata)
  } else {
    keep = !grepl("nobad", odata)
  }
  odata = odata[ keep ]
  
  
  sum_files = sum_files[ !basename(sum_files) %in% full_data]
  ofiles = ofiles[ !basename(ofiles) %in% odata]
  
  combined_file = c("threshold_summary_1440_goodQA.rds")
  if (drop_bad) {
    combined_file = paste0("nobad_", combined_file)
  } 
  
  sum_files = sum_files[ !basename(sum_files) %in% combined_file]
  
  vars = sub("nobad_", "", basename(sum_files))
  vars = sub("_summary.*", "", vars)
  names(sum_files) = vars

  ovars = sub("nobad_", "", basename(ofiles))
  ovars = sub("_overall.*", "", ovars)
  names(ofiles) = ovars
  
  combined = map_df(combined_file, function(x) {
    res = read_rds(x)
    res
  })
  combined = combined %>% 
    gather(measure, value,
           q0,q25,q50,q75,q100,sd,mean,min,max)  
  combined = combined %>% 
    filter(is.finite(value))
  
  
  
  names(full_data) = full_data
  names(odata) = odata
  
  x = full_data[1]
  outfile = here(paste0(app, "long_summary_data.rds"))
  
  if (!file.exists(outfile) | rerun) {
    fdf = map_df(full_data, function(x) {
      res = read_rds(x)
      stopifnot("n" %in% colnames(res))
      res = res %>% 
        gather(measure, value,
               q0,q25,q50,q75,q100,sd,mean,min,max)  
      res = res %>% 
        filter(is.finite(value))
      return(res)
    }, .id = "file")
    fdf = fdf %>% 
      mutate(sub_good = grepl("1368", file))
    fdf$file = NULL
    fdf = fdf %>% 
      mutate_at(.vars = vars(day, time, value), as.numeric)
    fdf = fdf %>% 
      arrange(day, time, measure, value)
    fdf = fdf %>% 
      mutate(time_of_day = biobankr::min_to_time(time),
             nobad = drop_bad)
    tz(fdf$time_of_day) = "UTC"    
    fdf = as_data_frame(fdf)
    write_rds(fdf, outfile,
              compress = "xz")
  } else { 
    fdf = read_rds(outfile)
  }
  
  outfile = here(paste0(app, "long_overall_data.rds"))
  
  if (!file.exists(outfile) | rerun) {
    odf = map_df(odata, function(x) {
      res = read_rds(x)
      stopifnot("n" %in% colnames(res))
      res = res %>% 
        gather(measure, value,
               q0,q25,q50,q75,q100,sd,mean,min,max)  
      res = res %>% 
        filter(is.finite(value))
      return(res)
    }, .id = "file")
    odf = odf %>% 
      mutate(sub_good = grepl("1368", file))
    odf$file = NULL
    odf = odf %>% 
      mutate_at(.vars = vars(time, value), as.numeric)
    odf = odf %>% 
      arrange(time, measure, sub_good, value)
    odf = odf %>% 
      mutate(time_of_day = biobankr::min_to_time(time),
             nobad = drop_bad)
    tz(odf$time_of_day) = "UTC"    
    odf = as_data_frame(odf)
    write_rds(odf, outfile,
              compress = "xz")
  } else { 
    odf = read_rds(outfile)
  }
  
  
  x = sum_files[1]
  
  L = list(
    summary = sum_files,
    overall = ofiles
  )
  type = "summary"
  for (type in names(L)) {
    run_files = L[[type]]
    fname = paste0(app, "long", 
                   ifelse(type != "summary", type, ""),
                   "_full_data.rds")
    outfile = here(fname)
    if (!file.exists(outfile) | rerun) {
      ss = map(run_files, function(x) {
        print(x)
        res = read_rds(x)
        stopifnot("n" %in% colnames(res))
        res = gather(res, measure, value,
                     q0,q25,q50,q75,q100,sd,mean,min,max,n)
        # res = res %>% 
        #   filter(!measure %in% c("q0", "q100"))      
        # res = gather(res,
        #              variable, cat,
        #              -day, -time, -measure, -value)
        cn = colnames(res)
        if (grepl("age_accel", x)) {
          cn[grepl("age_cat", cn)] =  "age_accel_cat"
        }
        if (grepl("age_assess", x)) {
          cn[grepl("age_cat", cn)] = "age_assess_cat"
        }
        colnames(res) = cn
        
        res = res %>% 
          mutate(sub_good = grepl("1368", x),
                 nobad = grepl("nobad", x)  
          )
        if (type == "overall") {
          res$day = NA_real_
        }
        res
      })
      uvars = unique(vars)
      uvars = intersect(uvars, names(ss))
      long = vector(mode = "list", length = length(uvars))
      names(long) = uvars
      for (ivar in uvars) {
        xx = ss[ names(ss) == ivar]
        cn = colnames(xx[[1]])
        cn = colnames(xx)
        res = bind_rows(xx)
        res = as_data_frame(res)
        long[[ivar]] = res
      }
      
      
      long = long %>% 
        map(function(x) {
          x %>% 
            mutate_at(.vars = vars(day, time, value), as.numeric)
        })
      
      long = long %>% 
        map(function(x) {
          x %>% 
            arrange(day, time, measure, value)
        })
      
      long = long %>% 
        map(function(x) {
          x = x %>% 
            mutate(time_of_day = biobankr::min_to_time(time))
          tz(x$time_of_day) = "UTC"
          x
        })
      long = long %>% 
        map(function(x) {
          x %>% 
            filter(is.finite(value))
        })
      
      write_rds(long, outfile,
                compress = "xz")
    } else {
      long = read_rds(outfile)
    }
  }
  
}
