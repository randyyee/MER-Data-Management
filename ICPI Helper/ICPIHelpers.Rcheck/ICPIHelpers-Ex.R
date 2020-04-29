pkgname <- "ICPIHelpers"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "ICPIHelpers-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ICPIHelpers')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("convert_new_msd_CHIPS")
### * convert_new_msd_CHIPS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_new_msd_CHIPS
### Title: Converts semi-long MSD to wide format for CHIPS
### Aliases: convert_new_msd_CHIPS

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_new_msd_CHIPS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_new_msd_HITS")
### * convert_new_msd_HITS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_new_msd_HITS
### Title: Converts the semi-long MSD into a wide format with QTRs, APR,
###   and Target for every year
### Aliases: convert_new_msd_HITS

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_new_msd_HITS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_new_msd")
### * read_new_msd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_new_msd
### Title: Read semi-long MSD
### Aliases: read_new_msd

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file, to_lower = TRUE, save_rds = TRUE, remove_txt = FALSE)
{
    if (stringr::str_detect(file, ".txt") == FALSE)
        file <- paste0(file, ".txt")
    df <- data.table::fread(file, sep = "\t", colClasses = "character",
        showProgress = FALSE)
    df <- tibble::as_tibble(df)
    df <- df %>% dplyr::mutate_at(dplyr::vars(dplyr::matches("qtr",
        ignore.case = TRUE)), ~as.double(.)) %>% dplyr::mutate_at(dplyr::vars(dplyr::matches("targets",
        ignore.case = TRUE)), ~as.double(.)) %>% dplyr::mutate_at(dplyr::vars(dplyr::matches("cumulative",
        ignore.case = TRUE)), ~as.double(.))
    df <- df %>% dplyr::mutate_if(is.logical, ~as.character(.)) %>%
        dplyr::mutate_if(is.character, ~ifelse(. == "", NA, .))
    if (to_lower == TRUE)
        df <- dplyr::rename_all(df, ~tolower(.))
    newfile <- stringr::str_replace(file, "txt", "rds")
    if (save_rds == TRUE)
        saveRDS(df, newfile)
    if (remove_txt == TRUE)
        file.remove(file)
    return(df)
  }



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_new_msd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
