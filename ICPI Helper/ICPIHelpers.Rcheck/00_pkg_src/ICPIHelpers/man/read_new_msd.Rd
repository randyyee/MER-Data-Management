\name{read_new_msd}
\alias{read_new_msd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read semi-long MSD
}
\description{
Reads semi-long MSD
}
\usage{
read_new_msd(file, to_lower = TRUE, save_rds = TRUE, remove_txt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{to_lower}{
%%     ~~Describe \code{to_lower} here~~
}
  \item{save_rds}{
%%     ~~Describe \code{save_rds} here~~
}
  \item{remove_txt}{
%%     ~~Describe \code{remove_txt} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Randy Yee
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
    df <- df \%>\% dplyr::mutate_at(dplyr::vars(dplyr::matches("qtr",
        ignore.case = TRUE)), ~as.double(.)) \%>\% dplyr::mutate_at(dplyr::vars(dplyr::matches("targets",
        ignore.case = TRUE)), ~as.double(.)) \%>\% dplyr::mutate_at(dplyr::vars(dplyr::matches("cumulative",
        ignore.case = TRUE)), ~as.double(.))
    df <- df \%>\% dplyr::mutate_if(is.logical, ~as.character(.)) \%>\%
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
