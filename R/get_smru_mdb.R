##' @title Download .mdb files from the SMRU server
##'
##' @description fetches .mdb files from SMRU server and saves to a \code{dest}-ination directory
##' 

##' @param d a data frame of observations including Argos KF error ellipse info
##'

##'
##' @examples
##' ## fit rw model to one seal with Argos KF data
##' data(ellie)
##' fit <- fit_ssm(ellie, model = "rw", time.step = 24)
##' 
##' ## time series plots of predicted value fits
##' plot(fit, what = "predicted", type = 1)
##'
##' ## fit crw model to both seals, with Argos KF & LS data 
##' data(ellies)
##' fits <- fit_ssm(ellies, model = "crw", time.step = 24)
##'
##' ## track plots of fits for both seals
##' plot(fits, what = "predicted", type = 2)
##'
##' @importFrom dplyr group_by do rowwise ungroup select mutate slice "%>%"
##' @importFrom utils download.file unzip
##'
##' @export
get_smru_mdb <- function(cid, dest = "mdb", usr = "imos", pwd = "imos")
{
  download.file(paste0("http://",user,":",pwd,"@www.smru.st-and.ac.uk/protected/", cid, "/db/", cid, ".zip"), 
                destfile = file.path(dest, paste0(cid, ".zip")),
                method = "libcurl", 
                quiet = FALSE, 
                mode = "w"
  )
  unzip(file.path(dest, paste0(cid, ".zip")), exdir = file.path(dest, "."))  
  system(paste0("rm ", file.path(dest, paste0(cid, ".zip"))))
  system(paste0("cp ", file.path(dest, paste0(cid, ".mdb")), file.path(dest, "prev", paste0(cid, ".mdb"))))
  
}
