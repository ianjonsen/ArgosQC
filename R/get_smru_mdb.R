##' @title Download .mdb files from the SMRU server
##'
##' @description fetches .mdb files from SMRU server and saves to a \code{dest}-ination directory
##' 
##' @param d a data frame of observations including Argos KF error ellipse info
##'
##'
##' @examples
##'
##' @importFrom dplyr group_by do rowwise ungroup select mutate slice "%>%"
##' @importFrom utils download.file unzip
##'
##' @export

get_smru_mdb <- function(cids, dest = "mdb", usr = "imos", pwd = "imos")
{
  
  fn <- function(cid, dest = dest, usr = usr, pwd = pwd) {
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
  
  cids %>% 
    purrr::walk( ~ getSMRUct(.x, dest = dest, usr = usr, pwd = pwd))
  
}
