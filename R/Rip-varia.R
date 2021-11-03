##
## IPv4
##
## 

##
##
## setOldClass(Classes, prototype, where, test = FALSE, S4Class)
## .OldClassesList
## 
## extends
## selectSuperClasses
##
# selectSuperClasses('IPv4')
# [1] "vector"
## 
## https://stackoverflow.com/questions/12100856/combining-s4-and-s3-methods-in-a-single-function
##
# setClass("A")                    # define a class

# f3 <- function(x, ...)           # S3 generic, for S3 dispatch    
#     UseMethod("f3")
# setGeneric("f3")                 # S4 generic, for S4 dispatch, default is S3 generic
# f3.A <- function(x, ...) {}      # S3 method for S4 class
# setMethod("f3", "A", f3.A)       # S4 method for S4 class
## 
## 
## 
## NextMethod :
## Math functions operate on the rounded numbers, return a plain
## vector.  The next method will always be the default, usually a primitive.
# setMethod("Math", "rnum",function(x) callNextMethod(round(as.numeric(x), x@d)))
## 
## 
## valueClass
##

## 
##________________________________________________________________________________________________________________________

## cf. sort -> order (en fait pbm de def de xtfrm)
## 
# getMethod('is.integer')
# Error in getMethod("is.integer") : aucune fonction générique n'est trouvée pour 'is.integer'
## 
## Error in .setupMethodsTables(fdef, initialize = TRUE) : tentative d'obtenir le slot "group" d'un objet d'une classe élémentaire ("NULL") sans slots
##
# setGeneric("is.integer") 
## ‘is.integer’ dispatches internally;  methods can be defined, but the generic function is implicit, and cannot be changed.
## 
## setOldClass("is.integer")
## 
## 
## 
# setMethod(
#   "is.integer"
#   ## 
#   , signature(x = "IPv4")
#   , function(x) FALSE
#   ##, where = topenv(parent.frame())
# )
## 
## marche pour IPV4 mais nécessite de redéfinir TOUTES les classes
## 
# is.integer <- function(x, ...)           # S3 generic, for S3 dispatch    
#     UseMethod("is.integer")
# ##
# is.integer.IPv4 <- function(x) FALSE


## 
if(F) setMethod(
  "["
  ## 
  , signature(x = "IPv4", i = "numeric")
  ##
  , function(x, i, ...) {  
    ##
    idx <- as.integer(i)
    ##
    ## idx < 0
    ##
    if( (max.idx <- max(idx,na.rm=T))>length(x) ) stop("index out of bounds ", max.idx)
    ##
    ip <- new('IPv4')
    ##
    ip@.Data <- rep(NA_integer_, length(idx))
    ##
    ipv4.idx <- x@.Data[idx] +1L
    ##
    ##print(ipv4.idx)
    ##
    ##
    ##
    if(
      !all( is.na( ipv4.idx ) )
    ){     
      ##
      if( is.null( x@ipv4 ) ) stop( "NULL ipr")
      ##
      if( is.null( x@length ) ) stop( "NULL length")
      ##
      ##ip@ipr <- .Call("Ripaddr_IPv4r_subset0", x, ipv4r.idx[which( !is.na(ipv4r.idx) ) ] )
      ##
      ip@ipv4 <- x@ipv4[ ipv4.idx[which( !is.na(ipv4.idx) ) ]  ]
      ##
      ip.len <- length(ip@ipv4)
      ##
      ip@.Data[ which( !is.na( ipv4.idx ) ) ] <- ( 1:ip.len ) - 1L
      ##
      ip@length <- ip.len
    }
    ##
    ip
  }
)
##
if(F)setMethod(
  "[<-"
  , "IPv4"
  , function (x, i, j, ..., value){
    ##
    if( 
      ( max1<-max(i,na.rm=T) )>( max2<-max(x@.Data,na.rm=T) )
    ) stop("index out-of-bounds: ", max1, max2)
    ##
    if( class(value)!='IPv4' ) value <- new('IPv4', as.character(value))
#     show(value)
#     print(value@.Data)
#     print(i)
    ##if(length(i)!=length(value)) error('length mismatch')
#     print(x@ipv4)
#     print(x@ipv4[x@.Data+1])
    ##
    ## xpd
    ipv4    <- x@ipv4[x@.Data+1]
    ipv4[i] <- value@ipv4[value@.Data+1]
    ##
    ##x@ipv4  <- ipv4[!is.na(ipv4)]
    ##
    x@.Data[i] <- value@.Data
    ## re-idx
    nna          <- !is.na(x@.Data)
    ##
#     print(idx[nna])
#     print( ipv4[idx[nna]] )
    ##
    x@ipv4       <- ipv4[ nna ]
    ##
    idx          <- cumsum(nna) - 1L
    ##idx          <- idx - 1L
    x@.Data[nna] <- idx[nna]
    ##
    x@length     <- length(x@ipv4)
    ##
    x
  }
)

## union: IP,IPv4r*,IPv6r*
# setMethod(
#   "rbind2"
#   , signature(x = "IPv4r", y="ANY")
#   , function(x, y, ...){
#     ##print('any')
#     ##
#     if( missing(y) ) return(x)
#     ##
#     callGeneric(x, y)
#   }
# )

##
## IPv6
##

##
setMethod(
  "xtfrm"
  ## 
  , "IPv6"
  , function(x){
    ##
    idx <- .Call(
      "Rip_ipv6_qsort0" ## "Rip_ipv6_qsort_1"
      , x[ (nna <- !is.na(x)) ]
      , FALSE ## decreasing
    )+1L
    ##
    res           <- integer(length(x))
    res[nna][idx] <- seq.int(sum(nna))
    res[!nna]     <- NA
    res
    ##
    ##  -- ??? NaN ??? --
    ##
#     xpd <- x@.Data+1L
#     idx <- order(x@ipv6[xpd,2],x@ipv6[xpd,1], decreasing=F)
    ##
#     rv <- rep( NA_integer_, length(x)) ##  integer(length(x)) ## 
#     rv[idx] <- seq.int(length(x))## 
#     ## rv[is.na(x)] <- NA
#     rv
    ##
  }
)
##
# setMethod(
#   "xtfrm"
#   ## 
#   , "IPv6"
#   , function(x){
#     ##
#      .Call(
#         "Rip_ipv6_qsort_1"
#         , x
#       )+1L
#   }
# )
##
## 
## 
##
# setMethod(
#   "xtfrm"
#   ## 
#   , "IPv6"
#   , function(x){
#     ##order(
#     ##.Call( 'Rip_ipv6_cvtfl64_0', x)
#     ##
#     x.num <- .Call( 'Rip_ipv6_cvtxprecfl64_0', x)
#     ##order( x.num[,1], x.num[,2])
#     ##do.call('order', list( x.num[,1], x.num[,2]) )
#     ##
#      match( 
#       1:length(x)
#       , order( x.num[,1], x.num[,2])
#     )
#     ##)
#   }
# )

setMethod(
  "xtfrm"
  ## 
  , "IPv6r"
  , function(x){
    ##
     ##order(
     ##.Call( 'Rip_ipv6_cvtfl64_0', x)
     ##
     ip6 <- ipv6(x)
     ##
     ip6.lo <- .Call( 'Rip_ipv6_cvtxprecfl64_0', ip6[[1]])
     ##
     ip6.hi <- .Call( 'Rip_ipv6_cvtxprecfl64_0', ip6[[2]])
     ##
#      cbind(ip6.lo, ip6.hi )
     ##
     (
       ##order( ip6.lo[,1], ip6.lo[,2], ip6.hi[,1], ip6.hi[,2]) 
       ##
       match( 
        1:length(x)
        , order( ip6.lo[,1], ip6.lo[,2], ip6.hi[,1], ip6.hi[,2]) 
      )
     )
     ##)
  }
)

## https://github.com/hrbrmstr/punycode/blob/master/tools/winlibs.R
# Build against static libraries compiled with the Rtools
# if(!file.exists("../windows/libidn-1.30/include/idna.h")){
#   setInternet2()
#   download.file("https://github.com/rwinlib/libidn/archive/v1.30.zip", "lib.zip", quiet = TRUE)
#   dir.create("../windows", showWarnings = FALSE)
#   unzip("lib.zip", exdir = "../windows")
#   unlink("lib.zip")
# }
