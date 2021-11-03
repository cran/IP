## -----------------------------------------------------------------------------
##
library(IP)
## IPv4
ipv4("192.168.0.0")
## IPv4 range using CIDR notation
ipv4r("192.168.0.0/16")
## same thing using dash notation
ipv4r("192.168.0.0-192.168.255.255")
## same thing using an IPv4 object and an integer giving the number of addresses in the range
ipv4r(ipv4("192.168.0.0"), as.integer(2L^16 -1) )
## or 
ipv4r("192.168.0.0", as.integer(2L^16 -1) )

## ----warning=FALSE------------------------------------------------------------
## IPv6
ipv6("fe80::")
## IPv6 range using CIDR notation
ipv6r("fe80::/10")
## same thing using dash notation
ipv6r("fe80::-febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
## this overflows for ranges greater or equal to 2^54
try(ipv6r("fe80::", 2^118 -1 ))
## use this instead
ipv6r(ipv6("fe80::") , ipv6("fe80::") + (ipv6(1L) %<<% 118L -1L))

## ----warning=FALSE------------------------------------------------------------
## IP
(x <- ip(c("192.168.0.0", "fe80::") ) )
##
ip.version(x)
##
ip(ipv4(c("192.168.0.0", NA)), ipv6(c(NA, "fe80::")) )
##
ip(ipv4("192.168.0.0"), ipv6("fe80::"), append=T) 
## IP range using CIDR notation
ipr(c("192.168.0.0/16","fe80::/10") )
## same thing using dash notation
ipr(c( "192.168.0.0-192.168.255.255", "fe80::-febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff") )

## -----------------------------------------------------------------------------
##
ipv4(1L)
## this is really 4294967295
x<- ipv4(-1L)
##  
x > ipv4(0L)
##
ipv4(NA_integer_)
## same thing for IPv6
ipv6(1L)
##
x<- ipv6(-1L)
##
x > ipv6(0L)
##
ipv6(NA_integer_)

## -----------------------------------------------------------------------------
##
ipv4r("192.168.0.0-192.168.0.9")
##
ipv4r("192.168.0.10-192.168.0.19")

## -----------------------------------------------------------------------------
## 
x <- ipv4r("192.168.0.0/16")
## this also works for IPv6r and IPr objects
lo(x) ## low end
hi(x) ## high end

## -----------------------------------------------------------------------------
##
x <- ip(c("192.168.0.0", "fe80::") )
##
ipv4(x) ## IPv4 part
ipv6(x) ## IPv6 part
## keep the ipv4 or ipv6 part only
ipv4(x, drop=T)
ipv6(x, drop=T)

## -----------------------------------------------------------------------------
x <- ipv4(c(
  router = '192.168.0.0'
  , host1  = '192.168.0.1'
))
x
names(x)

## -----------------------------------------------------------------------------
x[1:2]
x[2:1]

## ----warning=FALSE------------------------------------------------------------
x[3:4] <- c( host2  = '192.168.0.2', host3  = '192.168.0.3' )
x[5] <- c( host4  = -1062731772L)
data.frame(n=names(x), x=x)
x <- c(x, x+5)
names(x)[6:10] <- paste("host", 5:9, sep="")

## ----warning=FALSE------------------------------------------------------------
ip0    <- ip()
ip0[3] <- ipv4(3L)
ip0[5] <- ipv6(5L)
ip0  
## same thing with NA
ip0    <- ip()
ip0[2] <- NA
ip0

## -----------------------------------------------------------------------------
x <- ipv4('192.168.0.0') 
((x + 1L) - ipv4(1L))==x
## mask complement
!ipv4.netmask(8)==ipv4.netmask(24)
## generate high end of the range form a mask
ipv4r(x, x|ipv4.hostmask(16) )==ipv4r('192.168.0.0/16')

## -----------------------------------------------------------------------------
## recycle
ipv4(c(0L,1L)) + ipv4(0:5L)
##
ipv4(c(0L,1L)) < ipv4(0:5L)
## even if dimensions don't match
ipv4(c(0L,1L)) + ipv4(0:6L)

## ----eval=FALSE---------------------------------------------------------------
#  ## note: beware of operators precedence here : `+` > `!`
#  ( !ipv4(0L) )+1L
#  .Machine$integer.max+1L

## -----------------------------------------------------------------------------
ipv4(c(NA,1L) ) == ipv4(1L)

## -----------------------------------------------------------------------------
x <- ipv4('192.168.0.0') + 0:4
x <- x[sample.int(length(x),length(x)*9, replace=T)]
table(x)

## -----------------------------------------------------------------------------
x=ip(c(
    router = '192.168.0.0'
    , host1  = '192.168.0.1'
))
## does not work yet
try(x[3:4] <- c( host2  = '192.168.0.2', host3  = '192.168.0.3' ))
##  we need to convert to IP first
x[3:4] <- ip(c( host2  = '192.168.0.2', host3  = '192.168.0.3' ))
## does not work because we cannot tell the IP version
try(x[5] <- c( host4  = -1062731772L))

