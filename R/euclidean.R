#' euclidean algorithm
#' @references wiki page link <https://en.wikipedia.org/wiki/Euclidean> algorithm
#' @description euclidean algorithm is to caculate the greatest common divisor of two integers
#' @param a A numeric number.
#' @param b A numeric number.
#' @returns A numeric number.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

euclidean <-
function(a,b){
        stopifnot(is.numeric(a)||is.numeric(b)||(a||b)!=0)
        r<-c(a,b)
        m<-max(r)
        n<-min(r)
        
        while (n!=0) {
            temp<-n
            n<-m%%n
            m<-temp
            
        }
        
        return(m)
    }
