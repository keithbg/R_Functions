#################################################################################
## Function to estimate PAM regression parameters for light curve data         ##
## Uses nonlinear regression ('nls')                                           ##
## Returns list: [[1]] regression parameters and [[2]] regression RSS          ##
## See also 6.4 Light Curves (p. 48-51) in the Junior PAM Operator's guide     ##
##     source: http://www.walz.com/downloads/manuals/junior-pam/jpm_071206.pdf ##
## -------------                                                               ##
## REG1                                                                        ##
##   ETR = ETRmPot*(1 - e^(-1*alpha*PPFD/ETRmPot))*(e^(-1*beta*PPFD/ETRmPot))  ##
##   with ETRm = ETRmPot*(alpha/(alpha+beta))*(beta/(alpha+beta))^(beta/alpha) ##
## REG2                                                                        ##
##   ETR = ETRm*TANH(alpha*PPFD/ETRm)                                          ##
## Ek = ETRm/alpha                                                             ##
## Ib = ETRmPot/beta                                                           ##
## -------------                                                               ##
## 'REG1'   Parameters fit by regression 1 (Platt et al., 1980)                ##
## 'REG2'   Parameters fit by regression 2 (Jassby and Platt, 1976)            ##
##--------------                                                               ##
## Regression parameters:                                                      ##
## 'alpha'  init. slope of rapid light curve (RLC) ~ photosynth. efficiency    ##
## 'ETRm'   Maximum electron transport rate [umol/m2/s of electrons]           ##
## 'Ek'     Minimum saturating irradiance [umol/m2/s of photons]               ##
## (Note: Ek = ETRm/alpha)                                                     ##
## REG1 also returns two additonal parameters: 'beta' and 'ETRmPot'            ##
## with ETRmPot/beta representing the 'photoinhibition index'                  ##
## (i.e., PAR needed to photoinhibit ETRmPot by the factor of 1/e)             ##
#################################################################################

get.Pmax <- function(dat) {

   #regression 1
   reg1 <- try(nls(ETR ~ ETRmPot*(1 - exp(-1*alpha*PAR/ETRmPot))*exp(-1*beta1*PAR/ETRmPot),data=dat,
      start=list(ETRmPot=40,alpha=0.2,beta1=0.001)),silent=TRUE)
   if (class(reg1)=="nls") {
      REG1.alpha <- coef(reg1)["alpha"]
      REG1.beta <- coef(reg1)["beta1"]
      REG1.ETRmPot <- coef(reg1)["ETRmPot"]
      REG1.ETRm <- REG1.ETRmPot*(REG1.alpha/(REG1.alpha+REG1.beta))*(REG1.beta/(REG1.alpha+
         REG1.beta))^(REG1.beta/REG1.alpha)
      REG1.Ek <- REG1.ETRm/REG1.alpha
      REG1.RSS <- reg1$m$deviance()
   } else {
      REG1.alpha <- REG1.beta <- REG1.ETRmPot <- REG1.ETRm <- REG1.Ek <- REG1.RSS <- NA
   }
   
   #regression 2
   reg2 <- try(nls(ETR ~ ETRm*tanh(alpha*PAR/ETRm),data=dat,start=list(ETRm=40,alpha=0.2)),silent=TRUE)
   if (class(reg2)=="nls") {
      REG2.alpha <- coef(reg2)["alpha"]
      REG2.ETRm <- coef(reg2)["ETRm"]
      REG2.Ek <- REG2.ETRm/REG2.alpha
      REG2.RSS <- reg2$m$deviance()
   } else {
      REG2.alpha <- REG2.ETRm <- REG2.Ek <- REG2.RSS <- NA
   }
   
   out <- c(REG1.alpha,REG1.ETRm,REG1.Ek,REG1.beta,REG1.ETRmPot,REG2.alpha,REG2.ETRm,REG2.Ek)
   names(out) <- c("REG1.alpha","REG1.ETRm","REG1.Ek","REG1.beta","REG1.ETRmPot","REG2.alpha","REG2.ETRm","REG2.Ek")
      
   return(list("Params"=out,"RSS"=c("REG1"=REG1.RSS,"REG2"=REG2.RSS)))
}