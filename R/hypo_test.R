#'hypothesis test
#'@description hypothesis test
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examplesz
#'@export

hypo_test = function(normalized_data, p, f, factor_name = NULL, repeated_factor_name = NULL,
                     confound = NULL){#For repeated study design, samples should match.
  factors = p[factor_name]
  oneway.test(e[,1]~factors[,1]) #welch anova or t.test.
  o = userfriendlyscience::posthocTGH(x = factors[,1], y = e[,1],digits=5) #Also provide with the means and variances of normalized data!
  # when two group it is equivalent to t.test. When multiple group, then it is games howell.

}
