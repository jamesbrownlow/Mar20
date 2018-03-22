
bootstrp=function(a,b,number_of_permutations) {
combo=c(a,b)
diff.observed = mean(a) - mean(b)
diff.random=numeric(number_of_permutations)
for (i in 1 : number_of_permutations) {
 
    # Sample from the combined dataset without replacement
    shuffled = sample (combo, length(combo))
     
    a.random = shuffled[1 : length(a)] 
    b.random = shuffled[(length(a) + 1) : length(combo)]
 
    # Null (permuated) difference
    diff.random[i] = mean(a.random) - mean(b.random)
}
pvalue = sum(abs(diff.random) >= abs(diff.observed)) /
              number_of_permutations
rt=sum((diff.random) >= (diff.observed)) / number_of_permutations
lt=sum((diff.random) < (diff.observed)) / number_of_permutations
result<-list(origDiff=diff.observed,
             pval=pvalue,
             diffs=as.numeric(diff.random),
             rtail=rt,ltail=lt)
cat("Difference was ",diff.observed,"\n",
    "probability of observing = ",pvalue,"\n")
return(result)
}

 # now let's see how it works

# 
# a=rnorm(80,mean=4,sd=2)
# b=rnorm(96,mean=3,sd=3)
# 
# a=runif(2000, 1,4)
# b=runif(1500, 2, 3.5)
# 
# x=bootstrp(a,b,1000)
# 
# #to extract from a list you need to use double square brackes [[ ]]
# 
# meandiff=x[[1]]
# pvalu=x[[2]]
# x$pval
# x$rtail
# rand.diff=x[[3]]
# rtpval=x[[4]]
# ltpval=x[[5]]
# meandif=paste("mean difference = ",round(meandiff,digits=3))
# myp=paste("two sided pvalue = ",pvalu)
# hist(rand.diff,main=meandif,xlab=myp,breaks=20,prob=T,las=T)
# lines(density(rand.diff),col="green",lwd=4)
# abline(v=meandiff,col="red")
# t.test(a,b,var.equal=T)
# 
# #Save the bootstrp function to you working directory so you can use it
# #as an alternative method to check differences of means

