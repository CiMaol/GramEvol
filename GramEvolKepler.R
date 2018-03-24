library(gramEvol)

ruleDef<- list (expr = grule (op(expr, expr), func(expr), var),
                func = grule(sin, cos, log, sqrt),
                op   = grule('+','-','*'),
                var  = grule(distance,distance^n, n),
                n    = grule(1,2,3,4))

grammarDef<-CreateGrammar(ruleDef)

planets<-c("Venus","Earth","Mars","Jupiter","Saturn","Uranus")
distance<-c(0.72,1.00,1.52,5.20,9.53,19.10)
period<-c(0.61,1.00,1.84,11.90,29.40,83.50)

SymRegFitFunc<-function(expr) {
  result<- eval(expr)
  
  if(any(is.nan(result)))
    return(Inf)
  
  return (mean(log(1 + abs(period - result))))
}

ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc,
                           terminationCost = 0.21,
                           monitorFunc = print)