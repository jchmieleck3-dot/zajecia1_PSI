# Blok B - zadanie 6
ocena_kredytowa = function(dochod, zadluzenie){
  if(zadluzenie < 0.3 * dochod){
    komunikat = "KREDYT PRZYZNANY"
  } else if(zadluzenie <= 0.5 * dochod) {
    komunikat = "WYMAGA WERYFIKACJI"
  } else {
    komunikat = "KREDYT ODRZUCONY"}
  return(komunikat)
  }

#Sprawdzenie
cat("Dochód 10000, zadłużenie 2000: ", ocena_kredytowa(10000, 2000), "\n")
cat("Dochód 10000, zadłużenie 4000: ", ocena_kredytowa(10000, 4000), "\n")
cat("Dochód 10000, zadłużenie 6000: ", ocena_kredytowa(10000, 6000), "\n")

