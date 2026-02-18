# Blok A - zadanie 3
kostka = function(n){
  wynik = sample(c(1,2,3,4,5,6), n, replace = TRUE)
  return(wynik)
}

print("Rozkład dla małej próby - 10 rzutów:")
print(table(kostka(10)))

print("Rozkład dla średniej próby - 100 rzutów:")
print(table(kostka(100)))

print("Rozkład dla dużej próby - 10000 rzutów:")
print(table(kostka(10000)))
