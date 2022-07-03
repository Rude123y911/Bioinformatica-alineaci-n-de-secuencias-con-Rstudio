library(stringr)

aligment = alignment_score("CAT","CT", 1, 1)
sprintf("Puntuación alcanzada: %d", aligment$score)
print_alignment(aligment$list, aligment$m, aligment$n, aligment$x, aligment$y, aligment$pxy, aligment$pgap)

alignment_score = function(x, y, pxy, pgap){
  # x es un vector de la primera cadena de caracteres
  # y es la segunda cadena de caracteres
  # pxy penalización por no coincidir con los caracteres de x e y. En este caso puede ser
  # asignado, pero se determina como 1 por defecto
  # pgap es la penalización en el caso de que haya un un espacio (-) en la alineación
  
  # inicializando variables
  i = 0
  j = 0
  
  # Separando los strings por caracteres
  x = (as.vector(str_split_fixed(x, pattern = "", n = nchar(x))))
  y = (as.vector(str_split_fixed(y, pattern = "", n = nchar(y))))
  
  # Tamaño de los strings
  m = length(x)
  n = length(y)

  # Inicializando matriz donde estarán almacenadas las respuestas óptimas
  dp = (matrix(0, m+1, n+1))
 
  dp[1, ] =  pgap * c(0:(n)) # Se toma toda la fila y se llena
  dp[, 1] =  pgap * c(0:(m)) # Se toma toda la columna y se llena
  
  # Calculando el score más alto
  i = 2
  while (i < m+2){
    j = 2
    while (j < n+2){
      if (x[i-1] == y[j-1]){
        dp[i, j] = dp[(i - 1),(j - 1)]
      }else{
        dp[i, j] = min(dp[(i - 1),(j - 1)] + pxy,
                       dp[(i - 1), j] + pgap,
                       dp[i, (j - 1)] + pgap)
      }
      j = j + 1
    }
    i = i + 1
  }
  #cat("La puntuación es", max(length(x), length(y)) - 2*dp[m+1, n+1])
  score = max(length(x), length(y)) - 2*dp[m+1, n+1]
  newList <- list("m" = m, "n" = n, "score" = score, "list" = dp, "x" = x, "y" = y, "pxy" = pxy, "pgap" = pgap)
 return(newList)
}

#Extrayendo el óptimo alineamiento
print_alignment = function(dp, m, n, x, y, pxy, pgap){
  l = m + n
  m
  i = m
  j = n
  
  xpos = l
  ypos = l
  l
  #Los alineamientos óptimos
  xans = replicate(l+1, 0)
  yans = replicate(l+1, 0)
  while (i > 0 | j > 0){
    if (x[i] == y[j]){
      xans[xpos] = binaryToDecimal(x[i])
      yans[ypos] = binaryToDecimal(y[j])
      xpos = xpos -1
      ypos = ypos -1
      i = i - 1
      j = j - 1
    }else if((dp[i, j] + pxy) == (dp[(i+1), (j+1)])){
      xans[xpos] = binaryToDecimal(x[i])
      yans[ypos] = binaryToDecimal(y[j])
      xpos = xpos -1
      ypos = ypos -1
      i = i - 1
      j = j - 1
    }else if((dp[i, (j+1)] + pgap) == dp[(i+1),(j+1)]){
      xans[xpos] = binaryToDecimal(x[i])
      yans[ypos] = binaryToDecimal('-')
      xpos = xpos -1
      ypos = ypos -1
      i = i - 1
    }else if((dp[(i+1),j] + pgap) == dp[(i+1), (j+1)]){
      xans[xpos] = binaryToDecimal('-')
      yans[ypos] = binaryToDecimal(x[j])
      xpos = xpos -1
      ypos = ypos -1
      j = j - 1
    }
  }
  while(xpos > 0){
    if (i > 0){
      i = i - 1
      xans[xpos] = binaryToDecimal(x[i+1])
      xpos = xpos - 1
    }else{
      xans[xpos] = binaryToDecimal('-')
      xpos = xpos - 1
    }
  }
    while(ypos > 0){
      if (j > 0){
        j = j - 1
        yans[ypos] = binaryToDecimal(y[j+1])
        ypos = ypos - 1
      }else{
        yans[ypos] = binaryToDecimal('-')
        ypos = ypos - 1
      }
    }
  id = 1
  i = l
  while (i > 1){
    if ((intToUtf8(yans[i+1]) == '-') & intToUtf8(xans[i+i]) == '-'){
      id = i + 1
      break
    } 
    i = i - 1
  }
  #Imprimiendo la respuesta final
  print("La alineación óptima es ")
  i = id
  x_seq = rep(NA, m)
  while (i < l-1){
   x_seq[i] = intToUtf8(xans[i+2])
    i = i + 1
  }
  print(x_seq)
  
  #Y
  i = id
  y_seq = rep(NA, n)
  while (i < l-1){
    y_seq[i] = intToUtf8(yans[i+2])
    i = i + 1
  }
  print(y_seq)
}


# Function para convertir binario a decimal
binaryToDecimal = function(n)
{
  n = as.integer(pryr::bits(n))
  num = n
  dec_value = 0
  
  # Initializing base value to 1, i.e 2^0
  base = 1
  
  temp = num;
  while (temp > 0) {
    last_digit = temp %% 10
    temp = as.integer(temp / 10)
    
    dec_value =  dec_value + last_digit * base
    
    base = base * 2
  }
  
  return(dec_value)
}


