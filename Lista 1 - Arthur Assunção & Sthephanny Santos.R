# ==================== INFORMAÇÕES:
# Aluno: Arthur Alves Assunção – 12211BCC054
# Aluna: Sthephanny Caroline da Silva Santos – 12211BCC044
#
# LISTA E DADOS ANEXADOS JUNTO AO SCRIPT
#

# ==================== EX 01

# (a)
vetorEx1.A <- c()

for(i in 1:21){
  vetorEx1.A[i] = i+9
}

print(vetorEx1.A)

# (b) 
vetorEx1.B <- c()

for(i in 1:21){
  vetorEx1.B[i] = 31-i 
}

print(vetorEx1.B)

# (c)
vetorEx1.C <- c(vetorEx1.A, vetorEx1.B)

print(vetorEx1.C)



# ==================== EX 02
# Use a função help do R para descobrir o funcionamento das funções rep e seq. Em seguida,  utilize estas funções para resolver os seguintes itens:
?rep
?seq

# (a)
vetorDoisA <- rep(c(2, 4, 6, 8), times = 10)
print(vetorDoisA)

# (b)
vetorDoisB <- c(rep(c(2, 4, 6, 8), times = 10), 2) 
print(vetorDoisB)



# ==================== EX 03

# (a)

vetorEx3.A <- c()

for(n in 1:11){
  vetorEx3.A[n] <- (((n + 19)^2) + (4 * (n + 19)))
}

print(vetorEx3.A)

# (b)

vetorEx3.B <- c()

for(n in 1:11){
  vetorEx3.B[n] <- ((3^(n + 9))/(n + 9) + (2^(n + 9))/(n + 9)^2)
}

print(vetorEx3.B)



# ==================== EX 04
set.seed(2009) 
sorteios <- sample(1:100, 40, replace = TRUE)

# (a)
pares <- sorteios[sorteios %% 2 == 0]
quantidadePares <- length(pares)
quantidadePares

# (b) 
maiores_70 <- sorteios[sorteios > 70]
quantidadeMaiores_70 <- length(maiores_70)
quantidadeMaiores_70

# (c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?
?which
posicoesImpares <- which(sorteios %% 2 != 0)
posicoesImpares



# ==================== EX 05
acertos <- 0
contador <- 0

simula_dado <- function(){
  
  while(acertos != 2){
    dadoEx5 <- sample(x = 1:6, size = 1)
    contador <- contador + 1
    
    if(dadoEx5 == 4){
      acertos <- acertos + 1
    }
  }
  return(contador)
}

paste("Foram necessárias:", resultado <- simula_dado(),"jogadas")



# ==================== EX 06
simula_dado <- function(){
  contador <- 0
  acertos <- 0
  
  while(acertos != 2){
    dadoEx5 <- sample(1:6, 1)
    contador <- contador + 1
    
    if(dadoEx5 == 4){
      acertos <- acertos + 1
    }
  }
  return(contador)
}

replicacoes <- 10000
quantidades <- numeric(replicacoes)

for(i in 1:replicacoes){
  quantidades[i] <- simula_dado()
}

mediaQuantidades <- mean(quantidades)
paste("A média de jogadas necessárias é:", mediaQuantidades)



# ==================== EX 07
fibonacci <- function(n){
  
  vetorFiboEx7 <- c()
  
  if(n == 0){
    return(    vetorFiboEx7 <- c(0))
  }
  if(n == 1){
    return(vetorFiboEx7 <- c(1))
  }
  if(n == 2){
    return(vetorFiboEx7 <- c(1,1))
  }
  else{
    vetorFiboEx7[1] = 1
    vetorFiboEx7[2] = 1
    
    for(i in 3:n){
      vetorFiboEx7[i] <- vetorFiboEx7[i - 1] + vetorFiboEx7[i - 2] 
    }
    return(vetorFiboEx7)
  }
}

print(fibonacci(10))



# ==================== EX 08
sorteioAmigoOculto <- function() {
  participantes <- c("Michael", "Dwight", "Jim", "Kevin", "Creed")
  sorteados <- sample(participantes, length(participantes))
  
  if (any(participantes == sorteados)) {
    return(0) 
  } else {
    return(1)
  }
}

replicacoes <- 100000
resultados <- numeric(replicacoes)

for (i in 1:replicacoes) {
  resultados[i] <- sorteioAmigoOculto()
}

proporcaoErrados <- mean(resultados == 0)
paste("A proporção de sorteios que deram errado é:", proporcaoErrados)



# ==================== EX 09
resultadoEx9 <- c()

for(i in 1:100000){
  desempateEx9 <- 0
  
  somaInicialEx9 <- sum(sample(x = 1:6, size = 2, replace = TRUE))
  
  if(somaInicialEx9 == 7 || somaInicialEx9 == 11){
    resultadoEx9[i] <- 1
  }else if(somaInicialEx9 == 2 || somaInicialEx9 == 3 || somaInicialEx9 == 12){
    resultadoEx9[i] <- 0
  }else{
    while((desempateEx9 != 7) && (desempateEx9 != somaInicialEx9)){
      desempateEx9 <- sum(sample(x = 1:6, size = 2, replace = TRUE))
      
      if(desempateEx9 == 7){
        resultadoEx9[i] <- 0
      }
      if(desempateEx9 == somaInicialEx9){
        resultadoEx9[i] <- 1
      }
    }
  }
}

mean(resultadoEx9 == 1)



# ==================== EX 10

# (a)
passeioLuke <- function(L, N = 20) {
  while(L > 0 && L < N) {
    moeda <- sample(c(-1, 1), 1, replace = TRUE)
    L <- L + moeda
  }
  
  if (L == N) { 
    return(1) 
  } else { 
    return(0) 
  }
}

# (b)
replicaPasseio <- function(L, N = 20, replicacoes = 10000) {
  resultados <- numeric(replicacoes)
  for (i in 1:replicacoes) {
    resultados[i] <- passeioLuke(L, N)
  }
  
  proporcaoCasa <- mean(resultados)
  return(proporcaoCasa)
}

# (c)
?sapply
valoresL <- 1:19
proporcoes <- sapply(valoresL, function(L) replicaPasseio(L))

dadosGrafico <- data.frame(L = valoresL, Proporcao = proporcoes)

# meu pc QUASE MORRE rodando isso:
library(ggplot2)
ggplot(dadosGrafico, aes(x = L, y = Proporcao)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Proporção de Luke chegando à casa",
       x = "Valor de L",
       y = "Proporção de sucesso") +
  theme_minimal()



# ==================== EX 11
# (a)

dadoEx11 <- c("R", "L", "D", "U")

x <- 0
y <- 0

Link <- c(x, y)

for(i in 1:8){
  resultadoEx11 <- sample(x = dadoEx11, size = 1, replace = TRUE)
  
  if(resultadoEx11 == "R"){
    Link <- c(x <- x + 1, y <- y)
  }else if(resultadoEx11 == "L"){
    Link <- c(x <- x - 1, y <- y)
  }else if(resultadoEx11 == "U"){
    Link <- c(x <- x, y <- y + 1)
  }else if(resultadoEx11 == "D"){
    Link <- c(x <- x, y <- y - 1)
  }
}

print(Link)

# (b)

dadoEx11 <- c("R", "L", "D", "U")

caminhoLinkX <- c()
caminhoLinkY <- c()

LinkEstaNaOrigem <- 0

for(j in 1:10000){
  x <- 0
  y <- 0
  
  Link <- c(x, y)
  
  for(i in 1:8){
    resultadoEx11 <- sample(x = dadoEx11, size = 1, replace = TRUE)
    
    if(resultadoEx11 == "R"){
      Link <- c(x <- x + 1, y <- y)
    }else if(resultadoEx11 == "L"){
      Link <- c(x <- x - 1, y <- y)
    }else if(resultadoEx11 == "U"){
      Link <- c(x <- x, y <- y + 1)
    }else if(resultadoEx11 == "D"){
      Link <- c(x <- x, y <- y - 1)
    }
  }
  
  caminhoLinkX[j] <- x #histórico de X
  caminhoLinkY[j] <- y #histórico de Y
  
  if(x == 0 && y == 0){
    LinkEstaNaOrigem <- LinkEstaNaOrigem + 1
  }
  
} 

print(LinkEstaNaOrigem/j) #Essa probabilidade representa a porcentagem de chance que o Link tem de voltar a origem (cerca de 7%)

# (c)

linkPlano <- function(n){
  if(n %% 2 != 0){
    return(paste("O número", n, "é um número ímpar e devido a isso, é impossível que o Link retorne a sua origem pois a quantidade de vezes em que Link se movimenta não o permite alcançar a origem novamente!"))
  }
  else{
    dadoEx11 <- c("R", "L", "D", "U")
    
    caminhoLinkX <- c()
    caminhoLinkY <- c()
    
    LinkEstaNaOrigem <- 0
    
    for(j in 1:10000){
      x <- 0
      y <- 0
      
      Link <- c(x, y)
      
      for(i in 1:n){
        resultadoEx11 <- sample(x = dadoEx11, size = 1, replace = TRUE)
        
        if(resultadoEx11 == "R"){
          Link <- c(x <- x + 1, y <- y)
        }else if(resultadoEx11 == "L"){
          Link <- c(x <- x - 1, y <- y)
        }else if(resultadoEx11 == "U"){
          Link <- c(x <- x, y <- y + 1)
        }else if(resultadoEx11 == "D"){
          Link <- c(x <- x, y <- y - 1)
        }
      }
      
      caminhoLinkX[j] <- x #histórico de X
      caminhoLinkY[j] <- y #histórico de Y
      
      if(x == 0 && y == 0){
        LinkEstaNaOrigem <- LinkEstaNaOrigem + 1
      }
    }
    return(LinkEstaNaOrigem/j)
  }
}

linkPlano(6)



# ==================== EX 12

simulaPartida <- function() {
  stevenSeq <- c(0, 1, 0)
  garnitSeq <- c(0, 0, 1)
  
  lancamentos <- numeric(0)
  
  repeat {
    lancamentos <- c(lancamentos, sample(c(0, 1), 1))
    
    if (length(lancamentos) >= 3) {
      ultimosTres <- tail(lancamentos, 3)
      
      if (all(ultimosTres == stevenSeq)) {
        return("steven")
      }
      
      if (all(ultimosTres == garnitSeq)) {
        return("garnit")
      }
    }
  }
}

replicaExperimento <- function(replicacoes = 10000) {
  resultados <- replicate(replicacoes, simulaPartida())
  
  proporcaoGarnit <- mean(resultados == "garnit")
  return(proporcaoGarnit)
}

mediaVitoriasGarnit <- replicaExperimento()
paste("A média de vitórias de Garnit é:", mediaVitoriasGarnit)


# ==================== EX 13
library(ggplot2)

dadosEx13 <- read.table(file = "dados.txt", header = TRUE, sep = ";")
str(dadosEx13)

# (a)
summary(dadosEx13)

ggplot(data = dadosEx13, aes(x = Genero)) + geom_bar(color = "black", fill = "darkred") + labs(y = "Contagem de Mortos", x = "Sexo") + theme_dark()

# Através do gráfico, percebemos que Shipman tinha preferência por assassinar mulheres

# (b)

ggplot(data = dadosEx13, aes(x = Idade)) + geom_histogram(color = "black", fill = "darkred", bins = 8) + labs(y = "Contagem de Mortos") + theme_dark()

# Através do gráfico, notamos que Shipman tinha preferência por matar pessoas com uma idade mais avançada

homem = dadosEx13[dadosEx13$Genero == "Men", ]
mulher = dadosEx13[dadosEx13$Genero == "Women", ]

ggplot(data = homem, aes(x = Idade)) + geom_histogram(color = "black", fill = "darkred", bins = 40) + labs(y = "Contagem de Mortos") + theme_dark()

ggplot(data = mulher, aes(x = Idade)) + geom_histogram(color = "black", fill = "darkred", bins = 30) + labs(y = "Contagem de Mortos") + theme_dark()

# Os bins dos histogramas dos data.frame de "homem" e "mulher" foram aumentados para melhor entendimento do gráfico

#No histograma dos homens assassinados por Shipman notamos que, além de matar poucos homens, Shipman tinha preferência por matar homens acima dos 70 anos. Percebemos também, que não há nenhuma vítima do sexo masculino com idade entre 50 e 60.

#Já no histograma das mulheres, além de uma quantidade maior em relação aos homens vimos que existe apenas um pequena lacuna entre 60 e 65 anos, tendo Shipman feito vítimas do sexo feminino dos 50 até os 90 anos. Notamos também, que ele possuia uma preferência por mulheres com uma idade mais avançado, sendo que o gráfico tem um aumento muito grande de valores próximo aos 70 anos.

# (c)

ggplot(data = dadosEx13, aes(x = Idade)) + geom_boxplot(color = "black", fill = "darkred")

#Observamos através do histograma que a mediana das idades das vítimas de Shipman era em torno dos 77 anos, além da alta quantidade de mortos com idaade entre 72 a 83 anos, apesar de haver algumas vítimas com idades inferiores, nos informando que a vítima com menor idade possuia 41 anos.

# (d)

ggplot(data = dadosEx13, aes(x = LocalDaMorte, fill = Genero)) + geom_bar() + scale_fill_manual(values = c("Men" = "#4169E1", "Women" = "#FF69B4"), labels = c("Men" = "Homem", "Women" = "Mulher")) + scale_x_discrete(labels = c("Nursing home" = "Casa de Repouso", "Own home" = "Casa do Paciente")) + labs(y = "Contagem de Vítimas", x = "Local da Morte", fill = "Gênero") + theme_dark()

#Observamos através do gráfico de barras que a grande maioria das vítimas de Shipman morreram em sua própria casa.

# (e)

ggplot(data = dadosEx13, aes(x = AnoDaMorte, fill = Genero)) + geom_bar(colour = "black") + scale_fill_manual(values = c("Men" = "#4169E1", "Women" = "#FF69B4"), labels = c("Men" = "Homem", "Women" = "Mulher")) + labs(y = "Contagem de Vítimas", x = "Ano da Morte", fill = "Gênero") + theme_dark()

#Analisando o gráfico de barras, notamos que foram poucos os anos em que ele não matou homens, além disso, notamos também que houve uma maior quantidade assassinatos na década de 90. Percebemos também que há anos em que não houveram vítimas feitas por Shipman.

# (f)

paste("Ao analisarmos todos os dados obtidos, podemos concluir que Shipman tinha preferência por matar mulheres acima dos 70 anos em suas próprias casas. No total foram", length(mulher$Idade), "assassinadas. Conforme os anos de sua primeira vítima feita, ele passou a matar cada vez mais, alcançando o pico de assassinatos por volta do ano de 1997")

# ==================== EX 14

# (a)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)

dataPrimata <- read.table("primatas.txt",header = TRUE, sep = ':')
str(dataPrimata)
summary(dataPrimata)

dataPrimata$especie <- as.factor(dataPrimata$especie)
dataPrimata$genero <- as.factor(dataPrimata$genero)
str(dataPrimata)


# (b)
# Gráfico de barras para contar espécies
ggplot(data = dataPrimata, aes(x = especie)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Contagem de Espécies", x = "Espécie", y = "Frequência") +
  theme_light()

# Gráfico de barras para contar gênero por espécie
ggplot(data = dataPrimata, aes(x = especie, fill = genero)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Frequência de Machos e Fêmeas por Espécie", x = "Espécie", y = "Frequência") +
  theme_light()


# (c)
# Filtrar apenas bonobos
bonobos <- dataPrimata[dataPrimata$especie == "bonobo", ]

ggplot(bonobos, aes(x = genero, y = altura, fill = genero)) + 
  geom_boxplot() + 
  labs(title = "Comparação de Tamanho entre Machos e Fêmeas - Bonobos", x = "Gênero", y = "Tamanho (cm)") +
  theme_light()


# Filtrar apenas chimpanzés
chimpanzes <- dataPrimata[dataPrimata$especie == "chimpanze", ]

ggplot(chimpanzes, aes(x = genero, y = altura, fill = genero)) + 
  geom_boxplot() + 
  labs(title = "Comparação de Tamanho entre Machos e Fêmeas - Chimpanzés", x = "Gênero", y = "Tamanho (cm)") +
  theme_light()


# (d)
# Filtrar apenas fêmeas
femeas <- dataPrimata[dataPrimata$genero == "femea", ]

# Gráfico comparativo entre fêmeas das duas espécies
ggplot(femeas, aes(x = especie, y = altura, fill = especie)) + 
  geom_boxplot() + 
  labs(title = "Comparação de Tamanho entre Fêmeas - Bonobos e Chimpanzés", x = "Espécie", y = "Tamanho (cm)") +
  theme_light()

# Filtrar apenas machos
machos <- dataPrimata[dataPrimata$genero == "macho", ]

# Gráfico comparativo entre machos das duas espécies
ggplot(machos, aes(x = especie, y = altura, fill = especie)) + 
  geom_boxplot() + 
  labs(title = "Comparação de Tamanho entre Machos - Bonobos e Chimpanzés", x = "Espécie", y = "Tamanho (cm)") +
  theme_light()


# (e)
# R: Ao gerarmos todos os 6 gráficos, dá pra perceber que entre os primatas machos, os chimpanzés são basicamente bem mais pesados e atingem alturas maiores do que os bonobos. Já entre as fêmeas, as bonobos são mais altas, só que acabam sendo menos pesadas quando comparadas com as fêmeas dos chimpanzés. Então, em termos de espécies, os bonobos de modo geral são mais baixos e leves que os chimpanzés machos, mas têm um peso bem parecido com as fêmeas de chimpanzés, embora elas sejam, menos em estatura.


# (f) A partir das variáveis tamanho, peso e genero, construa um modelo de árvore de decisão utilizando estruturas condicionais que seja capaz de prever a espécie de uma observação. Calcule a acurácia do modelo.

n <- round(0.8 * nrow(dataPrimata))

indicesTreino <- sample(1:nrow(dataPrimata), size = n, replace = FALSE)

treino <- dataPrimata[indicesTreino,]
teste <- dataPrimata[-indicesTreino,]

modeloArvore <- rpart(formula = especie ~ . , data = treino, method = "class")

rpart.plot(modeloArvore, extra = 101)

previsao <- predict(modeloArvore, newdata = teste, method = "class")

previsao[previsao[,1] > previsao[,2]] <- "bonobo"
previsao[previsao[,1] < previsao[,2]] <- "chimpanze"

mean(previsao == teste$especie)