#Todas

#install.packages("rattle")
#install.packages("twitteR")
install.packages("cluster")
install.packages("fpc")
library(cluster)
#d1 <- read.csv("C:/Users/rayan/Documents/datasciencewithR/clusters/csv/bd-dec17-age-specific-birth-rates.csv", header = TRUE, sep = ",")
#d$country = as.numeric(as.factor(d$country))
library(readxl)


##################################### <<< 2010 >>> ####################################################
dados2010 <- read_excel ("C:/Users/JSILVA/Documents/2010.xls")
View(dados2010)

dados2010 <- dados2010[which(dados2010$'Área de Enquadramento' == 'MEDICINA'), ]

View(dados2013)

dados2013$Ano <- NULL
dados2013$Cód.Área <- NULL
dados2013$`Área de Enquadramento` <- NULL
dados2013$Cód.IES <- NULL
dados2013$`Nome da IES` <- NULL
dados2013$`Sigla da IES` <- NULL
dados2013$`Org. Acadêmica` <- NULL
dados2013$`Cód. Município` <- NULL
dados2013$`Município do Curso` <- NULL
dados2013$`UF do Curso` <- NULL
dados2013$`Nr. Cursos da Unidade` <- NULL
dados2013$`Cód. Cursos da Unidade` <- NULL
dados2013$`Conluintes Inscritos` <- NULL
dados2013$`Concluintes Participantes` <- NULL
dados2013$`Nota Bruta - FG` <- NULL
dados2013$`Nota Padronizada - FG` <- NULL
dados2013$`Nota Bruta - CE` <- NULL
dados2013$`Nota Padronizada - CE` <- NULL
dados2013$`Nota Contínua do Enade` <- NULL
dados2013$'Obs:' <- NULL


View(dados2010)

dados2010 <- na.omit(dados2010)


dados2013$`Categ. Administrativa` = as.numeric(as.factor(dados2013$`Categ. Administrativa`))
dados2013$`Região do Curso` = as.numeric(as.factor(dados2013$`Região do Curso`))
dados2013$`Conceito Enade` = as.numeric(as.factor(dados2013$`Conceito Enade`))


'''
dados2010$'Nome da IES' = as.numeric(as.factor(dados2010$'Nome da IES'))
dados2010$'Sigla da IES' = as.numeric(as.factor(dados2010$'Sigla da IES'))
dados2010$'Categ. Administrativa' = as.numeric(as.factor(dados2010$'Categ. Administrativa'))
dados2010$`Org. Acadêmica` = as.numeric(as.factor(dados2010$`Org. Acadêmica`))
dados2010$`Município do Curso` = as.numeric(as.factor(dados2010$`Município do Curso`))
dados2010$`Região do Curso` = as.numeric(as.factor(dados2010$`Região do Curso`))
dados2010$`UF do Curso` = as.numeric(as.factor(dados2010$`UF do Curso`))
dados2010$`Conceito Enade` = as.numeric(as.factor(dados2010$`Conceito Enade`))
'''

plot(dados2013$'Conceito Enade', type = "p", main = "plot(x, type = \"s\")", cex = 1, col = "blue")
#plotar coluna

center<- scale(dados2013) #df padrão centraliza e / ou dimensiona as colunas de uma matriz numérica
# kmeans - visa particionar os pontos em k grupos de forma que a soma dos quadrados dos pontos aos centros de cluster designados seja minimizada
kmf<-kmeans(center, 4) # guarda dentro de 'kmf' o resultado do que feito na chamda da função 'kmeans'
attributes(kmf) # mostra os atributos da variável 'kmf'
c1<-cbind(kmf$cluster)
c1

plot (dados2013, col = kmf$cluster)

plot(dados2013$`Região do Curso`, dados2013$`Conceito Enade`)


plot(kmf$cluster)

clusplot(center, kmf$cluster, main = "Representação 2D dos Clusters", shade = TRUE, labels = 1, lines = 5, stand = TRUE)


kmf$cluster


View(evadidos)
remove(evadidos$`Sigla da IES`)






####################### ESTATISTICA 2010, 2013 e 2016 ###############################################
#dados2010Nordeste <- dados2013[which(dados2013$`Região do Curso` == 2 ), ]
dados2010 <- read_excel ("C:/Users/JSILVA/Documents/2010.xls")
View(dados2010)
dados2010 <- na.omit(dados2010)

dados2013 <- read_excel ("C:/Users/JSILVA/Documents/2013.xlsx")
View(dados2013)
dados2013 <- na.omit(dados2013)

dados2016 <- read_excel ("C:/Users/JSILVA/Documents/2016.xlsx")
View(dados2016)
dados2016 <- na.omit(dados2016)


#Media #################
mediaRegião2010 = aggregate(dados2010$n_abaixo, by=list(dados2010= dados2010$Região), FUN=sum)
mediaRegião2010 = aggregate(dados2010$`Nota Enade Concluintes`, by=list(dados2010= dados2010$Região), FUN=mean)
View(mediaRegião2010)
mediaRegião2010

#dados2010        x
#1 CENTRO-OESTE 3.293873
#2     NORDESTE 3.369464
#3        NORTE 2.195594
#4          SUL 3.301945
#5     SUDESTE 2.630274
mean(x = dados2010$`Nota Enade Concluintes`) #Media
# 2.913977

mediaRegião2013 = aggregate(dados2013$`Nota Contínua do Enade`, by=list(dados2013= dados2013$`Região do Curso`), FUN=mean)
View(mediaRegião2013)
mediaRegião2013


#dados2013        x
#1 CENTRO-OESTE 3.159725
#2     NORDESTE 2.772170
#3        NORTE 1.696512
#5          SUL 3.063256
#4      SUDESTE 2.594779
mean(x = dados2013$`Nota Contínua do Enade`) #Media
#2.678036


mediaRegião2016 = aggregate(dados2016$`Conceito Enade Contínuo`, by=list(dados2016= dados2016$`Região do Curso`), FUN=mean)
View(mediaRegião2016)
mediaRegião2016

#dados2016        x
#1 CENTRO-OESTE 2.767286
#2     NORDESTE 2.685591
#3        NORTE 2.058280
#5          SUL 2.932474
#4      SUDESTE 2.462845

mean(x = dados2016$`Conceito Enade Contínuo`) #Media
#2.578901
########################



#####Desvio Padrão #######
desvioRegião2010 = aggregate(dados2010$`Nota Enade Concluintes`, by=list(dados2010= dados2010$Região), FUN=sd)
View(desvioRegião2010)
desvioRegião2010

#dados2010         x
#1 CENTRO-OESTE 0.9935705
#2     NORDESTE 0.7842351
#3        NORTE 0.9025825
#4          SUL 0.9663588
#5     SUDESTE 1.0710874
sd(dados2010$`Nota Enade Concluintes`) # Desvio Padrão
#1.047118

desvioRegião2013 = aggregate(dados2013$`Nota Contínua do Enade`, by=list(dados2013= dados2013$`Região do Curso`), FUN=sd)
View(desvioRegião2013)
desvioRegião2013

#dados2013         x
#1 CENTRO-OESTE 0.9596516
#2     NORDESTE 0.8718442
#3        NORTE 1.0280957
#5          SUL 0.9008852
#4      SUDESTE 1.0572855
sd(dados2013$`Nota Contínua do Enade`) # Desvio Padrão
#[1] 1.038782

desvioRegião2016 = aggregate(dados2016$`Conceito Enade Contínuo`, by=list(dados2016= dados2016$`Região do Curso`), FUN=sd)
View(desvioRegião2016)
desvioRegião2016

#dados2016         x
#1 CENTRO-OESTE 0.7349721
#2     NORDESTE 0.6836695
#3        NORTE 1.0237942
#5          SUL 0.6191698
#4      SUDESTE 0.8758910


sd(dados2016$`Conceito Enade Contínuo`) # Desvio Padrão
#[1] 0.8285361
##########################




# Abaixo da media (Conceito 1 e 2)

#dados2010      abaixo    total %
#1 CENTRO-OESTE 1         11    09,1
#2     NORDESTE 1         30    03,3
#3        NORTE 5         15    33,3
#4          SUL 2         26    07,7
#5     SULDESTE 18        63    28,6
#total 27 c 1 e 2         145   18,6


#dados2013      abaixo    total %
#1 CENTRO-OESTE 2         12    16,7
#2     NORDESTE 8         34    23,5
#3        NORTE 8         14    57,1
#4          SUL 1         29    03,4
#5     SULDESTE 20        77    26,0
#total 39 c 1 e 2         166   23,5

#dados2016      abaixo    total %
#1 CENTRO-OESTE 2      12
#2     NORDESTE 7      39
#3        NORTE 7      16
#4          SUL 2      31
#5     SUDESTE 19     78
#total c 1 e 2  37    176











#################### Media CATEGORIA #################
mediacateg2010 = aggregate(dados2010$`Nota Enade Concluintes`, by=list(dados2010= dados2010$`Categoria Administrativa`), FUN=mean)
View(mediacateg2010)
mediacateg2010

#categoria        x
#1   PRIVADA 2.395784
#2   PÚBLICA 3.493133
mean(x = dados2010$`Nota Enade Concluintes`) #Media
# 2.913977


desvcateg2010 = aggregate(dados2010$`Nota Enade Concluintes`, by=list(dados2010= dados2010$`Categoria Administrativa`), FUN=sd)
View(desvcateg2010)
desvcateg2010

#dados2010         x    t   am
#1   PRIVADA 0.9739503  76  25
#2   PÚBLICA 0.7958221  68  2
#                       144 27
sd(x = dados2010$`Nota Enade Concluintes`) #desvio
# [1] 1.047118











#############2013
mediacateg2013 = aggregate(dados2013$`Nota Contínua do Enade`, by=list(dados2013= dados2013$`Categ. Administrativa`), FUN=mean)
View(mediacateg2013)
mediacateg2013

#categoria                        x
#1 Privada com fins lucrativos 1.965709   0.8546822   30    17
#2 Privada sem fins lucrativos 2.373741   0.9610745   70    19
#3            Pública Estadual 3.548161   0.7117628   18    1
#4             Pública Federal 3.305361   0.8442038   43    1
#5           Pública Municipal 2.684698   0.7632654   5     1

#2.678036
desvcateg2013 = aggregate(dados2013$`Nota Contínua do Enade`, by=list(dados2013= dados2013$`Categ. Administrativa`), FUN=sd)



#Tudo 2013 sum=soma sd=desvio mean=media
categ2013DIV = aggregate(dados2013$n_numero_categoria, by=list(dados2013= dados2013$n_categoria), FUN=sum)
View(categ2013DIV)
categ2013DIV

#dados2013        x
#1   privada 2.251331   0.9450193   100   36
#2   publica 3.324559   0.8212382   66    3

mean(x = dados2013$`Nota Contínua do Enade`) #Media
#2.678036









##########2016
mediacateg2016 = aggregate(dados2016$`Conceito Enade Contínuo`, by=list(dados2016= dados2016$`Categoria Administrativa`), FUN=mean)
mediacateg2016 = aggregate(dados2016$n_numero_categoria, by=list(dados2016= dados2016$`Categoria Administrativa`), FUN=sum)
View(mediacateg2016)
mediacateg2016



Pessoa Jurídica de Direito Privado - Com fins lucrativos - Sociedade Civil
2.221001
0.9549061
6
2


Pessoa Jurídica de Direito Privado - Sem fins lucrativos - Associação de Utilidade Pública
2.149968
#NAO TEM
1
0

Pessoa Jurídica de Direito Privado - Sem fins lucrativos - Fundação
2.163854
0.7324806
8
1


Pessoa Jurídica de Direito Privado - Sem fins lucrativos - Sociedade
2.378213
#NAO TEM
1
0

Pessoa Jurídica de Direito Público - Estadual
3.012647
0.7658396
23
3

Pessoa Jurídica de Direito Público - Federal
3.084488
0.4629271
47
1

Pessoa Jurídica de Direito Público - Municipal
2.357942
0.9192265
5
2

Privada com fins lucrativos
2.144712
0.6622889
26
9

Privada sem fins lucrativos
2.320465
0.8993554
59
19




#Tudo 2016 sum=soma sd=desvio mean=media
categ2016DIV = aggregate(dados2016$`Conceito Enade Contínuo`, by=list(dados2016= dados2016$n_categoria), FUN=sd)
View(categ2016DIV)
categ2016DIV

#dados2016        x
#1   privada    2.255792    0.8176916    101    31
#2   publica    3.014020    0.6208825    75     6





##########################################################################################################







#################################################   ESTADOS ###############################################################################################
#Tudo 2010 sum=soma sd=desvio mean=media
estados2010 = aggregate(dados2010$`Nota Enade Concluintes`, by=list(dados2010= dados2010$`Código da UF`), FUN=sd)
View(estados2010)
estados2010

AC	1	0	2.975.419	0
AL	2	0	3.551.889	0.7603952
AM	3	0	2.683.169	0.6182239
BA	6	0	3.145.971	0.5375126
CE	5	0	3.710.728	0.5919656
DF	4	0	3.180.057	11.761.011
ES	3	0	3.029.205	10.655.911
GO	2	0	3.613.294	0.9556893
MA	3	1	2.524.497	14.224.017
MG	20	10	2.188.103	13.832.416
MS	3	0	3.668.337	0.5790669
MT	2	1	2.640.390	16.147.999
PA	3	0	2.368.249	0.1528788
PB	4	0	3.027.591	0.8277690
PE	3	0	3.227.555	0.2319193
PI	4	0	3.460.355	0.5608130
PR	7	0	3.946.367	0.6302817
RJ	16	4	2.766.488	0.7720839
RN	2	0	4.464.542	0.3433997
RO	3	2	1.939.339	14.260.578
RR	1	0	3.008.192	0
RS	10	0	3.659.338	0.4781344
SC	9	2	2.403.624	0.9655385
SE	1	0	4.413.643	0
SP	24	4	2.858.073	0.8755536
TO	3	3	1.260.810	0.6413136


#Tudo 2013 sum=soma sd=desvio mean=media
estados2013 = aggregate(dados2013$`Nota Contínua do Enade`, by=list(dados2013= dados2013$'UF do Curso'), FUN=sd)
View(estados2013)
estados2013

AC	1		0	2,8	0,0
AL	2		1	2,6	1,0
AM	2		1	1,5	1,8
BA	5		1	2,8	0,9
CE	7		0	3,1	0,5
DF	4		1	3,2	1,2
ES	5		1	2,9	1,5
GO	3		0	3,5	1,0
MA	3		1	2,2	1,3
MG	27		7	2,5	1,0
MS	3		0	3,4	0,5
MT	2		1	2,3	1,0
PA	2		1	1,8	0,4
PB	6		3	2,5	1,0
PE	3		0	2,6	0,4
PI	4		1	2,7	1,1
PR	8		0	3,7	0,7
RJ	17		6	2,2	0,8
RN	3		1	3,3	1,4
RO	4		3	1,6	1,0
RR	1		0	2,7	0,0
RS	11		1	2,9	1,2
SC	10		0	2,7	0,4
SE	1		0	2,8	0,0
SP	28		6	2,9	1,1
TO	4		3	1,3	1,2


#Tudo 2016 sum=soma sd=desvio mean=media
estados2016 = aggregate(dados2016$`Conceito Enade Contínuo`, by=list(dados2016= dados2016$`Sigla da UF`), FUN=sd)
View(estados2016)
estados2016



####################################################################################################################################################################
dados2016$cont <- 1
View(dados2016)
qtdcateg2016 = aggregate(dados2016$cont, by=list(dados2016= dados2016$`Região do Curso`), FUN=sum)
View(qtdcateg2016)
qtdcateg2016


mean(x = dados2016$`Conceito Enade Contínuo`) #Media
#2.578901
########################



sum(dados2013Nordeste$`Conceito Enade`) # soma

mean(x = dados2013Nordeste$`Conceito Enade`) #Media

mean(dados2013Nordeste$`Conceito Enade`) #Media

sd(dados2013Nordeste$`Conceito Enade`) # Desvio Padrão
sd(dados2013Nordeste$`Conceito Enade`, na.rm=TRUE)


median(dados2013Nordeste$`Conceito Enade`) #mediana


var(dados2013Nordeste$`Conceito Enade`) #Variância
# quadrado do desvio-padrão; a esperança matemática do quadrado do desvio de uma variável aleatória.
#segundo momento de uma distribuição de frequências, tomando como origem a média.


max(dados2013Nordeste$`Conceito Enade`)#maximo
min(dados2013Nordeste$`Conceito Enade`)#minimo
quantile(dados2013Nordeste$'Conceito Enade') # Quartis


dados2013Nordeste$'Conceito Enade' = as.numeric(as.factor(dados2013Nordeste$'Conceito Enade'))
dados2013Nordeste



install.packages("dplyr") ## install

library("dplyr")

grouping

z = aggregate(dados2013$`Conceito Enade`, by=list(dados2013= dados2013$`Região do Curso`), FUN=mean)

z

plot(z$x , col = z$dados2013 )



z$dados2013 = as.numeric(as.factor(z$dados2013))

#########################################################################################################################################################################







##################################### <<< 2010 >>> ####################################################
dados2010 <- read_excel ("C:/Users/JSILVA/Documents/2010.xls")
View(dados2010)
remove(dados2010)
dados2010 <- dados2013[which(dados2013$'Área de Enquadramento' == 'MEDICINA'), ]

View(dados2010)

dados2010$Área <- NULL
dados2010$IES <- NULL
dados2010$Sigla <- NULL
dados2010$`Código da UF` <- NULL
dados2010$Município <- NULL
#dados2010$`Conceito Enade Faixa` <- NULL
dados2010$`Nota Enade Concluintes` <- NULL
dados2010$n_numero_categoria <- NULL
dados2010$n_abaixo <- NULL



View(dados2010)

dados2010 <- na.omit(dados2010)


dados2010$Região = as.numeric(as.factor(dados2010$Região))
dados2010$`Categoria Administrativa` = as.numeric(as.factor(dados2010$`Categoria Administrativa`))
dados2010$`Conceito Enade Faixa` = as.numeric(as.factor(dados2010$`Conceito Enade Faixa`))

plot(dados2010$`Conceito Enade Faixa`, type = "p", main = "plot(x, type = \"s\")", cex = 1, col = "blue")
#plotar coluna

center<- scale(dados2010) #df padrão centraliza e / ou dimensiona as colunas de uma matriz numérica
# kmeans - visa particionar os pontos em k grupos de forma que a soma dos quadrados dos pontos aos centros de cluster designados seja minimizada
kmf<-kmeans(center, 4) # guarda dentro de 'kmf' o resultado do que feito na chamda da função 'kmeans'
attributes(kmf) # mostra os atributos da variável 'kmf'
c1<-cbind(kmf$cluster)
c1

plot (dados2010, col = kmf$cluster)

plot(dados2010$Região, dados2010$`Conceito Enade Faixa`)


plot(kmf$cluster)

clusplot(center, kmf$cluster, main = "Representação 2D dos Clusters", shade = TRUE, labels = 1, lines = 5, stand = TRUE , , xlab = "Componente 1", ylab = "Componente 2")


kmf$cluster


View(evadidos)
remove(evadidos$`Sigla da IES`)









##################################### <<< 2013 >>> ######################################################################################################################
dados2013 <- read_excel ("C:/Users/JSILVA/Documents/2013.xlsx")
View(dados2013)

dados2013 <- dados2013[which(dados2013$'Área de Enquadramento' == 'MEDICINA'), ]

View(dados2013)

dados2013$Ano <- NULL
dados2013$Cód.Área <- NULL
dados2013$`Área de Enquadramento` <- NULL
dados2013$Cód.IES <- NULL
dados2013$`Nome da IES` <- NULL
dados2013$`Sigla da IES` <- NULL
dados2013$`Org. Acadêmica` <- NULL
dados2013$`Cód. Município` <- NULL
dados2013$`Município do Curso` <- NULL
dados2013$`UF do Curso` <- NULL
dados2013$`Nr. Cursos da Unidade` <- NULL
dados2013$`Cód. Cursos da Unidade` <- NULL
dados2013$`Conluintes Inscritos` <- NULL
dados2013$`Concluintes Participantes` <- NULL
dados2013$`Nota Bruta - FG` <- NULL
dados2013$`Nota Padronizada - FG` <- NULL
dados2013$`Nota Bruta - CE` <- NULL
dados2013$`Nota Padronizada - CE` <- NULL
dados2013$`Nota Contínua do Enade` <- NULL
dados2013$'Obs:' <- NULL
dados2013$`Categ. Administrativa` <- NULL
dados2013$n_numero_categoria <- NULL
dados2013$n_abaixo <- NULL
dados2013$n_categoria <- NULL


View(dados2013)

dados2013 <- na.omit(dados2013)


dados2013$`Categ. Administrativa` = as.numeric(as.factor(dados2013$`Categ. Administrativa`))
dados2013$`Região do Curso` = as.numeric(as.factor(dados2013$`Região do Curso`))
dados2013$`Conceito Enade` = as.numeric(as.factor(dados2013$`Conceito Enade`))


'''
dados2010$'Nome da IES' = as.numeric(as.factor(dados2010$'Nome da IES'))
dados2010$'Sigla da IES' = as.numeric(as.factor(dados2010$'Sigla da IES'))
dados2010$'Categ. Administrativa' = as.numeric(as.factor(dados2010$'Categ. Administrativa'))
dados2010$`Org. Acadêmica` = as.numeric(as.factor(dados2010$`Org. Acadêmica`))
dados2010$`Município do Curso` = as.numeric(as.factor(dados2010$`Município do Curso`))
dados2010$`Região do Curso` = as.numeric(as.factor(dados2010$`Região do Curso`))
dados2010$`UF do Curso` = as.numeric(as.factor(dados2010$`UF do Curso`))
dados2010$`Conceito Enade` = as.numeric(as.factor(dados2010$`Conceito Enade`))
'''

plot(dados2013$'Conceito Enade', type = "p", main = "plot(x, type = \"s\")", cex = 1, col = "blue")
#plotar coluna

center<- scale(dados2013) #df padrão centraliza e / ou dimensiona as colunas de uma matriz numérica
# kmeans - visa particionar os pontos em k grupos de forma que a soma dos quadrados dos pontos aos centros de cluster designados seja minimizada
kmf<-kmeans(center, 4) # guarda dentro de 'kmf' o resultado do que feito na chamda da função 'kmeans'
attributes(kmf) # mostra os atributos da variável 'kmf'
c1<-cbind(kmf$cluster)
c1

plot (dados2013, col = kmf$cluster)

plot(dados2013$`Região do Curso`, dados2013$`Conceito Enade`)


plot(kmf$cluster)

clusplot(center, kmf$cluster, main = "Representação 2D dos Clusters", shade = TRUE, labels = 1, lines = 5, stand = TRUE)


kmf$cluster


View(evadidos)
remove(evadidos$`Sigla da IES`)



##################################### <<< 2016 >>> ####################################################
dados2016 <- read_excel ("C:/Users/JSILVA/Documents/2016.xlsx")
View(dados2016)
remove(dados2016)
dados2016 <- dados2013[which(dados2013$'Área de Enquadramento' == 'MEDICINA'), ]

View(dados2016)

dados2016$`Nome da IES` <- NULL
dados2016$`Sigla da IES` <- NULL
dados2016$`Município do Curso` <- NULL
dados2016$`Sigla da UF` <- NULL
dados2016$`Conceito Enade Contínuo` <- NULL
dados2016$n_categoria <- NULL
dados2016$n_numero_categoria <- NULL
dados2016$n_abaixo <- NULL



View(dados2016)

dados2016 <- na.omit(dados2016)


dados2016$Região = as.numeric(as.factor(dados2016$Região))
dados2016$`Categoria Administrativa` = as.numeric(as.factor(dados2016$`Categoria Administrativa`))
dados2016$`Conceito Enade Faixa` = as.numeric(as.factor(dados2016$`Conceito Enade Faixa`))

plot(dados2016$`Conceito Enade Faixa`, type = "p", main = "plot(x, type = \"s\")", cex = 1, col = "blue")
#plotar coluna

center<- scale(dados2016) #df padrão centraliza e / ou dimensiona as colunas de uma matriz numérica
# kmeans - visa particionar os pontos em k grupos de forma que a soma dos quadrados dos pontos aos centros de cluster designados seja minimizada
kmf<-kmeans(center, 4) # guarda dentro de 'kmf' o resultado do que feito na chamda da função 'kmeans'
attributes(kmf) # mostra os atributos da variável 'kmf'
c1<-cbind(kmf$cluster)
c1

plot (dados2016, col = kmf$cluster)

plot(dados2016$Região, dados2016$`Conceito Enade Faixa`)


plot(kmf$cluster)

clusplot(center, kmf$cluster, main = "Representação 2D dos Clusters", shade = TRUE, labels = 1, lines = 5, stand = TRUE , , xlab = "Componente 1", ylab = "Componente 2")


kmf$cluster


View(evadidos)
remove(evadidos$`Sigla da IES`)






####################### ESTATISTICA ###############################################
dados2013Nordeste <- dados2013[which(dados2013$`Região do Curso` == 2 ), ]
View(dados2013Nordeste)



sum(dados2013Nordeste$`Conceito Enade`) # soma

mean(x = dados2013Nordeste$`Conceito Enade`) #Media

mean(dados2013Nordeste$`Conceito Enade`) #Media

sd(dados2013Nordeste$`Conceito Enade`) # Desvio Padrão
sd(dados2013Nordeste$`Conceito Enade`, na.rm=TRUE)


median(dados2013Nordeste$`Conceito Enade`) #mediana


var(dados2013Nordeste$`Conceito Enade`) #Variância
# quadrado do desvio-padrão; a esperança matemática do quadrado do desvio de uma variável aleatória.
#segundo momento de uma distribuição de frequências, tomando como origem a média.


max(dados2013Nordeste$`Conceito Enade`)#maximo
min(dados2013Nordeste$`Conceito Enade`)#minimo
quantile(dados2013Nordeste$'Conceito Enade') # Quartis


dados2013Nordeste$'Conceito Enade' = as.numeric(as.factor(dados2013Nordeste$'Conceito Enade'))
dados2013Nordeste



install.packages("dplyr") ## install

library("dplyr")

grouping

z = aggregate(dados2013$`Conceito Enade`, by=list(dados2013= dados2013$`Região do Curso`), FUN=mean)

z

plot(z$x , col = z$dados2013 )



z$dados2013 = as.numeric(as.factor(z$dados2013))





################### Ajuda
dados$x <- NULL
head(dados)

for(dado in evadidos){
  print(evadidos.`Conceito Enade`)
}


dados2010[which(dados2010$UF == 'PI'), ]
View(dados2010[which(dados2010$UF == 'PI'), ] )

remove(dados2010)



##################################### Hierarquical Clustering ################################################
install.packages("factoextra")
install.packages("purrr")
data <- evadidos
d <- dist(data, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
# Compute with agnes (make sure you have the package cluster)
hc2 <- agnes(data, method = "complete")
# Agglomerative coefficient
hc2$ac
## [1] 0.9317012
#Let’s compare the methods discussed
# vector of methods to compare
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
# function to compute coefficient
ac <- function(x) {
  agnes(data, method = x)$ac
}
map_dbl(m, ac)
## from ‘purrr’ package
## average single complete ward
## 0.9241325 0.9215283 0.9317012 0.9493598
#Ward’s method gets us the highest agglomerative coefficient. Let us look at its dendogram.
hc3 <- agnes(data, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
hc4 <- diana(data)
# Divise coefficient
hc4$dc
## [1] 0.9305939
# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")
clust <- cutree(hc4, k = 3) #k = numero de clusters
factoextra::fviz_cluster(list(data = data, cluster = clust)) ## from ‘factoextra’ package
pltree (hc4, pendurar = -1, cex = 0,6)
