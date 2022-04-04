
######### CARREGA AS BIBLIOTECAS ######### 
library(rvest)
library(stringr)
library(dplyr)
library(corrplot)
library(psych)
library(robustHD)
library(neuralnet)
library(caret)
library(boot)
library(partykit)
library(rpart)
library(rpart.plot)
library(ipred)
library(rattle)
library(RColorBrewer)
library(Metrics)
library(party)
library(car)
library(fastDummies)
library(ranger)
library(robustHD)
library(NeuralNetTools)
library (gbm)
library(MLmetrics)
library(arm)

#########  LÊ CSVs #########

#cria um data frame para utilizarmos para o modelo rede neuronal
#carregar df_original
df <- read.delim(file.choose(),
                               header=TRUE, sep=";", dec=",")

#Carregar df_normalizado
df_rede_neuronal <- read.delim(file.choose(),
                               header=TRUE, sep=";", dec=",")

#CARREGAR CSV "df_limpo_desnormalizado"
df_limpo_desnormalizado <- read.delim(file.choose(),
                                      header=TRUE, sep=";", dec=",")


######### DATA UNDERSTANDING: WEB SCRAPING #########

df <- data.frame(stringsAsFactors = FALSE,
                 Hotel_Name=character(0),
                 score_data=character(0),
                 price_data=character(0),
                 distance_to_center=character(0),
                 Estrelas=character(0),
                 breakfast=character(0),
                 Reviews_qty=character(0),
                 Free_Cancel=character(0),
                 only_x_left=character(0)
)



#inicializa a variavél BookingPAge com o URL

firsturl <- "https://www.booking.com/searchresults.pt-pt.html?label=gen173nr-1DCAEoggI46AdIM1gEaLsBiAEBmAEfuAEXyAEM2AED6AEBiAIBqAIDuAL9gc_9BcACAdICJGYwODg5ZTY3LTg3MjItNDljOS05ODYyLTdiZjcxNGI0MjllYtgCBOACAQ&sid=e9addf3aca6351fcf50b432e7171357d&tmpl=searchresults&checkin_month=12&checkin_monthday=23&checkin_year=2020&checkout_month=12&checkout_monthday=24&checkout_year=2020&class_interval=1&dest_id=-3414440&dest_type=city&dtdisc=0&from_sf=1&group_adults=2&group_children=0&inac=0&index_postcard=0&label_click=undef&no_rooms=1&postcard=0&raw_dest_type=city&room1=A%2CA&sb_price_type=total&shw_aparth=1&slp_r_match=0&src=index&src_elem=sb&srpvid=5a6c583f424b01cb&ss=Banguecoque&ss_all=0&ssb=empty&sshis=0&ssne=Banguecoque&ssne_untouched=Banguecoque&top_ufis=1"
url <- "https://www.booking.com/searchresults.pt-pt.html?label=gen173nr-1DCAEoggI46AdIM1gEaLsBiAEBmAEfuAEXyAEM2AED6AEBiAIBqAIDuAL9gc_9BcACAdICJGYwODg5ZTY3LTg3MjItNDljOS05ODYyLTdiZjcxNGI0MjllYtgCBOACAQ&sid=e9addf3aca6351fcf50b432e7171357d&tmpl=searchresults&checkin_month=12&checkin_monthday=23&checkin_year=2020&checkout_month=12&checkout_monthday=24&checkout_year=2020&class_interval=1&dest_id=-3414440&dest_type=city&dtdisc=0&from_sf=1&group_adults=2&group_children=0&inac=0&index_postcard=0&label_click=undef&no_rooms=1&postcard=0&raw_dest_type=city&room1=A%2CA&sb_price_type=total&shw_aparth=1&slp_r_match=0&src=index&src_elem=sb&srpvid=5a6c583f424b01cb&ss=Banguecoque&ss_all=0&ssb=empty&sshis=0&ssne=Banguecoque&ssne_untouched=Banguecoque&top_ufis=1"

#inicializa a variável numPage com o valor 1
numHoteis <- 0

#Verifica quantos hoteis estão disponíveis no booking.com
H_disp <- read_html(firsturl)
qty_disp <- H_disp %>% html_nodes(xpath = '//*[@class="results-meta  sr_results_footer__container"]')
qty_disp <- html_text(qty_disp)
Extract_qty <- "[:digit:].[:digit:]+"
qty_disp <- str_extract(qty_disp, Extract_qty)
qty_disp <- as.numeric(gsub("[[:space:]]", "", qty_disp))


#percorre um ciclo até que a variável numPage chegue ao valor 40
#40 é o número de páginas que existem com hóteis em Banguecoque
while(numHoteis < qty_disp){
  
  if (numHoteis >= 25) url <- paste(firsturl, "&rows=25&offset=", numHoteis, sep="")
  
  
  BookingPage <- read_html(url)
  Sys.sleep(2.5)
  
  
  #RECOLHE O NOME DE CADA HOTEL 
  Hotel_Name <- html_nodes(BookingPage, xpath = '//*[@class="sr-hotel__name
"]') 
  Hotel_Name <- html_text(Hotel_Name)
  
  
  #RECOLHE o SCORE MÉDIO DE REVIEW DE HÓSPEDES 
  score_badge_html <- html_nodes(BookingPage,xpath = '//*[@data-score]')
  score_data <- html_attr(score_badge_html, "data-score")
  score_data <- as.numeric(str_replace_all(score_data, "," , "."))
  
  
  #RECOLHE A DISTÂNCIA DA PROPRIEDADE AO CENTRO
  distance_to_center <- html_nodes(BookingPage, xpath = '//*[@class="sr_card_address_line"]')
  distance_to_center <- html_text(distance_to_center)
  distance_to_center <- gsub("^.*?mapa","", distance_to_center)
  distance_to_center <- gsub("do.*","", distance_to_center)
  distance_to_center <- str_replace(distance_to_center, ",", ".")
  distance_to_center <- str_trim(str_replace(distance_to_center, "km", ""))
  
  #Verifica os valores em metros e transforma todos para KM
  is_in_meters <-  grep(" m", distance_to_center) 
  distance_to_center[is_in_meters] <- paste(as.numeric(gsub(".*?([0-9]+).*", "\\1", distance_to_center[is_in_meters]))/1000) 
  distance_to_center <-as.numeric(distance_to_center)
  
  
  #RECOLHE PREÇO QUARTO 1 NOITE PARA 2 PESSOAS 
  price_data <- html_nodes(BookingPage, xpath = '//*[@class="prco-inline-block-maker-helper"]')
  price_data <- html_text(price_data)
  price_data <- str_replace_all(price_data, "Pre.o" , "Preco") #troca o preço por preco
  price_data <- str_replace_all(price_data, "[[\r\n]]" , "") #remove o \n
  price_data <- str_extract(price_data, "Preco atual..[[:digit:]]+|Preco..[[:digit:]]+")
  price_data <- as.numeric(str_extract(price_data, "[[:digit:]]+")) 
  
  
  #DETECTA AS POLÍTICAS DO QUARTO:CANCELAMENTO GRATUITO E PEQUENO-ALMOÇO INCLUÍDO
  Room_Policies <- BookingPage %>% html_nodes(xpath = '//*[@class="sr_card_room_policies__container"]')
  Room_Policies <- html_text(Room_Policies)
  
  
  #DETECTA SE O PEQUENO-ALMOÇO ESTÁ INCLUÍDO E RETORNA YES OU NO 
  breakfast <- str_detect(Room_Policies,"Pequeno-almoço incluído") # detecta se o Pequeno Almoço está incluído e retorna true ou false
  breakfast <- str_replace(breakfast, "TRUE", "1") #transforma TRUE em 1, como pede o enunciado
  breakfast <- str_replace(breakfast, "FALSE", "0") #transforma FALSE em 0, como pede o enunciado
  breakfast <- as.numeric(breakfast)
  
  #DETECTA SE TEM CANCELAMENTO GRATUIRO E RETORNA YES OU NO 
  Free_Cancel <- str_detect(Room_Policies,"Cancelamento GRATUITO") # detecta se o cancelamento é gratuito e retorna true ou false
  Free_Cancel <- str_replace(Free_Cancel, "TRUE", "1") #transforma TRUE em 1, como pede o enunciado
  Free_Cancel <- str_replace(Free_Cancel, "FALSE", "0") #transforma TRUE em 0, como pede o enunciado
  Free_Cancel <- as.numeric(Free_Cancel)
  
  
  #BUSCA ESTRELAS
  Estrelas <- BookingPage %>% html_nodes(xpath = '//*[@data-class]')
  Estrelas <- as.numeric(html_attr(Estrelas, "data-class"))
  Estrelas <- na_if(Estrelas, 0) # Converte todos os hoteis com 0 estrelas para NA 
  
  
  #RECOLHE O NÚMERO DE REVIEWS
  Reviews_qty <- BookingPage %>% html_nodes(xpath = '//*[@class="reviewFloater reviewFloaterBadge__container"]')
  Reviews_qty <- html_text(Reviews_qty)
  Reviews_qty <- as.character(str_extract_all(Reviews_qty, "...... comentários")) # seleciona o número de comentários
  Reviews_qty <- str_replace(Reviews_qty, "[[:alpha:]]+ " , "") #Remove, caso tiver, alguma letra no ínicio do string
  Reviews_qty <- str_trim(str_replace_all(Reviews_qty, "comentários" , "")) #remove a palavra comentários
  Reviews_qty <- as.numeric(str_replace(Reviews_qty, "[\\s]" , "")) #Remove os espaços em branco e transforma em numeric
  #para as entradas "character(0)" os NAs serão introduzidos por coerção
  
  
  #DETECTA SE TEM INDICAÇÃO OU NÃO DE QUE "SÓ RESTAM N QUARTOS PARA RESERVA NO BOOKING"
  only_x_left <- BookingPage %>% html_nodes(xpath = '//*[@class="js_sr_persuation_msg"]')
  only_x_left <- html_text(only_x_left, )
  only_x_left <- str_replace_all(only_x_left, "[\r\n]" , "") #remove o \n
  only_x_left <- str_detect(only_x_left, "[apenas]") # detecta se tem a indicação
  only_x_left <- str_replace(only_x_left, "TRUE", "1") #tranforma true em 1
  only_x_left <- str_replace(only_x_left, "FALSE", "0") #tranforma false em 0
  only_x_left <- as.numeric(only_x_left)
  
  
  df <- rbind(df,
              data.frame(stringsAsFactors = FALSE,
                         Hotel_Name=Hotel_Name,
                         score_data=score_data,
                         price_data=price_data,
                         distance_to_center=distance_to_center,
                         Estrelas=Estrelas,
                         breakfast=breakfast,
                         Reviews_qty=Reviews_qty,
                         Free_Cancel=Free_Cancel,
                         only_x_left=only_x_left
              ))
  
  numHoteis <- numHoteis + 25
  
  
  
}

######### DATA PREPARATION: LIMPEZA DOS DADOS - REMOVE DUPLICADOS E NAS  - CRIA FICHEIRO CSV ORIGINAL  ###############

#Remove todos os hoteis duplicados 
df <- df %>% distinct(Hotel_Name, .keep_all = TRUE)


#Remove tas as linhas com alguma variável com valor NA
df <- na.omit(df)


#CRIA CSV COM OS DADOS DO WEB SCRAPING 
write.csv2(df[,2:9], "df_original.csv", row.names=FALSE) 

######### DATA UNDERSTANDING: HISTOGRAMAS E BOXPLOSTS #########

#Score
hist(df$score_data,main = "Score Médio de Review dos Hóspedes",
     xlab = "Score Médio",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")
boxplot(df$score_data,
        main = "Score Médio de Review dos Hóspedes",
        xlab = "Score Médio",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

#Price
hist(df$price_data,
     main = "Preço de 1 quarto para 1 noite para 2 pessoas",
     xlab = "Preço",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$price_data,
        main = "Preço de 1 quarto para 1 noite para 2 pessoas",
        xlab = "Preço em Euros",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

#distance_to_center_data
hist(df$distance_to_center,
     main = "Distância ao centro da Cidade",
     xlab = "Distância",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$distance_to_center,
        main = "Distância ao centro da Cidade",
        xlab = "Distância em Km",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

#estrelas

discrete.histogram(df$Estrelas, 
                   main = "N.º de Estrelas de cada Hotel",
                   prob.col = "#4ADCD2",
                   freq=TRUE,
                   xlab = "Estrelas"
                   )
     
     
boxplot(df$Estrelas,
        main = "N.º de Estrelas de cada Hotel",
        xlab = "Estrelas",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

#reviews
hist(df$Reviews_qty,
     main = "N.º de Reviews já escritas por Hóspedes para cada Hotel",
     xlab = "N.ºReviews",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$Reviews_qty,
        main = "N.º de Reviews já escritas por Hóspedes para cada Hotel",
        xlab = "N.ºReviews",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)


#breakfast
hist(df$breakfast,
     main = "Tem pequeno-almoço incluído no preço?",
     xlab = "1 = Sim / 0 = não",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$breakfast,
        main = "Tem pequeno-almoço incluído no preço?",
        xlab = "1 = Sim / 0 = não",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)


#Only_x_left
hist(df$only_x_left,
     main = "Há aviso de que há poucos quartos disponíveis?",
     xlab = "1 = Sim / 0 = não",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$only_x_left,
        main = "Há aviso de que há poucos quartos disponíveis?",
        xlab = "1 = Sim / 0 = não",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

#Free_Cancel
hist(df$Free_Cancel,
     main = "O Cancelamento é gratuito?",
     xlab = "1 = Sim / 0 = não",
     ylab = "Quantidade Observações",
     col = "#4ADCD2",
     border = "black")

boxplot(df$Free_Cancel,
        main = "O Cancelamento é gratuito?",
        xlab = "1 = Sim / 0 = não",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

######### DATA PREPARATION: LIMPEZA DE DADOS - REMOÇÃO OUTLIERS - CRIA CSV PARA DESNORMALIZAR #########

#cria função para remover os outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)]<-NA
  y[x > (qnt[2] + H)]<-NA
  y
}
#Cria função para remover os outliers severos. 
remove_outliers_severos <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 3 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)]<-NA
  y[x > (qnt[2] + H)]<-NA
  y
}

#Remove os outliers das variáveis não binária (exceto Estrelas)
df$score_data <- remove_outliers(df$score_data)
df$distance_to_center <- remove_outliers(df$distance_to_center)

#Removeremos somente os outliers severos da df$price_data pois é a nossa variável alvo
df$price_data <- remove_outliers_severos(df$price_data)

#Remove tas as linhas com alguma variável com valor NA
df <- na.omit(df)

#CRIA CSV PARA DESNORMALIZAR E RETIRA A COLUNA HOTEL_NAME
write.csv2(df[,2:9], "df_limpo_desnormalizado.csv", row.names=FALSE) 

######### DATA UNDERSTANDING: CORRELAÇÃO DE PEARSON ########

#corrplot
correlation <- cor(df[,2:9])
par(oma = c(0, 0, 0, 0)) # space around for text
corrplot.mixed(correlation,
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "ellipse"
               )

#Correlation matrix
round(correlation, 3)



######### DATA UNDERSTANDING: SCATTERPLOT - GRÁFICO DE DISPERSÃO ##########

#scatterplot
plot(df[,2:9], pch = 19, lower.panel = NULL) # regression line (y~x)

######### DATA PREPARATION: NORMALIZAÇÃO - CRIA CSV COM OS DADOS NORMALIZADOS - CRIA FUNÇÃO PARA DESNORMALIZAR OS DADOS  ##########################

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

df$Reviews_qty <- normalize(df$Reviews_qty)
df$Estrelas <- normalize(df$Estrelas)
df$score_data <- normalize(df$score_data)
df$price_data <- normalize(df$price_data)
df$distance_to_center <- normalize(df$distance_to_center)
df$breakfast <- normalize(df$breakfast)
df$Free_Cancel <- normalize(df$Free_Cancel)
df$only_x_left <- normalize(df$only_x_left)

#CRIA CSV DA NORMALIZAÇÃO E RETIRA A COLUNA HOTEL_NAME
write.csv2(df[,2:9], "df_normalizado.csv", row.names=FALSE) 

#CRIA FUNÇÃO PARA DESNORMALIZAR OS DADOS

minvec <- sapply(df_limpo_desnormalizado[2],min) 
maxvec <- sapply(df_limpo_desnormalizado[2],max) 

denormalMm <- function(x,minval,maxval) { 
  
  x*(maxval-minval) + minval }


######### DATA PREPARATION: CRIA DFS PARA USAR NOS MODELOS #########

#cria um data frame para utilizarmos para a regressão linear
df_regr_linear <-df_rede_neuronal 
#cria um data frame para utilizarmos para a árvore de decisão
df_arvore_decisao <-df_rede_neuronal 

######### MODELING: MODELO REDE NEURONAL #########

#Divisão dos dados em dois conjuntos Treino (90%) e Teste (10%)
set.seed(100)
index_1<-sample(1:nrow(df_rede_neuronal),round(nrow(df_rede_neuronal)*0.90))
Neural_train_set<-df_rede_neuronal[index_1,]
Neural_test_set<-df_rede_neuronal[-index_1,]



#Cria o modelo RNA
Neural_Model_1 <- neuralnet(price_data ~  only_x_left + score_data + distance_to_center + Estrelas + breakfast  +  
                              Free_Cancel, data=Neural_train_set, hidden=c(5), linear.output=FALSE, 
                            threshold =0.01, rep=20)

#Representa graficamente o Modelo RNA
plot(Neural_Model_1, rep = "best")

#Verifica importância das variáveis
garson(Neural_Model_1)

#verifica o erro e os pesos do modelo
Neural_Model_1$result.matrix

# Faz previsão
temp_test_Neural_Model_1 <- subset(Neural_test_set, select = c( "only_x_left", "score_data", "distance_to_center", "Estrelas"  , "breakfast",  "Free_Cancel"))
Neural_Model_1_results <- predict(Neural_Model_1, temp_test_Neural_Model_1, rep= min(Neural_Model_1$result.matrix[1,]) )
DF_Neural_Model_1_results <- data.frame(actual = Neural_test_set[,2], prediction = Neural_Model_1_results, Error = Neural_test_set[,2]-Neural_Model_1_results)


#Calcula RMSE e Relative RMSE da rede Neuronal
RMSE_Neural_Model_1<- RMSE(Neural_Model_1_results,Neural_test_set[,2])
Relative_RMSE_RMSE_Neural_Model_1 <- (RMSE_Neural_Model_1 / mean(Neural_test_set[,2]) ) *100

#DESNORMALIZA OS DADOS
Neural_Model_1.results.euro<-denormalMm(Neural_Model_1_results, minvec, maxvec)
Neural_Model_1.results.euro<-round(Neural_Model_1.results.euro, 2)
ACTUAL.EURO<-denormalMm(Neural_test_set[,2], minvec, maxvec)


#TABELA DE COMPARAÇÃO PREÇOS ATUAIS, PREVISTOS E ERROS  
DF_RESULTS_Model_1 <- data.frame(ACTUAL = Neural_test_set[,2], ACTUAL.EURO, PREDICTION = Neural_Model_1_results, PREDICTION.EURO = Neural_Model_1.results.euro, ERROR = Neural_test_set[,2]-Neural_Model_1_results)


#representação gráfica da comparação entre os preços atuais e os preços previstos pela RNA com Retropropagação dá-se pelo seguinte gráfico: 
plot(Actual.euro ,Neural_Model_1.results.euro,col='red',main='PREÇO ATUAL VS. PREÇO PREVISTO ',pch=18,cex=1, xlab= "PREÇO ATUAL (???)", ylab= "PREÇO PREVISTO (???)")



#Não satisfeitos com os resultados deste modelo de RNA, tendo em vista o valor do RMSE e RRMSE, seguimos com a tentativa de fazer
#Construir RNA utilizando algorithm = "backprop" e ajustes na taxa de aprendizagem


#Cria o modelo RNA com retropropagção e ajuste de tx. de aprendizagem
Neural_Model_Backprop <- neuralnet(price_data ~  only_x_left + score_data + distance_to_center + Estrelas + breakfast  +  
                                     Free_Cancel, data=Neural_train_set, hidden=c(2,1), linear.output=FALSE, 
                                   threshold =0.01, algorithm = "backprop", learningrate = 0.001, stepmax = 1e7)

#Representa graficamente o Modelo RNA com retropropagção e ajuste de tx. de aprendizagem
plot(Neural_Model_Backprop, rep = "best")

#Representação dos pesos generalizados 
gwplot(Neural_Model_Backprop, selected.covariate= "only_x_left")
gwplot(Neural_Model_Backprop, selected.covariate="score_data")
gwplot(Neural_Model_Backprop, selected.covariate="distance_to_center")
gwplot(Neural_Model_Backprop, selected.covariate="Estrelas")
gwplot(Neural_Model_Backprop, selected.covariate="breakfast")
gwplot(Neural_Model_Backprop, selected.covariate="Free_Cancel")

#verifica o erro e os pesos do modelo
Neural_Model_Backprop$result.matrix


# Faz a previsão
temp_test_Neural_Model_Backprop <- subset(Neural_test_set, select = c( "only_x_left", "score_data", "distance_to_center", "Estrelas"  , "breakfast",  "Free_Cancel"))
Neural_Model_Backprop.results <- predict(Neural_Model_Backprop, temp_test_Neural_Model_Backprop)
DF_Neural_Model_Backprop <- data.frame(actual = Neural_test_set[,2], prediction = Neural_Model_Backprop.results, Error = Neural_test_set[,2]-Neural_Model_Backprop.results)


#Calcula RMSE e Relative RMSE da rede Neuronal com backpropagation
RMSE_Modelo_Rede_NeuronaL_Backprop<- RMSE(Neural_Model_Backprop.results,Neural_test_set[,2])
Relative_RMSE_Rede_Neuronal_Test_Backprop <- (RMSE_Modelo_Rede_NeuronaL_Backprop / mean(Neural_test_set[,2]) ) *100

#DESNORMALIZA OS DADOS
Neural_Model_Backprop.results.euro<-denormalMm(Neural_Model_Backprop.results, minvec, maxvec)
Neural_Model_Backprop.results.euro<-round(Neural_Model_Backprop.results.euro, 2)
ACTUAL.EURO<-denormalMm(Neural_test_set[,2], minvec, maxvec)


#TABELA DE COMPARAÇÃO PREÇOS ATUAIS, PREVISTOS E ERROS  
DF_RESULTS_BACKPROP <- data.frame(ACTUAL = Neural_test_set[,2], ACTUAL.EURO, PREDICTION = Neural_Model_Backprop.results, PREDICTION.EURO = Neural_Model_Backprop.results.euro, ERROR = Neural_test_set[,2]-Neural_Model_Backprop.results)


#representação gráfica da comparação entre os preços atuais e os preços previstos pela RNA com Retropropagação dá-se pelo seguinte gráfico: 
plot(ACTUAL.EURO ,Neural_Model_Backprop.results.euro,col='forestgreen',main='PREÇO ATUAL VS. PREÇO PREVISTO',pch=18,cex=1, xlab= "PREÇO ATUAL (???)", ylab= "PREÇO PREVISTO (???)")

######### MODELING: MODELO REGRESSÃO LINEAR #########

Dados <- df_regr_linear

#Processo de criação de dummies nas Estrelas, distance_to_center e score_data
Dados$distance_to_center_cut <- cut(Dados$distance_to_center, 
                                    breaks=c(-Inf, 0.13357401, 0.35018051, 0.71119134, Inf), 
                                    labels=c("1","2","3", "4"))
Dados$score_data_cut <- cut(Dados$score_data, 
                            breaks=c(-Inf,0.058252427, 0.5161290, 0.8387097, Inf), 
                            labels=c("1","2","3", "4"))
Dados <- dummy_cols(Dados, select_columns = "distance_to_center_cut")
Dados <- dummy_cols(Dados, select_columns = "score_data_cut")
Dados <- dummy_cols(Dados, select_columns = "Estrelas")


#Divisões dos conjuntos
set.seed(100)
index_1<-sample(1:nrow(Dados),round(nrow(Dados)*0.8))
train_1<-Dados[index_1,]
teste_1<-Dados[-index_1,]

teste_euro <- denormalMm(teste_1[2], minvec, maxvec)
treino.euro<-as.data.frame(round((denormalMm(train_1[2], minvec, maxvec)),2))

#Regressão múltipla com todas as variáveis 
model1_lm <- lm(price_data ~ score_data+distance_to_center+breakfast+Estrelas+Reviews_qty+Free_Cancel+only_x_left, data = train_1)
#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro - Regressão múltipla com todas as variáveis 
pred1_lm <- predict(model1_lm, newdata = teste_1)
RMSE_1_norm <- RMSE(pred1_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro <- denormalMm(pred1_lm, minvec, maxvec)
tabela <- data.frame(Real=teste_euro,Previsão=previsoes_euro, Diferença=abs(teste_euro-previsoes_euro))

#Transforma os preços reais do conjunto de teste em valores numerários; Calcula o RMSE e RRMSE da regressão Múltipla com todas as variáveis 
teste_euro.num<-as.numeric(unlist((teste_euro)))
RMSE_1 <- RMSE(previsoes_euro, teste_euro.num)
RRMSE_1 <- RMSE_1/mean(teste_euro.num)*100


#Regressão múltipla com todas as variáveis e dummy nas Estrelas 
model2_lm <- lm(price_data ~ score_data+distance_to_center+breakfast+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75+Reviews_qty+Free_Cancel+only_x_left, data = train_1)
#Cria o modelo previsto e tabela comparatativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE 
pred2_lm <- predict(model2_lm, newdata = teste_1)
RMSE_2_norm <- RMSE(pred2_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro2 <- denormalMm(pred2_lm, minvec, maxvec)
tabela2 <- data.frame(Real=teste_euro,Previsão=previsoes_euro2, Diferença=abs(teste_euro-previsoes_euro2))
RMSE_2 <- RMSE(previsoes_euro2, teste_euro.num)
RRMSE_2 <- RMSE_2/mean(teste_euro.num)*100

#Regressão múltipla sem as variáveis binárias(exceto breakfast) e dummy nas Estrelas
model3_lm <- lm(price_data ~ score_data+distance_to_center+breakfast+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75+Reviews_qty, data = train_1)
#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE
pred3_lm <- predict(model3_lm, newdata = teste_1)
RMSE_3_norm <- RMSE(pred3_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro3 <- denormalMm(pred3_lm, minvec, maxvec)
tabela3 <- data.frame(Real=teste_euro,Previsão=previsoes_euro3, Diferença=abs(teste_euro-previsoes_euro3))
RMSE_3 <- RMSE(previsoes_euro3, teste_euro.num)
RRMSE_3 <- RMSE_3/mean(teste_euro.num)*100

#Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas 
model4_lm <- lm(price_data ~ score_data+distance_to_center+breakfast+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75, data = train_1)
#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE
pred4_lm <- predict(model4_lm, newdata = teste_1)
RMSE_4_norm <- RMSE(pred4_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro4 <- denormalMm(pred4_lm, minvec, maxvec)
tabela4 <- data.frame(Real=teste_euro,Previsão=previsoes_euro4, Diferença=abs(teste_euro-previsoes_euro4))
RMSE_4 <- RMSE(previsoes_euro4, teste_euro.num)
RRMSE_4 <- RMSE_4/mean(teste_euro.num)*100


#Regressão múltipla com Cross-validation repeated k-fold com 5 repetições, sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas 
MR.control<-trainControl(method="repeatedcv", number=10, repeats=10)
model5_lm<-train(price_data ~ score_data+distance_to_center+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75+breakfast, data = train_1,method="lm", trControl=MR.control)

#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE
pred5_lm <- predict(model5_lm, newdata = teste_1)
RMSE_5_norm <- RMSE(pred5_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro5 <- denormalMm(pred5_lm, minvec, maxvec)
tabela5 <- data.frame(Real=teste_euro,Previsão=previsoes_euro5, Diferença=abs(teste_euro-previsoes_euro5))
RMSE_5 <- RMSE(previsoes_euro5, teste_euro.num)
RRMSE_5 <- RMSE_5/mean(teste_euro.num)*100

#Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas e distância 
model6_lm <- lm(price_data ~ score_data+distance_to_center_cut_1+distance_to_center_cut_2+distance_to_center_cut_3+breakfast+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75, data = train_1)

#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE
pred6_lm <- predict(model6_lm, newdata = teste_1)
RMSE_6_norm <- RMSE(pred6_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro6 <- denormalMm(pred6_lm, minvec, maxvec)
tabela6 <- data.frame(Real=teste_euro,Previsão=previsoes_euro6, Diferença=abs(teste_euro-previsoes_euro6))
RMSE_6 <- RMSE(previsoes_euro6, teste_euro.num)
RRMSE_6 <- RMSE_6/mean(teste_euro.num)*100

#Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas, distância e score
model7_lm <- lm(price_data ~ score_data_cut_1+score_data_cut_2+distance_to_center_cut_1+distance_to_center_cut_2+distance_to_center_cut_3+breakfast+Estrelas_0+Estrelas_0.25+Estrelas_0.5+Estrelas_0.75, data = train_1)

#Cria o modelo previsto e tabela comparativa entre o valor real, previsto e erro; Calcula o RMSE e RRMSE
pred7_lm <- predict(model7_lm, newdata = teste_1)
RMSE_7_norm <- RMSE(pred7_lm, as.numeric(unlist(teste_1[2])))

previsoes_euro7 <- denormalMm(pred7_lm, minvec, maxvec)
tabela7 <- data.frame(Real=teste_euro,Previsão=previsoes_euro7, Diferença=abs(teste_euro-previsoes_euro7))
RMSE_7 <- RMSE(previsoes_euro7, teste_euro.num)
RRMSE_7 <- RMSE_7/mean(teste_euro.num)*100


#Representa Graficamente o melhor modelo
plot(teste_euro.num, previsoes_euro6, main = "Preços das estadias em Bangkok: Previstos vs Reais", xlab = "Reais", ylab = "Previstos")
abline(0, 1)

#Cria tabela com valor real, previsto e intervalo de confiança de 95% para o melhor modelo
pred6_lm_max_min <- predict(model6_lm, newdata = teste_1, interval = 'confidence')
previsoes_euro_max_min <- data.frame(denormalMm(pred6_lm_max_min, minvec, maxvec))
minlm <- as.numeric(unlist(previsoes_euro_max_min[2]))
maxlm <- as.numeric(unlist(previsoes_euro_max_min[3]))
tabela8<- data.frame(Real=teste_euro.num,Previsão=previsoes_euro_max_min[1], Mínimo=minlm, Máximo=maxlm)

#Calcula a quantidade e percentagem de registos dentro do intervalo de confiança de 95%
total = 0
for(row in 1:nrow(tabela8)){
  real <- tabela8[row ,"Real"]
  min <- tabela8[row, "Mínimo"]
  max <- tabela8[row, "Máximo"]
  
  if (min <= real | real >= max){
    
    total = total + 1
  }  
}

totalpct <- total/131


######### MODELING: MODELO ÁRVORE DE DECISÃO #########


#Divide os dados em conjunto de treino e de conjunto de teste
set.seed(100)
index_1<-sample(1:nrow(df_arvore_decisao),round(nrow(df_arvore_decisao)*0.8))
train_1<-df_arvore_decisao[index_1,]
test_1<-df_arvore_decisao[-index_1,]


#Cria o modelo de Árvore de Decisão sem poda
model_tree<-rpart(formula=price_data~.,data=train_1,method="anova")

set.seed(100)

model_tree_party1<-as.party(model_tree)

#Sumário dos modelos sem poda
summary(model_tree)

#Plot da Árvore de Decisão com poda - Sem reamostragem - Com boxplot
plot(as.party(model_tree),"Árvore")

#Plot da Árvore de Decisão com poda - Sem reamostragem 
fancyRpartPlot(model_tree, caption = NULL)

#Outra representação
prp(model_tree,main="Àrvore de Decisão",type=2,fallen.leaves=TRUE,roundint=FALSE)

#Mostra como as divisões foram feitas
rpart.rules(model_tree,roundint=FALSE)

#Mostra o impacto de cada uma das variáveis no modelo
model_tree$variable.importance

#Mostra onde a poda deve ser feita - em CP
printcp(model_tree)

#Mostra graficamente onde a poda deve ser feita 
plotcp(model_tree)

#Mostra CP com xerror é menor
cp<-which.min(model_tree$cptable[,"xerror"])

#Cria o modelo de previsto
pred <- predict(model_tree, newdata = test_1)

#Comparação dos valores previstos com os valores reais
plot(test_1$price_data,pred,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

#Calculo do RMSE RRMSE antes de fazer a poda
model_tree_RMSE=RMSE(y_pred = pred, y_true = test_1$price_data)
model_tree_RRMSE=(model_tree_RMSE/mean(test_1$price_data))*100

#Poda da Árvore
model_tree_new<-prune(model_tree,cp=0.021346)

#Mostra a distribuição depois da poda
model_tree_party2<-as.party(model_tree_new)

#Representação Gráfica da Árvore depois da poda - Com boxplot
plot(as.party(model_tree_new),"Árvore")

#Outra representação
prp(model_tree_new,main="Àrvore de Decisão",type=2,fallen.leaves=TRUE,roundint=FALSE)

#Representação Gráfica da Árvore depois da poda
fancyRpartPlot(model_tree_new, caption = NULL)

#Mostra o impacto de cada uma das variáveis no modelo
model_tree_new$variable.importance

#Mostra o CP
printcp(model_tree_new)

#Sumário dos modelos com poda
summary(model_tree_new)

#Cria modelo previsto da árvore com poda
pred2 <- predict(model_tree_new, newdata = test_1)

#Comparação dos valores previstos com os valores reais
plot(test_1$price_data,pred2,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

#Cria tabela de comparação entre o valor do real, previsto e erro
tabela<-data.frame(VReais=test_1$price_data,VPrevistos=pred2)
tabela$error<-with(tabela,test_1$price_data-pred2)

#RMSE e RRMSE da Árvore com poda
model_tree_new_RMSE=RMSE(y_pred = pred2, y_true = test_1$price_data)
model_tree_new_RRMSE=(model_tree_new_RMSE/mean(test_1$price_data))*100

Model_tree.results.euro<-denormalMm(test_1$price_data, minvec, maxvec)
round(Model_tree.results.euro, 2)

Model_tree.predict.euro<-denormalMm(pred, minvec, maxvec)
round(Model_tree.predict.euro, 2)

results <- data.frame(actual = test_1$price_data, prediction = pred,Error = test_1$price_data-pred ,Test.euro = Model_tree.results.euro, Predict.euro = Model_tree.predict.euro, Diferença=abs(Model_tree.predict.euro-Model_tree.results.euro))


#Modelo da Árvore com os parâmetros optimizados
model_tree_best<-rpart(formula=price_data~.,data=train_1,method="anova", control=rpart.control(xval=10,maxdepth =4,minsplit = 60,minbucket=45))

set.seed(100)

model_tree_party1<-as.party(model_tree_best)

#Sumário dos modelos sem poda
summary(model_tree_best)

#Plot da Árvore de Decisão com poda - Sem reamostragem - Com boxplot
plot(as.party(model_tree_best),"Árvore")

#Plot da Árvore de Decisão com poda - Sem reamostragem 
fancyRpartPlot(model_tree_best, caption = NULL)

#Outra representação
prp(model_tree_best,main="Àrvore de Decisão",type=2,fallen.leaves=TRUE,roundint=FALSE)

#Mostra como as divisões foram feitas
rpart.rules(model_tree_best,roundint=FALSE)

#Mostra o impacto de cada uma das variáveis no modelo
model_tree_best$variable.importance

#Mostra onde a poda deve ser feita - em CP
printcp(model_tree_best)

#Mostra CP com xerror é menor
cp<-which.min(model_tree_best$cptable[,"xerror"])

#Cria o modelo de previsto
pred3 <- predict(model_tree_best, newdata = test_1)

#Comparação dos valores previstos com os valores reais
plot(test_1$price_data,pred3,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

#Calculo do RMSE e RRMSE
model_tree_best_RMSE=RMSE(y_pred = pred3, y_true = test_1$price_data)
model_tree_best_RRMSE=(model_tree_best_RMSE/mean(test_1$price_data))*100

Model_tree.results.euro2<-denormalMm(test_1$price_data, minvec, maxvec)
round(Model_tree.results.euro2, 2)

Model_tree.predict.euro2<-denormalMm(pred, minvec, maxvec)
round(Model_tree.predict.euro2, 2)

results2 <- data.frame(actual = test_1$price_data, prediction = pred3,Error = test_1$price_data-pred3 ,Test.euro = Model_tree.results.euro2, Predict.euro = Model_tree.predict.euro2, Diferença=abs(Model_tree.predict.euro2-Model_tree.results.euro2))

#Cria modelo de Árvore com Bagging
cv.control<-trainControl(method="repeatedcv",number=10,repeats=10,savePredictions="final")
model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=200,trControl=cv.control)

#tentativa 1
#model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=100,trControl=cv.control)

#tentativa 2
#model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=200,trControl=cv.control)

#tentativa 3
#model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=300,trControl=cv.control)

#tentativa 4
#model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=400,trControl=cv.control)

#tentativa 5
#model_bag<-train(price_data ~.,data=train_1,method="treebag",nbagg=500,trControl=cv.control)

set.seed(100)

model_bag

plot(varImp(model_bag),main="Importância das Variáveis com o modelo de árvore com bagging")

#Cria o modelo de previsão
model_tree_previsao<-predict(model_bag, newdata=test_1)

#Faz a comparação dos valores reais e previstos - método Bagging
plot(test_1$price_data,model_tree_previsao,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

#Cria tabela dos valores reais, previtos e erro
tabela1<-data.frame(VReais=test_1$price_data,VPrevistos=model_tree_previsao)
tabela$error1<-with(tabela,test_1$price_data-model_tree_previsao)

#RMSE e RRMSE do modelo em Árvore com Bagging 
model_tree_bagging_RMSE=RMSE(y_pred = model_tree_previsao, y_true = test_1$price_data)
model_tree_bagging_RRMSE=(model_tree_bagging_RMSE/mean(test_1$price_data))*100

Model_tree.results.euro3<-denormalMm(test_1$price_data, minvec, maxvec)
round(Model_tree.results.euro3, 2)

Model_tree.predict.euro3<-denormalMm(model_tree_previsao, minvec, maxvec)
round(Model_tree.predict.euro3, 2)

results3 <- data.frame(actual = test_1$price_data, prediction = model_tree_previsao,Error = test_1$price_data-model_tree_previsao ,Test.euro = Model_tree.results.euro3, Predict.euro = Model_tree.predict.euro3, Diferença=abs(Model_tree.predict.euro3-Model_tree.results.euro3))


#Cria modelo de Árvore com Boosting(GB) 
cv.control<-trainControl(method="repeatedcv",number=10,repeats=10,savePredictions="final")
#tentativa final - melhor modelo
tune_gbm<-expand.grid(interaction.depth=2,n.trees=300,shrinkage=0.005,n.minobsinnode=10)
model_boosting<-train(price_data~.,data=train_1,method="gbm",metric="RMSE",trControl=cv.control,tuneGrid=tune_gbm)

#1ºtentativa
#tune_gbm<-expand.grid(interaction.depth=c(1,2,3,4,5),n.trees=500,shrinkage=0.001,n.minobsinnode=c(5,6,7,8,9,10,11,12,13,14,15))

#2ºtentativa
#tune_gbm<-expand.grid(interaction.depth=2,n.trees=c(100,200,300,400,500,600,700,800,900,1000),shrinkage=c(0.001,0.005,0.01,0.05,0.1),n.minobsinnode=c(8,9,10,11,12))

#3ºtentativa
#tune_gbm<-expand.grid(interaction.depth=2,n.trees=300,shrinkage=0.005,n.minobsinnode=c(8,9,10,11,12))

set.seed(100)

model_boosting

#Representa graficamente o modelo com Boosting(GB)
# O plot só funcionará para para os modelos que tiverem em que os parâmetros estão agrupados em lista, sendo que nosso melhor modelo não está.
# Este plot só funciona para as tentativas 1, 2 e 3
#plot(model_boosting)

#Cria o modelo previsto
model_tree_previsao2<-predict(model_boosting, newdata=test_1)

#Faz a comparação dos valores reais e previstos - método Boosting(GB)
plot(test_1$price_data,model_tree_previsao2,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

plot(varImp(model_boosting),main="Importância das Variáveis com o modelo de árvore obtido com Boosting(GBM)")

#Cria Tabela comparativa entre o valor real, previsto e erro. 
tabela2<-data.frame(VReais=test_1$price_data,VPrevistos=model_tree_previsao2)
tabela$error2<-with(tabela2,test_1$price_data-model_tree_previsao2)

#RMSE do modelo em Árvore com Boosting(GB)
model_tree_gb_RMSE=RMSE(y_pred = model_tree_previsao2, y_true = test_1$price_data)
model_tree_gb_RRMSE=(model_tree_gb_RMSE/mean(test_1$price_data))*100

Model_tree.results.euro3<-denormalMm(test_1$price_data, minvec, maxvec)
round(Model_tree.results.euro3, 2)

Model_tree.predict.euro3<-denormalMm(model_tree_previsao2, minvec, maxvec)
round(Model_tree.predict.euro3, 2)

results4 <- data.frame(actual = test_1$price_data, prediction = model_tree_previsao2,Error = test_1$price_data-model_tree_previsao2 ,Test.euro = Model_tree.results.euro3, Predict.euro = Model_tree.predict.euro3, Diferença=abs(Model_tree.predict.euro3-Model_tree.results.euro3))


#Cria modelo de Árvore de Decisão com RandomForest
cv.control<-trainControl(method="repeatedcv",number=10,repeats=10,savePredictions="final")
tune_forest<-expand.grid(mtry=6,splitrule="variance",min.node.size=9)
model_forest<-train(price_data~.,data=train_1,method="ranger",num.trees=1000,metric="RMSE",tuneGrid=tune_forest,importance="impurity",trControl=cv.control)

#1ºtentativa
#tune_forest<-expand.grid(mtry=c(1,2,3,4,5,6),splitrule="variance",min.node.size=c(1,2,3,4,5,6,7,8,9))

model_forest

set.seed(100)

#Representa Graficamente o modelo de Árvore de Decisão com RandomForest
#Funcionará apenas para a primeira tentativa. 
#plot(model_forest)

#Cria o modelo previsto
model_tree_previsao4<-predict(model_forest, newdata=test_1)

#Faz a comparação dos valores reais e previstos - com RandomForest
plot(test_1$price_data,model_tree_previsao4,main="Árvore model_tree: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

plot(varImp(model_forest),main="Importância das Variáveis com o modelo de árvore obtido com RandomForest")

#Cria tabela comparativa entre o valor real, previsto e erro -com RandomForest
tabela4<-data.frame(VReais=test_1$price_data,VPrevistos=model_tree_previsao4)
tabela$error4<-with(tabela4,test_1$price_data-model_tree_previsao4)

#RMSE do modelo de Árvore de Decisão com RandomForest
model_forest_RMSE=RMSE(y_pred = model_tree_previsao4, y_true = test_1$price_data)
model_forest_RRMSE=(model_forest_RMSE/mean(test_1$price_data))*100

Model_tree.results.euro4<-denormalMm(test_1$price_data, minvec, maxvec)
round(Model_tree.results.euro4, 2)

Model_tree.predict.euro4<-denormalMm(model_tree_previsao, minvec, maxvec)
round(Model_tree.predict.euro4, 2)

results5 <- data.frame(actual = test_1$price_data, prediction = model_tree_previsao,Error = test_1$price_data-model_tree_previsao ,Test.euro = Model_tree.results.euro2, Predict.euro = Model_tree.predict.euro2, Diferença=abs(Model_tree.predict.euro2-Model_tree.results.euro2))

#Cria DF comparativo com todos os RMSEs dos modelos em Árvore 
tabarv=rbind(data.frame(model = "Árvore Inicial", 
                 RMSE = round(model_tree_RMSE, 5)),
      data.frame(model = "Árvore com Poda", 
                 RMSE = round(model_tree_new_RMSE, 5)),
      data.frame(model = " Melhor Árvore (CART)", 
                 RMSE = round(model_tree_best_RMSE, 5)),
      data.frame(model = "Árvore com bagging", 
                 RMSE = round(model_tree_bagging_RMSE, 5)),
      data.frame(model = "Árvore com boosting(gb)", 
                 RMSE = round(model_tree_gb_RMSE, 5)),
      data.frame(model = "Árvore com randomforests", 
                 RMSE = round(model_forest_RMSE, 5)))


######### EVALUATION: TABELA COMPARATIVA DE TODOS OS RMSEs #########


modelos <- rbind(data.frame(model = "Árvore Inicial", 
                 RMSE = round(model_tree_RMSE, 5),RRMSE=round(model_tree_RRMSE,5)),
      data.frame(model = "Árvore com Poda", 
                 RMSE = round(model_tree_new_RMSE, 5),RRMSE=round(model_tree_new_RRMSE,5)),
      data.frame(model = "Melhor Árvore (CART)", 
                 RMSE = round(model_tree_best_RMSE, 5),RRMSE=round(model_tree_best_RRMSE,5)),
      data.frame(model = "Árvore com bagging", 
                 RMSE = round(model_tree_bagging_RMSE, 5),RRMSE=round(model_tree_bagging_RRMSE,5)),
      data.frame(model = "Árvore com boosting(gb)", 
                 RMSE = round(model_tree_gb_RMSE, 5),RRMSE=round(model_tree_gb_RRMSE,5)),
      data.frame(model = "Árvore com randomforests", 
                 RMSE = round(model_forest_RMSE, 5),RRMSE=round(model_forest_RRMSE,5)),
      data.frame(model = "Rede Neuronal", 
                 RMSE = round(RMSE_Neural_Model_1, 5),RRMSE=round(Relative_RMSE_RMSE_Neural_Model_1,5)),
      data.frame(model = "Regressão Múltipla com todas as variáveis", 
                 RMSE = round(RMSE_1_norm, 5),RRMSE=round(RRMSE_1,5)),
      data.frame(model = "Regressão múltipla com todas as variáveis e dummy nas Estrelas", 
                 RMSE = round(RMSE_2_norm, 5),RRMSE=round(RRMSE_2,5)),
      data.frame(model = "Regressão múltipla sem as variáveis binárias(exceto breakfast) e dummy nas Estrelas", 
                 RMSE = round(RMSE_3_norm, 5),RRMSE=round(RRMSE_3,5)),
      data.frame(model = "Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas", 
                 RMSE = round(RMSE_4_norm, 5),RRMSE=round(RRMSE_4,5)),
      data.frame(model = "Regressão múltipla com Cross-validation repeated k-fold com 5 repetições, sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas", 
                 RMSE = round(RMSE_5_norm, 5),RRMSE=round(RRMSE_5,5)),
      data.frame(model = "Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas e distância", 
                 RMSE = round(RMSE_6_norm, 5),RRMSE=round(RRMSE_6,5)),
      data.frame(model = "Regressão múltipla sem as variáveis binárias(exceto breakfast) nem quantidade de reviews e dummy nas Estrelas, distância e score", 
                 RMSE = round(RMSE_7_norm, 5),RRMSE=round(RRMSE_7,5)),
      data.frame(model = "Rede Neuronal com Backpropagation", 
                 RMSE = round(RMSE_Modelo_Rede_NeuronaL_Backprop, 5),RRMSE=round(Relative_RMSE_Rede_Neuronal_Test_Backprop,5))
      )
View(modelos)







