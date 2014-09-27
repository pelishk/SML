
library(RCurl)
library(reshape2)

IFE<- "C:/Users/manuel.barrera/Desktop/LN test"
setwd(IFE)

getwd()

descargaLN<-function(url="http://listanominal.ife.org.mx/ubicamodulo/PHP/int_est_sal_txt.php",ws=''){
  
  
  if(ws=='') {
    ws<-getwd()
  }else {
    ws<-ws
  }
  
  
  if(!url.exists(url)){
    stop('\n LA URL es equivocada, corr?gela')
  } else {
    
    setwd(ws)

    
    sexo<-as.list(
      parse(text=paste(
        'postForm("http://listanominal.ife.org.mx/ubicamodulo/PHP/int_est_sal_txt.php",
               .params=list(edo = paste(',1:32,'), tipo = "sx"),
               .encoding = "UTF-8",binary = NA,style = "POST", .checkParams = TRUE)',
        sep=''
      )
      )
    )
  sexodatos<- lapply(sexo,eval)
  sexodatos<-gsub('<br>','\n',sexodatos) 
  sexopego<-as.list(parse(text=paste(
    'write.table(sexodatos[[',1:32,']],paste("e",',1:32,',".txt",sep="")
                ,sep="|",append = FALSE, quote = F,
                na = " ", dec = ".", row.names = F,
                col.names = F, qmethod="double",
                fileEncoding = "UTF-8")'
    )))
  lapply(sexopego,eval)  
    
  
    edad<-as.list(
      parse(text=paste(
        'postForm("http://listanominal.ife.org.mx/ubicamodulo/PHP/int_est_sal_txt.php",
        .params=list(edo = paste(',1:32,'), tipo = "ge"),
        .encoding = "UTF-8",binary = NA,style = "POST", .checkParams = TRUE)',
        sep=''
      )
    )
      )  
    edaddatos<-lapply(edad,eval)
    edaddatos<-gsub('<br>','\n',edaddatos)
    edaddatos<-gsub('&oacute;','o',edaddatos)
    edadpego<-as.list(parse(text=paste(
      'write.table(edaddatos[[',1:32,']],paste("e",',1:32,',".txt",sep="")
                ,sep="|",append = FALSE, quote = F,
      na = " ", dec = ".", row.names = F,
      col.names = F, qmethod="double",
      fileEncoding = "UTF-8")'
    )))
    lapply(edaddatos,eval)  
 
  }  
}



Sys.time()
descargaLN()
Sys.time()

# Hace una lista de los archivos en la carpeta elegida

archivos<- list.files(IFE)

# separa la lista entre los que inician con "e" y los que inician con "s". Crea
# la lista de los archivos de "sexo" y "edad".

subcadena<-substr(archivos, 1, 1)

listado.s<-subset(archivos, subcadena=="s", names(archivos))
listado.e<-subset(archivos, subcadena=="e", names(archivos))

# Lee los arhivos y los agrega a un data frame conglomerado para "sexo" y "edad"

bysexo<- do.call("rbind", lapply(listado.s, read.table, header=T, sep="|"))
byedad<- do.call("rbind", lapply(listado.e, read.table, header=T, sep="|"))

# Crea las llaves. Cada Estado tiene secciones únicas, aún así, se agrega una 
# validación.

bysexo$llave<- bysexo$edo*1000000 + bysexo$secc
byedad$llave<- byedad$edo*1000000 + byedad$secc

# Pasa a formato "wide" la base de edad.

byedad <- dcast(byedad, edo + dto + mun + secc + llave ~ gpo_edad, value.var="lista")

#####################################################################################
##### Aquí tiene que haber un if para saber si todas las llaves son únicas###########

duplicated(bysexo$llave)

duplicated(byedad$llave)

##### Sólo debemos saber si el vector contiene únicamente "FALSE"
#####################################################################################

# Hace el data frame final y exporta el .csv

LN<- subset(bysexo, llave>0, c("llave","edo","dto","mun","secc", "hom_ln", "muj_ln"))

LN<- merge(LN, subset(byedad, llave>0, names(byedad)[5:17]), by="llave", all=T)

write.csv(LN, paste("LN_", Sys.Date(),".csv", sep=""))

##### taller
