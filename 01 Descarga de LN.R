
library(RCurl)

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
  sexodatos<-  lapply(sexo,eval)
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


archivos<- list.files(IFE)

subcadena<-substr(archivos, 1, 1)

listado.s<-subset(archivos, subcadena=="s", names(archivos))
listado.e<-subset(archivos, subcadena=="e", names(archivos))

bysexo<- do.call("rbind", lapply(listado.s, read.table, header=T, sep="|"))
byedad<- do.call("rbind", lapply(listado.e, read.table, header=T, sep="|"))


