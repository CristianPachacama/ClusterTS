# Correccion de Estaciones Vazoes a Mensuales --------------

library(dplyr)
EstacionesMes=data.frame(Estacion=names(BDDv)[-c(1,2)],stringsAsFactors = F)

# Ejemplo
x = "advs(122)"
pos1=regexpr('\\(',x)
pos2=regexpr('\\)',x)
substr(x, pos1+1,pos2-1)

#Correccion de Lista de Estaciones VAZOES

pos=gregexpr('\\([0-9]+',EstacionesMes$Estacion)
pos1=unlist(pos)+1

pos2=c()
for(i in 1:length(pos)){
  a=attr(pos[[i]],"match.length")
  pos2[i]=pos1[i]+a-2
}
pos2

Codigo = as.numeric(substr(EstacionesMes$Estacion, pos1,pos2))
Codigo

EstacionesMes$Codigo_ONS = Codigo

# Juntar por Codigo con Estaciones Diarias
aux = left_join(EstacionesMes,vazoes_code,by="Codigo_ONS")
names(aux)[1] = "Estacion"
aux$Tipo="Vazoes"

vazoes_code=aux[,c(-3)]
#Guardar para usar en APP -------
getwd()
save(vazoes_code,file="Data/VazoesCode.RData")
