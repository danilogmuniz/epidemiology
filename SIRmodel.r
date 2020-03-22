#==========================================================
#Simulações numéricas no modelo SIR. Afinal, porque nao?
#rm(list=ls()) #limpando a area de trabalho
library(deSolve) #carregando o pacotes
#----------------------------------------------------------
#soh o bom e velho modelo SIR
#mais informacoes aqui:
#https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

SIR = function(t, y, parms)
{
  with(as.list(c(y, parms)), 
       {
         N = S+I+R
         
         dS = -beta*I*S/N
         
         dI = beta*I*S/N - gamma*I
         
         dR = gamma*I
         
         return(list(c(S = dS, I = dI, R = dR)))
       })
}
#corzinhas para os graficos
cols = c("dodgerblue","red","grey10")
#----------------------------------------------------------

#vamos rodar dois cenarios

#aqui, cada pessoa infecta, em media, uma pessoa por dia
t1 = ode(y = c(S=1e6, I = 3, R = 0),
         times = seq(from=1, to=200, by=0.1),
         func = SIR,
         parms = c(beta=1, gamma=1/15))
head(t1)

#convertendo os dados para porcentagem
dat1 = as.data.frame(t1)
head(dat1)
dat1[,-1] = dat1[,-1]/(1e6+3)*100
head(dat1)

#pico da infeccao
peak1 = which.max(t1[,"I"])
t1[peak1,]
round(dat1[peak1,])

#700 mil infectatos no apice da epidemia. O.O !!!

#aqui, eh soh 1/5, ou uma pessoa a cada 5 dias
t2 = ode(y = c(S=1e6, I = 3, R = 0),
         times = seq(from=1, to=200, by=0.1),
         func = SIR,
         parms = c(beta=.2, gamma=1/15))

head(t2)
#convertendo os dados para porcentagem
dat2 = as.data.frame(t2)
dat2[,-1] = dat2[,-1]/(1e6+3)*100
head(dat2)

#pico da epidemia
peak2 = which.max(t2[,"I"])
t2[peak2,]
dat2[peak2,]

#300 mil infectados, ainda preocupante!

#vamos desenhar graficos bonitinhos
#cenario 1
png("SIR1.png", height=12, width=16, units="cm",
    res=450)
{
  par(las=1, bty="l", mgp=c(3,1,0), mar=c(5,5,3, 2),
      oma=c(0,0,0,0),
      cex.lab=1.5, mfrow=c(1,1), cex.main=2)
  cols = c("dodgerblue","red","grey10")
  matplot(x=dat1$time,y=dat1[,-1], type="l", lty=1, 
          col=cols, lwd=2, xlab="Tempo (dias)", 
          ylab="Porcentagem da população",
          main="Cenário de alto contato")
  legend("right", col=cols, lty=1, lwd=2, bty="n",
         legend=c("Saudáveis", "Infectados (doentes)", "Recuperados"))
  
}
dev.off()

#cenario 2
png("SIR2.png", height=12, width=16, units="cm",
    res=450)
{
  par(las=1, bty="l", mgp=c(3,1,0), mar=c(5,5,3, 2),
      oma=c(0,0,0,0),
      cex.lab=1.5, mfrow=c(1,1), cex.main=2)
  cols = c("dodgerblue","red","grey10")
  matplot(x=dat2$time,y=dat2[,-1], type="l", lty=1, 
          col=cols, lwd=2, xlab="Tempo (dias)", 
          ylab="Porcentagem da população",
          main="Cenário de contato reduzido")
  legend("left", col=cols, lty=1, lwd=2, bty="n",
         legend=c("Saudáveis", "Infectados (doentes)", "Recuperados"))
  
}
dev.off()

#comparando cenarios
library(plotrix)
library(scales)
fcols = alpha(c("red", "dodgerblue"), 0.75)
png("SIR1x2.png", height=12, width=16, units="cm",
    res=450)
{
  
  par(las=1, bty="l", mgp=c(3,1,0), mar=c(5,5,3, 2),
      oma=c(0,0,0,0),
      cex.lab=1.5, mfrow=c(1,1), cex.main=2)
  
  plot(0, type="n", ylim=c(0,80), xlim=c(0,200),
     xlab="Tempo (dias)", 
     ylab="Porcentagem de infectados",
     main="Comparação entre cenários")
  
  #altas gambiarras pra desenhar uma area colorida
  polygon(x=c(0, dat1$time,200),
          y=c(0, dat1$I, 0), col = fcols[1], border=NA)
  
  polygon(x=c(0, dat2$time,200),
          y=c(0, dat2$I, 0), col = fcols[2], border=NA)

  legend("topright", col=fcols, pch=15, bty="n",
         legend=c("Alto contato", 
                "Contato reduzido"))
}
dev.off()

#um grafico soh com os dois cenarios
png("SIR.png", height=24, width=16, units="cm",
    res=450)
{
par(las=1, bty="l", mgp=c(3,1,0), mar=c(4,5,3, 2),
    oma=c(2,2,0,0),
    cex.lab=2, mfrow=c(2,1), cex.main=2)
cols = c("dodgerblue","red","grey10")
matplot(y=t1[,-1],x=t1[,1], type="l", lty=1, 
        col=cols, yaxt="n",
        lwd=2, xlab="", ylab="",
        main="Alto contato")
legend("right", col=cols, lty=1, lwd=2, bty="n",
        legend=c("Saudáveis", "DOENTES", "Recuperados"))

axis(side=2, at=seq(from=0, to=10, by=2)*1e5, 
     labels = c("zero", "200 mil", "400 mil", "600 mil",
                "800 mil", "1 milhão"))

matplot(y=t2[,-1],x=t2[,1], type="l", lty=1, 
        col=c("dodgerblue","red","grey10"), yaxt="n",
        lwd=2, xlab="", ylab="",
        main="Contato reduzido")

axis(side=2, at=seq(from=0, to=10, by=2)*1e5, 
    labels = c("zero", "200 mil", "400 mil", "600 mil",
                "800 mil", "1 milhão"))

legend("right", col=cols, lty=1, lwd=2, bty="n",
       legend=c("Saudáveis", "DOENTES", "Recuperados"))

par(las=3)
mtext(side=2, outer=TRUE, cex=2.5, line = 0, 
      text = "Número de pessoas")
par(las=1)
mtext(side=1, outer=TRUE, cex=2.5, line = 0, 
      text = "Tempo (dias)")
}
dev.off()

#==========================================================
