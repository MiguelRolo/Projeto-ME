# Análise Discritiva e Exploratória de Dados

# Resumo Estatístico (média, mediana, mínimo, máximo, etc)

library(readr)
student_mat_v2 <- read_delim("student-mat-v2.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(student_mat_v2)

#summary(student_mat_v2)


## Variáveis qualitativas - Tabelas de frequência (Frequências relativas/absolutas/acumuladas)

## Escola (school)


(ni.1 = table(student_mat_v2$school))# frequências absolutas 
(fi.1 = prop.table(ni.1))            # frequências relativas 
(Ni.1 = cumsum(ni.1))                # frequências absolutas acumuladas 
(Fi.1 = cumsum(fi.1))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.school = data.frame(i=1:nrow(ni.1), 
                                   xi=names(ni.1), 
                                   ni=as.integer(ni.1), 
                                   fi=as.numeric(fi.1), 
                                   Ni=as.integer(Ni.1), 
                                   Fi=as.numeric(Fi.1))) 

## Sexo (sex)


(ni.2 = table(student_mat_v2$sex))  # frequências absolutas 
(fi.2 = prop.table(ni.2))            # frequências relativas 
(Ni.2 = cumsum(ni.2))                # frequências absolutas acumuladas 
(Fi.2 = cumsum(fi.2))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.sex = data.frame(i=1:nrow(ni.2), 
                                        xi=names(ni.2), 
                                        ni=as.integer(ni.2), 
                                        fi=as.numeric(fi.2), 
                                        Ni=as.integer(Ni.2), 
                                        Fi=as.numeric(Fi.2))) 

## Idade (age)


(ni.3 = table(student_mat_v2$age))  # frequências absolutas 
(fi.3 = prop.table(ni.3))            # frequências relativas 
(Ni.3 = cumsum(ni.3))                # frequências absolutas acumuladas 
(Fi.3 = cumsum(fi.3))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.age = data.frame(i=1:nrow(ni.3), 
                                     xi=names(ni.3), 
                                     ni=as.integer(ni.3), 
                                     fi=as.numeric(fi.3), 
                                     Ni=as.integer(Ni.3), 
                                     Fi=as.numeric(Fi.3))) 

## Faltas (absences)


(ni.4 = table(student_mat_v2$absences))  # frequências absolutas 
(fi.4 = prop.table(ni.4))            # frequências relativas 
(Ni.4 = cumsum(ni.4))                # frequências absolutas acumuladas 
(Fi.4 = cumsum(fi.4))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.absences = data.frame(i=1:nrow(ni.4), 
                                     xi=names(ni.4), 
                                     ni=as.integer(ni.4), 
                                     fi=as.numeric(fi.4), 
                                     Ni=as.integer(Ni.4), 
                                     Fi=as.numeric(Fi.4))) 

## Horas de estudo (studytime)

student_mat_v2$studytime <- factor(student_mat_v2$studytime,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("< 2 horas", "2 a 5 horas", "5 a 10 horas", "> 10 horas"))

(ni.5 = table(student_mat_v2$studytime))  # frequências absolutas 
(fi.5 = prop.table(ni.5))            # frequências relativas 
(Ni.5 = cumsum(ni.5))                # frequências absolutas acumuladas 
(Fi.5 = cumsum(fi.5))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.studytime = data.frame(i=1:nrow(ni.5), 
                                          xi=names(ni.5), 
                                          ni=as.integer(ni.5), 
                                          fi=as.numeric(fi.5), 
                                          Ni=as.integer(Ni.5), 
                                          Fi=as.numeric(Fi.5))) 

## Reprovações de ano (failures)

(ni.6 = table(student_mat_v2$failures))  # frequências absolutas 
(fi.6 = prop.table(ni.2))            # frequências relativas 
(Ni.6 = cumsum(ni.2))                # frequências absolutas acumuladas 
(Fi.6 = cumsum(fi.2))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.failures = data.frame(i=1:nrow(ni.6), 
                                           xi=names(ni.6), 
                                           ni=as.integer(ni.6), 
                                           fi=as.numeric(fi.6), 
                                           Ni=as.integer(Ni.6), 
                                           Fi=as.numeric(Fi.6))) 


## Aulas Extra Pagas (paid)

(ni.7 = table(student_mat_v2$paid))  # frequências absolutas 
(fi.7 = prop.table(ni.7))            # frequências relativas 
(Ni.7 = cumsum(ni.7))                # frequências absolutas acumuladas 
(Fi.7 = cumsum(fi.7))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.paid = data.frame(i=1:nrow(ni.7), 
                                          xi=names(ni.7), 
                                          ni=as.integer(ni.7), 
                                          fi=as.numeric(fi.7), 
                                          Ni=as.integer(Ni.7), 
                                          Fi=as.numeric(Fi.7))) 

## Tempo Livre (freetime)

student_mat_v2$freetime <- factor(student_mat_v2$freetime,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Muito Baixo", "Baixo", "Normal", "Alto", "Muito Alto"))

(ni.8 = table(student_mat_v2$freetime))  # frequências absolutas 
(fi.8 = prop.table(ni.8))            # frequências relativas 
(Ni.8 = cumsum(ni.8))                # frequências absolutas acumuladas 
(Fi.8 = cumsum(fi.8))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.freetime = data.frame(i=1:nrow(ni.8), 
                                      xi=names(ni.8), 
                                      ni=as.integer(ni.8), 
                                      fi=as.numeric(fi.8), 
                                      Ni=as.integer(Ni.8), 
                                      Fi=as.numeric(Fi.8))) 


## Notas 1º Semestre (G1)

(ni.9 = table(student_mat_v2$G1))  # frequências absolutas 
(fi.9 = prop.table(ni.9))            # frequências relativas 
(Ni.9 = cumsum(ni.9))                # frequências absolutas acumuladas 
(Fi.9 = cumsum(fi.9))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.G1 = data.frame(i=1:nrow(ni.9), 
                                          xi=names(ni.9), 
                                          ni=as.integer(ni.9), 
                                          fi=as.numeric(fi.9), 
                                          Ni=as.integer(Ni.9), 
                                          Fi=as.numeric(Fi.9))) 


## Notas 2º Semestre (G2)

(ni.10 = table(student_mat_v2$G2))  # frequências absolutas 
(fi.10 = prop.table(ni.10))            # frequências relativas 
(Ni.10 = cumsum(ni.10))                # frequências absolutas acumuladas 
(Fi.10 = cumsum(fi.10))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.G2 = data.frame(i=1:nrow(ni.10), 
                                    xi=names(ni.10), 
                                    ni=as.integer(ni.10), 
                                    fi=as.numeric(fi.10), 
                                    Ni=as.integer(Ni.10), 
                                    Fi=as.numeric(Fi.10))) 





# Construção de tabela de frequências através de classes

# Variável estatística: absences 

cortes.11 = c(0, 5, 10, 20, 40, 75) #limites das classes 

#transformar os dados em classes 
(classes.11 = cut(x=student_mat_v2$absences, breaks=cortes.11, include.lowest=TRUE, 
right=TRUE)) 

#colocar na tabela de dados as classes 
#student_mat_v2$absences_classes = classes.11 

#View(student_mat_v2) 

(ni.11 = table(classes.11))            # frequências absolutas 
(fi.11 = prop.table(ni.11))            # frequências relativas 
(Ni.11 = cumsum(ni.11))                # frequências absolutas acumuladas 
(Fi.11 = cumsum(fi.11))                # frequências relativas acumuladas 

# tabelas de frequências 
(tabela.frequencias.absences.classes = data.frame(i=1:nrow(ni.11), 
                                   classei=names(ni.11), 
                                   ni=as.integer(ni.11), 
                                   fi=as.numeric(fi.11), 
                                   Ni=as.integer(Ni.11), 
                                   Fi=as.numeric(Fi.11))) 



# Gráficos (Circulares, Barras, Histogramas)

# c("blue","pink","yellow") - to change colors on graphics

# Gráfico de Barras (School) - Frequências Absolutas

barplot(ni.1, col=2:3, xlab="Escola", ylab="Frequências Absolutas", ylim=c(0, 400))  # força o eixo y a ir até 400

#Gráfico de Barras (School) - Frequeências Relativas (%)

barplot(prop.table(ni.1) * 100, 
        col= 4:5, 
        xlab="Escola", 
        ylab="Percentagem (%)", 
        ylim=c(0, 100),
        main="Distribuição Percentual por Escola")


# Gráfico Circular (School) - Frequências Absolutas

pie(ni.1, labels=paste(ni.1),  col=rainbow(5), main="Número de alunos em cada escola") 
legend("topright", legend=names(ni.1), fill=rainbow(5))

# Gráfico Circular (School) - Frequências relativas (%)

pie(fi.1, labels=paste(round(fi.1 * 100, 2), "%"),  col=c("green","red"), 
    main="Percentagem de alunos em cada escola") 
legend("topright", legend=names(ni.1), fill=c("green","red")) 




# Histograma (Absences) 

hist(student_mat_v2$absences, 
     breaks=cortes.11, 
     include.lowest=TRUE, 
     right=TRUE,  
     freq=TRUE, 
     main="Histograma de Faltas", 
     xlab="Classes", 
     ylab="Frequências Absolutas", 
     col=2, 
     xaxt="n")                  # para poder definir o eixo dos xx 
     axis(side=1, at=cortes.11)  # definir os valores para o eixo dos xx igual às classes 
     
     

 
    

     library(kableExtra)
     
     kable(tabela.frequencias.school, "html", 
           caption = "Frequency Table of Absences") %>%
       kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                     full_width = F)






