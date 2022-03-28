'#######################
'# Tarea 3 - Modelos ARIMA  #
'#######################

' Preparación

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 3"

wfopen "PBI2007t.xlsx"  'Abrir archivo
pagestruct(freq=q, start=1980.1)
smpl @all 'Muestra: todos los datos

' Pregunta 2: ARIMA datos trimestrales

' Inciso A

graph g1.line pbi ' Evidencia de tendencia, estacionalidad y heterocedasticidad.
graph g1d.line(m) d(pbi,1,4) dlog(pbi,1,4)
genr ly1s4 = dlog(pbi,1,4)
ly1s4.correl(20) ' AR(1) SAR(4) o MA(1) SMA(4)

graph g2.line cpri ' Evidencia de tendencia, estacionalidad y heterocedasticidad.
graph g2d.line(m) d(cpri) dlog(cpri) d(cpri,1,4) dlog(cpri,1,4)
genr ly2s4 = dlog(cpri,1,4)
ly2s4.correl(20) 'SAR(4) o SMA(4)

graph g3.line inv ' Evidencia de tendencia y estacionalidad.
graph g3d.line(m) d(inv) d(inv,1,4) 
genr y3s4 = d(inv,1,4)
y3s4.correl(20)  'SAR(4) o SMA(4)

graph g4.line expo ' Fuerte evidencia de tendencia. También estacionalidad y heterocedasticidad.
graph g4d.line(m) d(expo) d(expo,1,4) dlog(expo) dlog(expo,1,4) 
genr ly4s4 = dlog(expo,1,4)
ly4s4.correl(20) 'SAR(4) o SMA(4)

graph g5.line impo ' Fuerte evidencia de tendencia. También estacionalidad y heterocedasticidad.
graph g5d.line(m) d(impo) d(impo,1,4) dlog(impo) dlog(impo,1,4) 
genr ly5s4 = dlog(impo,1,4)
ly5s4.correl(20)' SAR(4) o SMA(4)

graph g6.line cgob ' Fuerte evidencia de tendencia. También estacionalidad y heterocedasticidad.
graph g6d.line(m) d(cgob) d(cgob,1,4) dlog(cgob) dlog(cgob,1,4) 
genr ly6s4 = dlog(cgob,1,4)
ly6s4.correl(20)'  AR(1) SAR(4) o MA(1) SMA(4)

' Inciso B

' Primera serie

equation y1ar.ls dlog(pbi,1,4) c ar(1) sar(4)
equation y1ma.ls dlog(pbi,1,4) c ma(1) sma(4)

table(3,2) tabla1
tabla1(1,1) = "AIC"
tabla1(2,1) = "SC"
tabla1(3,1) = "HQ"
tabla1(1,2) = y1ar.@aic
tabla1(2,2) = y1ar.@SC
tabla1(3,2) = y1ar.@HQ
tabla1(1,3) = y1ma.@aic
tabla1(2,3) = y1ma.@SC
tabla1(3,3) = y1ma.@HQ

' Se elige el modelo MA(1) SMA(4).
' Entonces, ARIMA(0,1,1) x (0,1,4)

' Segunda serie

equation y2ar.ls dlog(cpri,1,4) c sar(4)
equation y2ma.ls dlog(cpri,1,4) c  sma(4)

table(3,2) tabla2
tabla2(1,1) = "AIC"
tabla2(2,1) = "SC"
tabla2(3,1) = "HQ"
tabla2(1,2) = y2ar.@aic
tabla2(2,2) = y2ar.@SC
tabla2(3,2) = y2ar.@HQ
tabla2(1,3) = y2ma.@aic
tabla2(2,3) = y2ma.@SC
tabla2(3,3) = y2ma.@HQ

' Se elige modelo SMA(4)

' Tercera serie

equation y3ar.ls d(inv,1,4) c sar(4)
equation y3ma.ls d(inv,1,4) c  sma(4)

table(3,2) tabla3
tabla3(1,1) = "AIC"
tabla3(2,1) = "SC"
tabla3(3,1) = "HQ"
tabla3(1,2) = y3ar.@aic
tabla3(2,2) = y3ar.@SC
tabla3(3,2) = y3ar.@HQ
tabla3(1,3) = y3ma.@aic
tabla3(2,3) = y3ma.@SC
tabla3(3,3) = y3ma.@HQ

' Se elige modelo SMA(4)

' Cuarta serie

equation y4ar.ls dlog(expo,1,4) c sar(4)
equation y4ma.ls dlog(expo,1,4) c  sma(4)

table(3,2) tabla4
tabla4(1,1) = "AIC"
tabla4(2,1) = "SC"
tabla4(3,1) = "HQ"
tabla4(1,2) = y4ar.@aic
tabla4(2,2) = y4ar.@SC
tabla4(3,2) = y4ar.@HQ
tabla4(1,3) = y4ma.@aic
tabla4(2,3) = y4ma.@SC
tabla4(3,3) = y4ma.@HQ

' Se elige modelo SMA(4)

' Quinta serie

equation y5ar.ls dlog(impo,1,4) c sar(4)
equation y5ma.ls dlog(impo,1,4) c  sma(4)

table(3,2) tabla5
tabla5(1,1) = "AIC"
tabla5(2,1) = "SC"
tabla5(3,1) = "HQ"
tabla5(1,2) = y5ar.@aic
tabla5(2,2) = y5ar.@SC
tabla5(3,2) = y5ar.@HQ
tabla5(1,3) = y5ma.@aic
tabla5(2,3) = y5ma.@SC
tabla5(3,3) = y5ma.@HQ

' Se elige modelo SMA(4)

' Sexta serie

equation y6ar.ls dlog(cgob,1,4) c ar(1) sar(4)
equation y6ma.ls dlog(cgob,1,4) c  ma(1) sma(4)


table(3,2) tabla6
tabla6(1,1) = "AIC"
tabla6(2,1) = "SC"
tabla6(3,1) = "HQ"
tabla6(1,2) = y6ar.@aic
tabla6(2,2) = y6ar.@SC
tabla6(3,2) = y6ar.@HQ
tabla6(1,3) = y6ma.@aic
tabla6(2,3) = y6ma.@SC
tabla6(3,3) = y6ma.@HQ

' Se elige el modelo MA(4) SMA(4)

' Inciso C - Proceso de control

y1ma.correl(20)  ' Comportamiento similar a ruido blanco
y2ma.correl(20)  ' Comportamiento similar a ruido blanco
y3ma.correl(20)  ' Comportamiento similar a ruido blanco
y4ma.correl(20)  ' Comportamiento un poco similar a ruido blanco
y5ma.correl(20)  ' Comportamiento similar a ruido blanco
y6ma.correl(20)  ' Comportamiento similar a ruido blanco

' Inciso D

range 1980.01 2023.04

' Modelos para pronóstico

y1ma.makemodel(y1mamod)
y2ma.makemodel(y2mamod)
y3ma.makemodel(y3mamod)
y4ma.makemodel(y4mamod)
y5ma.makemodel(y5mamod)
y6ma.makemodel(y6mamod)

' Estableciendo el periodo de pronóstico
smpl 2020.02 2023.04

' Solución dinámica
y1mamod.solveopt(s = s, d = d)
y2mamod.solveopt(s = s, d = d)
y3mamod.solveopt(s = s, d = d)
y4mamod.solveopt(s = s, d = d)
y5mamod.solveopt(s = s, d = d)
y6mamod.solveopt(s = s, d = d)

' Realizando los pronósticos
solve y1mamod
solve y2mamod
solve y3mamod
solve y4mamod
solve y5mamod
solve y6mamod

genr li1=pbi_0m - 1.96*pbi_0s
genr ls1=pbi_0m + 1.96*pbi_0s

genr li2=cpri_0m - 1.96*cpri_0s
genr ls2=cpri_0m + 1.96*cpri_0s

genr li3=inv_0m - 1.96*inv_0s
genr ls3=inv_0m + 1.96*inv_0s

genr li4=expo_0m - 1.96*expo_0s
genr ls4=expo_0m + 1.96*expo_0s

genr li5=impo_0m - 1.96*impo_0s
genr ls5=impo_0m + 1.96*impo_0s

genr li6=cgob_0m - 1.96*cgob_0s
genr ls6=cgob_0m + 1.96*cgob_0s

graph preds1.line pbi_0m li1 ls1
graph preds2.line cpri_0m li2 ls2
graph preds3.line inv_0m li3 ls3
graph preds4.line expo_0m li4 ls4
graph preds5.line impo_0m li5 ls5
graph preds6.line cgob_0m li6 ls6

show preds1 preds2 preds3 preds4 preds5 preds6

' Graficando los pronósticos contra los valores de las series inciales

smpl @all

graph preds1t.line pbi_0m pbi
graph preds2t.line cpri_0m cpri
graph preds3t.line inv_0m inv
graph preds4t.line expo_0m expo
graph preds5t.line impo_0m impo
graph preds6t.line cgob_0m cgob

show preds1t  preds2t preds3t preds4t preds5t preds6t

