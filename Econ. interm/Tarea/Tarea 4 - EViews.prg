' Tarea 4 - Grupo 4 - Pregunta 1 '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 4"

' Inciso A

import "maastricht.dta"
pagestruct(freq=q, start=1966.3)
wfsave "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 4\maastricht.wf1"

' Inciso B

genr lrm3 = log(m3/ipd)
genr lgdp = log(gdp)

' Inciso C

freeze(adf1_lrm3)lrm3.uroot(adf, t) 'Tendencia y constante
freeze(adf2_lrm3)lrm3.uroot(adf, dif = 1)    'Primeras diferencias

freeze(adf1_lgdp)lgdp.uroot(adf, t) 'Tendencia y constante
freeze(adf2_lgdp)lgdp.uroot(adf, dif = 1)    'Primeras diferencias

freeze(adf1_intrate1)intrate1.uroot(adf, t) 'Tendencia y constante
freeze(adf2_intrate1)intrate1.uroot(adf, dif = 1)    'Primeras diferencias

' Conclusión: todas las series son I(1)

' Inciso D

VAR var01.ls 1 2 d(lrm3) d(lgdp) d(intrate1)
freeze(var01lagsel) var01.laglen(5)

' Menor AIC: VAR de orden 2. Proceso VAR(2)

' Inciso E

' ¿Ruido blanco?
freeze(var01qstats) var01.qstats(12)

' Sí. Pareciera que sí es ruido blanco pues no se rechaza la H0 (solo hay problemas en el rezago 12).

' Inciso F

freeze(var01cause) var01.testexog

' SÍ dicotomiza, pues las variables nominales no causan a lo granger a las reales. No se rechaza la hipótesis nula de que los coeficientes de dlrm3 y de dintrate1 en la ecuación de dlgdp sea cero.

' Inciso G

' Creación del modelo
var01.makemodel(var01mod) @prefix s_

' Rango de datos amplio para pronóstico
range 1966.3 2002.4

'periodo de pronóstico 
smpl 2002.2 2002.4

'métodos de simulación=estocástica y solución=dinámica
var01mod.solveopt(s=s, d=d)

' Realización del pronóstico
solve var01mod

' Muestra total
smpl @all

' Gráficos

genr rm3 = exp(lrm3)
genr rm3_p = exp(lrm3_0m)

genr gdp = exp(lgdp)
genr gdp_p = exp(lgdp_0m)


graph g1.line rm3_p rm3
graph g2.line gdp_p gdp
graph g3.line intrate1_0m intrate1
show g1 g2 g3



