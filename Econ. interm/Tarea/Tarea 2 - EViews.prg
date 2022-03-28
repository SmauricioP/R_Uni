'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Tarea 2 - Pregunta 6 
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

' Nota: La explicación detallada se encuentra en el archivo do

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 2"

' Leer la data

wfopen brumm.dta

'' Inciso A
''''''''''''''''''''''''''

equation mcoeq1.ls inflat c money output
show mcoeq1

'' Inciso B
''''''''''''''''''''''''''

mcoeq1.wald c(2)=1, c(3)=-1 ' Hipótesis débil
mcoeq1.wald c(1) = 0,  c(2)=1, c(3)=-1 ' Hipótesis fuerte


'' Inciso C
''''''''''''''''''''''''''
group g1 output inflat
g1.scat(s,t=scat1)

mcoeq1.makeresids mcoeq1_res
show mcoeq1_res


'' Inciso D
''''''''''''''''''''''''''

mcoeq1.white ' Hay heterocedasticidad

' Se utilizará la corrección de White

equation mcorobeq1.ls(cov = white) inflat c money output


' Se calculará el ponderador asumiendo la forma exponencial.

genr log_res2 = log(mcoeq1_res^2)
equation pond.ls log_res2 c money output

fit log_pond

genr p = 1/(exp(log_pond))^0.5

' Variables corregidas

genr inflatp    = inflat * p
genr moneyp = money* p
genr outputp  = output* p

equation mcpeq1.ls inflatp c moneyp outputp
mcpeq1.white ' Se corrigió la heterocedasticidad.

'' Inciso E
''''''''''''''''''''''''''

equation mc2eeq1.tsls inflat c money output @ initial school inv
mc2eeq1.endogtest output  'No es endogena

'' Inciso F
''''''''''''''''''''''''''
equation mc2eeq2.tsls inflat c output money@ initial school inv
mc2eeq2.endogtest output  'No es endogena

' No hay evidencia de MC2E



