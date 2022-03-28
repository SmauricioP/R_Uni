' Establecer el working directory

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 1"

' Leer la data

wfopen nerlove.dta

'' Inciso a
''''''''''''''''''''''''''''
' De manera similar, se generan los logaritmos de las variables

genr lct = log(ct)
lct.label(r) Log costo total

genr lq  = log(q)
lq.label(r) Log cantidad

genr lpl = log(pl)
lpl.label(r) Log precio trabajo

genr lpk = log(pk)
lpk.label(r) Log precio capital

genr lpg = log(pg)
lpg.label(r) Log precio gasolina


' Ahora, se realiza la regresión lineal

equation olseq1.ls lct c lq lpl lpk lpg

freeze(eq1) olseq1
show eq1

'' Inciso b
''''''''''''''''''''''''''''

scalar r = 1/(olseq1.@coef(2)) 
show r

scalar a1 = r*olseq1.@coef(3)
scalar a2 = r*olseq1.@coef(4)
scalar a3 = r*olseq1.@coef(5)

show a1
show a2
show a3

delete r

'' Inciso c
''''''''''''''''''''''''''''

' Test para comprobar rendimientos a escala. El coeficiente que pertence a
' el precio del trabajo es el tercero

freeze(wald1_olseq1) olseq1.wald c(3) = 1
show wald1_olseq1

'' Inciso d
''''''''''''''''''''''''''''

' Test para comprobar que el grado de homogeneidad es de uno en pecios 

freeze(wald2_olseq1)olseq1.wald c(3) + c(4) + c(5) = 1
show wald2_olseq1

'' Inciso e
''''''''''''''''''''''''''''

' Agrupando...
group grp 1 lq lpl lpk lpg

' ... y convirtiendo a matriz

matrix X = @convert(grp)

matrix y = @convert(lct)

' Calculando los betas OLS (ordinary least squares o mínimos cuadrados ordinarios) manualmente:

matrix beta_ols = @inverse(@transpose(X) * X) * (@transpose(X) * y)

' Restricciones impuestas

matrix(1,5) R
R.fill 0,0,1,1,1

matrix(1,1) l
l.fill 1

' Finalmente, calculando los betas estimados para el modelo restringido

matrix beta_restrols = @inverse(@transpose(X)*X)*@transpose(R)*@inverse(R*@inverse(@transpose(X)*X)*@transpose(R)) * (l - R*beta_ols) + beta_ols

freeze(betarest) beta_restrols
show betarest

'' Inciso f
''''''''''''''''''''''''''''
' Se generan las nuevas variables y se crea la regresión

genr lct_div_pg = log(ct/pg)
lct_div_pg.label(r) Log ct/pg

genr lpl_div_pg = log(pl/pg)
lpl_div_pg.label(r) Log pl/pg

genr lpk_div_pg = log(pk/pg)
lpk_div_pg.label(r) Log pk/pg

equation olseq2.ls lct_div_pg c lq lpl_div_pg lpk_div_pg

freeze(eq2) olseq2
show olseq2

'' Inciso g
''''''''''''''''''''''''''''

' Estimación por máxima verosimilitud

' Se define el programa:

LOGL mle01
mle01.append @param c(1) 0.7 c(2) 0.4 c(3) -0.2 c(4) 0.4 c(5) 5
mle01.append @logl logl1
mle01.append resid=lct-c(1)-c(2)*lq-c(3)*lpl-c(4)*lpk-c(5)*lpg
mle01.append sigma2=c(5)^2
mle01.append logl1=log(@dnorm(resid/@sqrt(sigma2)))-log(sigma2)/2

' Mostrar resultados de MLE (maximum likelihood estimation)

mle01.ml
show mle01


