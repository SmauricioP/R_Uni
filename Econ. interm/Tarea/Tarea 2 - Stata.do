********************************************************************************
*                 Econometría intermedia - Tarea 2                             *
*                            Pregunta 6                                        *
********************************************************************************

* Preparación
**************

// Establecer el working directory

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 2"

// Asegurar que la memoria esté vacía

clear all

// Leer el archivo

use "brumm.dta"

// Exploracuón breve de datos

	des         // Atributos de cada variable
	sum, detail // Descriptivos detallados

//  Algunos gráficos, necesarios para tener una idea de la distribución

hist inflat, freq name(hinfl)
hist money, freq name(hmon)
hist output, freq name(hout)  // Posible forma de campana

graph combine hinfl hmon hout

************
* Inciso A *
************

* Estimación por MCO del modelo

reg inflat money output
estimates store mcoeq1 // Guardar la regresión
estimates table mcoeq1 ,star stats(N r2_a aic bic) // Resultados principales

************
* Inciso B *
************

* Prueba de hipótesis débil y fuerte

test (money = 1) (output = -1) // Forma débil de la hipótesis. Se rechaza

test (money = 1) (output = -1) (_cons = 0) // Forma fuerte de la hipótesis. 

************
* Inciso C *
************

* Scatterplot

scatter inflat output || lfitci inflat output, ciplot(rline) name(scfitci1)

cor inflat output // Correlación negativa, de acuerdo al gráfico.

reg inflat output
estimates store mcoeq2
estimates table mcoeq2 ,star stats(N r2_a aic bic)

// Se prueba que el coeficiente es menor a cero, manualmente

gen b1 = _b[output]
mat vareq2 = e(V)
scalar N = e(N)
scalar varb1 = vareq2[1,1]
scalar seb1 = sqrt(varb1)
scalar t_stat = b1/seb1
scalar p_val = t(N-2,t_stat)

display as text _dup(59) "-" _newline as result "Prueba t-student de una cola" _newline _newline as text "H0: beta_1 = 0" _newline "Ha: beta_1 < 0" _newline _newline as text "Valor t = " as result(round(t_stat),.0001) _newline as text "P-value = " as result(round(p_val,0.0001)) _newline _newline as result "Se rechaza la hipótesis nula." as text " Hay evidencia para establecer" _newline "que el parámetro es menor a cero." _newline _dup(59) "-" 

************
* Inciso D *
************

estimates restore mcoeq1 // Volviendo a los estimados iniciales
predict eq1_resid, residuals

hist eq1_resid, freq name(hresideq1) // Histogramas de los residuales
kdensity eq1_resid, name(kresideq1)  // Estimación kernel de la densidad

estat imtest, white // Test de White para heterocedasticidad. Hay evidencia.

reg inflat money output, r
estimates store mcoeq1_rob
matrix list e(V)

* Para la corección, mínimos cuadrados generalizados factibles. 

* Paso 1: Regresión para estimar coeficientes del ponderador

gen log_resid2 = log(eq1_resid^2)
reg log_resid2 money output
estimates store mcgf_pond

* Paso 2: Fitted de la regresión. Esto es el log de la

predict log_p
gen p = 1/sqrt(exp(log_p))

* Paso 3: Corregir las variables

gen inflatp = inflat * p
gen moneyp  = money * p
gen outputp = output * p

reg inflatp moneyp outputp
estimates store mcpeq1

estat imtest, white // Probando nuevamente hipótesis de heterocedasticidad.

************
* Inciso E *
************

* Evidencias de endogeneidad del producto.

ivreg inflat money (output = initial school inv)
ivendog // No se rechaza H0. Posiblemente exogeneidad de output.

************
* Inciso E *
************

ivreg inflat output (money = initial school inv)
ivendog // No hay evidencias para output ni para inflat. No se requiere MC2E

