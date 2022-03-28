********************************************************************************
*                 Econometría intermedia - Tarea 1                             *
*                            Pregunta 5                                        *
********************************************************************************

// Establecer el working directory

cd "C:\Users\Mauca\Google Drive\Universidad\Ciclo regular\8vo ciclo\Econometría intermedia\Tareas\Tarea 1"

// Por si se ha realizado algun trabajo en la sesión, limpiar la memoria

clear all

// Leer el archivo

use "nerlove.dta"

// Pequeño análisis exploratorio

	des // Atributos de cada variable
	sum ,detail // Descriptivos detallados

// A continuación, histogramas y densidades estimadas para tener una idea de la forma  que podría tomar la distribución teórica de los datos.
	
hist ct, freq name(hct)
hist q, freq name(hq)
hist pl, freq name(hpl)
hist pg, freq name(hpg)
hist pk, freq name(hpk)

graph combine hct hq hpl hpg hpk // Histogramas combinados

kdensity ct, name(denct)
kdensity q, name(denq)
kdensity pl, name(denpl)
kdensity pg, name(denpg)
kdensity pk, name(denpk)

graph combine denct denq denpl denpg denpk // Densidades (estimación kernel)

************
* Inciso a *
************

// 1. Lo primero que se debe realizar es hallar los logaritmos de las variables.

gen lct = ln(ct)
	label variable lct "Log costo total"
	
gen lq  = ln(q)
	label variable lq "Log cantidad"

gen lpl = ln(pl)
	label variable lpl "Log precio trabajo"

gen lpk = ln(pk)
	label variable lpk "Log precio capital"

gen lpg = ln(pg)
	label variable lpg "Log precio gasolina"

* De esta manera, la función de costos asumida es Cobb-Douglas y los coeficientes se interpretarán como elasticidades.

// 2. Estimación por mínimos cuadrados ordinarios 

reg lct lq lpl lpk lpg // Eq1
estimates store mcoeq1 // Guardar la regresión
estimates table mcoeq1 ,star stats(N r2_a aic bic) // Resultados principales

* Guardando las estimaciones

scalar b0 = _b[_cons]
scalar b1 = _b[lq]
scalar b2 = _b[lpl]
scalar b3 = _b[lpk]
scalar b4 = _b[lpg]

************
* Inciso b *
************

// 1. Se halla "r" directamente de b1

scalar r = 1/b1
display r

// 2. De esta manera, se puede despejar los coeficientes alfa:

forvalues i = 2/4 {
    local j = `i' - 1
    scalar a`j' = r*b`i'
}

* Viendo cada elasticidad
forvalues i = 2/4 {
    local i = `i' - 1
    display "a`i': " round(a`i',0.00001)
}

************
* Inciso c *
************

// 1.- Prueba de hipótesis: beta_2 = 1

test lpl = 1

* No se rechaza la hipótesis a un nivel de significancia del 0.05. De hecho, el p-value tomó un valor cercano a 0.05. De esta manera, es probable que el parámetro de lpl sea igual a uno.

************
* Inciso d *
************

// 1.- Prueba de hipótesis: beta_2 + beta_3 + beta_4 = 1 (homogénea de grado 1)

test lpl + lpk + lpg = 1

* No se rechaza la hipótesis a un nivel de significancia del 0.05. De esta manera, es probable que la función de costos sea homogénea de grado uno en precios.

************
* Inciso e *
************

// 1.- Imponer la restricción de homogeneidad

constraint define 1 lpl + lpk + lpg = 1

// 2.- Realizar la estimación por mínimos cuadrados restringidos

cnsreg lct lq lpl lpk lpg, constraint(1)
estimates store mcreq1 // Guardar la regresión
estimates table mcreq1 ,star stats(N aic bic)

************
* Inciso f *
************

// 1.- La ecuación estimada será diferente y cumplirá con la restricción. De esta manera, se deben generar las nuevas variables

gen lct_div_pg = ln(ct/pg)
	label variable lct_div_pg "Log ct/pg"

gen lpl_div_pg = ln(pl/pg)
	label variable lpl_div_pg "Log pl/pg"

gen lpk_div_pg = ln(pk/pg)
	label variable lpk_div_pg "Log pk/pg"

// 2.- Regresión de la nueva ecuación

reg lct_div_pg lq lpl_div_pg lpk_div_pg
estimates store mcoeq2 // Guardar la regresión
estimates table mcoeq2 ,star stats(N r2 aic bic)

************
* Inciso g *
************

mlexp (ln(normalden(lct, {xb: lq lpl lpk lpg _cons}, {sigma})))
estimates store mleeq1 // Guardar la regresión
estimates table mleeq1 ,star stats(N aic bic)

// Finalmente, cuadro comparativo de todos los modelos

estimates table mcoeq1 mcreq1 mcoeq2 mleeq1 ,star stats(N aic bic)