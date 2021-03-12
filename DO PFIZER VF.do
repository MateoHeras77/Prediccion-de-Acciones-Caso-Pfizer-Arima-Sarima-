 ** MODELOS ARIMA Y/O SARIMA
clear all
cls
* DATOS
use "C:\Users\xavie\Desktop\PFIZER\BASE DE DATOS PRECIO DE LAS ACCIONES PFIZER.dta"  
* Definir la variable temporal
egen float tiempo = seq(), from(2010) to(2021) block(12)
egen float MESES= seq(), from(1) to(12) block(1)
gen Fecha= ym( tiempo , MESES )
format Fecha %tm
tsset Fecha

**************************************** 
*************DESCRIPTIVOS***************
**************************************** 
** Se trabajará con el precio de cierre de la acción
tsline close,  ytitle("Precio de cierre de la acción de Pfizer Inc.(dólares)")

sum close
 
table tiempo, contents(mean close sd close) // promedio por año
table MESES, contents(mean close sd close) // promedio por mes

graph box close, over (MESES) ytitle("Precio mensual promedio de cierre de la acción de Pfizer Inc.(dólares)") // no presenta una evidencia clara de estacionalidad 
graph box close, over (tiempo) ytitle("Precio anual promedio de cierre de la acción de Pfizer Inc.(dólares)")

 
**************************************** 
*MODELOS ARIMA: METODOLOGIA BOX-JENKINS*
**************************************** 

*==========*
 *Grafica*
*==========*
 tsline close
 
*=============================*
 *Pruebas de  estacionariedad:
*=============================*

		 *Test Phillips Perron:
		 pperron close // Tiene Raiz Unitaria
		 
		 *Dicker Fuller Aumentado:
		 dfuller close, trend regress lags(12) // La Serie tiene Raiz Unitaria (no significativa la tendencia)
		 dfuller close, regress nocons lags(12) // Tiene Raiz Unitaria
		 dfuller close, nocons lags(12) // Tiene Raiz Unitaria
		 dfuller close,  lags(12) // Tiene Raiz Unitaria
		 
		 *Generalized least square (GLS) detrended augmented Docker Fuller:
		 dfgls close // Tiene Raiz Unitaria
		 
		 *KPSS
		  kpss close // Rechazo la Ho.
		  
	** EN GENERAL LA SERIE NO ES ESTACIONARIA
	
*===============================*
*Usando la primera diferencia:
*===============================*
*==========*
 *Grafica*
*==========*
sum D.close
twoway (line D.close Fecha), yline(`r(mean)') //(Precio de cierre ) 

*=============================*
 *Pruebas de  estacionariedad:
*=============================*

		 *Test Phillips Perron:
		 pperron D.close // La serie es estacionaria
		 
		 *Dicker Fuller Aumentado:
		 dfuller D.close, trend regress lags(12) // La serie es estacionaria (tendencia no significativa)
		 dfuller D.close, regress nocons lags(12) // La serie es estacionaria
		 dfuller D.close, nocons lags(12) // La serie es estacionaria
		 dfuller D.close,  lags(12) // La serie es estacionaria
		 		 
		 *KPSS
		  kpss D.close // Acepta la Ho.

*======================================================================*
*Analizamos la cantidad de medias moviles y autorregresivos necesarios:
*======================================================================*

ac D.close // Nos dice el numero de medias moviles 
*(2-5-7)
pac D.close // Nos dice el numero de autorregresivos
*(2-5-7-12)

*=============*
*Modelos Arima
*=============*
** Mediante ensayo y error (Tomando en cuenta el criterio de Akaike)

arima close, arima(2,1,2) nolog // no significativa la constante
estat ic // 495.9689
arima close, arima(2,1,2)  nocons nolog  // **Mejor Modelo** y parsimonioso
estat ic // 494.9347
arima close, arima(2,1,5)  nocons nolog
estat ic // 497.4327
arima close, arima(2,1,7)  nocons nolog
estat ic // 501.1107

arima close, arima(5,1,2)  nocons nolog  
estat ic // 497.9091
arima close, arima(5,1,5)  nocons nolog
estat ic // 502.2356
arima close, arima(5,1,7)  nocons nolog
estat ic // 503.5761

arima close, arima(7,1,2)  nocons nolog 
estat ic // 501.1131
arima close, arima(7,1,5)  nocons nolog
estat ic // 498.79
*arima close, arima(7,1,7)  nocons nolog // no converge
*estat ic 
arima close, arima(12,1,2)  nocons nolog  
estat ic // 508.6144
arima close, arima(12,1,5)  nocons nolog
estat ic // 508.9008
arima close, arima(12,1,7)  nocons nolog
estat ic // 505.7918


*=======================*
*Pruebas de ruido blanco
*=======================*
arima close, arima(2,1,2) nocons nolog

	predict error, resid

	wntestq error // No se R. Ho asi que los residuos son ruido blanco
	wntestb error 

	ac error //verificar que la media sea 0
	pac error

	summ error
	tsline error, yline(`r(mean)')

*============*
*Pronosticar:
*============*
quietly arima close, arima(2,1,2) nocons nolog
tsappend, add (6)
predict PrecioEst1 , y dyn(tm(2021m2)) 

twoway (line close Fecha) (line PrecioEst1 Fecha if Fecha <= tm(2021m2)) ///
(line PrecioEst1 Fecha if Fecha >= tm(2021m2), lcolor(dkorange))

*====================*
*Medidas de Precision
*=====================*
fcstats close PrecioEst1 , graph // (Theil's U 0.94776447) (RMSE  1.4907657)



*****************************************
*MODELOS SARIMA: METODOLOGIA BOX-JENKINS*
*****************************************

*==========*
 *Grafica*
*==========*
 tsline close
 
** ver (EVIEWS) FACTOR ESTACIONAL
** Aparentemente la serie no presenta estacionalidad
** Se realizó la prueba de HEGY: donde concluimos que no existe evidencia de
* raíz unitaria estacional
 
*==========================*
 *Diferenciacion Estacional*
*==========================*
tsline DS12.close

		 *Test Phillips Perron:
		 pperron DS12.close // La serie es estacionaria 
		 
		 *Dicker Fuller Aumentado:
		 dfuller DS12.close, trend regress lags(12) // La serie es estacionaria 
		 dfuller DS12.close, regress nocons lags(12) // La serie es estacionaria 
		 dfuller DS12.close, nocons lags(12) // La serie es estacionaria 
		 dfuller DS12.close,  lags(12) // La serie es estacionaria 
		 
		  
*========================================*
*Identificacion procesos
*========================================*
corrgram DS12.close

ac DS12.close // SMA(1) MA(2)

pac DS12.close  // SAR(1) AR(2)

*=============*
*SARIMA*
*=============*
** Mediante ensayo y error (Tomando en cuenta el criterio de Akaike)

/*arima close , arima (p,d,q) sarima (P,D,Q,s)*/

arima close , arima (2,1,2) sarima (1,1,1,12) nocons nolog // El mejor modelo
estat ic // 481.604

arima close , arima (2,1,2) sarima (1,0,1,12) nocons nolog // 
estat ic //  494.9577


*=======================*
*Pruebas de ruido blanco
*=======================*
quietly arima close , arima (2,1,2) sarima (1,1,1,12) nocons nolog
	predict error2, resid
	wntestq error2 // Los errores tienen ruido blanco
	wntestb error2 
	
	ac error2 //verificar que la media sea 0
	pac error2
	
	summ error2
	tsline error2, yline(`r(mean)')

*============*
*Pronosticar:
*============*
quietly arima close , arima (2,1,2) sarima (1,1,1,12) nocons nolog 
*tsappend, add (6)
predict PrecioEst2 , y dyn(tm(2021m2)) 

twoway (line close Fecha) (line PrecioEst2 Fecha if Fecha <= tm(2021m2)) ///
(line PrecioEst2 Fecha if Fecha >= tm(2021m2), lcolor(dkorange))

*====================*
*Medidas de Precision
*=====================*

fcstats close PrecioEst2 , graph // (Theil's U  1.0484865) (RMSE  1.6330117)


* Comparación de la precision 
fcstats close PrecioEst1 
fcstats close PrecioEst2

twoway (line close Fecha) (line PrecioEst1 Fecha) 
twoway (line close Fecha) (line PrecioEst2 Fecha)

** EL MEJOR AJUSTE EN ESTE CASO ES EL MODELO ARIMA ***

** TABLA COMPARATIVA
** ARIMMA
arima close, arima(2,1,2) nocons nolog
est store ARIMA

** SARIMA
arima close , arima (2,1,2) sarima (1,1,1,12) nocons nolog 
est store SARIMA

esttab ARIMA SARIMA, mtitles

** FIN

