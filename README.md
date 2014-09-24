---
Fecha: 
"Tuesday, September 23, 2014"
---

Este script es para poder automatizar la descarga de la Lista Nominal de la página del INE. 

http://www.ine.mx/es/web/portal/inicio

Tendrá las siguientes etapas:

* Establecer la ruta de trabajo.
* Función para descargar los .txt de la Lista Nominal de las 32 entidades, por edad y sexo.
* Agrupar los .txt en un data frame para continuar con la transformación de los datos.
* Hacer las transformaciones convenientes.
* Exportar en .xls las tablas deseadas.