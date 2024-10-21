"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Empleado
	instanceVariableNames: 'legajo direccion telefono sueldo nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Empleado guid: (GUID fromString: '{449B804D-71EA-44DE-B429-1A4CB51801E4}')!
Empleado comment: ''!
!Empleado categoriesForClass!Kernel-Objects! !
!Empleado methodsFor!

iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Empleado"

	legajo := unLegajo.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel.
	!

modDireccion:unaDireccion
"Modifica la direccion del Empleado"
direccion:=unaDireccion.!

modLegajo: unLegajo
	"Modifica el Legajo del Empleado"

	legajo := unLegajo!

modNombre:unNombre
"Modifica el nombre del Empleado"
nombre:=unNombre.!

modSueldo:unSueldo
	"Modifica el sueldo del Empleado"

	sueldo := unSueldo.!

modTelefono:unTelefono
"Modifica el telefono del Empleado"
telefono =unTelefono .!

verDireccion
"Retorna la direccion del Empleado"
^direccion.!

verLegajo
"Retorna el Legajo del Empleado"
^legajo.!

verNombre
"Retorna el nombre del Empleado"
^nombre.!

verSueldo
"Retorna el sueldo del Empleado"
^sueldo.!

verTelefono
"Retorna el telefono del Empleado"
^telefono.! !
!Empleado categoriesFor: #iniEmpleadoLegajo:nom:dire:tel:!public! !
!Empleado categoriesFor: #modDireccion:!public! !
!Empleado categoriesFor: #modLegajo:!public! !
!Empleado categoriesFor: #modNombre:!public! !
!Empleado categoriesFor: #modSueldo:!public! !
!Empleado categoriesFor: #modTelefono:!public! !
!Empleado categoriesFor: #verDireccion!public! !
!Empleado categoriesFor: #verLegajo!public! !
!Empleado categoriesFor: #verNombre!public! !
!Empleado categoriesFor: #verSueldo!public! !
!Empleado categoriesFor: #verTelefono!public! !

!Empleado class methodsFor!

crearEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Empleado inicializada"

	^(self new) iniEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.! !
!Empleado class categoriesFor: #crearEmpleadoLegajo:nom:dire:tel:!public! !

