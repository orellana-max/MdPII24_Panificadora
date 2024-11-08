"Filed out from Dolphin Smalltalk"!

Object subclass: #Empleado
	instanceVariableNames: 'legajo direccion telefono sueldo nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Empleado guid: (GUID fromString: '{449b804d-71ea-44de-b429-1a4cb51801e4}')!

Empleado comment: ''!

!Empleado categoriesForClass!No category! !

!Empleado methodsFor!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Leg: ', legajo printString , ' | Nom: ' , nombre , ' | Dir: ' , direccion , ' | Tel: '
				, telefono, ' | sueldo ', sueldo printString.
	^cadena!

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

printOn: aStream
	aStream
		nextPutAll: 'Leg: ' , legajo printString , ' | ';
		nextPutAll: 'Nom:  ' , nombre , ' | ';
		nextPutAll: 'Dir:  ' , direccion , ' | ';
		nextPutAll: 'Tel:  ' , telefono , ' | ';
		nextPutAll: 'Sueldo:  ';
		print: sueldo printString.!

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

!Empleado categoriesForMethods!
imprimir!public! !
iniEmpleadoLegajo:nom:dire:tel:!public! !
modDireccion:!public! !
modLegajo:!public! !
modNombre:!public! !
modSueldo:!public! !
modTelefono:!public! !
printOn:!public! !
verDireccion!public! !
verLegajo!public! !
verNombre!public! !
verSueldo!public! !
verTelefono!public! !
!

!Empleado class methodsFor!

crearEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Empleado inicializada"

	^(self new) iniEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.! !

!Empleado class categoriesForMethods!
crearEmpleadoLegajo:nom:dire:tel:!public! !
!

