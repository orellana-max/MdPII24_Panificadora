"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Panadero
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Panadero guid: (GUID fromString: '{924BF294-35D3-4F4D-A558-48E649E78E2B}')!
Panadero comment: ''!
!Panadero categoriesForClass!Kernel-Objects! !
!Panadero methodsFor!

iniPanaderoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Empleado"

	legajo := unLegajo.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel.
	sueldo := 0!

producirProducto:unProducto cant:unaCant
"Produce la cantidad de un producto"
|unidad|
unidad:= ''.
(unProducto verPorDocena) ifTrue: [unidad := ' Docenas'] .

unProducto aumentarStock: unaCant.
Transcript show: 'se produjo ' , (unaCant  printString), unidad,' de ', unProducto verNombre; cr.
Transcript show: ' (Stock: ' , unProducto verStock printString , '  )' ; cr.
! !
!Panadero categoriesFor: #iniPanaderoLegajo:nom:dire:tel:!public! !
!Panadero categoriesFor: #producirProducto:cant:!public! !

!Panadero class methodsFor!

crearPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Panadero inicializada"

	^self new
		iniPanaderoLegajo: unLegajo
		nom: unNom
		dire: unaDire
		tel: unTel! !
!Panadero class categoriesFor: #crearPanaderoLegajo:nom:dire:tel:!public! !

