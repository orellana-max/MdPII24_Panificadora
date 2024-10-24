"Filed out from Dolphin Smalltalk"!

Empleado subclass: #Panadero
	instanceVariableNames: 'puesto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Panadero guid: (GUID fromString: '{924bf294-35d3-4f4d-a558-48e649e78e2b}')!

Panadero comment: ''!

!Panadero categoriesForClass!Kernel-Objects! !

!Panadero methodsFor!

imprimir
^'Panadero Puesto:  ', puesto, ' | ', super imprimir.!

iniPanaderoLegajo: unLegajo puesto: unPuesto nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Panadero"

	self iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	puesto := unPuesto.
	sueldo := 800000.
	
	
!

producirProducto:unProducto cant:unaCant
"Produce la cantidad de un producto"
|unidad|
unidad:= ''.
(unProducto verPorDocena) ifTrue: [unidad := ' Docenas'] .

unProducto aumentarStock: unaCant.
Transcript show: 'se produjo ' , (unaCant  printString), unidad,' de ', unProducto verNombre; cr.
Transcript show: ' (Stock: ' , unProducto verStock printString , '  )' ; cr.
! !

!Panadero categoriesForMethods!
imprimir!public! !
iniPanaderoLegajo:puesto:nom:dire:tel:!public! !
producirProducto:cant:!public! !
!

!Panadero class methodsFor!

crearPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Panadero inicializada"

	^(self new) iniPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.! !

!Panadero class categoriesForMethods!
crearPanaderoLegajo:nom:dire:tel:!public! !
!

