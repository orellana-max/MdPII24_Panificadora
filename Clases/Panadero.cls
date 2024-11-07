"Filed out from Dolphin Smalltalk"!

Empleado subclass: #Panadero
	instanceVariableNames: 'puesto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Panadero guid: (GUID fromString: '{924bf294-35d3-4f4d-a558-48e649e78e2b}')!

Panadero comment: ''!

!Panadero categoriesForClass!cat_max1! !

!Panadero methodsFor!

imprimir
^'Panadero Puesto:  ', puesto, ' | ', super imprimir.!

iniPanaderoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel puesto: unPuesto
	"Inicializa una instancia de Panadero"

	super iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	puesto := unPuesto.
	sueldo := 800000.
	
	
!

printOn: aStream
	aStream
		nextPutAll: 'Panadero Puesto:  ';
		nextPutAll: puesto , ' | '.
		super printOn: aStream.!

producirProducto:unProducto cant:unaCant
"Produce la cantidad de un producto"
|unidad|
unidad:= ''.
(unProducto verPorDocena) ifTrue: [unidad := ' Docenas'] .

unProducto aumentarStock: unaCant.
Transcript show: 'se produjo ' , (unaCant  printString), unidad,' de ', unProducto verNombre; cr.
Transcript show: ' (Stock: ' , unProducto verStock printString , '  )' ; cr.
!

verPuesto
^puesto.! !

!Panadero categoriesForMethods!
imprimir!public! !
iniPanaderoLegajo:nom:dire:tel:puesto:!public! !
printOn:!public! !
producirProducto:cant:!public! !
verPuesto!public! !
!

!Panadero class methodsFor!

crearPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel puesto: unPuesto
	"Retorna una instancia de Panadero inicializada"

	^self new
		iniPanaderoLegajo: unLegajo
		nom: unNom
		dire: unaDire
		tel: unTel
		puesto: unPuesto! !

!Panadero class categoriesForMethods!
crearPanaderoLegajo:nom:dire:tel:puesto:!public! !
!

