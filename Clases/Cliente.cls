"Filed out from Dolphin Smalltalk"!

Object subclass: #Cliente
	instanceVariableNames: 'nroCliente nombre direccion telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idCliente'!

Cliente guid: (GUID fromString: '{b9dc76af-91b0-47bd-9ddd-07ac331b74fc}')!

Cliente comment: ''!

!Cliente categoriesForClass!cat_max1! !

!Cliente methodsFor!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Cliente Nro:  ' , nroCliente printString , ' | Nom:  ' , nombre , ' | Dir:  ' , direccion
				, ' | Tel:  ' , telefono.
	^cadena!

iniClienteNom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Cliente"

	nroCliente := Cliente nextNroCliente.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel!

modDireccion:unaDireccion
"Modifica la direccion del cliente"
direccion:=unaDireccion.!

modNombre:unNombre
"Modifica el nombre del cliente"
nombre:=unNombre.!

modTelefono:unTelefono
"Modifica el telefono del cliente"
telefono =unTelefono .!

printOn: aStream
	aStream
		nextPutAll: 'Cliente Nro:  ';
		nextPutAll: nroCliente printString , ' | ';
		nextPutAll: 'Nom:  ';
		nextPutAll: nombre , ' | ';
		nextPutAll: 'Dir:  ';
		nextPutAll: direccion , ' | ';
		nextPutAll: 'Tel:  ';
		nextPutAll: telefono.!

verDireccion
"Retorna la direccion del cliente"
^direccion.!

verNombre
"Retorna el nombre del cliente"
^nombre.!

verNroCliente
"Retorna el numero del cliente"
^nroCliente.!

verTelefono
"Retorna el telefono del cliente"
^telefono.! !

!Cliente categoriesForMethods!
imprimir!public! !
iniClienteNom:dire:tel:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
printOn:!public! !
verDireccion!public! !
verNombre!public! !
verNroCliente!public! !
verTelefono!public! !
!

!Cliente class methodsFor!

crearClienteNom:unNom dire:unaDire tel:unTel
"Retorna una instancia de Cliente inicializada"

^(self new) iniClienteNom:unNom dire: unaDire tel: unTel.!

initialize
idCliente := 1.
!

nextNroCliente
	"Retorna un id unico para una instancia nueva de Cliente"

	| id |
	id := idCliente.
	idCliente := idCliente + 1.
	^id! !

!Cliente class categoriesForMethods!
crearClienteNom:dire:tel:!public! !
initialize!public! !
nextNroCliente!public! !
!

