﻿"Filed out from Dolphin Smalltalk"!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProducto tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProd'!

Producto guid: (GUID fromString: '{7948a066-db33-4125-bacc-ab5d39ab576d}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

aumentarStock:cantidad
"aumenta la cantidad del stock del producto"
stock:=stock + cantidad.!

disminuirStock: cantidad
	"aumenta la cantidad del stock del producto"

	stock := stock - cantidad.!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Producto nro: ', nroProducto printString , ' | Nom: ' , nombreProducto , ' | tipo: ' , tipo , ' | Strock: '
				, stock printString , ' | Precio: '
				, precio printString , ' | porDoc: '
				, porDocena printString.
	^cadena!

iniProductoNombre: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := Producto nextId.
	nombreProducto := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false!

modNombre: unNombre
	"Modifica el nombre del producto"

	nombreProducto := unNombre!

modPorDocena
	"Modifica si se vende por docena o no"

	porDocena:= porDocena not.!

modPrecio: unPrecio
	"modifica el precio del producto"

	precio := unPrecio.!

modStock: unStock
	"modifica el stock del producto"

	unStock >= 0
		ifTrue: [stock := unStock]
		ifFalse: 
			[Transcript show: 'Clas Producto, modStock: unStock'.
			Transcript show: ' No se puede tener stock menor a cero'].
!

modTipo: unTipo
	"modifica el tipo del producto"

	tipo:= unTipo.!

verNombre
	"retorna el nombre del producto"

	^nombreProducto.!

VerNombre
	"retorna el nombre del producto"

	^nombreProducto!

verNroProducto
"retorna el Numero del producto"
^nroProducto.!

verPorDocena
	"devuelve true si se vende por docena, false en caso contrario"

	^porDocena.!

verPrecio
	"retorna el precio del producto"

	^precio.!

verStock
"retorna el stock"
^stock.!

verTipo
	"retorna el tipo del producto"

	^tipo.! !

!Producto categoriesForMethods!
aumentarStock:!public! !
disminuirStock:!public! !
imprimir!public! !
iniProductoNombre:tip:prec:!public! !
modNombre:!public! !
modPorDocena!public! !
modPrecio:!public! !
modStock:!public! !
modTipo:!public! !
verNombre!public! !
VerNombre!public! !
verNroProducto!public! !
verPorDocena!public! !
verPrecio!public! !
verStock!public! !
verTipo!public! !
!

!Producto class methodsFor!

crearProductoNombre:unNom tip:unTipo prec:unPrecio
"Retorna una instancia de Producto inicializada"
^(self new) iniProductoNombre: unNom tip: unTipo prec: unPrecio.!

initialize
	"Inicia la Variable de Clase idProd"

	idProd := 1!

nextId
	"retorna un id unico para una nueva instancia de producto"

	| id |
	id := idProd.
	idProd := idProd + 1.
	^id! !

!Producto class categoriesForMethods!
crearProductoNombre:tip:prec:!public! !
initialize!public! !
nextId!public! !
!
