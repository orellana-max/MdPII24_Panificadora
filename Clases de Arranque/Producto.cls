"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProd tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Producto guid: (GUID fromString: '{7948A066-DB33-4125-BACC-AB5D39AB576D}')!
Producto comment: ''!
!Producto categoriesForClass!Kernel-Objects! !
!Producto methodsFor!

aumentarStock:cant
"aumenta la cantidad del stock del producto"
stock:=stock + cant.!

disminuirStock: cant
	"aumenta la cantidad del stock del producto"

	stock := stock - cant!

imprimir
"retorna una cadena con los datos"
|cadena|

cadena := nroProducto  printString , ' | ', nombreProd , ' | ', tipo , ' | ',  (stock printString), ' | ', (precio printString) , ' | porDoc: ', (porDocena printString).
^cadena.!

iniProductoNro: unNum nom: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := unNum.
	nombreProd := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false!

modNombre: unNombre
	"Modifica el nombre del producto"

	nombreProd := unNombre!

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

	^nombreProd.!

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
!Producto categoriesFor: #aumentarStock:!public! !
!Producto categoriesFor: #disminuirStock:!public! !
!Producto categoriesFor: #imprimir!public! !
!Producto categoriesFor: #iniProductoNro:nom:tip:prec:!public! !
!Producto categoriesFor: #modNombre:!public! !
!Producto categoriesFor: #modPorDocena!public! !
!Producto categoriesFor: #modPrecio:!public! !
!Producto categoriesFor: #modStock:!public! !
!Producto categoriesFor: #modTipo:!public! !
!Producto categoriesFor: #verNombre!public! !
!Producto categoriesFor: #verNroProducto!public! !
!Producto categoriesFor: #verPorDocena!public! !
!Producto categoriesFor: #verPrecio!public! !
!Producto categoriesFor: #verStock!public! !
!Producto categoriesFor: #verTipo!public! !

!Producto class methodsFor!

crearProductoNro:unNum nom:unNom tip:unTipo prec:unPrecio
"Retorna una instancia de Producto inicializada"
^(self new) iniProductoNro: unNum nom: unNom tip: unTipo prec: unPrecio.! !
!Producto class categoriesFor: #crearProductoNro:nom:tip:prec:!public! !

