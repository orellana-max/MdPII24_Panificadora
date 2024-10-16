"Filed out from Dolphin Smalltalk"!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProd tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProd'!

Producto guid: (GUID fromString: '{7948a066-db33-4125-bacc-ab5d39ab576d}')!

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

iniProductoNombre: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := Producto nextId.
	nombreProd := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false!

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

nombre
	"retorna el nombre del producto"

	^nombreProd.!

nombre: unNombre
	"Modifica el nombre del producto"

	nombreProd := unNombre.!

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
modPorDocena!public! !
modPrecio:!public! !
modStock:!public! !
modTipo:!public! !
nombre!public! !
nombre:!public! !
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
	"Inicia la Variable de Clase nextId"
	idProd := 1.!

nextId
	| id |
	id := idProd.
	idProd := idProd + 1.
	^id! !

!Producto class categoriesForMethods!
crearProductoNombre:tip:prec:!public! !
initialize!public! !
nextId!public! !
!

