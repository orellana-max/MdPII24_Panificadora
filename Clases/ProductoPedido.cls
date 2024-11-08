"Filed out from Dolphin Smalltalk"!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroProducto cantidad nroPedido pedido costoUnitario'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroProductoPedido'!

ProductoPedido guid: (GUID fromString: '{9ae45669-d20e-4e41-a18f-54a008d55e0e}')!

ProductoPedido comment: ''!

!ProductoPedido categoriesForClass!No category! !

!ProductoPedido methodsFor!

imprimir
"Devuelve una cadena con los datos del producto pedido"
|cadena|

cadena := 'Producto Pedido ID: ', idProductoPedido printString,' Producto Nro: ', nroProducto printString,' Cant: ', cantidad printString,' total $', self verCosto printString .

^cadena!

iniProductoPedidoNroProd:unNroProduct cantidad:cant costoUnitario:cost
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroPP.
nroProducto := unNroProduct.
nroPedido := 0.
pedido := nil.
cantidad:= cant.
costoUnitario:=cost.!

iniProductoPedidoUnProd:unProducto cantidad:cant 
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroPP.
nroProducto := unProducto verNroProducto .
nroPedido := 0.
pedido := nil.
cantidad:= cant.
costoUnitario:=unProducto verPrecio.!

iniProductoPedidoUnProd:unProducto Pedido: unPedido cantidad:cant 
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroPP.
nroProducto := unProducto verNroProducto .
nroPedido := unPedido verNroPedido.
pedido := unPedido.
cantidad:= cant.
costoUnitario:=unProducto verPrecio.!

modCantidad:unNro
	cantidad:=unNro.!

modCostoUnitario:unNro
	costoUnitario =unNro.!

modNroPedido: unNroPedido
	nroPedido := unNroPedido!

setPedido: unPedido
	pedido := unPedido.
	nroPedido := unPedido verNroPedido!

sumarCantidad:unaCant
	cantidad:=cantidad + unaCant.!

verCantidad
	^cantidad.!

verCosto
	| total |
	total := cantidad * costoUnitario.
	^total!

verCostoUnitario
	^costoUnitario.!

verIdPP
	^idProductoPedido.!

verNrodelPedido
	^nroPedido.!

verNroProducto
	^nroProducto.! !

!ProductoPedido categoriesForMethods!
imprimir!public! !
iniProductoPedidoNroProd:cantidad:costoUnitario:!public! !
iniProductoPedidoUnProd:cantidad:!public! !
iniProductoPedidoUnProd:Pedido:cantidad:!public! !
modCantidad:!public! !
modCostoUnitario:!public! !
modNroPedido:!public! !
setPedido:!public! !
sumarCantidad:!public! !
verCantidad!public! !
verCosto!public! !
verCostoUnitario!public! !
verIdPP!public! !
verNrodelPedido!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:unProducto cantidad:cant

	^(self new) iniProductoPedidoUnProd: unProducto cantidad: cant.!

crearProductoPedido:unProducto pedido:unPedido cantidad:cant

	^(self new) iniProductoPedidoUnProd: unProducto Pedido: unPedido cantidad: cant.!

crearProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo

	^(self new) iniProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo.!

initialize 
    nroProductoPedido := 0.
!

nextNroPP
	"retorna un id unico para una nueva instancia de producto"
	(nroProductoPedido isNil) ifTrue: [nroProductoPedido := 0].
	nroProductoPedido := nroProductoPedido + 1.
	^nroProductoPedido.
! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:cantidad:!public! !
crearProductoPedido:pedido:cantidad:!public! !
crearProductoPedidoNroProd:cantidad:costoUnitario:!public! !
initialize!public! !
nextNroPP!public! !
!

