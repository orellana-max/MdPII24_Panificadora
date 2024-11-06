"Filed out from Dolphin Smalltalk"!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroProducto cantidad nroPedido pedido costoUnitario'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroProductoPedido'!

ProductoPedido guid: (GUID fromString: '{9ae45669-d20e-4e41-a18f-54a008d55e0e}')!

ProductoPedido comment: ''!

!ProductoPedido categoriesForClass!Kernel-Objects! !

!ProductoPedido methodsFor!

iniProductoPedidoNroProd:unNroProduct cantidad:cant costoUnitario:cost
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroPP.
nroProducto := unNroProduct.
nroPedido := pedido verNroPedido.
cantidad:= cant.
costoUnitario:=cost.!

modCantidad:unNro
	cantidad=cantidad+unNro.!

modCostoUnitario:unNro
	costoUnitario =unNro.!

modNroPedido: unNroPedido
	nroPedido := unNroPedido!

verCantidad
	^cantidad.!

verCosto
	| total |
	total := cantidad * costoUnitario.
	^total!

verCostoUnitario
	^costoUnitario.!

verNroProducto
	^nroProducto.! !

!ProductoPedido categoriesForMethods!
iniProductoPedidoNroProd:cantidad:costoUnitario:!public! !
modCantidad:!public! !
modCostoUnitario:!public! !
modNroPedido:!public! !
verCantidad!public! !
verCosto!public! !
verCostoUnitario!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:unProducto cantidad:cant

	^(self new) iniProductoPedidoNroProd: unProducto verNroProducto cantidad: cant costoUnitario: unProducto verCosto.!

crearProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo

	^(self new) iniProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo.!

initialize [
    nroProductoPedido := 0.
]!

nextNroPP
	"retorna un id unico para una nueva instancia de producto"
	(nroProductoPedido isNil) ifTrue: [nroProductoPedido := 0].
	nroProductoPedido := nroProductoPedido + 1.
	^nroProductoPedido.
! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:cantidad:!public! !
crearProductoPedidoNroProd:cantidad:costoUnitario:!public! !
initialize!public! !
nextNroPP!public! !
!

