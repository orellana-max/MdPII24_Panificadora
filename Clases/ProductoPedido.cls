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

iniProductoPedido:unId nroProd:unNroProduct pedido:unPedido cantidad:cant costoUnitario:cost
"inicializa una instancia de ProductoPedido"

idProductoPedido := unId.
nroProducto := unNroProduct.
pedido := unPedido.
nroPedido := pedido verNroPedido.
cantidad:= cant.
costoUnitario:=cost.!

modCantidad:unNro
	cantidad=cantidad+unNro.!

modCostoUnitario:unNro
	costoUnitario =unNro.!

verCantidad
	^cantidad.!

verCosto [
    | precio total |
    precio := costoUnitario .
    total := cantidad * precio.
    
    ^ total. 
]!

verCostoUnitario
	^costoUnitario.!

verNroProducto
	^nroProducto.! !

!ProductoPedido categoriesForMethods!
iniProductoPedido:nroProd:pedido:cantidad:costoUnitario:!public! !
modCantidad:!public! !
modCostoUnitario:!public! !
verCantidad!public! !
verCosto!public! !
verCostoUnitario!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:nroProducto pedido:unPedido cant:cantidad
nroProductoPedido := nroProductoPedido +1.
^(self new) iniProductoPedido: nroProductoPedido nroProd: nroProducto pedido: unPedido cantidad: cantidad.!

initialize [
    nroProductoPedido := 0.
]! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:pedido:cant:!public! !
initialize!public! !
!

