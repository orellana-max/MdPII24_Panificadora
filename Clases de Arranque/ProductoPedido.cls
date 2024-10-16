"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroProducto cantidad nroPedido pedido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroProductoPedido'!
ProductoPedido guid: (GUID fromString: '{9AE45669-D20E-4E41-A18F-54A008D55E0E}')!
ProductoPedido comment: ''!
!ProductoPedido categoriesForClass!Kernel-Objects! !
!ProductoPedido methodsFor!

costo
"retornar el costo del producto pedido"!

iniProductoPedido:unId nroProd:unNroProduct pedido:unPedido cantidad:cant
"inicializa una instancia de ProductoPedido"

idProductoPedido := unId.
nroProducto := unNroProduct.
pedido := unPedido.
nroPedido := pedido verNroPedido.
cantidad:= cant.! !
!ProductoPedido categoriesFor: #costo!public! !
!ProductoPedido categoriesFor: #iniProductoPedido:nroProd:pedido:cantidad:!public! !

!ProductoPedido class methodsFor!

crearProductoPedido:nroProducto pedido:unPedido cant:cantidad
nroProductoPedido := nroProductoPedido +1.
^(self new) iniProductoPedido: nroProductoPedido nroProd: nroProducto pedido: unPedido cantidad: cantidad.! !
!ProductoPedido class categoriesFor: #crearProductoPedido:pedido:cant:!public! !

