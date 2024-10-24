"Filed out from Dolphin Smalltalk"!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroPedido nroProducto cantidad pedido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroProductoPedido'!

ProductoPedido guid: (GUID fromString: '{9ae45669-d20e-4e41-a18f-54a008d55e0e}')!

ProductoPedido comment: ''!

!ProductoPedido categoriesForClass!Kernel-Objects! !

!ProductoPedido methodsFor!

costo [
    | precio total |

    "Llama a la clase CatalogoProductos para obtener el precio del producto según nroProducto"
    precio := Panificadora precioProducto: nroProducto.

    "Calcula el costo total multiplicando la cantidad de productos por el precio unitario"
    total := cantidad * precio.
    
    ^ total.  "Devuelve el costo total del ProductoPedido"
]!

iniProductoPedido:unNroProduct  cantidad:cant pedido:unPedido
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroProdPed.
nroProducto := unNroProduct.
cantidad:= cant.
pedido := unPedido.
nroPedido := pedido verNroPedido.
!

modCantidad:unNro
	cantidad=cantidad+unNro.!

verCantidad
	^cantidad!

verIdProdPed
	"Retorna el nro de ID del Producto Pedidio"

	^idProductoPedido!

verNroPedido
	"Retorna el nro de pedido asociado al Producto pedido"

	^pedido verNroPedido!

verNroProducto
	"Retorna el nro de Pedido de ProductoPedido"

	^nroProducto! !

!ProductoPedido categoriesForMethods!
costo!public! !
iniProductoPedido:cantidad:pedido:!public! !
modCantidad:!public! !
verCantidad!public! !
verIdProdPed!public! !
verNroPedido!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:nroProducto pedido:unPedido cant:cantidad
nroProductoPedido := nroProductoPedido +1.
^(self new) iniProductoPedido: nroProductoPedido nroProd: nroProducto pedido: unPedido cantidad: cantidad.!

initialize
	[nroProductoPedido := 0]!

nextNroProdPed
	"retorna un id unico para una nueva instancia de ProductoPedido"

	| id |
	id := nroProductoPedido.
	nroProductoPedido := nroProductoPedido + 1.
	^id! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:pedido:cant:!public! !
initialize!public! !
nextNroProdPed!public! !
!

