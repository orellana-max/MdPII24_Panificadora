"Filed out from Dolphin Smalltalk"!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroProducto cantidad nroPedido pedido'
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

iniProductoPedido:unId nroProd:unNroProduct pedido:unPedido cantidad:cant
"inicializa una instancia de ProductoPedido"

idProductoPedido := unId.
nroProducto := unNroProduct.
pedido := unPedido.
nroPedido := pedido verNroPedido.
cantidad:= cant.!

modCantidad:unNro
	cantidad=cantidad+unNro.!

verCantidad
	^cantidad! !

!ProductoPedido categoriesForMethods!
costo!public! !
iniProductoPedido:nroProd:pedido:cantidad:!public! !
modCantidad:!public! !
verCantidad!public! !
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

