"Filed out from Dolphin Smalltalk"!

Object subclass: #Pedido
	instanceVariableNames: 'nroPedido fecha fechaEntrega estado nroCliente nroRepartidorAsignado listaProductosPedidos total'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroPedidoStatic'!

Pedido guid: (GUID fromString: '{d2e215d0-9920-4aa0-84c6-40a60b01ec3a}')!

Pedido comment: ''!

!Pedido categoriesForClass!Kernel-Objects! !

!Pedido methodsFor!

agregarProductoPedido: productoPedido
	"Agrega un ProductoPedido a la lista de ProductosPedidos"

	listaProductosPedidos add: productoPedido!

eliminarProductoPedido: unProductoPedido
	"Elimina un ProductoPedido de la lista de Productos Pedidos"

	| id nroProd |
	unProductoPedido notNil
		ifTrue: 
			[id := unProductoPedido verIdProdPed .
			nroProd := unProductoPedido verNroProducto .
			(listaProductosPedidos includes: unProductoPedido)
				ifTrue: 
					[listaProductosPedidos remove: unProductoPedido.
					MessageBox warning: 'Se elimino ProductoPedido id: ' , id printString , ' con el Producto Nro: ' , nroProd printString]
				ifFalse: 
					[MessageBox
						warning: 'No existe ProductoPedido id:  ' , id printString , ' con el Producto Nro: ' , nroProd printString , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El ProductoPedido es vacio']!

iniPedidoNroCliente: unNroCliente fechaEnt: fEnt  listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := Pedido nextNroPedido.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed!

modEstado: unEstado
	estado := unEstado.!

modiRepartidorAsignado: nroRepartidor
	nroRepartidorAsignado := nroRepartidor.!

verEstado
	^estado.!

verFecha
"Retorna la fecha de la creacion del pedido"
	^fecha.!

verFechaEntrega
	^fechaEntrega.!

verListaProductosPedidos
	"Retorna la lista de ProductosPedidos"

	^listaProductosPedidos!

verNroPedido
	^nroPedido.!

verNroRepartidor
	^nroRepartidorAsignado!

verTotal
	^total.! !

!Pedido categoriesForMethods!
agregarProductoPedido:!public! !
eliminarProductoPedido:!public! !
iniPedidoNroCliente:fechaEnt:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFecha!public! !
verFechaEntrega!public! !
verListaProductosPedidos!public! !
verNroPedido!public! !
verNroRepartidor!public! !
verTotal!public! !
!

!Pedido class methodsFor!

crearPedido:unNroClien listProdPed:lista
|f|
nroPedidoStatic := nroPedidoStatic +1.

f := Date tomorrow .
Transcript show: 'fecha elegida de entrega: ', f printString; cr.

^(self new) iniPedidoNro: nroPedidoStatic fechaEnt: f nroCli: unNroClien listPP: lista.!

crearPedido:pani nroClien:unNroClien listProdPed:lista
|f|
nroPedidoStatic := nroPedidoStatic +1.

f := Date tomorrow .
Transcript show: 'fecha elegida de entrega: ', f printString; cr.

^(self new) iniPedidoNro: nroPedidoStatic panificadora: pani fechaEnt: f nroCli: unNroClien listPP: lista.!

initialize
[nroPedidoStatic := 1]!

nextNroPedido
	"retorna un id unico para una nueva instancia de Pedido"

	| id |
	id := nroPedidoStatic .
	nroPedidoStatic := nroPedidoStatic + 1.
	^id! !

!Pedido class categoriesForMethods!
crearPedido:listProdPed:!public! !
crearPedido:nroClien:listProdPed:!public! !
initialize!public! !
nextNroPedido!public! !
!

