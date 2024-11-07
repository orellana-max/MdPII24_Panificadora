"Filed out from Dolphin Smalltalk"!

Object subclass: #Pedido
	instanceVariableNames: 'nroPedido fecha fechaEntrega estado nroCliente nroRepartidorAsignado listaProductosPedidos total'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroPedidoStatic'!

Pedido guid: (GUID fromString: '{d2e215d0-9920-4aa0-84c6-40a60b01ec3a}')!

Pedido comment: ''!

!Pedido categoriesForClass!cat_max1! !

!Pedido methodsFor!

agregarProductoPedido: nroProducto cantidad: cantidad
   | productoExistente nuevoProductoPedido |
   
   productoExistente := listaProductosPedidos detect: [ :p | p verNroProducto = nroProducto ] ifNone: [ nil ].
   
   productoExistente
      ifNotNil: [ productoExistente modCantidad: (productoExistente verCantidad + cantidad) ]
      ifNil: [
         nuevoProductoPedido := ProductoPedido new.
         nuevoProductoPedido iniProductoPedido: (listaProductosPedidos size + 1)
                                nroProd: nroProducto
                                pedido: self
                                cantidad: cantidad.
         listaProductosPedidos add: nuevoProductoPedido
      ].!

eliminarProductoPedido: nroProducto
   | productoAEliminar |
   productoAEliminar := listaProductosPedidos detect: [ :p | p verNroProducto = nroProducto ] ifNone: [ nil ].
   productoAEliminar
      ifNotNil: [
         listaProductosPedidos remove: productoAEliminar.
      ]
      ifNil: [
         Transcript show: 'Producto no encontrado en el pedido'; cr.
      ].!

iniPedidoNro: unNroPedi fechaEnt: fEnt nroCli: unNroCliente listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := unNroPedi.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed!

modEstado: unEstado
	estado := unEstado.!

modiRepartidorAsignado: numeroRepartidor
	nroRepartidorAsignado := numeroRepartidor.!

verEstado
	^estado.!

verFechaEntrega
	^fechaEntrega.!

verListaProductosPedidos
   | listaTexto |
   listaTexto := String streamContents: [ :stream |
      listaProductosPedidos do: [ :producto |
         stream
            nextPutAll: ', Nro Producto: ', producto verNroProducto printString;
            nextPutAll: ', Cantidad: ', producto verCantidad printString;
            nextPutAll: ', Nro Pedido: ', producto verNroPedido printString;
            nextPutAll: '; '.
      ].
   ].
   Transcript show: listaTexto; cr.!

verNroPedido
	^nroPedido.!

verRepartidor
	^nroRepartidorAsignado.!

verTotal
   total := 0.
   listaProductosPedidos do: [ :productoPedido |
      total := total + productoPedido verCosto .
   ].

   ^ total.! !

!Pedido categoriesForMethods!
agregarProductoPedido:cantidad:!public! !
eliminarProductoPedido:!public! !
iniPedidoNro:fechaEnt:nroCli:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFechaEntrega!public! !
verListaProductosPedidos!public! !
verNroPedido!public! !
verRepartidor!public! !
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

^(self new) iniPedidoNro: nroPedidoStatic panificadora: pani fechaEnt: f nroCli: unNroClien listPP: lista.! !

!Pedido class categoriesForMethods!
crearPedido:listProdPed:!public! !
crearPedido:nroClien:listProdPed:!public! !
!

