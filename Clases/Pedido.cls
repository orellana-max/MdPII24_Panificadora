"Filed out from Dolphin Smalltalk"!

Object subclass: #Pedido
	instanceVariableNames: 'nroPedido fecha fechaEntrega estado nroCliente nroRepartidorAsignado listaProductosPedidos total'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroPedidoStatic'!

Pedido guid: (GUID fromString: '{d2e215d0-9920-4aa0-84c6-40a60b01ec3a}')!

Pedido comment: ''!

!Pedido categoriesForClass!No category! !

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

agregarProductoPedidoProd: unProducto cantidad: cantidad
   | PPExistente nuevoPP |
   
   PPExistente := listaProductosPedidos detect: [ :p | p verNroProducto = unProducto verNroProducto ] ifNone: [ nil ].
   
   PPExistente ifNotNil: [ PPExistente sumarCantidad:  cantidad.]
      ifNil: [
        nuevoPP := ProductoPedido crearProductoPedido: unProducto pedido: self cantidad: cantidad.
	listaProductosPedidos add: nuevoPP.
	total := 0.
	listaProductosPedidos do: [ :pp | total := total + pp verCosto.].
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

imprimir

|cad|
cad:= 'Pedido Nro: ', nroPedido printString, ' Fecha: ', fecha printString, ' Fecha Entrega: ', fechaEntrega printString, 
' Cliente Nro: ', nroCliente printString, ' Repartidor Nro: ', nroRepartidorAsignado printString,
' total: $', self verTotal printString. 
^cad.!

imprimirTrpt

|cad|
cad:= '--------------------------------------------------------------'.
Transcript show: cad;cr.
Transcript nextPutAll: 'Pedido Nro: ', nroPedido printString;cr.
Transcript nextPutAll: 'Fecha: ', fecha printString;cr.
Transcript nextPutAll: 'Fecha Entrega: ', fechaEntrega printString;cr.
Transcript nextPutAll: 'Cliente Nro: ', nroCliente printString;cr.
Transcript nextPutAll: 'Repartidor Asignado Nro: ', nroRepartidorAsignado printString;cr.
Transcript nextPutAll: 'Total: $', self verTotal printString; cr. 
Transcript show: cad;cr.!

iniPedidoNroCli: unNroCliente fechaEnt: fEnt  listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := Pedido nextId.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed.
	total := 0.
	listaProductosPedidos do: [ :pp | 
		total := total + pp verCosto.
		pp setPedido: self.
	].
!

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

verNroCliente
^nroCliente.!

verNroPedido
	^nroPedido.!

verRepartidor
	^nroRepartidorAsignado.!

verTotal
   total := 0.
   listaProductosPedidos do: [ :pp | total := total + pp verCosto .
   ].
   ^ total.! !

!Pedido categoriesForMethods!
agregarProductoPedido:cantidad:!public! !
agregarProductoPedidoProd:cantidad:!public! !
eliminarProductoPedido:!public! !
imprimir!public! !
imprimirTrpt!public! !
iniPedidoNroCli:fechaEnt:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFechaEntrega!public! !
verListaProductosPedidos!public! !
verNroCliente!public! !
verNroPedido!public! !
verRepartidor!public! !
verTotal!public! !
!

!Pedido class methodsFor!

crearPedidoNroClien:unNroClien listProdPed: lista
	"Crea un pedido con el numero de cliente y la lista de productos pedidos"

	| fEnt |
	fEnt := Date tomorrow.
	^self new iniPedidoNroCli: unNroClien fechaEnt: fEnt listPP: lista.!

crearPedidoNroCliente:unNroClien fechaEntrega:unaFecha listProdPed:lista
"Crea un pedido con el numero de cliente y la lista de productos pedidos"

^(self new) iniPedidoNroCli: unNroClien fechaEnt: unaFecha listPP: lista.

!

initialize

nroPedidoStatic :=1.!

nextId

|id|
id := nroPedidoStatic.
nroPedidoStatic := nroPedidoStatic +1.
^id.! !

!Pedido class categoriesForMethods!
crearPedidoNroClien:listProdPed:!public! !
crearPedidoNroCliente:fechaEntrega:listProdPed:!public! !
initialize!public! !
nextId!public! !
!

