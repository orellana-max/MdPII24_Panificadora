"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Pedido
	instanceVariableNames: 'nroPedido fecha fechaEntrega estado nroCliente nroRepartidorAsignado listaProductosPedidos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroPedidoStatic'!
Pedido guid: (GUID fromString: '{D2E215D0-9920-4AA0-84C6-40A60B01EC3A}')!
Pedido comment: ''!
!Pedido categoriesForClass!Kernel-Objects! !
!Pedido methodsFor!

iniPedidoNro: unNroPedi fechaEnt: fEnt nroCli: unNroCliente listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := unNroPedi.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed!

modiRepartidorAsignado: numeroRepartidor
	nroRepartidorAsignado := numeroRepartidor.!

verNroPedido
	^nroPedido.! !
!Pedido categoriesFor: #iniPedidoNro:fechaEnt:nroCli:listPP:!public! !
!Pedido categoriesFor: #modiRepartidorAsignado:!public! !
!Pedido categoriesFor: #verNroPedido!public! !

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
!Pedido class categoriesFor: #crearPedido:listProdPed:!public! !
!Pedido class categoriesFor: #crearPedido:nroClien:listProdPed:!public! !

