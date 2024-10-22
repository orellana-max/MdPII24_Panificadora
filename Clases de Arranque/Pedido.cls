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

verNroPedido
	^nroPedido.!

verRepartidor
	^nroRepartidorAsignado.!

verTotal
	^total.! !

!Pedido categoriesForMethods!
iniPedidoNro:fechaEnt:nroCli:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFechaEntrega!public! !
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

