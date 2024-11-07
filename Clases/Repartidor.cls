"Filed out from Dolphin Smalltalk"!

Empleado subclass: #Repartidor
	instanceVariableNames: 'nroRepartidor listaPedidosEntregados nroPedidoAsignado'
	classVariableNames: 'UnNuevoNroRepartidor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Repartidor guid: (GUID fromString: '{e520e4a8-e744-4822-9cf7-93bf17a6e60e}')!

Repartidor comment: ''!

!Repartidor categoriesForClass!cat_max1! !

!Repartidor methodsFor!

agregarPedidoRealizado: unNroPedido
	"agrega un pedido a la lista de pedidos realizados"

	listaPedidosEntregados add: unNroPedido!

asignarPedido: nroPedido
"asigna un nuevo numero de pedido para entregar"
nroPedidoAsignado := nroPedido.!

imprimir
^'Repartidor nro:  ', nroRepartidor printString , ' | ', super imprimir.!

iniRepartidorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Repartidor"

	self
		iniEmpleadoLegajo: unLegajo
		nom: unNombre
		dire: unaDire
		tel: unTel.
	nroRepartidor := Repartidor nuevoNroRepartidor.
	sueldo := 500000.
	nroPedidoAsignado := 0.
	listaPedidosEntregados := OrderedCollection new!

liberarRepartidor
"Solamente cambia a cero el pedido asignado, asi el repartidor estara libre para un hacer un nuevo pedido "
self agregarPedidoRealizado: nroPedidoAsignado.
nroPedidoAsignado := 0.!

printOn: aStream
	aStream
		nextPutAll: 'Repartidor nro:  ';
		nextPutAll: nroRepartidor printString , ' | '.
	super printOn: aStream!

repartir: pedido
"Reparte el pedido asigando"
|nroPed|
nroPed := pedido verNroPedido.
self asignarPedido: nroPed.
self liberarRepartidor.

Transcript show: 'El repartidor nro : ', nroRepartidor printString, ' repartio el pedido nro: ', nroPed printString ; cr.!

verListaPedidosEntregados
"retorna la lista de pedidos asignados"

^listaPedidosEntregados.!

verNroPedidoAsignado
	^nroPedidoAsignado .!

verNroRepartidor
	^nroRepartidor.! !

!Repartidor categoriesForMethods!
agregarPedidoRealizado:!public! !
asignarPedido:!public! !
imprimir!public! !
iniRepartidorLegajo:nom:dire:tel:!public! !
liberarRepartidor!public! !
printOn:!public! !
repartir:!public! !
verListaPedidosEntregados!public! !
verNroPedidoAsignado!public! !
verNroRepartidor!public! !
!

!Repartidor class methodsFor!

crearRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Repartidor inicializada"

	^(self new) iniRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

nuevoNroRepartidor
"Retorna un nuevo nuevo de repartidor, primero se asegura que este inicializada la variable de clase "
(UnNuevoNroRepartidor isNil) ifTrue: [UnNuevoNroRepartidor := 0].
UnNuevoNroRepartidor := UnNuevoNroRepartidor + 1.
^UnNuevoNroRepartidor.! !

!Repartidor class categoriesForMethods!
crearRepartidorLegajo:nom:dire:tel:!public! !
nuevoNroRepartidor!public! !
!

