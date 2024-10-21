"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Repartidor
	instanceVariableNames: 'nroRepartidor pedidosEntregados nroPedidoAsignado'
	classVariableNames: 'UnNuevoNroRepartidor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Repartidor guid: (GUID fromString: '{E520E4A8-E744-4822-9CF7-93BF17A6E60E}')!
Repartidor comment: ''!
!Repartidor categoriesForClass!Kernel-Objects! !
!Repartidor methodsFor!

agregarPedidoRealizado: unNroPedido
"agrega un pedido a la lista de pedidos realizados"
pedidosEntregados add: unNroPedido.!

asignarPedido: numeroPedido
"asigna un nuevo numero de pedido para entregar"
nroPedidoAsignado := numeroPedido.!

iniRepartidorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel 
	"Inicializa una instancia de Repartidor"
	

	self iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	nroRepartidor := Repartidor nuevoNroRepartidor.
	sueldo := 500000.
	nroPedidoAsignado := 0.
	pedidosEntregados := OrderedCollection new.!

liberarRepartidor
"Solamente cambia a cero el pedido asignado, asi el repartidor estara libre para un hacer un nuevo pedido "
self agregarPedidoRealizado: nroPedidoAsignado.
nroPedidoAsignado := 0.!

verNroRepartidor
	^nroRepartidor.! !
!Repartidor categoriesFor: #agregarPedidoRealizado:!public! !
!Repartidor categoriesFor: #asignarPedido:!public! !
!Repartidor categoriesFor: #iniRepartidorLegajo:nom:dire:tel:!public! !
!Repartidor categoriesFor: #liberarRepartidor!public! !
!Repartidor categoriesFor: #verNroRepartidor!public! !

!Repartidor class methodsFor!

crearRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Repartidor inicializada"

	^(self new) iniRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

nuevoNroRepartidor
"Retorna un nuevo nuevo de repartidor, primero se asegura que este inicializada la variable de clase "
(UnNuevoNroRepartidor isNil) ifTrue: [UnNuevoNroRepartidor := 0].
UnNuevoNroRepartidor := UnNuevoNroRepartidor + 1.
^UnNuevoNroRepartidor.! !
!Repartidor class categoriesFor: #crearRepartidorLegajo:nom:dire:tel:!public! !
!Repartidor class categoriesFor: #nuevoNroRepartidor!public! !

