"Filed out from Dolphin Smalltalk"!

Empleado subclass: #Vendedor
	instanceVariableNames: 'nroVendedor pedidosVendidos'
	classVariableNames: 'UnNuevoNroVendedor'
	poolDictionaries: ''
	classInstanceVariableNames: 'idVendedor'!

Vendedor guid: (GUID fromString: '{4355ccf0-7eea-498a-9e87-ccef4959daed}')!

Vendedor comment: ''!

!Vendedor categoriesForClass!No category! !

!Vendedor methodsFor!

agregarPedidoVendido: unNroPedido
	"Agrega un pedido a la lista de pedidos vendidos del vendedor"

	pedidosVendidos add: unNroPedido!

imprimir
^'Vendedor nro:  ', nroVendedor printString , ' | ', super imprimir.!

iniVendedorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Vendedor"

	self iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	"nroVendedor := Vendedor nuevoNumeroVendedor."
	nroVendedor := Vendedor nextNroVendedor.
	pedidosVendidos := OrderedCollection new.
	sueldo := 600000.!

printOn: aStream
	aStream
		nextPutAll: 'Vendedor nro:  ';
		nextPutAll: nroVendedor printString , ' | '.
	super printOn: aStream!

verNroVendedor
"Retorna el numero de vendedor"

^nroVendedor.!

verPedidosVendidos
	"retorna la listab de los pedidos vendidos por el vendedor"

	^pedidosVendidos! !

!Vendedor categoriesForMethods!
agregarPedidoVendido:!public! !
imprimir!public! !
iniVendedorLegajo:nom:dire:tel:!public! !
printOn:!public! !
verNroVendedor!public! !
verPedidosVendidos!public! !
!

!Vendedor class methodsFor!

crearVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Vendedor inicializada"

	^(self new) iniVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

initialize
	idVendedor := 0
	"UnNuevoNroVendedor := 0."!

nextNroVendedor
	"Retorna un id unico para una instancia nueva de Vendedor"
	| id |

	(idVendedor isNil) ifTrue: [idVendedor := 0].
	id := idVendedor.
	idVendedor := idVendedor + 1.
	^id.!

nuevoNumeroVendedor
"Retorna un nuevo numero de vendedor, primero se asegura que ya fue inicializada la variable de clase"
(UnNuevoNroVendedor isNil) ifTrue: [UnNuevoNroVendedor := 0].
UnNuevoNroVendedor := UnNuevoNroVendedor + 1.
^UnNuevoNroVendedor.! !

!Vendedor class categoriesForMethods!
crearVendedorLegajo:nom:dire:tel:!public! !
initialize!public! !
nextNroVendedor!public! !
nuevoNumeroVendedor!public! !
!

