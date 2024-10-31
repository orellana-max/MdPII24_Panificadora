"Filed out from Dolphin Smalltalk"!

Empleado subclass: #Vendedor
	instanceVariableNames: 'nroVendedor listaPedidosVendidos'
	classVariableNames: 'UnNuevoNroVendedor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Vendedor guid: (GUID fromString: '{4355ccf0-7eea-498a-9e87-ccef4959daed}')!

Vendedor comment: ''!

!Vendedor categoriesForClass!Kernel-Objects! !

!Vendedor methodsFor!

agregarPedidoVendido: unNroPedido
	"Agrega un pedido a la lista de pedidos vendidos del vendedor"

	listaPedidosVendidos add: unNroPedido!

imprimir
^'Vendedor nro:  ', nroVendedor printString , ' | ', super imprimir.!

iniVendedorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Vendedor"

	self
		iniEmpleadoLegajo: unLegajo
		nom: unNombre
		dire: unaDire
		tel: unTel.
	nroVendedor := Vendedor nuevoNumeroVendedor.
	listaPedidosVendidos := OrderedCollection new.
	sueldo := 600000!

printOn: aStream
	aStream
		nextPutAll: 'Vendedor nro:  ';
		nextPutAll: nroVendedor printString , ' | '.
	super printOn: aStream!

verListaPedidosVendidos
	"retorna la listab de los pedidos vendidos por el vendedor"

	^listaPedidosVendidos!

verNroVendedor
"Retorna el numero de vendedor"

^nroVendedor.! !

!Vendedor categoriesForMethods!
agregarPedidoVendido:!public! !
imprimir!public! !
iniVendedorLegajo:nom:dire:tel:!public! !
printOn:!public! !
verListaPedidosVendidos!public! !
verNroVendedor!public! !
!

!Vendedor class methodsFor!

crearVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Vendedor inicializada"

	^(self new) iniVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

nuevoNumeroVendedor
"Retorna un nuevo numero de vendedor, primero se asegura que ya fue inicializada la variable de clase"
(UnNuevoNroVendedor isNil) ifTrue: [UnNuevoNroVendedor := 0].
UnNuevoNroVendedor := UnNuevoNroVendedor + 1.
^UnNuevoNroVendedor.! !

!Vendedor class categoriesForMethods!
crearVendedorLegajo:nom:dire:tel:!public! !
nuevoNumeroVendedor!public! !
!

