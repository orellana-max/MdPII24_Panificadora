"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Vendedor
	instanceVariableNames: 'nroVendedor pedidosVendidos'
	classVariableNames: 'UnNuevoNroVendedor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Vendedor guid: (GUID fromString: '{4355CCF0-7EEA-498A-9E87-CCEF4959DAED}')!
Vendedor comment: ''!
!Vendedor categoriesForClass!Kernel-Objects! !
!Vendedor methodsFor!

agregarPedidoVendido: unNroPedido
"Agrega un pedido a la lista de pedidos vendidos del vendedor"

pedidosVendidos add: unNroPedido.!

iniVendedorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Vendedor"

	self iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	nroVendedor := Vendedor nuevoNumeroVendedor.
	pedidosVendidos := OrderedCollection new.
	sueldo := 600000.!

verNroVendedor
"Retorna el numero de vendedor"

^nroVendedor.! !
!Vendedor categoriesFor: #agregarPedidoVendido:!public! !
!Vendedor categoriesFor: #iniVendedorLegajo:nom:dire:tel:!public! !
!Vendedor categoriesFor: #verNroVendedor!public! !

!Vendedor class methodsFor!

crearVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Vendedor inicializada"

	^(self new) iniVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

nuevoNumeroVendedor
"Retorna un nuevo numero de vendedor, primero se asegura que ya fue inicializada la variable de clase"
(UnNuevoNroVendedor isNil) ifTrue: [UnNuevoNroVendedor := 0].
UnNuevoNroVendedor := UnNuevoNroVendedor + 1.
^UnNuevoNroVendedor.! !
!Vendedor class categoriesFor: #crearVendedorLegajo:nom:dire:tel:!public! !
!Vendedor class categoriesFor: #nuevoNumeroVendedor!public! !

