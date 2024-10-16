"Filed out from Dolphin Smalltalk"!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaProductos listaPedidos listaEmpleados listaClientes listaProveedores listaRepartidores'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Panificadora guid: (GUID fromString: '{17e874c9-7a3b-4335-a009-8ba9896a9a22}')!

Panificadora comment: ''!

!Panificadora categoriesForClass!Kernel-Objects! !

!Panificadora methodsFor!

agregarCliente: cliente
	"Agrega un Cliente a la lista de Clientes"

	listaClientes add: cliente.!

agregarCliente: unNum nom: unNom dire: unaDire tel: unTel
	"Crea y agrega un Cliente a la lista de Clientes"

	"| clien |
	clien := Cliente crearClienteNro: unNum nom: unNom dire: unaDire tel: unTel.
	listaClientes add: clien."!

agregarPedidoCliente: unCliente prods: ProductosPedidos
	"Agrega un Pedido a la lista de Pedidos"
	"|n |
	n:= unCliente verNroCliente.
	listaPedidos add: (Pedido crearPedido: n listProdPed: ProductosPedidos)."
	!

agregarProducto: producto
	"Agrega un producto a la lista de productos"

	listaProductos add: producto!

agregarProducto:unNom tip:unTipo prec:unPrecio
"Crea y Agrega un producto a la lista de poductos"
|prod|
prod:= Producto crearProductoNombre: unNom tip: unTipo prec: unPrecio .
listaProductos add: prod.!

asignarRepartidorPedido: nroPedido
"asignar un repartidor libre al pedido"

|pedido repartidor|
"pedido:= listaPedidos detect: [:ped | ped verNroPedido = nroPedido] ifNone: [^nil]."
"seguir con buscar repartidor libre...."
repartidor := listaRepartidores detect: [:rep | rep libre = true] ifNone:[^nil].

"asignar repartidor"
"pedido modiRepartidorAsignado: (repartidor  verNroRepartidor )."!

direccion
	"retorna la Direccion de la Panificadora"

	^direccion.!

direccion: unaDire
	"Modifica la Direccion de la Panificadora"

	direccion := unaDire.!

eliminarCliente: cliente
	"Elimina un Cliente de la lista de Clientes"

	| msj nom|
	nom:= cliente verNombre.
	msj := 'No existe Cliente ' , nom , ' para eliminar. '.

	(listaClientes includes: cliente) ifTrue: [listaClientes remove: cliente.
	Transcript show: ('se elimino Cliente' , nom); cr] ifFalse: [ Transcript show: msj; cr ].
	
!

eliminarProducto: producto
	"Elimina un producto de la lista de poductos"

	| msj nom |
	nom := producto verNombre.
	msj := 'No existe producto: ' , nom , ' en la lista.' .
	(listaProductos includes: producto)
		ifTrue: 
			[listaProductos remove: producto.
			Transcript show: 'se elimino Producto: ' , nom; cr]
		ifFalse: 
			[Transcript show: msj; cr]!

fabricarProducto: unProducto pana: unPanadero cantidad: unaCantidad
	"Pone a producir un producto asignando al panadero correspondiente"

	"unPanadero producirProducto: unProducto cant:unaCantidad."

	!

iniPanificadoraNom: unNom dire: unaDire tel: unTel
	"Inicializa una instancia de Panificadora"

	nombre := unNom.
	direccion := unaDire.
	telefono := unTel.
	listaProductos := OrderedCollection new.
	listaPedidos := OrderedCollection new.
	listaEmpleados := OrderedCollection new.
	listaClientes := OrderedCollection new.
	listaProveedores := OrderedCollection new!

modNombre: unNombre
	"Modifca el nombre de la Panificadora"

	nombre := unNombre.!

modTelefono: unTelefono
	"Modifca el telefono de la Panificadora"

	telefono := unTelefono.!

repartirPedido:unPedido
"verificar si el pedido tiene repartidor y asignar uno"
"verificar si el pedido esta en estado listo para repartir"

"unPedido repartir."
"agregar a viajes realizados del repartidor"!

traerProducto: nroProd
	"Retorna el producto de nro de producto parado por parametro, nil en caso contrario"

	^listaProductos detect: [:pro | pro verNroProducto = nroProd] ifNone: [^nil].!

verListaClientes
	"Retorna la lista de Clientes"

	^listaClientes.!

verListaProductos
	"Retorna la lista de productos"

	^listaProductos!

verNombre
"retorna el nombre de la Panificadora"
^nombre.!

verTelefono
"retorna el telefono de la Panificadora"
^telefono.! !

!Panificadora categoriesForMethods!
agregarCliente:!public! !
agregarCliente:nom:dire:tel:!public! !
agregarPedidoCliente:prods:!public! !
agregarProducto:!public! !
agregarProducto:tip:prec:!public! !
asignarRepartidorPedido:!public! !
direccion!public! !
direccion:!public! !
eliminarCliente:!public! !
eliminarProducto:!public! !
fabricarProducto:pana:cantidad:!public! !
iniPanificadoraNom:dire:tel:!public! !
modNombre:!public! !
modTelefono:!public! !
repartirPedido:!public! !
traerProducto:!public! !
verListaClientes!public! !
verListaProductos!public! !
verNombre!public! !
verTelefono!public! !
!

!Panificadora class methodsFor!

crearPanificadoraNom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Panificadora inicializada"

	^self new
		iniPanificadoraNom: unNom
		dire: unaDire
		tel: unTel! !

!Panificadora class categoriesForMethods!
crearPanificadoraNom:dire:tel:!public! !
!

