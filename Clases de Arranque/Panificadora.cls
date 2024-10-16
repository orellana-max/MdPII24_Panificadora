"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaProductos listaPedidos listaEmpleados listaClientes listaProveedores listaRepartidores'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Panificadora guid: (GUID fromString: '{17E874C9-7A3B-4335-A009-8BA9896A9A22}')!
Panificadora comment: ''!
!Panificadora categoriesForClass!Kernel-Objects! !
!Panificadora methodsFor!

agregarCliente: cliente
	"Agrega un Cliente a la lista de Clientes"

	listaClientes add: cliente.!

agregarCliente: unNum nom: unNom dire: unaDire tel: unTel
	"Crea y agrega un Cliente a la lista de Clientes"

	| clien |
	clien := Cliente crearClienteNro: unNum nom: unNom dire: unaDire tel: unTel.
	listaClientes add: clien.!

agregarPedidoCliente: unCliente prods: ProductosPedidos
	"Agrega un Pedido a la lista de Pedidos"
	|n |
	n:= unCliente verNroCliente.
	listaPedidos add: (Pedido crearPedido: n listProdPed: ProductosPedidos).
	!

agregarProducto: Producto
	"Agrega un producto a la lista de productos"
	listaProductos add: Producto.!

agregarProducto:unNum nom:unNom tip:unTipo prec:unPrecio
"Agrega un prodcto a la lista de poductos"
|prod|
prod:= Producto crearProductoNro: unNum nom: unNom tip: unTipo prec: unPrecio.
listaProductos add: prod.!

asignarRepartidorPedido: nroPedido
"asignar un repartidor libre al pedido"

|pedido repartidor|
pedido:= listaPedidos detect: [:ped | ped verNroPedido = nroPedido] ifNone: [^nil].
"seguir con buscar repartidor libre...."
repartidor := listaRepartidores detect: [:rep | rep libre = true] ifNone:[^nil].

"asignar repartidor"
pedido modiRepartidorAsignado: (repartidor  verNroRepartidor ).!

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
	msj := 'No existe producto: ' , nom , ' para eliminar. '.
	(listaProductos includes: producto)
		ifTrue: 
			[listaProductos remove: producto.
			Transcript show: 'se elimino Producto: ' , nom; cr]
		ifFalse: 
			[Transcript show: msj; cr]!

fabricarProducto:unProducto pana:unPanadero cantidad:unaCantidad
"Pone a producir un producto asignando al panadero correspondiente"
unPanadero producirProducto: unProducto cant:unaCantidad.!

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

modDireccion: unaDire
	"Modifica la Direccion de la Panificadora"

	direccion := unaDire.!

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

verDireccion
	"retorna la Direccion de la Panificadora"

	^direccion.!

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
!Panificadora categoriesFor: #agregarCliente:!public! !
!Panificadora categoriesFor: #agregarCliente:nom:dire:tel:!public! !
!Panificadora categoriesFor: #agregarPedidoCliente:prods:!public! !
!Panificadora categoriesFor: #agregarProducto:!public! !
!Panificadora categoriesFor: #agregarProducto:nom:tip:prec:!public! !
!Panificadora categoriesFor: #asignarRepartidorPedido:!public! !
!Panificadora categoriesFor: #eliminarCliente:!public! !
!Panificadora categoriesFor: #eliminarProducto:!public! !
!Panificadora categoriesFor: #fabricarProducto:pana:cantidad:!public! !
!Panificadora categoriesFor: #iniPanificadoraNom:dire:tel:!public! !
!Panificadora categoriesFor: #modDireccion:!public! !
!Panificadora categoriesFor: #modNombre:!public! !
!Panificadora categoriesFor: #modTelefono:!public! !
!Panificadora categoriesFor: #repartirPedido:!public! !
!Panificadora categoriesFor: #traerProducto:!public! !
!Panificadora categoriesFor: #verDireccion!public! !
!Panificadora categoriesFor: #verListaClientes!public! !
!Panificadora categoriesFor: #verListaProductos!public! !
!Panificadora categoriesFor: #verNombre!public! !
!Panificadora categoriesFor: #verTelefono!public! !

!Panificadora class methodsFor!

crearPanificadoraNom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Panificadora inicializada"

	^self new
		iniPanificadoraNom: unNom
		dire: unaDire
		tel: unTel! !
!Panificadora class categoriesFor: #crearPanificadoraNom:dire:tel:!public! !

