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

agregarProveedor: unProveedor
	"Agrega un Provedor a la lista de productos"

	listaProveedores add: unProveedor!

asignarRepartidorPedido: nroPedido
"asignar un repartidor libre al pedido"

|pedido repartidor|
"pedido:= listaPedidos detect: [:ped | ped verNroPedido = nroPedido] ifNone: [^nil]."
"seguir con buscar repartidor libre...."
repartidor := listaRepartidores detect: [:rep | rep libre = true] ifNone:[^nil].

"asignar repartidor"
"pedido modiRepartidorAsignado: (repartidor  verNroRepartidor )."!

eliminarCliente: unCliente
	"Elimina un Cliente de la lista de Clientes"

	| nom nro |
	nom := unCliente verNombre.
	nro:= unCliente verNroProveedor.
	(listaClientes includes: unCliente)
		ifTrue: 
			[listaClientes remove: unCliente.
			MessageBox warning:  'Se elimino Cliente Nro: ' ,nro printString, ' Nombre: ', nom.]
		ifFalse: 
			[MessageBox warning:  'No existe Cliente Nro: ' ,nro printString, ' Nombre: ', nom , ' en la lista.' .]
	
!

eliminarProducto: unProducto
	"Elimina un producto de la lista de poductos"

	| nom nro |
	unProducto notNil
		ifTrue: 
			[nom := unProducto verNombre.
			nro := unProducto verNroProducto.
			(listaProductos includes: unProducto)
				ifTrue: 
					[listaProductos remove: unProducto.
					MessageBox warning: 'Se elimino Producto Nro: ' , nro printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Producto Nro: ' , nro printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Producto es vacio']!

eliminarProveedor: unProveedor
	"Elimina un Provedor de la lista de poductos"

	| nom nro |
	nom := unProveedor verNombre.
	nro:= unProveedor verNroProveedor.
	(listaProveedores includes: unProveedor)
		ifTrue: 
			[listaProveedores remove: unProveedor.
			MessageBox warning:  'Se elimino Provedor Nro: ' ,nro printString, ' Nombre: ', nom.]
		ifFalse: 
			[MessageBox warning:  'No existe Proveedor Nro: ' ,nro printString, ' Nombre: ', nom , ' en la lista.' .]!

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

modDireccion: unaDire
	"Modifica la Direccion de la Panificadora"

	direccion := unaDire!

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

traerPrecioProducto:nroProducto
"Retorna el precio de un producto, cero si no  existe producto en la lista de Productos"
|pro prec|
pro := self traerProducto: nroProducto.
prec := 0.0.
(pro notNil) ifTrue: [prec := pro verPrecio ].

^prec.
!

traerProducto: nroProd
	"Retorna el producto de nro de producto parado por parametro, nil en caso contrario"

	^listaProductos detect: [:pro | pro verNroProducto = nroProd] ifNone: [^nil].!

verDireccion
	"retorna la Direccion de la Panificadora"

	^direccion!

verListaClientes
	"Retorna la lista de Clientes"

	^listaClientes.!

verListaProductos
	"Retorna la lista de productos"

	^listaProductos!

verListaProveedores
	"Retorna la lista de Proveedores"

	^listaProveedores!

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
agregarProveedor:!public! !
asignarRepartidorPedido:!public! !
eliminarCliente:!public! !
eliminarProducto:!public! !
eliminarProveedor:!public! !
fabricarProducto:pana:cantidad:!public! !
iniPanificadoraNom:dire:tel:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
repartirPedido:!public! !
traerPrecioProducto:!public! !
traerProducto:!public! !
verDireccion!public! !
verListaClientes!public! !
verListaProductos!public! !
verListaProveedores!public! !
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

