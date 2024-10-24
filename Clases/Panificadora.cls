"Filed out from Dolphin Smalltalk"!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaClientes listaEmpleados listaPanaderos listaPedidos listaProductos listaProveedores listaRepartidores listaVendedores'
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

agregarEmpleado: empleado
	"Agrega un Empleado a la lista de Empleados"

	listaEmpleados add: empleado.!

agregarPanadero: panadero
	"Agrega un panadero a la lista de panaderos"

	listaPanaderos add: panadero!

agregarPedido: pedido
	"Agrega un pedido a la lista de Pedidos"

	listaPedidos add: pedido.!

agregarProducto: producto
	"Agrega un producto a la lista de productos"

	listaProductos add: producto!

agregarProveedor: unProveedor
	"Agrega un Provedor a la lista de productos"

	listaProveedores add: unProveedor!

agregarRepartidor: repartidor
	"Agrega un repartidor a la lista de repartidores"

	listaRepartidores add: repartidor!

agregarVendedor: vendedor
	"Agrega un Vendedor a la lista de Vendedor"

	listaVendedores add: vendedor.!

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

	unCliente notNil
		ifTrue: 
			[ | nom nro |
			nom := unCliente verNombre.
			nro := unCliente verNroCliente.
			(listaClientes includes: unCliente)
				ifTrue: 
					[listaClientes remove: unCliente.
					MessageBox warning: 'Se elimino Cliente Nro: ' , nro printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Cliente Nro: ' , nro printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Cliente es vacio']!

eliminarEmpleado: unEmpleado
	"Elimina un Empleado de la lista de Empleados"

	unEmpleado notNil
		ifTrue: 
			[ | nom leg |
			nom := unEmpleado verNombre.
			leg := unEmpleado verLegajo .
			(listaEmpleados includes: unEmpleado)
				ifTrue: 
					[listaEmpleados remove: unEmpleado.
					MessageBox warning: 'Se elimino Empleado Legajo ' , leg printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Empleado legajo ' , leg printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Empleado es vacio']!

eliminarPanadero: unPanadero
	"Elimina un Panadero de la lista de Panaderos"

	unPanadero notNil
		ifTrue: 
			[ | nom leg |
			nom := unPanadero verNombre.
			leg := unPanadero verLegajo .
			(listaPanaderos includes: unPanadero)
				ifTrue: 
					[listaPanaderos remove: unPanadero.
					MessageBox warning: 'Se elimino Panadero Legajo ' , leg printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Panadero legajo ' , leg printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Panadero es vacio']!

eliminarPedido: unPedido
	"Elimina un Pedido de la lista de Pedidos"

	| fecha nro |
	unPedido notNil
		ifTrue: 
			[fecha := unPedido verFecha .
			nro := unPedido verNroPedido .
			(listaPedidos includes: unPedido)
				ifTrue: 
					[listaPedidos remove: unPedido.
					MessageBox warning: 'Se elimino Pedido Nro: ' , nro printString , ' Fecha: ' , fecha printString]
				ifFalse: 
					[MessageBox
						warning: 'No existe Pedido Nro: ' , nro printString , ' Fecha: ' , fecha printString , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Pedido es vacio']!

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
	"Elimina un Provedor de la lista de Proveedores"

	| nom nro |
	unProveedor notNil
		ifTrue: 
			[nom := unProveedor verNombre.
			nro := unProveedor verNroProveedor.
			(listaProveedores includes: unProveedor)
				ifTrue: 
					[listaProveedores remove: unProveedor.
					MessageBox warning: 'Se elimino Proveedor Nro: ' , nro printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Proveedor Nro: ' , nro printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Proveedor es vacio']!

eliminarRepartidor: unRepartidor
	"Elimina un Repartidor de la lista de Repartidores"

	| nom nro |
	unRepartidor notNil
		ifTrue: 
			[nom := unRepartidor verNombre.
			nro := unRepartidor verNroRepartidor .
			(listaRepartidores includes: unRepartidor)
				ifTrue: 
					[listaRepartidores remove: unRepartidor.
					MessageBox warning: 'Se elimino Repartidor Nro: ' , nro printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Repartidor Nro: ' , nro printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Repartidor es vacio']!

eliminarVendedor: unVendedor
	"Elimina un Vendedor de la lista de Vendedores"

	| nom nro |
	unVendedor notNil
		ifTrue: 
			[nom := unVendedor verNombre.
			nro := unVendedor verNroVendedor.
			(listaVendedores includes: unVendedor)
				ifTrue: 
					[listaVendedores remove: unVendedor.
					MessageBox warning: 'Se elimino Vendedor Nro: ' , nro printString , ' Nombre: ' , nom]
				ifFalse: 
					[MessageBox
						warning: 'No existe Vendedor Nro: ' , nro printString , ' Nombre: ' , nom , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El Vendedor es vacio']!

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
	listaProveedores := OrderedCollection new.
	listaPanaderos := OrderedCollection new.
	listaVendedores := OrderedCollection new.
	listaRepartidores := OrderedCollection new.!

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

traerPrecioProducto: nroProducto
	"Retorna el precio de un producto, cero si no  existe producto en la lista de Productos"

	| pro prec |
	pro := self traerProductoNro: nroProducto.
	prec := 0.0.
	pro notNil ifTrue: [prec := pro verPrecio].
	^prec!

traerProductoNro: nroProducto
	"Retorna el producto de nro de producto pasado por parametro, nil en caso contrario"

	^listaProductos detect: [:pro | pro verNroProducto = nroProducto] ifNone: [^nil]!

verDireccion
	"retorna la Direccion de la Panificadora"

	^direccion!

verListaClientes
	"Retorna la lista de Clientes"

	^listaClientes.!

verListaEmpleados
	"Retorna la lista de Empleados"

	^listaEmpleados .!

verListaPanaderos
	"Retorna la lista de Panaderos"

	^listaPanaderos .!

verListaPedidos
	"Retorna la lista de Pedidos"

	^listaPedidos .!

verListaProductos
	"Retorna la lista de productos"

	^listaProductos!

verListaProveedores
	"Retorna la lista de Proveedores"

	^listaProveedores!

verListaRepartidores
	"Retorna la lista de Repartidores"

	^listaRepartidores .!

verListaVendedores
	"Retorna la lista de Vendedores"

	^listaVendedores .!

verNombre
"retorna el nombre de la Panificadora"
^nombre.!

verTelefono
"retorna el telefono de la Panificadora"
^telefono.! !

!Panificadora categoriesForMethods!
agregarCliente:!public! !
agregarEmpleado:!public! !
agregarPanadero:!public! !
agregarPedido:!public! !
agregarProducto:!public! !
agregarProveedor:!public! !
agregarRepartidor:!public! !
agregarVendedor:!public! !
asignarRepartidorPedido:!public! !
eliminarCliente:!public! !
eliminarEmpleado:!public! !
eliminarPanadero:!public! !
eliminarPedido:!public! !
eliminarProducto:!public! !
eliminarProveedor:!public! !
eliminarRepartidor:!public! !
eliminarVendedor:!public! !
fabricarProducto:pana:cantidad:!public! !
iniPanificadoraNom:dire:tel:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
repartirPedido:!public! !
traerPrecioProducto:!public! !
traerProductoNro:!public! !
verDireccion!public! !
verListaClientes!public! !
verListaEmpleados!public! !
verListaPanaderos!public! !
verListaPedidos!public! !
verListaProductos!public! !
verListaProveedores!public! !
verListaRepartidores!public! !
verListaVendedores!public! !
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

