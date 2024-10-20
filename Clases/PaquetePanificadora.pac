| package |
package := Package name: 'PaquetePanificadora'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Panificadora;
	add: #Producto;
	add: #Proveedor;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box').

package!

"Class Definitions"!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaProductos listaPedidos listaEmpleados listaClientes listaProveedores listaRepartidores'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProducto tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProd'!

Object subclass: #Proveedor
	instanceVariableNames: 'nroProveedor direccion telefono tipoProductos nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProveedor'!

"End of package definition"!

"Source Globals"!

"Classes"!

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

Producto guid: (GUID fromString: '{7948a066-db33-4125-bacc-ab5d39ab576d}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

aumentarStock:cantidad
"aumenta la cantidad del stock del producto"
stock:=stock + cantidad.!

disminuirStock: cantidad
	"aumenta la cantidad del stock del producto"

	stock := stock - cantidad.!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Producto nro: ', nroProducto printString , ' | Nom: ' , nombreProducto , ' | tipo: ' , tipo , ' | Strock: '
				, stock printString , ' | Precio: '
				, precio printString , ' | porDoc: '
				, porDocena printString.
	^cadena!

iniProductoNombre: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := Producto nextId.
	nombreProducto := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false!

modNombre: unNombre
	"Modifica el nombre del producto"

	nombreProducto := unNombre!

modPorDocena
	"Modifica si se vende por docena o no"

	porDocena:= porDocena not.!

modPrecio: unPrecio
	"modifica el precio del producto"

	precio := unPrecio.!

modStock: unStock
	"modifica el stock del producto"

	unStock >= 0
		ifTrue: [stock := unStock]
		ifFalse: 
			[Transcript show: 'Clas Producto, modStock: unStock'.
			Transcript show: ' No se puede tener stock menor a cero'].
!

modTipo: unTipo
	"modifica el tipo del producto"

	tipo:= unTipo.!

verNombre
	"retorna el nombre del producto"

	^nombreProducto.!

VerNombre
	"retorna el nombre del producto"

	^nombreProducto!

verNroProducto
"retorna el Numero del producto"
^nroProducto.!

verPorDocena
	"devuelve true si se vende por docena, false en caso contrario"

	^porDocena.!

verPrecio
	"retorna el precio del producto"

	^precio.!

verStock
"retorna el stock"
^stock.!

verTipo
	"retorna el tipo del producto"

	^tipo.! !

!Producto categoriesForMethods!
aumentarStock:!public! !
disminuirStock:!public! !
imprimir!public! !
iniProductoNombre:tip:prec:!public! !
modNombre:!public! !
modPorDocena!public! !
modPrecio:!public! !
modStock:!public! !
modTipo:!public! !
verNombre!public! !
VerNombre!public! !
verNroProducto!public! !
verPorDocena!public! !
verPrecio!public! !
verStock!public! !
verTipo!public! !
!

!Producto class methodsFor!

crearProductoNombre:unNom tip:unTipo prec:unPrecio
"Retorna una instancia de Producto inicializada"
^(self new) iniProductoNombre: unNom tip: unTipo prec: unPrecio.!

initialize
	"Inicia la Variable de Clase idProd"

	idProd := 1!

nextId
	"retorna un id unico para una nueva instancia de producto"

	| id |
	id := idProd.
	idProd := idProd + 1.
	^id! !

!Producto class categoriesForMethods!
crearProductoNombre:tip:prec:!public! !
initialize!public! !
nextId!public! !
!

Proveedor guid: (GUID fromString: '{4d142482-4e17-4a30-bff2-851d3c202d10}')!

Proveedor comment: ''!

!Proveedor categoriesForClass!Kernel-Objects! !

!Proveedor methodsFor!

imprimir
	"Retorna los datos de la instancia de Proveedor"

	| cad |
	cad := 'Proveedor Nro: ', nroProveedor printString, ' | Nom: ', nombre, ' | Dir: ', direccion, ' | Tel: ', telefono, ' | Tipo: ', tipoProductos.
	^cad.!

iniProveedorNombre: unNom dir: unaDir tel: unTel tip: unTipo
	"Inicializa una instancia de Proveedor"

	nroProveedor := Proveedor nextId.
	nombre := unNom.
	direccion := unaDir.
	telefono := unTel.
	tipoProductos := unTipo!

modDireccion: unaDire
	"modifica la direccion del proovedor"

	direccion := unaDire!

modNombre: unNom
	"modifica el nombre del proovedor"

	nombre := unNom.!

modTelefono: unTel
	"modifica el telefono del proovedor"

	telefono := unTel!

modTipoProductos: unTipo
	"modifica el tipo de Producto que proovee el proveedor"

	tipoProductos := unTipo!

verDireccion
	"retorna la direccion del proovedor"

	^direccion!

verNombre
	"retorna el nombre del proovedor"

	^nombre.!

verNroProveedor
	"retorna el numero del proveedor"

	^nroProveedor.!

verTelefono
	"retorna el telefono del proveedor"

	^telefono!

verTipoProductos
	"retorna el tipo de Producto que proovee el proveedor"

	^tipoProductos.! !

!Proveedor categoriesForMethods!
imprimir!public! !
iniProveedorNombre:dir:tel:tip:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
modTipoProductos:!public! !
verDireccion!public! !
verNombre!public! !
verNroProveedor!public! !
verTelefono!public! !
verTipoProductos!public! !
!

!Proveedor class methodsFor!

crearProveedorNombre: unNom dir: unaDir tel: unTel tip: unTipo
	"retorna una instancia de Proveedor inicializada"

	^self new
		iniProveedorNombre: unNom
		dir: unaDir
		tel: unTel
		tip: unTipo!

initialize
	"inicializa la variable de Clase idProveedor"

	idProveedor := 1!

nextId
	"retorna un id unico para una nueva instancia de Proveedor"

	| id |
	id := idProveedor.
	idProveedor := idProveedor + 1.
	^id! !

!Proveedor class categoriesForMethods!
crearProveedorNombre:dir:tel:tip:!public! !
initialize!public! !
nextId!public! !
!

"Binary Globals"!

