| package |
package := Package name: 'PaquetePanificadora'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Panificadora;
	add: #Producto;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'Dolphin').

package!

"Class Definitions"!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaProductos listaPedidos listaEmpleados listaClientes listaProveedores listaRepartidores'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProd tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProd'!

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

Producto guid: (GUID fromString: '{7948a066-db33-4125-bacc-ab5d39ab576d}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

aumentarStock:cant
"aumenta la cantidad del stock del producto"
stock:=stock + cant.!

disminuirStock: cant
	"aumenta la cantidad del stock del producto"

	stock := stock - cant!

imprimir
"retorna una cadena con los datos"
|cadena|

cadena := nroProducto  printString , ' | ', nombreProd , ' | ', tipo , ' | ',  (stock printString), ' | ', (precio printString) , ' | porDoc: ', (porDocena printString).
^cadena.!

iniProductoNombre: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := Producto nextId.
	nombreProd := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false!

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

nombre
	"retorna el nombre del producto"

	^nombreProd.!

nombre: unNombre
	"Modifica el nombre del producto"

	nombreProd := unNombre.!

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
modPorDocena!public! !
modPrecio:!public! !
modStock:!public! !
modTipo:!public! !
nombre!public! !
nombre:!public! !
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
	"Inicia la Variable de Clase nextId"

	idProd := 1!

nextId
	| id |
	id := idProd.
	idProd := idProd + 1.
	^id! !

!Producto class categoriesForMethods!
crearProductoNombre:tip:prec:!public! !
initialize!public! !
nextId!public! !
!

"Binary Globals"!

