| package |
package := Package name: 'pkg_master'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Cliente;
	add: #Empleado;
	add: #Panadero;
	add: #Panificadora;
	add: #Pedido;
	add: #Producto;
	add: #ProductoPedido;
	add: #Proveedor;
	add: #Repartidor;
	add: #Vendedor;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box').

package!

"Class Definitions"!

Object subclass: #Cliente
	instanceVariableNames: 'nroCliente nombre direccion telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idCliente'!

Object subclass: #Empleado
	instanceVariableNames: 'legajo direccion telefono sueldo nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Panificadora
	instanceVariableNames: 'nombre direccion telefono listaClientes listaEmpleados listaPanaderos listaPedidos listaProductos listaProveedores listaRepartidores listaVendedores'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Pedido
	instanceVariableNames: 'nroPedido fecha fechaEntrega estado nroCliente nroRepartidorAsignado listaProductosPedidos total'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroPedidoStatic'!

Object subclass: #Producto
	instanceVariableNames: 'nroProducto nombreProducto tipo stock precio porDocena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProd'!

Object subclass: #ProductoPedido
	instanceVariableNames: 'idProductoPedido nroPedido nroProducto cantidad pedido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroProductoPedido'!

Object subclass: #Proveedor
	instanceVariableNames: 'nroProveedor direccion telefono tipoProductos nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProveedor'!

Empleado subclass: #Panadero
	instanceVariableNames: 'puesto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Empleado subclass: #Repartidor
	instanceVariableNames: 'nroRepartidor listaPedidosEntregados nroPedidoAsignado'
	classVariableNames: 'UnNuevoNroRepartidor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Empleado subclass: #Vendedor
	instanceVariableNames: 'nroVendedor listaPedidosVendidos'
	classVariableNames: 'UnNuevoNroVendedor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

Cliente guid: (GUID fromString: '{b9dc76af-91b0-47bd-9ddd-07ac331b74fc}')!

Cliente comment: ''!

!Cliente categoriesForClass!Kernel-Objects! !

!Cliente methodsFor!

iniClienteNom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Cliente"

	nroCliente := Cliente nextNroCliente.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel!

modDireccion:unaDireccion
"Modifica la direccion del cliente"
direccion:=unaDireccion.!

modNombre:unNombre
"Modifica el nombre del cliente"
nombre:=unNombre.!

modTelefono:unTelefono
"Modifica el telefono del cliente"
telefono =unTelefono .!

verDireccion
"Retorna la direccion del cliente"
^direccion.!

verNombre
"Retorna el nombre del cliente"
^nombre.!

verNroCliente
"Retorna el numero del cliente"
^nroCliente.!

verTelefono
"Retorna el telefono del cliente"
^telefono.! !

!Cliente categoriesForMethods!
iniClienteNom:dire:tel:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
verDireccion!public! !
verNombre!public! !
verNroCliente!public! !
verTelefono!public! !
!

!Cliente class methodsFor!

crearClienteNro:unNum nom:unNom dire:unaDire tel:unTel
"Retorna una instancia de Cliente inicializada"
^(self new) iniClienteNro: unNum nom: unNom dire: unaDire tel: unTel.!

initialize
[ idCliente := 1]
!

nextNroCliente
	"Retorna un id unico para una instancia nueva de Cliente"

	| id |
	id := idCliente.
	idCliente := idCliente + 1.
	^id! !

!Cliente class categoriesForMethods!
crearClienteNro:nom:dire:tel:!public! !
initialize!public! !
nextNroCliente!public! !
!

Empleado guid: (GUID fromString: '{449b804d-71ea-44de-b429-1a4cb51801e4}')!

Empleado comment: ''!

!Empleado categoriesForClass!Kernel-Objects! !

!Empleado methodsFor!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Legajo: ', legajo printString , ' | Nom: ' , nombre , ' | direccion ' , direccion , ' | Tel: '
				, telefono, ' | sueldo ', sueldo printString.
	^cadena!

iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Empleado"

	legajo := unLegajo.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel.
	!

modDireccion:unaDireccion
"Modifica la direccion del Empleado"
direccion:=unaDireccion.!

modLegajo: unLegajo
	"Modifica el Legajo del Empleado"

	legajo := unLegajo!

modNombre:unNombre
"Modifica el nombre del Empleado"
nombre:=unNombre.!

modSueldo:unSueldo
	"Modifica el sueldo del Empleado"

	sueldo := unSueldo.!

modTelefono:unTelefono
"Modifica el telefono del Empleado"
telefono =unTelefono .!

verDireccion
"Retorna la direccion del Empleado"
^direccion.!

verLegajo
"Retorna el Legajo del Empleado"
^legajo.!

verNombre
"Retorna el nombre del Empleado"
^nombre.!

verSueldo
"Retorna el sueldo del Empleado"
^sueldo.!

verTelefono
"Retorna el telefono del Empleado"
^telefono.! !

!Empleado categoriesForMethods!
imprimir!public! !
iniEmpleadoLegajo:nom:dire:tel:!public! !
modDireccion:!public! !
modLegajo:!public! !
modNombre:!public! !
modSueldo:!public! !
modTelefono:!public! !
verDireccion!public! !
verLegajo!public! !
verNombre!public! !
verSueldo!public! !
verTelefono!public! !
!

!Empleado class methodsFor!

crearEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Empleado inicializada"

	^(self new) iniEmpleadoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.! !

!Empleado class categoriesForMethods!
crearEmpleadoLegajo:nom:dire:tel:!public! !
!

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

Pedido guid: (GUID fromString: '{d2e215d0-9920-4aa0-84c6-40a60b01ec3a}')!

Pedido comment: ''!

!Pedido categoriesForClass!Kernel-Objects! !

!Pedido methodsFor!

agregarProductoPedido: productoPedido
	"Agrega un ProductoPedido a la lista de ProductosPedidos"

	listaProductosPedidos add: productoPedido!

eliminarProductoPedido: unProductoPedido
	"Elimina un ProductoPedido de la lista de Productos Pedidos"

	| id nroProd |
	unProductoPedido notNil
		ifTrue: 
			[id := unProductoPedido verIdProdPed .
			nroProd := unProductoPedido verNroProducto .
			(listaProductosPedidos includes: unProductoPedido)
				ifTrue: 
					[listaProductosPedidos remove: unProductoPedido.
					MessageBox warning: 'Se elimino ProductoPedido id: ' , id printString , ' con el Producto Nro: ' , nroProd printString]
				ifFalse: 
					[MessageBox
						warning: 'No existe ProductoPedido id:  ' , id printString , ' con el Producto Nro: ' , nroProd printString , ' en la lista.']]
		ifFalse: [MessageBox warning: 'El ProductoPedido es vacio']!

iniPedidoNroCliente: unNroCliente fechaEnt: fEnt  listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := Pedido nextNroPedido.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed!

modEstado: unEstado
	estado := unEstado.!

modiRepartidorAsignado: nroRepartidor
	nroRepartidorAsignado := nroRepartidor.!

verEstado
	^estado.!

verFecha
"Retorna la fecha de la creacion del pedido"
	^fecha.!

verFechaEntrega
	^fechaEntrega.!

verListaProductosPedidos
	"Retorna la lista de ProductosPedidos"

	^listaProductosPedidos!

verNroPedido
	^nroPedido.!

verNroRepartidor
	^nroRepartidorAsignado!

verTotal
	^total.! !

!Pedido categoriesForMethods!
agregarProductoPedido:!public! !
eliminarProductoPedido:!public! !
iniPedidoNroCliente:fechaEnt:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFecha!public! !
verFechaEntrega!public! !
verListaProductosPedidos!public! !
verNroPedido!public! !
verNroRepartidor!public! !
verTotal!public! !
!

!Pedido class methodsFor!

crearPedido:unNroClien listProdPed:lista
|f|
nroPedidoStatic := nroPedidoStatic +1.

f := Date tomorrow .
Transcript show: 'fecha elegida de entrega: ', f printString; cr.

^(self new) iniPedidoNro: nroPedidoStatic fechaEnt: f nroCli: unNroClien listPP: lista.!

crearPedido:pani nroClien:unNroClien listProdPed:lista
|f|
nroPedidoStatic := nroPedidoStatic +1.

f := Date tomorrow .
Transcript show: 'fecha elegida de entrega: ', f printString; cr.

^(self new) iniPedidoNro: nroPedidoStatic panificadora: pani fechaEnt: f nroCli: unNroClien listPP: lista.!

initialize
[nroPedidoStatic := 1]!

nextNroPedido
	"retorna un id unico para una nueva instancia de Pedido"

	| id |
	id := nroPedidoStatic .
	nroPedidoStatic := nroPedidoStatic + 1.
	^id! !

!Pedido class categoriesForMethods!
crearPedido:listProdPed:!public! !
crearPedido:nroClien:listProdPed:!public! !
initialize!public! !
nextNroPedido!public! !
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
	"retorna el nombre del Producto"

	^nombreProducto.!

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

ProductoPedido guid: (GUID fromString: '{9ae45669-d20e-4e41-a18f-54a008d55e0e}')!

ProductoPedido comment: ''!

!ProductoPedido categoriesForClass!Kernel-Objects! !

!ProductoPedido methodsFor!

costo [
    | precio total |

    "Llama a la clase CatalogoProductos para obtener el precio del producto según nroProducto"
    precio := Panificadora precioProducto: nroProducto.

    "Calcula el costo total multiplicando la cantidad de productos por el precio unitario"
    total := cantidad * precio.
    
    ^ total.  "Devuelve el costo total del ProductoPedido"
]!

iniProductoPedido:unNroProduct  cantidad:cant pedido:unPedido
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroProdPed.
nroProducto := unNroProduct.
cantidad:= cant.
pedido := unPedido.
nroPedido := pedido verNroPedido.
!

modCantidad:unNro
	cantidad=cantidad+unNro.!

verCantidad
	^cantidad!

verIdProdPed
	"Retorna el nro de ID del Producto Pedidio"

	^idProductoPedido!

verNroPedido
	"Retorna el nro de pedido asociado al Producto pedido"

	^pedido verNroPedido!

verNroProducto
	"Retorna el nro de Pedido de ProductoPedido"

	^nroProducto! !

!ProductoPedido categoriesForMethods!
costo!public! !
iniProductoPedido:cantidad:pedido:!public! !
modCantidad:!public! !
verCantidad!public! !
verIdProdPed!public! !
verNroPedido!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:nroProducto pedido:unPedido cant:cantidad
nroProductoPedido := nroProductoPedido +1.
^(self new) iniProductoPedido: nroProductoPedido nroProd: nroProducto pedido: unPedido cantidad: cantidad.!

initialize
	[nroProductoPedido := 0]!

nextNroProdPed
	"retorna un id unico para una nueva instancia de ProductoPedido"

	| id |
	id := nroProductoPedido.
	nroProductoPedido := nroProductoPedido + 1.
	^id! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:pedido:cant:!public! !
initialize!public! !
nextNroProdPed!public! !
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

Panadero guid: (GUID fromString: '{924bf294-35d3-4f4d-a558-48e649e78e2b}')!

Panadero comment: ''!

!Panadero categoriesForClass!Kernel-Objects! !

!Panadero methodsFor!

imprimir
^'Panadero Puesto:  ', puesto, ' | ', super imprimir.!

iniPanaderoLegajo: unLegajo puesto: unPuesto nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Panadero"

	self iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	puesto := unPuesto.
	sueldo := 800000.
	
	
!

producirProducto:unProducto cant:unaCant
"Produce la cantidad de un producto"
|unidad|
unidad:= ''.
(unProducto verPorDocena) ifTrue: [unidad := ' Docenas'] .

unProducto aumentarStock: unaCant.
Transcript show: 'se produjo ' , (unaCant  printString), unidad,' de ', unProducto verNombre; cr.
Transcript show: ' (Stock: ' , unProducto verStock printString , '  )' ; cr.
! !

!Panadero categoriesForMethods!
imprimir!public! !
iniPanaderoLegajo:puesto:nom:dire:tel:!public! !
producirProducto:cant:!public! !
!

!Panadero class methodsFor!

crearPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Panadero inicializada"

	^(self new) iniPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.! !

!Panadero class categoriesForMethods!
crearPanaderoLegajo:nom:dire:tel:!public! !
!

Repartidor guid: (GUID fromString: '{e520e4a8-e744-4822-9cf7-93bf17a6e60e}')!

Repartidor comment: ''!

!Repartidor categoriesForClass!Kernel-Objects! !

!Repartidor methodsFor!

agregarPedidoRealizado: unNroPedido
	"agrega un pedido a la lista de pedidos realizados"

	listaPedidosEntregados add: unNroPedido!

asignarPedido: nroPedido
"asigna un nuevo numero de pedido para entregar"
nroPedidoAsignado := nroPedido.!

imprimir
^'Repartidor nro:  ', nroRepartidor printString , ' | ', super imprimir.!

iniRepartidorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Repartidor"

	self
		iniEmpleadoLegajo: unLegajo
		nom: unNombre
		dire: unaDire
		tel: unTel.
	nroRepartidor := Repartidor nuevoNroRepartidor.
	sueldo := 500000.
	nroPedidoAsignado := 0.
	listaPedidosEntregados := OrderedCollection new!

liberarRepartidor
"Solamente cambia a cero el pedido asignado, asi el repartidor estara libre para un hacer un nuevo pedido "
self agregarPedidoRealizado: nroPedidoAsignado.
nroPedidoAsignado := 0.!

repartir: pedido
"Reparte el pedido asigando"
|nroPed|
nroPed := pedido verNroPedido.
self asignarPedido: nroPed.
self liberarRepartidor.

Transcript show: 'El repartidor nro : ', nroRepartidor printString, ' repartio el pedido nro: ', nroPed printString ; cr.!

verListaPedidosEntregados
"retorna la lista de pedidos asignados"

^listaPedidosEntregados.!

verNroPedidoAsignado
	^nroPedidoAsignado .!

verNroRepartidor
	^nroRepartidor.! !

!Repartidor categoriesForMethods!
agregarPedidoRealizado:!public! !
asignarPedido:!public! !
imprimir!public! !
iniRepartidorLegajo:nom:dire:tel:!public! !
liberarRepartidor!public! !
repartir:!public! !
verListaPedidosEntregados!public! !
verNroPedidoAsignado!public! !
verNroRepartidor!public! !
!

!Repartidor class methodsFor!

crearRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Repartidor inicializada"

	^(self new) iniRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel.!

nuevoNroRepartidor
"Retorna un nuevo nuevo de repartidor, primero se asegura que este inicializada la variable de clase "
(UnNuevoNroRepartidor isNil) ifTrue: [UnNuevoNroRepartidor := 0].
UnNuevoNroRepartidor := UnNuevoNroRepartidor + 1.
^UnNuevoNroRepartidor.! !

!Repartidor class categoriesForMethods!
crearRepartidorLegajo:nom:dire:tel:!public! !
nuevoNroRepartidor!public! !
!

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

"Binary Globals"!

