| package |
package := Package name: 'pkg_maxi'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #App;
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
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #App
	instanceVariableNames: 'var1'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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
	instanceVariableNames: 'idProductoPedido nroProducto cantidad nroPedido pedido costoUnitario'
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
	instanceVariableNames: 'nroVendedor pedidosVendidos'
	classVariableNames: 'UnNuevoNroVendedor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

App guid: (GUID fromString: '{ff893e23-44c7-48a8-9911-5f052bcd62df}')!

App comment: ''!

!App categoriesForClass!cat_max1! !

!App class methodsFor!

cargarCliente:pan

| cant nom dir tel c |

"Ingresar cantidad de Clientes a cargar "
cant := Prompter prompt: 'Cantidad de Clientes a cargar  '.
cant := cant asNumber.

1 to: cant do: [:i | 
nom:= Prompter prompt: 'ingrese nombre: '.
dir:= Prompter prompt: 'ingrese direccion: '.
tel:= Prompter prompt: 'ingrese telefono: '.
c:= Cliente crearClienteNom: nom dire: dir tel: tel.
Transcript show: 'Se creo el Cliente Nro ', c verNroCliente printString , 'nom: ',  c verNombre; cr.
pan agregarCliente: c.
Transcript show: 'Se agrego un Nuevo Cliente a la Panificadora' ; cr.
]!

cargarClientesPanificadora:pan
"Carga Clientes a la Panificadora"

	pan agregarCliente: (Cliente crearClienteNom: 'Carlos' dire: 'calle 401'  tel: '445-121').
	pan agregarCliente: (Cliente crearClienteNom: 'Raul' dire: 'calle 402'  tel: '445-122').
	pan agregarCliente: (Cliente crearClienteNom: 'Clara' dire: 'calle 403'  tel: '445-123').
	pan agregarCliente: (Cliente crearClienteNom: 'Manuel' dire: 'calle 404'  tel: '445-124').
	pan agregarCliente: (Cliente crearClienteNom: 'Rita' dire: 'calle 405'  tel: '445-125').
	pan agregarCliente: (Cliente crearClienteNom: 'Jose' dire: 'calle 405'  tel: '445-126').
	pan agregarCliente: (Cliente crearClienteNom: 'Juan' dire: 'calle 405'  tel: '445-127').

!

cargarPanadero:pan

|dic cant p leg nom dir tel   op pue |
"Dictionary para puestos de Panadero"
dic := Dictionary new at:'1' put:'Pastelero'; at:'2' put:'Panadero'; at:'3' put:'Facturero'; at:'4' put:'Maestro'; yourself.

"Ingresar cantidad de Panaderos a cargar "
cant := Prompter prompt: 'Ingrese cantidad de Panaderos a cargar  '.
cant := cant asNumber.

1 to: cant do: [:i | 
	Transcript cr.
	"Creamos un Panadero manualmente"
	leg:= Prompter prompt: 'ingrese legajo: '.
	leg:= leg asNumber.
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	pue := '0'.
	[pue = '0'] whileTrue: [
		op:= Prompter prompt: 'Ingrese nro de puesto: 1- Pastelero, 2- Panadero, 3- Facturero, 4- Maestro'. 
		pue:= dic at: op ifAbsent: ['0'].
		].
	p:= Panadero crearPanaderoLegajo: leg nom: nom dire: dir tel: tel puesto: pue.
	Transcript show: 'Se creo con exito el Panadero ', p verPuesto , ' leg: ', p verLegajo printString , ' nom: ',  p verNombre ; cr.
	
	pan agregarPanadero: p.
	Transcript show: 'Se agrego un Nuevo Panadero a la Panificadora'.
]!

cargarPanaderosPanificadora: pan
	"Agregamos algunos Panaderos a la panaficadora"

	pan agregarPanadero: (Panadero crearPanaderoLegajo: 2 nom: 'Maxi' dire: 'calle 102' tel: '123-564' puesto: 'Pastelero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 3 nom: 'Daniel' dire: 'calle 103' tel: '123-564' puesto: 'Panadero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 4 nom: 'Julio' dire: 'calle 104' tel: '123-564' puesto: 'Maestro').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 5 nom: 'Exequiel' dire: 'calle 105' tel: '123-564' puesto: 'Pastelero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 6 nom: 'Marcela' dire: 'calle 106' tel: '123-564' puesto: 'Panadero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 7 nom: 'Fabiana' dire: 'calle 107' tel: '123-564' puesto: 'Maestro').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 8 nom: 'Claudia' dire: 'calle 108' tel: '123-564' puesto: 'Facturero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 9 nom: 'Walter' dire: 'calle 109' tel: '123-564' puesto: 'Facturero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 10 nom: 'Leon' dire: 'calle 100' tel: '123-564' puesto: 'Maestro').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 11 nom: 'Ivan' dire: 'calle 101' tel: '123-564' puesto: 'Pastelero').
	pan agregarPanadero: (Panadero crearPanaderoLegajo: 12 nom: 'Laureano' dire: 'calle 102' tel: '123-564' puesto: 'Facturero').!

cargarProducto:pan

|pro dic cant nom tip prec op |

"Dictionary para Tipo de Producto"
dic := Dictionary new at:'1' put:'Pastel'; at:'2' put:'Pan'; at:'3' put:'Factura'; at:'4' put:'Tarta'; at:'5' put:'Galleta'; yourself.

"Ingresar cantidad de Productos a cargar "
cant := Prompter prompt: 'Ingrese cantidad de Productos a cargar  '.
cant := cant asNumber.

1 to: cant do: [:i | 
	Transcript cr.
	"Creamos un Producto manualmente"
	nom:= Prompter prompt: 'ingrese nombre del Producto: '.
	tip := '0'.
	[tip = '0'] whileTrue: [
		op:= Prompter prompt: 'ingrese nro de tipo: 1- Pastel, 2- Pan, 3- Factura, 4- Tarta, 5- Galleta'. 
		tip:= dic at: op ifAbsent: ['0'].
	].
	prec:= Prompter prompt: 'ingrese Precio:'.
	prec:= prec asNumber.

	pro:= Producto crearProductoNombre: nom tip: tip prec: prec.
	Transcript show:  'Se creo con exito el Producto Nro: ', pro verNroProducto printString , ' Nom: ', pro verNombre , ' Tipo ',  pro verTipo; cr.
	
	pan agregarProducto: pro.
	Transcript show: 'Se agrego un Nuevo Producto a la Panificadora'.
]!

cargarProductosPanificadora:pan

"Creamos y cargamos varios Productos"
pan agregarProducto: (Producto crearProductoNombre:'Torta Oreo 1,5 Kg' tip: 'Pastel' prec: 8000.5).
pan agregarProducto: (Producto crearProductoNombre:'Torta Oreo 2 Kg' tip: 'Pastel' prec: 12000.0).
pan agregarProducto: (Producto crearProductoNombre:'Torta Selva Negra 1,5 Kg' tip: 'Pastel' prec: 5000.0).
pan agregarProducto: (Producto crearProductoNombre:'Torta Selva Negra 2,8 Kg' tip: 'Pastel' prec: 9000.0).
pan agregarProducto: (Producto crearProductoNombre:'Tarta de Ricota' tip: 'Tarta' prec: 3000.0).
pan agregarProducto: (Producto crearProductoNombre:'Pastaflola' tip: 'Tarta' prec: 2500.0).
pan agregarProducto: (Producto crearProductoNombre:'Flauta 5 Kg' tip: 'Pan' prec: 7500.05).
pan agregarProducto: (Producto crearProductoNombre:'Flauta 10 Kg' tip: 'Pan' prec: 15000.05).
pan agregarProducto: (Producto crearProductoNombre:'Minion 5 Kg' tip: 'Pan' prec: 8000.0).
pan agregarProducto: (Producto crearProductoNombre:'Minion 10 Kg' tip: 'Pan' prec: 16000.0).
pan agregarProducto: (Producto crearProductoNombre:'Galletas 5 Kg' tip: 'Pan' prec: 8500.0).
pan agregarProducto: (Producto crearProductoNombre:'Galletas 10 Kg' tip: 'Pan' prec: 17000.0).
pan agregarProducto: (Producto crearProductoNombre:'Marinera c/sal 1 Kg' tip: 'Galleta' prec: 2200.0).
pan agregarProducto: (Producto crearProductoNombre:'Marinera s/sal 1 Kg' tip: 'Galleta' prec: 2000.0).
pan agregarProducto: (Producto crearProductoNombre:'Marinera Salvado 1 Kg' tip: 'Galleta' prec: 2100.0).
pan agregarProducto: (Producto crearProductoNombre:'Grisines c/sal 1 Kg' tip: 'Galleta' prec: 1900.0).
pan agregarProducto: (Producto crearProductoNombre:'Grisines s/sal 1 Kg' tip: 'Galleta' prec: 1800.0).
pan agregarProducto: (Producto crearProductoNombre:'Grisines Salvado 1 Kg' tip: 'Galleta' prec: 2000.0).
pan agregarProducto: (Producto crearProductoNombre:'Vigilante /doc' tip: 'Factura' prec: 3500.0).
pan agregarProducto: (Producto crearProductoNombre:'Canioncito /doc' tip: 'Factura' prec: 4000.0).
pan agregarProducto: (Producto crearProductoNombre:'Medialuna Manteca /doc' tip: 'Factura' prec: 4000.0).
pan agregarProducto: (Producto crearProductoNombre:'Medialuna Grasa /doc' tip: 'Factura' prec: 3800.0).
pan agregarProducto: (Producto crearProductoNombre:'Totita Negra /doc' tip: 'Factura' prec: 4000.0).
pan agregarProducto: (Producto crearProductoNombre:'Totita Blanca /doc' tip: 'Factura' prec: 4000.0).
pan agregarProducto: (Producto crearProductoNombre:'Churros /doc' tip: 'Factura' prec: 5000.0).
pan agregarProducto: (Producto crearProductoNombre:'Sacramento /doc' tip: 'Factura' prec: 4100.0).
!

cargarRepartidor:pan

|r cant leg nom dir tel |

"Ingresar cantidad de Repartidores a cargar "
cant := Prompter prompt: 'Ingrese cantidad de repartidores a cargar  '.
cant := cant asNumber.

1 to: cant do: [:i | 
	Transcript cr.
	"Creamos un Repartidor manualmente"
	leg:= Prompter prompt: 'ingrese legajo: '.
	leg asNumber.
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	r:= Repartidor crearRepartidorLegajo: leg nom: nom dire: dir tel: tel.
	Transcript show: 'Se creo con exito el Repartidor Nro ', r verNroRepartidor printString , 'nom: ',  r verNombre; cr.
	
	pan agregarRepartidor: r.
	Transcript show: 'Se agrego un Nuevo Repartidor a la Panificadora'; cr.
]!

cargarRepartidoresPanificadora:pan
"Creamos varios Repartidores y agregamos a la Lista de Repartidores"
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 201 nom: 'Federico' dire: 'calle 201' tel: '200-201').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 202 nom: 'Jaqueline' dire: 'calle 202' tel: '200-202').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 203 nom: 'Benjamin' dire: 'calle 203' tel: '200-203').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 204 nom: 'Tatiana' dire: 'calle 204' tel: '200-204').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 205 nom: 'Ana' dire: 'calle 205' tel: '200-205').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 206 nom: 'Joel' dire: 'calle 206' tel: '200-206').
!

cargarVendedor:pan

|v cant leg nom dir tel |

"Ingresar cantidad de Vendedores a cargar "
cant := Prompter prompt: 'Ingrese cantidad de Vendedores a cargar  '.
cant := cant asNumber.

1 to: cant do: [:i | 
	Transcript cr.
	"Creamos un Vendedor manualmente"
	leg:= Prompter prompt: 'ingrese legajo: '.
	leg:= leg asNumber.
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	v:= Vendedor crearVendedorLegajo: leg nom: nom dire: dir tel: tel.
	Transcript show:  'Se creo el Vendedor Nro', v verNroVendedor printString , ' nom: ',  v verNombre; cr.
	
	pan agregarVendedor: v.
	Transcript show: 'Se agrego un Nuevo Vendedor a la Panificadora'; cr.
]!

cargarVendedoresPanificadora:pan
"Creamos varios Vendedores y agregamos a la Lista de Vendedores"

	pan agregarVendedor: (Vendedor crearVendedorLegajo: 301 nom: 'Sandra' dire: 'calle 301' tel: '300-301').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 302 nom: 'Santiago' dire: 'calle 302' tel: '300-302').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 303 nom: 'Gisela' dire: 'calle 303' tel: '300-303').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 304 nom: 'Martin' dire: 'calle 304' tel: '300-304').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 305 nom: 'Marcelo' dire: 'calle 305' tel: '300-305').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 306 nom: 'Silvio' dire: 'calle 306' tel: '300-306').
!

mainCargaManual:pan

|op|

op :=''.
[op ~= '0']  whileTrue: [
	Transcript cr.
	Transcript show: 'Menu Carga Manual ';cr.
	Transcript show: 'Elige opción: ';cr.
	Transcript show: 'Opción 1: Cargar Clientes';cr.
	Transcript show: 'Opción 2: Cargar Panaderos';cr.
	Transcript show: 'Opción 3: Cargar Productos';cr.
	Transcript show: 'Opción 4: Cargar Repartidores ';cr.
	Transcript show: 'Opción 5: Cargar Vendedores ';cr.
	Transcript show: 'Opción 0: Salir';cr.

	op:= Prompter prompt: 'Menu Carga Manual. Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=5]) ifTrue:[
			(op = 1) ifTrue:[ Transcript show: 'seleccionó Cargar Clientes.';cr. 
					App cargarCliente: pan.
					op:=''. ].		
			(op = 2) ifTrue:[ Transcript show: 'seleccionó Cargar Panaderos.';cr. 
					App cargarPanadero: pan.
					op:=''. ].		
			(op = 3) ifTrue:[ Transcript show: 'seleccionó Cargar Productos.';cr. 
					App cargarProducto: pan.
					op:=''. ].		
			(op = 4) ifTrue:[ Transcript show: 'seleccionó Cargar Repartidores.';cr. 
					App cargarRepartidor: pan.
					op:=''. ].	
			(op = 5) ifTrue:[ Transcript show: 'seleccionó Cargar Vendedores.';cr. 
					App cargarVendedor: pan.
					op:=''. ].	
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. saliendo';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'No seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]
!

mainListarDatos:pan

|op lista|

op :=''.
[op ~= '0']  whileTrue: [
	Transcript cr.
	Transcript show: 'Menu Listar Datos';cr.
	Transcript show: 'Elige opción: ';cr.
	Transcript show: 'Opción 1: Listar Clientes';cr.
	Transcript show: 'Opción 2: Listar Panaderos';cr.
	Transcript show: 'Opción 3: Listar Productos';cr.
	Transcript show: 'Opción 4: Listar Repartidores ';cr.
	Transcript show: 'Opción 5: Listar Vendedores ';cr.
	Transcript show: 'Opción 0: Salir';cr.

	op:= Prompter prompt: 'Menu Listar Datos. Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	Transcript cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=5]) ifTrue:[
		(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. saliendo';cr.
					op:='0'.]
				ifFalse:[
				(op = 1) ifTrue:[ Transcript show: 'seleccionó Listar Clientes.';cr. 
						lista := pan verListaClientes.
						op:=''. ].		
				(op = 2) ifTrue:[ Transcript show: 'seleccionó Listar Panaderos.';cr. 
						lista := pan verListaPanaderos.
						op:=''. ].		
				(op = 3) ifTrue:[ Transcript show: 'seleccionó Listar Productos.';cr. 
						lista := pan verListaProductos .
						op:=''. ].		
				(op = 4) ifTrue:[ Transcript show: 'seleccionó Listar Repartidores.';cr. 
						lista := pan verListaRepartidores .
						op:=''. ].	
				(op = 5) ifTrue:[ Transcript show: 'seleccionó Listar Vendedores.';cr. 
						lista := pan verListaClientes .
						op:=''. ].	
				
				lista do:[:ob | Transcript show: (ob imprimir) ; cr].
				Transcript cr. ].
	]
	ifFalse: [ Transcript show: 'No seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
	].



!

mainPrincipal:pan
|op|

op :=''.
[op ~= '0']  whileTrue: [
	Transcript clear.
	Transcript show: 'MenuPrincipal: ';cr.
	Transcript show: 'Elige opción: ';cr.
	Transcript show: 'Opción 1: Cargar datos manualmente';cr.
	Transcript show: 'Opción 2: Listar datos';cr.
	Transcript show: 'Opción 3: Buscar datos';cr.
	Transcript show: 'Opción 4: Funciones ';cr.
	Transcript show: 'Opción 0: Salir';cr.
	op:= Prompter prompt: 'Menu Principal. Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=4]) ifTrue:[
			(op = 1) ifTrue:[ Transcript show: 'seleccionó 1 Cargar datos manualmente.';cr. 
			App mainCargaManual: pan.
					op:=''. ].		
			(op = 2) ifTrue:[ Transcript show: 'seleccionó 2 Listar datos. ';cr. 
					App mainListarDatos: pan.
					op:=''. ].		
			(op = 3) ifTrue:[ Transcript show: 'seleccionó 3 Buscar Datos';cr. 
					op:=''. ].		
			(op = 4) ifTrue:[ Transcript show: 'seleccionó 4 Funciones';cr. 
					op:=''. ].		
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. saliendo';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'no seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]
! !

!App class categoriesForMethods!
cargarCliente:!public! !
cargarClientesPanificadora:!public! !
cargarPanadero:!public! !
cargarPanaderosPanificadora:!public! !
cargarProducto:!public! !
cargarProductosPanificadora:!public! !
cargarRepartidor:!public! !
cargarRepartidoresPanificadora:!public! !
cargarVendedor:!public! !
cargarVendedoresPanificadora:!public! !
mainCargaManual:!public! !
mainListarDatos:!public! !
mainPrincipal:!public! !
!

Cliente guid: (GUID fromString: '{b9dc76af-91b0-47bd-9ddd-07ac331b74fc}')!

Cliente comment: ''!

!Cliente categoriesForClass!cat_max1! !

!Cliente methodsFor!

imprimir
	"retorna una cadena con los datos"

	| cadena |
	cadena := 'Cliente Nro:  ' , nroCliente printString , ' | Nom:  ' , nombre , ' | Dir:  ' , direccion
				, ' | Tel:  ' , telefono.
	^cadena!

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

printOn: aStream
	aStream
		nextPutAll: 'Cliente Nro:  ';
		nextPutAll: nroCliente printString , ' | ';
		nextPutAll: 'Nom:  ';
		nextPutAll: nombre , ' | ';
		nextPutAll: 'Dir:  ';
		nextPutAll: direccion , ' | ';
		nextPutAll: 'Tel:  ';
		nextPutAll: telefono.!

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
imprimir!public! !
iniClienteNom:dire:tel:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
printOn:!public! !
verDireccion!public! !
verNombre!public! !
verNroCliente!public! !
verTelefono!public! !
!

!Cliente class methodsFor!

crearClienteNom:unNom dire:unaDire tel:unTel
"Retorna una instancia de Cliente inicializada"

^(self new) iniClienteNom:unNom dire: unaDire tel: unTel.!

initialize
idCliente := 1.
!

nextNroCliente
	"Retorna un id unico para una instancia nueva de Cliente"

	| id |
	id := idCliente.
	idCliente := idCliente + 1.
	^id! !

!Cliente class categoriesForMethods!
crearClienteNom:dire:tel:!public! !
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
	cadena := 'Leg: ', legajo printString , ' | Nom: ' , nombre , ' | Dir: ' , direccion , ' | Tel: '
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

printOn: aStream
	aStream
		nextPutAll: 'Leg: ' , legajo printString , ' | ';
		nextPutAll: 'Nom:  ' , nombre , ' | ';
		nextPutAll: 'Dir:  ' , direccion , ' | ';
		nextPutAll: 'Tel:  ' , telefono , ' | ';
		nextPutAll: 'Sueldo:  ';
		print: sueldo printString.!

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
printOn:!public! !
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

|repartidor pedido|
"buscar el pedido"
pedido:= listaPedidos detect: [:ped | ped verNroPedido = nroPedido] ifNone: [^nil].
"seguir con buscar repartidor libre...."
repartidor:= listaRepartidores detect: [:rep | rep verNroPedidoAsignado = 0 ] ifNone:[^nil].

"asignar repartidor al pedido"
pedido modiRepartidorAsignado: repartidor verNroRepartidor.

"retorna el repartidor asignado"
^repartidor.

!

comprarProductosNroCliente: nroCliente listaProd: unaListaProdPedido
" un cliente compra un lista de productoPedido, este metodo hace todo el trabajo llamando a todos los metodos necesarios para que se concrete la compra"
|pe pro pana|

"crear el pedido y agregarlo a la lista de pedidos de la panificadora"
pe:= Pedido crearPedido: nroCliente listProdPed: unaListaProdPedido.
self agregarPedido: pe.

"producir cada productoPedido"
1 to: ( (pe verListaPedidos) size) do: [: prodPe |   
							pro := (self verListaProductos) detect: [: p| p verNroProducto = prodPe verNroProducto]. "busca el producto del productoPedido"
							pana := self seleccionarPanadero: pro. "selecciona un panadero segun el tipo de producto, si es un pastel se necesita un pastelero"
							pana producirProducto: pro cant: prodPe verCantidad. "Se pide al panadero producir el productoPedido"
							].

"modificar el estado del pedido, ahora ya esta lista para repartir"
pe modEstado: 'listo para repartir'.

" repartir producto"
self repartirPedido: pe.

"modificar el estado del pedido"
pe modEstado:'entregado'.



!

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
|repa|
((unPedido verRepartidor) isNil) ifTrue: [ 
								repa := self asignarRepartidorPedido: unPedido verNroPedido.
								repa asignarPedido: unPedido verNroPedido.
								].
"verificar si el pedido esta en estado listo para repartir"
(unPedido verEstado = 'listo para repartir') ifTrue: [ 
										Transcript show: 'Repartiendo el pedido'."unPedido repartir."
										repa liberarRepartidor. "agregar a viajes realizados del repartidor"
										]
								     ifFalse: [
										Transcript show: 'El pedido aun no esta listo para repartir'
										].
										
											


!

seleccionarPanadero: unProducto
"selecciona un panadero segun el tipo de producto a producir"
|dic pana|

dic := Dictionary new at:'Pastel' put:'Pastelero'; at:'Pan' put:'Panadero'; at:'Factura' put:'Facturero'; at:'Tarta' put:'Maestro'; at:'Galleta' put:'Maestro'; yourself.

pana := listaPanaderos detect: [:p| p verPuesto = dic at: (unProducto verTipo)].

^pana.!

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
comprarProductosNroCliente:listaProd:!public! !
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
seleccionarPanadero:!public! !
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

agregarProductoPedido: nroProducto cantidad: cantidad
   | productoExistente nuevoProductoPedido |
   
   productoExistente := listaProductosPedidos detect: [ :p | p verNroProducto = nroProducto ] ifNone: [ nil ].
   
   productoExistente
      ifNotNil: [ productoExistente modCantidad: (productoExistente verCantidad + cantidad) ]
      ifNil: [
         nuevoProductoPedido := ProductoPedido new.
         nuevoProductoPedido iniProductoPedido: (listaProductosPedidos size + 1)
                                nroProd: nroProducto
                                pedido: self
                                cantidad: cantidad.
         listaProductosPedidos add: nuevoProductoPedido
      ].!

eliminarProductoPedido: nroProducto
   | productoAEliminar |
   productoAEliminar := listaProductosPedidos detect: [ :p | p verNroProducto = nroProducto ] ifNone: [ nil ].
   productoAEliminar
      ifNotNil: [
         listaProductosPedidos remove: productoAEliminar.
      ]
      ifNil: [
         Transcript show: 'Producto no encontrado en el pedido'; cr.
      ].!

iniPedidoNro: unNroPedi fechaEnt: fEnt nroCli: unNroCliente listPP: listaDeProdPed
	"Retorna una instacia de Pedido inicializada"

	nroPedido := unNroPedi.
	fecha := Date today.
	fechaEntrega := fEnt.
	estado := 'en Preparacion'.
	nroCliente := unNroCliente.
	nroRepartidorAsignado := -1.
	listaProductosPedidos := listaDeProdPed!

modEstado: unEstado
	estado := unEstado.!

modiRepartidorAsignado: numeroRepartidor
	nroRepartidorAsignado := numeroRepartidor.!

verEstado
	^estado.!

verFechaEntrega
	^fechaEntrega.!

verListaProductosPedidos
   | listaTexto |
   listaTexto := String streamContents: [ :stream |
      listaProductosPedidos do: [ :producto |
         stream
            nextPutAll: ', Nro Producto: ', producto verNroProducto printString;
            nextPutAll: ', Cantidad: ', producto verCantidad printString;
            nextPutAll: ', Nro Pedido: ', producto verNroPedido printString;
            nextPutAll: '; '.
      ].
   ].
   Transcript show: listaTexto; cr.!

verNroPedido
	^nroPedido.!

verRepartidor
	^nroRepartidorAsignado.!

verTotal
   total := 0.
   listaProductosPedidos do: [ :productoPedido |
      total := total + productoPedido verCosto .
   ].

   ^ total.! !

!Pedido categoriesForMethods!
agregarProductoPedido:cantidad:!public! !
eliminarProductoPedido:!public! !
iniPedidoNro:fechaEnt:nroCli:listPP:!public! !
modEstado:!public! !
modiRepartidorAsignado:!public! !
verEstado!public! !
verFechaEntrega!public! !
verListaProductosPedidos!public! !
verNroPedido!public! !
verRepartidor!public! !
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

^(self new) iniPedidoNro: nroPedidoStatic panificadora: pani fechaEnt: f nroCli: unNroClien listPP: lista.! !

!Pedido class categoriesForMethods!
crearPedido:listProdPed:!public! !
crearPedido:nroClien:listProdPed:!public! !
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
	cadena := 'Producto nro: ', nroProducto printString , 
			' | Nom: ' , nombreProducto , 
			' | tipo: ' , tipo ,
			' | Strock: ' , stock printString ,
			' | Precio: ', precio printString , 
			' | porDoc: ', porDocena printString.
	^cadena!

iniProductoNombre: unNom tip: unTipo prec: unPrecio
	"Inicializa una instancia de Producto"

	nroProducto := Producto nextId.
	nombreProducto := unNom.
	tipo := unTipo.
	precio := unPrecio.
	stock := 0.
	porDocena := false.
	tipo = 'Factura' ifTrue: [porDocena := true]!

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

printOn: aStream
	aStream
		nextPutAll: 'Producto Nro:  ';
		nextPutAll: nroProducto printString , ' | ';
		nextPutAll: 'Nom:  ';
		nextPutAll: nombreProducto , ' | ';
		nextPutAll: 'Tipo:  ';
		nextPutAll: tipo , ' | ';
		nextPutAll: 'Precio:  ';
		nextPutAll: precio printString , ' | ';
		nextPutAll: 'Stock:  ';
		nextPutAll: stock printString!

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
printOn:!public! !
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
	(idProd isNil) ifTrue: [idProd := 0].
	idProd := idProd + 1.
	^idProd.
	"| id |
	id := idProd.
	idProd := idProd + 1.
	^id"! !

!Producto class categoriesForMethods!
crearProductoNombre:tip:prec:!public! !
initialize!public! !
nextId!public! !
!

ProductoPedido guid: (GUID fromString: '{9ae45669-d20e-4e41-a18f-54a008d55e0e}')!

ProductoPedido comment: ''!

!ProductoPedido categoriesForClass!Kernel-Objects! !

!ProductoPedido methodsFor!

iniProductoPedidoNroProd:unNroProduct cantidad:cant costoUnitario:cost
"inicializa una instancia de ProductoPedido"

idProductoPedido := ProductoPedido nextNroPP.
nroProducto := unNroProduct.
nroPedido := pedido verNroPedido.
cantidad:= cant.
costoUnitario:=cost.!

modCantidad:unNro
	cantidad=cantidad+unNro.!

modCostoUnitario:unNro
	costoUnitario =unNro.!

modNroPedido: unNroPedido
	nroPedido := unNroPedido!

verCantidad
	^cantidad.!

verCosto
	| total |
	total := cantidad * costoUnitario.
	^total!

verCostoUnitario
	^costoUnitario.!

verNroProducto
	^nroProducto.! !

!ProductoPedido categoriesForMethods!
iniProductoPedidoNroProd:cantidad:costoUnitario:!public! !
modCantidad:!public! !
modCostoUnitario:!public! !
modNroPedido:!public! !
verCantidad!public! !
verCosto!public! !
verCostoUnitario!public! !
verNroProducto!public! !
!

!ProductoPedido class methodsFor!

crearProductoPedido:unProducto cantidad:cant

	^(self new) iniProductoPedidoNroProd: unProducto verNroProducto cantidad: cant costoUnitario: unProducto verCosto.!

crearProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo

	^(self new) iniProductoPedidoNroProd: nroProd cantidad: cant costoUnitario: costo.!

initialize [
    nroProductoPedido := 0.
]!

nextNroPP
	"retorna un id unico para una nueva instancia de producto"
	(nroProductoPedido isNil) ifTrue: [nroProductoPedido := 0].
	nroProductoPedido := nroProductoPedido + 1.
	^nroProductoPedido.
! !

!ProductoPedido class categoriesForMethods!
crearProductoPedido:cantidad:!public! !
crearProductoPedidoNroProd:cantidad:costoUnitario:!public! !
initialize!public! !
nextNroPP!public! !
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

iniPanaderoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel puesto: unPuesto
	"Inicializa una instancia de Panadero"

	super iniEmpleadoLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel.
	puesto := unPuesto.
	sueldo := 800000.
	
	
!

printOn: aStream
	aStream
		nextPutAll: 'Panadero Puesto:  ';
		nextPutAll: puesto , ' | '.
		super printOn: aStream.!

producirProducto:unProducto cant:unaCant
"Produce la cantidad de un producto"
|unidad|
unidad:= ''.
(unProducto verPorDocena) ifTrue: [unidad := ' Docenas'] .

unProducto aumentarStock: unaCant.
Transcript show: 'se produjo ' , (unaCant  printString), unidad,' de ', unProducto verNombre; cr.
Transcript show: ' (Stock: ' , unProducto verStock printString , '  )' ; cr.
!

verPuesto
^puesto.! !

!Panadero categoriesForMethods!
imprimir!public! !
iniPanaderoLegajo:nom:dire:tel:puesto:!public! !
printOn:!public! !
producirProducto:cant:!public! !
verPuesto!public! !
!

!Panadero class methodsFor!

crearPanaderoLegajo: unLegajo nom: unNom dire: unaDire tel: unTel puesto: unPuesto
	"Retorna una instancia de Panadero inicializada"

	^self new
		iniPanaderoLegajo: unLegajo
		nom: unNom
		dire: unaDire
		tel: unTel
		puesto: unPuesto! !

!Panadero class categoriesForMethods!
crearPanaderoLegajo:nom:dire:tel:puesto:!public! !
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

printOn: aStream
	aStream
		nextPutAll: 'Repartidor nro:  ';
		nextPutAll: nroRepartidor printString , ' | '.
	super printOn: aStream!

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
printOn:!public! !
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

	pedidosVendidos add: unNroPedido!

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
	pedidosVendidos := OrderedCollection new.
	sueldo := 600000!

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

