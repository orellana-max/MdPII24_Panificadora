"Filed out from Dolphin Smalltalk"!

Object subclass: #App
	instanceVariableNames: 'var1'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

App guid: (GUID fromString: '{ff893e23-44c7-48a8-9911-5f052bcd62df}')!

App comment: ''!

!App categoriesForClass!No category! !

!App class methodsFor!

cargarCliente:pan

| nom dir tel c sigue |

sigue := true.
"Cargamos Clientes"
[sigue] whileTrue: [
	Transcript cr.
	"Creamos un Cliente manualmente"
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	c:= Cliente crearClienteNom: nom dire: dir tel: tel.
	MessageBox warning: 'Se creo el Cliente Nro ', c verNroCliente printString , 'Nombre: ',  c verNombre.
	pan agregarCliente: c.
	Transcript show: 'Se agrego un Nuevo Cliente a la Panificadora' ; cr.
	sigue:= MessageBox confirm: '¿Desea cargar otro Cliente?'.
]"termina de cargar Cliente"!

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

|dic cant p leg nom dir tel  op pue |
"Dictionary para puestos de Panadero"
dic := Dictionary new 
	at:'1' put:'Pastelero';
	at:'2' put:'Panadero';
	at:'3' put:'Facturero';
	at:'4' put:'Maestro';
	yourself.

"Ingresar cantidad de Panaderos a cargar "
cant:=''.
[cant isNumber] whileFalse: [
	cant := Prompter prompt: 'Ingrese cantidad de Panaderos a cargar  '.
	"comprobo que se ingreso un numero"
	(cant ='') ifTrue: [MessageBox warning: 'no ingreso un numero']
			ifFalse:[ ((cant detect: [ :c| c isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [cant := cant asNumber.]
												ifFalse:[MessageBox warning: 'no ingreso un numero'].
			]. 
	].

1 to: cant do: [:i |
	Transcript cr.
	"Creamos un Panadero manualmente"
	leg:=''.
	"comprobamos que se ingreso un numero"
	[leg isNumber] whileFalse: [
	leg := Prompter prompt: 'Ingrese Legajo:  '.
	(leg ='') ifTrue: [MessageBox warning: 'no ingreso un numero']
			ifFalse:[ ((leg detect: [ :le | le isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [leg := leg asNumber.]
								ifFalse:[MessageBox warning: 'no ingreso un numero'].
			]. 
	].
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	pue := '0'.
	[pue = '0'] whileTrue: [
		op:= Prompter prompt: 'Ingrese una opción de puesto: ', 
		'1- Pastelero, 2- Panadero, 3- Facturero, 4- Maestro'. 
		(dic includesKey: op) ifTrue: [ pue:= dic at: op ] 
						ifFalse:[ MessageBox warning: 'no ingreso una opción valida'.
								pue:='0'.].
	].
	p:= Panadero crearPanaderoLegajo: leg nom: nom dire: dir tel: tel puesto: pue.
	MessageBox warning: 'Se creo con exito el Panadero ', p verPuesto , ' Legajo: ', p verLegajo printString , ' Nombre: ',  p verNombre.
	
	pan agregarPanadero: p.
	Transcript show: 'Se agrego un Nuevo Panadero a la Panificadora';cr.
]!

cargarPanaderosPanificadora: pan
	"Agregamos algunos Panaderos a la panaficadora"

	pan agregarPanadero: (Panadero crearPanaderoLegajo: 1 nom: 'Daniel' dire: 'calle 101' tel: '123-564' puesto: 'Pastelero').
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

cargarPedidosAlaPanificadora: pan
	"Creamos y cargamos varios Pedidos a la panificadora"

	| listClient listPP list c n |
	listClient := pan verListaClientes.
	listPP := App crearListaProductosPedidos: pan.
	n := 1.
	1 to: 4 do:  [:i |
			listClient do: [:c | 
				list := OrderedCollection new.
				1 to: 4 do: [:k |
					list add: (listPP at: n).
					n := n + k].
			n := n -5.
			pan agregarPedido: (Pedido crearPedidoNroClien: c verNroCliente listProdPed: list)]
]!

cargarProducto:pan

|pro dic nom tip precio precio2 op sigue cant idx|
	
	"Dictionary para Tipo de Producto"
	dic := Dictionary new 
			at:'1' put:'Pastel'; 
			at:'2' put:'Pan'; 
			at:'3' put:'Factura'; 
			at:'4' put:'Tarta'; 
			at:'5' put:'Galleta'; 
			yourself.
	
	sigue := true.
	"Cargamos Productos"
	[sigue] whileTrue: [
		Transcript cr.
		"Creamos un Producto manualmente"
		nom:= Prompter prompt: 'ingrese nombre del Producto: '.
		tip := '0'.
		[tip = '0'] whileTrue: [
			op:= Prompter prompt: 'ingrese nro de tipo: ',
			'1- Pastel, 2- Pan, 3- Factura, 4- Tarta, 5- Galleta'. 
			tip:= dic at: op ifAbsent: ['0'].
			(tip = '0') ifTrue: [MessageBox warning: 'No ingreso una opción valida'.].
		].
		
		"Pedimos Precio, Comprobamos que ingrese un float"
		precio :=''.
		[precio isNumber] whileFalse: [
			precio := Prompter prompt: 'Ingrese Precio del Producto  '.
			cant := precio occurrencesOf: $.. "ocurrencias de puntos"
		
			((precio ='') or: [cant > 1]) "si ingreso varios puntos o vacio"
					ifTrue: [MessageBox warning: 'no ingreso un numero'] 
					ifFalse: [
					(cant = 0)  "si no tiene puntos"
							ifTrue: [ "Bloque si no contiene letras ni punto"
							((precio detect: [ :pre | pre isDigit not] ifNone:['vacio']) = 'vacio' ) 
							ifTrue: [precio := precio asNumber asFloat.] 
							ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
							].
					(cant = 1) "si tiene un punto"
							ifTrue:[
							idx := precio indexOf: $..
							precio2 := (precio copyFrom: 1 to: idx -1), (precio copyFrom: idx +1).
							((precio2 detect: [ :pre2 | pre2 isDigit not] ifNone:['vacio']) = 'vacio' ) 
								ifTrue: [ "Bloque si no contiene letras es un numero"
								precio := precio asNumber asFloat.
							] "fin Bloque si no contiene letras es un numero"
							ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
							].
					].
		].
		"Creamos el producto"
		pro:= Producto crearProductoNombre: nom tip: tip prec: precio.
		MessageBox warning:  'Se creo con exito el Producto Nro: ', pro verNroProducto printString , ' Nombre: ', pro verNombre , ' Tipo ',  pro verTipo.
		pan agregarProducto: pro.
		Transcript show: 'Se agrego un Nuevo Producto a la Panificadora';cr.
		sigue:= MessageBox confirm: '¿Desea cargar otro Producto?'.
	]."termina de cargar Productos"



!

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

|r leg nom dir tel sigue |

sigue := true.
"Cargamos Repartidores"
[sigue] whileTrue: [
	Transcript cr.
	"Creamos un Repartidor manualmente"
	leg:=''.
	"comprobamos que se ingreso un numero"
	[leg isNumber] whileFalse: [
	leg := Prompter prompt: 'Ingrese Legajo:  '.
	(leg ='') ifTrue: [MessageBox warning: 'no ingreso un numero']
			ifFalse:[ ((leg detect: [ :le | le isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [leg := leg asNumber.]
								ifFalse:[MessageBox warning: 'no ingreso un numero'].
			]. 
	].
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	r:= Repartidor crearRepartidorLegajo: leg nom: nom dire: dir tel: tel.
	MessageBox warning:  'Se creo con exito el Repartidor Nro ', r verNroRepartidor printString , 'nom: ',  r verNombre.
	pan agregarRepartidor: r.
	Transcript show: 'Se agrego un Nuevo Repartidor a la Panificadora'; cr.
	sigue:= MessageBox confirm: '¿Desea cargar otro Repartidor?'.
]. "Termina Carga de Repartidores"!

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

|v leg nom dir tel sigue|

sigue := true.
"Cargamos Vendedores"
[sigue] whileTrue: [
	Transcript cr.
	"Creamos un Vendedor manualmente"
	leg:=''.
	"comprobamos que se ingreso un numero"
	[leg isNumber] whileFalse: [
	leg := Prompter prompt: 'Ingrese Legajo:  '.
	(leg ='') ifTrue: [MessageBox warning: 'no ingreso un numero']
			ifFalse:[ ((leg detect: [ :le | le isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [leg := leg asNumber.]
								ifFalse:[MessageBox warning: 'no ingreso un numero'].
			]. 
	].
	nom:= Prompter prompt: 'ingrese nombre: '.
	dir:= Prompter prompt: 'ingrese direccion: '.
	tel:= Prompter prompt: 'ingrese telefono: '.
	v:= Vendedor crearVendedorLegajo: leg nom: nom dire: dir tel: tel.
	MessageBox warning:  'Se creo el Vendedor Nro', v verNroVendedor printString , ' nom: ',  v verNombre.
	pan agregarVendedor: v.
	Transcript show: 'Se agrego un Nuevo Vendedor a la Panificadora'; cr.
	sigue:= MessageBox confirm: '¿Desea cargar otro Vendedor?'.
].!

cargarVendedoresPanificadora:pan
"Creamos varios Vendedores y agregamos a la Lista de Vendedores"

	pan agregarVendedor: (Vendedor crearVendedorLegajo: 301 nom: 'Sandra' dire: 'calle 301' tel: '300-301').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 302 nom: 'Santiago' dire: 'calle 302' tel: '300-302').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 303 nom: 'Gisela' dire: 'calle 303' tel: '300-303').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 304 nom: 'Martin' dire: 'calle 304' tel: '300-304').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 305 nom: 'Marcelo' dire: 'calle 305' tel: '300-305').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 306 nom: 'Silvio' dire: 'calle 306' tel: '300-306').
!

crearListaProductosPedidos: pan
"devuelve una lista de productos pedidos"
|list pro listProductos|

ProductoPedido initialize.
list := OrderedCollection new.
listProductos := pan verListaProductos.
(listProductos isEmpty) ifFalse:[

1 to: (listProductos size) do: [:i | 
	pro :=  listProductos at: i.
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 1).
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 2).
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 3).
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 4).
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 5).
	list add: (ProductoPedido crearProductoPedido: pro cantidad: 6).
	].
].

^list.
!

imprimirPedidosCliente:pan
"imprime los pedidos realizados por un cliente"
|num listaPedidos c|
num:=''.
[num isNumber] whileFalse: [
	num := Prompter prompt: 'Ingrese número de Cliente a buscar los pedidos '.
	"comprobueba que se ingresa un numero"
	(num ='') ifTrue: [MessageBox warning: 'no ingreso un numero']
			ifFalse:[ ((num detect: [ :di| di isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [num := num asNumber.]
												ifFalse:[MessageBox warning: 'no ingreso un numero'].
			]. 
].
c:= pan traerClienteNro: num.
(c isNil) ifTrue: [ MessageBox warning: 'El número ingresado no pertenece a un cliente']
		ifFalse:[
			listaPedidos := (pan verListaPedidos) select:  [ : p | p verNroCliente =  num ].
			"impresion pedidos del cliente"
			(listaPedidos isEmpty) ifTrue: [MessageBox warning: 'No hay pedidos realizados por ese Cliente'.]
							ifFalse: [Transcript cr; show: 'Lista de pedidos del cliente Nro: ', num printString, ' Nombre: ', c verNombre ; cr.
									listaPedidos do:[: p | Transcript show:
													'Nro pedido: ', (p verNroPedido) printString ,
													' | Fecha Entrega: ', (p verFechaEntrega) printString,	
													' | Costo: ', (p verTotal) printString,
													' | Estado ', (p verEstado) printString; cr
												].
									].
]!

imprimirPedidosMayorAMenor: pan
"imprime los pedidos ordenados de mayor a menor costo"
|pedidosOrdenCosto |

pedidosOrdenCosto := (pan verListaPedidos) asSortedCollection: [ :x :y | x verTotal > y verTotal].

"imprimir los datos ordenados"
Transcript show: 'Lista de pedidos ordenados por costo de mayor a menor'; cr.
(pedidosOrdenCosto isEmpty) ifTrue: [Transcript  show: 'No se han realizado pedidos aun' ] 
					   ifFalse: [ pedidosOrdenCosto do:[ : p |
							Transcript show:
							'Nro Pedido ', (p verNroPedido) printString,
							' | Nro cliente: ', (p verNroCliente) printString,
							' | Fecha Entrega: ', (p verFechaEntrega) printString,	
							' | Costo: ', (p verTotal) printString; cr		
													].
							].!

imprimirPedidosMayorIgualA: pan
"imprime los pedidos mayores o iguales a un determinado costo ingresado por teclado"
|costo listaPedidos |

costo := ''.
"comprobamos que se ingreso un numero"
[costo isNumber] whileFalse: [
costo := Prompter prompt: 'Ingresar un valor para mostrar los pedidos cuyo costo sea mayor o igual a ese valor'.
(costo ='') ifTrue: [MessageBox warning: 'no ingreso un número']
		ifFalse:[ ((costo detect: [ :le | le isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [costo := costo asNumber.]
							ifFalse:[MessageBox warning: 'no ingreso un numero'].
		]. 
].
listaPedidos := (pan verListaPedidos) reject:  [ : p | p verTotal < costo]. "no selecciona aquellos pedidos con costo menor al costo ingresado por teclado"

"impresion pedidos mayores a un costo"
(listaPedidos isEmpty) ifTrue: [Transcript show: 'No hay pedidos mayores o iguales a ese costo'; cr]
				ifFalse:[Transcript show: 'Lista de pedidos de costo mayor a: ', costo printString; cr.
						listaPedidos do:[
									: p | Transcript show:
										'Nro cliente: ', (p verNroCliente) printString,
										' | Nro pedido: ', (p verNroPedido) printString ,
										' | Fecha: ', (p verFechaEntrega) printString,	
										' | Costo: ', (p verTotal) printString; cr		
									].
					].
!

load:pan
"inicializa las clases y Carga todos las instancias creadas para la panificadora"
Cliente initialize.
Empleado initialize.
Panadero initialize.
Pedido initialize.
Producto initialize.
ProductoPedido initialize.
Proveedor initialize.
Repartidor initialize.
Vendedor initialize.

App cargarClientesPanificadora:  pan.
App cargarPanaderosPanificadora: pan.
App cargarProductosPanificadora: pan.
App cargarRepartidoresPanificadora: pan.
App cargarVendedoresPanificadora: pan.
App cargarPedidosAlaPanificadora: pan.

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

mainFunciones: pan
|op|

op :=''.
Transcript clear.
[op ~= '0']  whileTrue: [
	Transcript show: 'Funciones: ';cr.
	Transcript show: 'Elige opción: ';cr.
	Transcript show: 'Opción 1: Imprimir pedidos de un cliente';cr.
	Transcript show: 'Opción 2: Imprimir pedidos mayores o iguales a un costo';cr.
	Transcript show: 'Opción 3: Imprimir todos los pedidos ordenados por costo de mayor a menor';cr.
	Transcript show: 'Opción 0: Volver al menú principal';cr.
	op:= Prompter prompt: 'Funciones. Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=3]) ifTrue:[
			(op = 1) ifTrue:[ Transcript show: 'seleccionó 1 Imprimir pedidos de un cliente.';cr. 
					App imprimirPedidosCliente: pan.
					op:=''. ].		
			(op = 2) ifTrue:[ Transcript show: 'seleccionó 2 .Imprimir pedidos iguales o mayores a un costo ';cr. 
					App imprimirPedidosMayorIgualA: pan.
					op:=''. ].		
			(op = 3) ifTrue:[ Transcript show: 'seleccionó 3 Imprimir todos los pedidos ordenados por costo de mayor a menor';cr. 
					App imprimirPedidosMayorAMenor: pan.
					op:=''. ].		
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. Volviendo al menú principal';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'no seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]!

mainListarDatos: pan
	| op lista volver impMenu |
	op := ''.
	volver := false.
	impMenu:= true. "si se imprime o no el menu de opciones de listar"
	Transcript clear.
	[volver not] whileTrue: [
			impMenu ifTrue:[
			Transcript show: '	MENU LISTAR DATOS ';cr; show: 'Elige opción: ';cr.
			Transcript show: 'Opción 1: Listar Clientes';cr.
			Transcript show: 'Opción 2: Listar Panaderos';cr.
			Transcript show: 'Opción 3: Listar Productos';cr.
			Transcript show: 'Opción 4: Listar Repartidores ';cr.
			Transcript show: 'Opción 5: Listar Vendedores ';cr.
			Transcript show: 'Opción 0: Salir';cr;cr.
			].
			op := Prompter prompt: 'MENU LISTAR DATOS. Ingrese opción: '.
			op = '' ifTrue:[op:='incorrecta'].
			op = '0'
				ifTrue: [volver := MessageBox confirm: '¿Desea Volver al menu anterior?']
				ifFalse: [
					op = '1' ifTrue: 
							[Transcript show: 'seleccionó Listar Clientes.';cr.
							lista := pan verListaClientes.
							op := ''].
					op = '2' ifTrue: 
							[Transcript show: 'seleccionó Listar Panaderos.'; cr.
							lista := pan verListaPanaderos.
							op := ''].
					op = '3' ifTrue: 
							[Transcript show: 'seleccionó Listar Productos.';cr.
							lista := pan verListaProductos.
							op := ''].
					op = '4' ifTrue: [Transcript show: 'seleccionó Listar Repartidores.';cr.
							lista := pan verListaRepartidores.
							op := ''].
					op = '5'
						ifTrue: [Transcript show: 'seleccionó Listar Vendedores.';cr.
							lista := pan verListaClientes.
							op := ''].
					op ~= ''
						ifTrue: [MessageBox warning: 'No seleccionó una opcion correcta'.
							impMenu :=false.
							op:=''.]
						ifFalse: [lista do: [:ob | Transcript show: ob imprimir;cr].
							Transcript cr.
							impMenu:=true ].
				].
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
					App mainFunciones: pan.
					op:=''. ].		
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. saliendo';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'no seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]
!

run:pan
"Carga todos las instancias creadas para la panificadora"
App load:pan.
"Arranca el menu principal"
App mainPrincipal: pan.! !

!App class categoriesForMethods!
cargarCliente:!public! !
cargarClientesPanificadora:!public! !
cargarPanadero:!public! !
cargarPanaderosPanificadora:!public! !
cargarPedidosAlaPanificadora:!public! !
cargarProducto:!public! !
cargarProductosPanificadora:!public! !
cargarRepartidor:!public! !
cargarRepartidoresPanificadora:!public! !
cargarVendedor:!public! !
cargarVendedoresPanificadora:!public! !
crearListaProductosPedidos:!public! !
imprimirPedidosCliente:!public! !
imprimirPedidosMayorAMenor:!public! !
imprimirPedidosMayorIgualA:!public! !
load:!public! !
mainCargaManual:!public! !
mainFunciones:!public! !
mainListarDatos:!public! !
mainPrincipal:!public! !
run:!public! !
!

