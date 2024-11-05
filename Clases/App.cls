"Filed out from Dolphin Smalltalk"!

Object subclass: #App
	instanceVariableNames: 'var1'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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
	Transcript cr.
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

