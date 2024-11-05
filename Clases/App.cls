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

cargarRepartidoresPanificadora:pan
"Creamos varios Repartidores y agregamos a la Lista de Repartidores"
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 201 nom: 'Federico' dire: 'calle 201' tel: '200-201').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 202 nom: 'Jaqueline' dire: 'calle 202' tel: '200-202').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 203 nom: 'Benjamin' dire: 'calle 203' tel: '200-203').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 204 nom: 'Tatiana' dire: 'calle 204' tel: '200-204').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 205 nom: 'Ana' dire: 'calle 205' tel: '200-205').
	pan agregarRepartidor: (Repartidor crearRepartidorLegajo: 206 nom: 'Joel' dire: 'calle 206' tel: '200-206').
!

cargarVendedoresPanificadora:pan
"Creamos varios Vendedores y agregamos a la Lista de Vendedores"

	pan agregarVendedor: (Vendedor crearVendedorLegajo: 301 nom: 'Sandra' dire: 'calle 301' tel: '300-301').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 302 nom: 'Santiago' dire: 'calle 302' tel: '300-302').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 303 nom: 'Gisela' dire: 'calle 303' tel: '300-303').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 304 nom: 'Martin' dire: 'calle 304' tel: '300-304').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 305 nom: 'Marcelo' dire: 'calle 305' tel: '300-305').
	pan agregarVendedor: (Vendedor crearVendedorLegajo: 306 nom: 'Silvio' dire: 'calle 306' tel: '300-306').
!

mainPrincipal
|op|

op :=''.
[op ~= '0']  whileTrue: [
	Transcript cr.
	Transcript show: 'MenuPrincipal: ';cr.
	Transcript show: 'Elige opcion: ';cr.
	Transcript show: 'Opción 1: Cargar datos manualmente';cr.
	Transcript show: 'Opción 2: Listar datos';cr.
	Transcript show: 'Opción 3: Buscar datos';cr.
	Transcript show: 'Opción 4: Funciones ';cr.
	Transcript show: 'Opción 0: Salir';cr.
	op:= Prompter prompt: 'Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=4]) ifTrue:[
			(op = 1) ifTrue:[ Transcript show: 'seleccionó , Cargar datos manualmente.';cr. 
					op:=''. ].		
			(op = 2) ifTrue:[ Transcript show: 'seleccionó 2';cr. 
					op:=''. ].		
			(op = 3) ifTrue:[ Transcript show: 'seleccionó 3';cr. 
					op:=''. ].		
			(op = 4) ifTrue:[ Transcript show: 'seleccionó 4';cr. 
					op:=''. ].		
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. saliendo';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'no seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]
! !

!App class categoriesForMethods!
cargarClientesPanificadora:!public! !
cargarPanaderosPanificadora:!public! !
cargarProductosPanificadora:!public! !
cargarRepartidoresPanificadora:!public! !
cargarVendedoresPanificadora:!public! !
mainPrincipal!public! !
!

