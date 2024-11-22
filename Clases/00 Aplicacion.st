| salir op pan volver sigue c p r v nom tel dir dic cant leg pue pro tip precio precio2 idx impMenu costo listaPedidos|

pan:= Panificadora crearPanificadoraNom: 'pa' dire: 'Av.7 123'  tel: '4257-8897'.
"Carga de datos predefinidos a la Panificadora"
App load: pan.

"Menu Principal"
op :=''.
salir := false.
[salir not]  whileTrue: [
	Transcript clear; show: '	MENU PRINCIPAL: '; cr; show: 'Elige opción: '; cr.
	Transcript show: 'Opción 1: Cargar datos manualmente';cr.
	Transcript show: 'Opción 2: Listar datos';cr.
	Transcript show: 'Opción 3: Buscar datos';cr.
	Transcript show: 'Opción 4: Funciones ';cr.
	Transcript show: 'Opción 0: Salir';cr;cr.
	op:= Prompter prompt: 'Menu Principal. Ingrese opción: '.

	(op = '0') ifTrue:[ salir:= MessageBox confirm: '¿Desea Salir del Programa'] 
	ifFalse: [
"1 - Cargar Datos"
	(op = '1') ifTrue:[ Transcript show: '1 - Cargar datos manualmente.';cr. 		
		"Menu carga de datos"
	op :=''.
	volver := false.
	[volver not]  whileTrue: [
		Transcript clear; show: '	 MENU CARGA DE DATOS '; cr; show: 'Elige opción: '; cr.
		Transcript show: 'Opción 1: Cargar Clientes';cr.
		Transcript show: 'Opción 2: Cargar Panaderos';cr.
		Transcript show: 'Opción 3: Cargar Productos';cr.
		Transcript show: 'Opción 4: Cargar Repartidores ';cr.
		Transcript show: 'Opción 5: Cargar Vendedores ';cr.
		Transcript show: 'Opción 0: Salir';cr;cr.
	
		op:= Prompter prompt: 'Menu Carga Manual. Ingrese opción: '.
	
		(op = '0') ifTrue:[volver:= MessageBox confirm: '¿Desea Volver al menu anterior?']
				ifFalse: [
		"1 - Carga de Clientes"
				(op = '1') ifTrue:[ Transcript show: '1 - Cargar Clientes.';cr. 
						sigue := true.
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
						].
						"Termina Carga de Clientes"
						op:=''. ].
		"2 - Carga de Panaderos"
				(op = '2') ifTrue:[ Transcript show: '2 - Cargar Panaderos.';cr.
						"Dictionary para puestos de Panadero"
						dic := Dictionary new 
							at:'1' put:'Pastelero'; at:'2' put:'Panadero'; at:'3' put:'Facturero'; at:'4' put:'Maestro';
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
						"Agregamos Panaderos cant de veces"
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
							"Agregar panadero a la panificadora"
							pan agregarPanadero: p.
							Transcript show: 'Se agrego un Nuevo Panadero a la Panificadora';cr.
						].
						"termina carga de Panadero"
						op:=''. ].
	
		"3 - Carga de Productos"
				(op = '3') ifTrue:[ Transcript show: '3 - Cargar Productos.';cr. 
						"Dictionary para Tipo de Producto"
						dic := Dictionary new at:'1' put:'Pastel'; at:'2' put:'Pan'; at:'3' put:'Factura'; at:'4' put:'Tarta'; at:'5' put:'Galleta'; yourself.
			
						sigue := true.
						"Cargamos Productos"
						[sigue] whileTrue: [ Transcript cr.
							"Creamos un Producto manualmente"
							nom:= Prompter prompt: 'ingrese nombre del Producto: '.
							tip := '0'.
							[tip = '0'] whileTrue: [ 
									op:= Prompter prompt: 'ingrese nro de tipo de Producto: ', '1- Pastel, 2- Pan, 3- Factura, 4- Tarta, 5- Galleta'. 
									tip:= dic at: op ifAbsent: ['0'].
									(tip = '0') ifTrue: [MessageBox warning: 'No ingreso una opción valida'.]. ].
							
							"Pedimos Precio, Comprobamos que ingrese un float"
							precio :=''.
							[precio isNumber] whileFalse: [  precio := Prompter prompt: 'Ingrese Precio del Producto  '.
								cant := precio occurrencesOf: $.. "ocurrencias de puntos"
								((precio ='') or: [(precio ='.') or: [cant > 1]]) ifTrue: [MessageBox warning: 'no ingreso un numero'] 
											ifFalse: [
												(cant = 0)  "si no tiene puntos"
													ifTrue: [ "Bloque si no contiene letras ni punto"
														((precio detect: [ :pre | pre isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [precio := precio asNumber asFloat.] 
																			ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
														].
												(cant = 1) "si tiene un punto"
													ifTrue:[ idx := precio indexOf: $..
														(idx = 1) ifTrue:[ precio:= '0',precio.
																	idx:= idx+1.].
														precio2 := (precio copyFrom: 1 to: idx -1), (precio copyFrom: idx +1).
														((precio2 detect: [ :pre2 | pre2 isDigit not] ifNone:['vacio']) = 'vacio' ) 
															ifTrue: [ "Bloque si no contiene letras es un numero"
															precio := precio asNumber asFloat. ] "fin Bloque si no contiene letras es un numero"
															ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
														].]. "se comprobo si ingreso un numero o no" 
								]. "fin while, ingreso un float o number"
							"Creamos el producto"
							pro:= Producto crearProductoNombre: nom tip: tip prec: precio.
							MessageBox warning:  'Se creo con exito el Producto Nro: ', pro verNroProducto printString , ' Nombre: ', pro verNombre , ' Tipo ',  pro verTipo.
							pan agregarProducto: pro.
							Transcript show: 'Se agrego un Nuevo Producto a la Panificadora';cr.
							sigue:= MessageBox confirm: '¿Desea cargar otro Producto?'.
						].	
						"termina bucle de cargar Productos"
						op:=''. ].
		"4 - Carga de Repartidores"
				(op = '4') ifTrue:[ Transcript show: '4 - Cargar Repartidores.';cr. 
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
						].
						"termina bucle de cargar Productos"
						op:=''. ].
		"5 - Carga de Vendedores"
				(op = '5') ifTrue:[ Transcript show: '5 - Cargar Vendedores.';cr. 
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
						].
						"termina bucle de cargar Vendedores"
						op:=''. ].		
				(op ~= '') ifTrue: [ MessageBox warning: 'No seleccionó una opcion correcta'. ].
				volver := false.
				Transcript cr. 
				]."no sale"
	]."salir Menu Carga de Datos"

"Termina menu Carga de Datos"				
	op:=''. ].
"2 - Listar Datos"
	(op = '2') ifTrue:[
	"Menu Listar de datos"
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
				Transcript show: 'Opción 6: Listar Pedidos ';cr.
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
					op = '6'
						ifTrue: [Transcript show: 'seleccionó Listar Pedidos.';cr.
							lista := pan verListaPedidos.
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
"2 - salir Menu Listar Datos"
	op:=''. ].
"3 - Buscar Datos"
	(op = '3') ifTrue:[ Transcript show: '3 - Buscar Datos';cr. 
	
	App imprimirPedidosCliente: pan.
	MessageBox warning: 'seguir'.
	App imprimirPedidosMayorIgualA: pan.
	MessageBox warning: 'seguir'.

	"Termina menu Buscar Datos"	
	op:=''. ].
"4 - Funciones"
	(op = '4') ifTrue: [
	Transcript clear.
	op := ''.
	volver := false.
	impMenu:= true. "si se imprime o no el menu de opciones de listar"
	[volver not] whileTrue: [
		impMenu ifTrue:[
		Transcript show: '    MENU FUNCIONES '; cr; show: 'Elige opción: '; cr.
		Transcript show: 'Opción 1: Listar pedidos (reducido)'; cr.
		Transcript show: 'Opción 2: Imprimir pedidos de un cliente';cr.
		Transcript show: 'Opción 3: Imprimir pedidos mayores o iguales a un costo';cr.
		Transcript show: 'Opción 4: Imprimir todos los pedidos ordenados por costo de mayor a menor';cr.
		Transcript show: 'Opción 5: Listar Pedidos con total > 20000'; cr.
		Transcript show: 'Opción 0: Salir'; cr; cr. ].
	
		op := Prompter prompt: 'Menú Funciones. Ingrese opción: '.
		op = '' ifTrue:[op:='incorrecta'].

		(op = '1') ifTrue: [
				Transcript show: 'Listando pedidos...'; cr.
				lista := pan verListaPedidos.
				
				lista isEmpty ifTrue: [
						Transcript show: 'No hay pedidos para mostrar.'; cr.
						] ifFalse: [
						"Transformar la lista de pedidos usando collect"
						lista := lista collect: [:pedido |
						'Pedido Nro: ', pedido verNroPedido printString,
						', Cliente Nro: ', pedido verNroCliente printString,
						', Total: ', pedido verTotal printString
						].
                
						"Mostrar los resultados"
						lista do: [:pedidoTexto | Transcript show: pedidoTexto; cr.].
						Transcript cr.
						].
				
				"Esperar confirmación del usuario antes de continuar"
				MessageBox confirm: 'Presione aceptar para volver al Menu Funciones'.
				op := ''
				]."fin op1"

		(op = '2') ifTrue:[ Transcript show: 'Seleccionó 2 Imprimir pedidos de un cliente.';cr. 
				"imprime los pedidos realizados por un cliente"
				num:=''.
				[num isNumber] whileFalse: [
					num := Prompter prompt: 'Ingrese número de Cliente a buscar los pedidos '.
					"comprobueba que se ingresa un numero"
					(num ='') ifTrue: [MessageBox warning: 'No ingreso un número']
							ifFalse:[ ((num detect: [ :di| di isDigit not] ifNone:['vacio']) = 'vacio' ) ifTrue: [num := num asNumber.]
																ifFalse:[MessageBox warning: 'No ingreso un número'].
							]. 
				].
				c:= pan traerClienteNro: num.
				(c isNil) ifTrue: [ MessageBox warning: 'El número ingresado no pertenece a un cliente']
						ifFalse:[ listaPedidos := (pan verListaPedidos) select:  [ : p | p verNroCliente =  num ].
							"impresion pedidos del cliente"
							(listaPedidos isEmpty) ifTrue: [MessageBox warning: 'No hay pedidos realizados por ese Cliente'.]
											ifFalse: [Transcript cr; show: 'Lista de pedidos del cliente Nro: ', num printString, ' Nombre: ', c verNombre ; cr.
													listaPedidos do:[: p | Transcript show: 'Nro pedido: ', (p verNroPedido) printString ,
																	' | Fecha Entrega: ', (p verFechaEntrega) printString,	
																	' | Costo: ', (p verTotal) printString, ' | Estado ', (p verEstado) printString; cr
																].
													].
						].
				op:=''. 
				]."fin op2"
		(op = '3') ifTrue:[ Transcript show: 'seleccionó 3 .Imprimir pedidos iguales o mayores a un costo ';cr. 
				"imprime los pedidos mayores o iguales a un determinado costo ingresado por teclado"
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
				op:=''. ]."fin op3"

		(op = '4') ifTrue:[ Transcript show: 'seleccionó 4 Imprimir todos los pedidos ordenados por costo de mayor a menor';cr. 
				"imprime los pedidos ordenados de mayor a menor costo"
				listaPedidos := (pan verListaPedidos) asSortedCollection: [ :x :y | x verTotal > y verTotal].

				"imprimir los datos ordenados"
				Transcript cr; show: 'Lista de pedidos ordenados por costo de mayor a menor'; cr.

				(listaPedidos isEmpty) ifTrue: [Transcript  show: 'No se han realizado pedidos aun' ] 
							ifFalse: [ listaPedidos do:[ : p | Transcript show: 'Nro Pedido ', (p verNroPedido) printString,
									' | Nro cliente: ', (p verNroCliente) printString, ' | Fecha Entrega: ', (p verFechaEntrega) printString,	
									' | Costo: $', (p verTotal) printString; cr		
								]. ].
				op:=''. ]."fin op4"
	
		(op = '5') ifTrue: [ Transcript show: 'Listando pedidos con total > 200000...'; cr.
				lista := pan verListaPedidos.
				"Seleccionar los pedidos cuyo total es mayor a 200000"
				pedidosConTotalAlto := lista select: [:pedido | pedido verTotal > 200000].
				pedidosConTotalAlto isEmpty ifTrue: [ Transcript show: 'No hay pedidos con total mayor a 200000.'; cr.
								] ifFalse: [
								"Mostrar los pedidos que cumplen con la condición"
								pedidosConTotalAlto do: [:pedido |
								Transcript show: 'Pedido Nro: ',pedido verNroPedido printString,
											', Cliente Nro: ', pedido verNroCliente printString,
											', Total: ', pedido verTotal printString; cr.].
								].
				op := ''
				]."fin op5"

		(op = '0') ifTrue: [
				volver := MessageBox confirm: '¿Desea salir del menú Funciones?'.
				op:=''.
				]."fin op0"
		
		(op ~= '') ifTrue: [MessageBox warning: 'No seleccionó una opcion correcta'.
				impMenu :=false.
				] ifFalse: [	Transcript cr.
						impMenu:=true
				].

	]."sale While"
	"Termina menu 4 Funciones"
	op:=''.
	].

	(op ~= '') ifTrue: [ MessageBox warning: 'No seleccionó una opcion correcta'. ].
	]."no sale"
]."salir"

