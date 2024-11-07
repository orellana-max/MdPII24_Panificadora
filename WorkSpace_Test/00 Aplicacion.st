| salir op pan volver sigue c p r v nom tel dir dic cant leg pue pro tip precio precio2 idx impMenu|

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
						dic := Dictionary new 
								at:'1' put:'Pastel'; at:'2' put:'Pan'; at:'3' put:'Factura'; at:'4' put:'Tarta'; at:'5' put:'Galleta'; 
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
	(op = '2') ifTrue:[ Transcript show: '2 - Listar datos';cr. 
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
"2 - salir Menu Listar Datos"
	op:=''. ].
"3 - Buscar Datos"
	(op = '3') ifTrue:[ Transcript show: '3 - Buscar Datos';cr. 


	"Termina menu Buscar Datos"	
	op:=''. ].
"4 - Funciones"
	(op = '4') ifTrue:[ Transcript show: '4 - Funciones';cr. 


	"Termina menu Funciones"	
	op:=''. ].
	(op ~= '') ifTrue: [ MessageBox warning: 'No seleccionó una opcion correcta'. ].
	]."no sale"
]."salir"

