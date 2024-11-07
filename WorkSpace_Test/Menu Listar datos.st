| op pan volver sigue c p r v nom tel dir dic cant leg pue pro tip precio precio2 idx lista impMenu|
pan:= Panificadora crearPanificadoraNom: 'pa' dire: 'Av.7 123'  tel: '4257-8897'.
"Carga Listar datos"
App load: pan.

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
	]."salir Menu Listar Datos"