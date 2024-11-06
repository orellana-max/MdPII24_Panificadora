|pro listPro dic nom tip prec cant |

listPro := OrderedCollection new.

"Dictionary para Tipo de Producto"
dic := Dictionary new at:'1' put:'Pastel'; at:'2' put:'Pan'; at:'3' put:'Factura'; at:'4' put:'Tarta'; at:'5' put:'Galleta'; yourself.


"Ingresar cantidad de productos a crear "
cant := Prompter prompt: 'Cantidad de productos a ingresar:  '.
cant := cant asNumber.

"Creamos productos manualmente"
1 to: cant  do: [ : i |  
			nom:= Prompter prompt: 'ingrese nombre del Producto: '.
			tip := '0'.
			[tip = '0'] whileTrue: [
			op:= Prompter prompt: 'ingrese nro de tipo: 1- Pastel, 2- Pan, 3- Factura, 4- Tarta, 5- Galleta'. 
			tip:= dic at: op ifAbsent: ['0'].
			].
			prec:= Prompter prompt: 'ingrese Precio:'.
			prec:= prec asNumber.

			pro:= Producto crearProductoNombre: nom tip: tip prec: prec.
			MessageBox warning:  'Se creo con exito el Producto Nro: ', pro verNroProducto printString , ' Nom: ', pro verNombre , ' Tipo ',  pro verTipo.

			"Agregamos el Producto a la lista"
			listPro add: pro.
		
			].

"Imprimir lista de productos"
Transcript show: 'Lista de Productos'; cr.
listPro do:[:pr | Transcript show: pr imprimir.
			Transcript cr.].



"Creamos y cargamos varios Productos"

pro:= Producto crearProductoNombre:'Torta Oreo 1,5 Kg' tip: 'Pastel' prec: 8000.5.
listPro add: pro.
pro:= Producto crearProductoNombre:'Torta Oreo 2 Kg' tip: 'Pastel' prec: 12000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Torta Selva Negra 1,5 Kg' tip: 'Pastel' prec: 5000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Torta Selva Negra 2,8 Kg' tip: 'Pastel' prec: 9000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Tarta de Ricota' tip: 'Tarta' prec: 3000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Pastaflola' tip: 'Tarta' prec: 2500.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Flauta 5 Kg' tip: 'Pan' prec: 7500.05.
listPro add: pro.
pro:= Producto crearProductoNombre:'Flauta 10 Kg' tip: 'Pan' prec: 15000.05.
listPro add: pro.
pro:= Producto crearProductoNombre:'Minion 5 Kg' tip: 'Pan' prec: 8000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Minion 10 Kg' tip: 'Pan' prec: 16000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Galletas 5 Kg' tip: 'Pan' prec: 8500.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Galletas 10 Kg' tip: 'Pan' prec: 17000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Marinera c/sal 1 Kg' tip: 'Galleta' prec: 2200.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Marinera s/sal 1 Kg' tip: 'Galleta' prec: 2000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Marinera Salvado 1 Kg' tip: 'Galleta' prec: 2100.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Grisines c/sal 1 Kg' tip: 'Galleta' prec: 1900.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Grisines s/sal 1 Kg' tip: 'Galleta' prec: 1800.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Grisines Salvado 1 Kg' tip: 'Galleta' prec: 2000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Vigilante /doc' tip: 'Factura' prec: 3500.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Canioncito /doc' tip: 'Factura' prec: 4000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Medialuna Manteca /doc' tip: 'Factura' prec: 4000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Medialuna Grasa /doc' tip: 'Factura' prec: 3800.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Totita Negra /doc' tip: 'Factura' prec: 4000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Totita Blanca /doc' tip: 'Factura' prec: 4000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Churros /doc' tip: 'Factura' prec: 5000.0.
listPro add: pro.
pro:= Producto crearProductoNombre:'Sacramento /doc' tip: 'Factura' prec: 4100.0.
listPro add: pro.


"Imprimir lista de productos"
Transcript show: 'Lista de Productos'; cr.
listPro do:[:pr | Transcript show: pr imprimir.
			Transcript cr.].







