|pro listPro dic nom tip prec |

listPro := OrderedCollection new.

"Dictionary para Tipo de Producto"
dic := Dictionary new at:'1' put:'Pastel'; at:'2' put:'Pan'; at:'3' put:'Factura'; at:'4' put:'Tarta'; at:'5' put:'Galleta'; yourself.

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
MessageBox warning:  'Se creo con exito el Producto Nro: ', pro verNroProducto printString , ' Nom: ', pro verNombre , ' Tipo ',  pro verTipo.
"Agregamos el Producto a la lista"
listPro add: pro.

"imprimimos el Producto creado en System Transcript"
pro printOn: Transcript.
Transcript cr.

"Creamos y cargamos varios Productos"
pro:= Producto crearProductoNombre: 'Flauta 10kg' tip: 'Pan' prec: 1000.
listPro add: pro.

"Listar los Productos"
Transcript cr.
Transcript show: 'Lista de Productos'; cr.

listPro do:[:pr | pr printOn: Transcript cr].
Transcript cr.
