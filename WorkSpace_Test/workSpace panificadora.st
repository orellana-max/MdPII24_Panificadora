|var1 var2 var3 pan prod prod2 cl1 cl2 list list2  |

Producto initialize.
pan := Panificadora crearPanificadoraNom: 'la fabrica de Pan' dire: 'Av. Calchaqui 1542' tel: '4255-8787'.

prod2:= Producto crearProductoNombre: 'Torta Oreo 2kg' tip: 'pasteleria'  prec: 15000.5.  
pan agregarProducto: prod2.
prod:= Producto crearProductoNombre: 'Flauta 10kg' tip: 'pan' prec: 25000.05.
pan agregarProducto: prod.

prod := pan traerProductoNro: 1.
prod modStock: 5.
Transcript show: (prod imprimir);cr.


pan eliminarProducto: prod.
pan eliminarProducto: prod.

list := (pan verListaProductos )collect:[:pro| pro verNombre].


cl1 := Cliente crearClienteNom: 'cliente01' dire: 'calle 01' tel: '+54 9 114578-4545'.
pan agregarCliente: cl1.
cl2 := Cliente crearClienteNom: 'cliente02' dire: 'calle 02' tel: '+55 3 994578-2222'.
pan agregarCliente: cl2.

pan eliminarCliente: cl2.
pan eliminarCliente: cl2.

list2:= pan verListaClientes.

list2 do:[:cli |
Transcript show: (cli verNombre).
Transcript show: ' - '.
Transcript show: (cli verDireccion).
Transcript show: ' / '; cr].

"pan eliminarProducto: prod."

Transcript show: (prod verNombre ).

list2 do:[:pro | Transcript show: (pro verNombre )].
var1 := Prompter prompt: 'ingrese algo'.