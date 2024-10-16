|var1 var2 var3 pan prod cl1 cl2 list list2|
pan := Panificadora crearPanificadoraNom: 'la fabrica de Pan' dire: 'Av. Calchaqui 1542' tel: '4255-8787'.

pan agregarProducto: 100 nom: 'Torta Oreo 2kg' tip: 'pasteleria' prec: 15000.5.
prod:= Producto crearProductoNro: 101 nom: 'Flauta 10kg' tip: 'pan' prec: 25000.05.
pan agregarProducto: prod.
prod := pan traerProducto: 101.
prod modPorDocena.
prod modStock: 5.

cl1 := Cliente crearClienteNro: 1 nom: 'cliente01' dire: 'calle 01' tel: '+54 9 114578-4545'.
pan agregarCliente: cl1.
cl2 := Cliente crearClienteNro: 2 nom: 'cliente02' dire: 'calle 02' tel: '+55 3 994578-2222'.
pan agregarCliente: cl2.
pan agregarCliente: 3 nom: 'cliente03' dire: 'calle 03' tel: '+549116565-2525'.
pan eliminarCliente: cl2.
pan eliminarCliente: cl2.

pan eliminarProducto: prod.
pan eliminarProducto: prod.


list := (pan verListaProductos )collect:[:pro| pro verNombre].
list2:= pan verListaClientes.

list2 do:[:cli |
Transcript show: (cli verNombre).
Transcript show: ' - '.
Transcript show: (cli verDireccion).
Transcript show: ' / '; cr].



"pan eliminarProducto: prod."

Transcript show: (prod verNombre ).

list2 do:[:pro | Transcript show: (pro verTipo)].
var1 := Prompter prompt: 'ingrese algo'.