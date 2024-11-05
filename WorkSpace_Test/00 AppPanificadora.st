| op pan list|
pan:= Panificadora crearPanificadoraNom: 'pa' dire: 'Av.7 123'  tel: '4257-8897'.
App mainPrincipal.
	
App cargarClientesPanificadora: pan.
App cargarPanaderosPanificadora: pan.
App cargarProductosPanificadora: pan.
App cargarRepartidoresPanificadora: pan.
App cargarVendedoresPanificadora: pan.

"Listar los Clientes"
Transcript cr.
Transcript nextPutAll: 'Lista de Clientes '; cr.
list := pan verListaClientes.
list do:[:cl | Transcript show: cl imprimir; cr].
Transcript cr.

"Listar los Panaderos"
Transcript cr.
Transcript nextPutAll: 'Lista de Panaderos '; cr.
list := pan verListaPanaderos.
list do:[:pa | Transcript show: pa imprimir; cr].
Transcript cr.

"Imprimir lista de productos"
Transcript cr.
Transcript show: 'Lista de Productos'; cr; cr.
list := pan verListaProductos.
list do:[:pr | Transcript show: pr imprimir; cr.].
Transcript cr.

"Listar los Repartidores"
Transcript cr.
Transcript nextPutAll: 'Lista de Repartidores '; cr.
Transcript cr.
list := pan verListaRepartidores.
list do:[:re | Transcript show: re imprimir; cr].
Transcript cr.








