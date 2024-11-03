"ver productosPedidos y pedidos, como se relacionan ,que se crea primero, o quien crea a quien."
"como obtener el costo del pedido, y el costo de cada productosPEdidos"


|var1 var2 var3 pan prod cl1 cl2 list list2 f fe|
pan := Panificadora crearPanificadoraNom: 'FabriPan' dire: 'Pan 152' tel: '4255-8787'.
pro:= Producto crearProductoNombre:

pro:= Producto crearProductoNombre:'Torta Oreo 1,5 Kg' tip: 'torta' prec: 8000.5.
pro:= Producto crearProductoNombre:'Torta Oreo 2 Kg' tip: 'torta' prec: 12000.0.
pro:= Producto crearProductoNombre:'Torta Selva Negra 1,5 Kg' tip: 'torta' prec: 5000.0.
pro:= Producto crearProductoNombre:'Torta Selva Negra 2,8 Kg' tip: 'torta' prec: 9000.0.
pro:= Producto crearProductoNombre:'Tarta de Ricota' tip: 'tarta' prec: 3000.0.
pro:= Producto crearProductoNombre:'pastaflola' tip: 'tarta' prec: 2500.0.
pro:= Producto crearProductoNombre:'Flauta 5 Kg' tip: 'pan' prec: 7500.05.
pro:= Producto crearProductoNombre:'Flauta 10 Kg' tip: 'pan' prec: 15000.05.
pro:= Producto crearProductoNombre:'Minion 5 Kg' tip: 'pan' prec: 8000.0.
pro:= Producto crearProductoNombre:'Minion 10 Kg' tip: 'pan' prec: 16000.0.
pro:= Producto crearProductoNombre:'Galletas 5 Kg' tip: 'pan' prec: 8500.0.
pro:= Producto crearProductoNombre:'Galletas 10 Kg' tip: 'pan' prec: 17000.0.
pro:= Producto crearProductoNombre:'Marinera c/sal 1 Kg' tip: 'galletas' prec: 2200.0.
pro:= Producto crearProductoNombre:'Marinera s/sal 1 Kg' tip: 'galletas' prec: 2000.0.
pro:= Producto crearProductoNombre:'Marinera Salvado 1 Kg' tip: 'galletas' prec: 2100.0.
pro:= Producto crearProductoNombre:'Grisines c/sal 1 Kg' tip: 'galletas' prec: 1900.0.
pro:= Producto crearProductoNombre:'Grisines s/sal 1 Kg' tip: 'galletas' prec: 1800.0.
pro:= Producto crearProductoNombre:'Grisines Salvado 1 Kg' tip: 'galletas' prec: 2000.0.


"imprimir lista"
pan verListaProductos do: [:p | Transcript show:( p imprimir ) ; cr].

var1 := ProductoPedido crearProductoPedido: 101 pedido: unPedido cant: cantidad

nroPedidoStatic := nroPedidoStatic +1.

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