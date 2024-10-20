| panf  prov pro pro2|

panf := Panificadora crearPanificadoraNom: 'Panifi' dire: 'DirePan 152' tel: '4255-8787'.

Proveedor initialize.

1 to: 5 do:[:i | prov := Proveedor crearProveedorNombre: ('proveedor ', i printString) dir: ('Dir ', i printString) tel: ('15- 5555 -000', i printString) tip: ('tipo ', i printString).
panf agregarProveedor:  prov.].

"Imprimir lista Proveedores"
Transcript show: 'imprimir lista Proveedores';cr.
panf verListaProveedores do: [:p | Transcript show:( p imprimir ); cr].
Transcript cr.

"Eliminar provedor existente"


Producto initialize.

1 to: 5 do:[:i | prod := Producto crearProductoNombre: ('producto ', i printString) tip: ('tipo ', i printString) prec: (1000.1 * i).
panf agregarProducto: prod.].

"Imprimir lista Productos"
Transcript show: 'Imprimir lista Productos';cr.
panf verListaProductos do: [:p | Transcript show:( p imprimir ) ; cr].
Transcript cr.

"Eliminar producto existente"
Transcript show: 'Eliminiar Producto existente (Producto nro 3)';cr.
pro :=panf traerProducto: 3.
panf eliminarProducto: pro.
"Imprimir lista Productos"
Transcript show: 'Imprimir lista Productos';cr.
panf verListaProductos do: [:p | Transcript show:( p imprimir ) ; cr].
Transcript cr.

"Eliminar producto no existente Vacio"
Transcript show: 'Eliminiar Producto NO existente Vacio ( nil )';cr.
pro2 :=panf traerProducto: 3.
panf eliminarProducto: pro2.
"Imprimir lista Productos"
Transcript show: 'Imprimir lista Productos';cr.
panf verListaProductos do: [:p | Transcript show:( p imprimir ) ; cr].
Transcript cr.


"Eliminar producto NO existente"
Transcript show: 'Eliminiar Producto NO existente (Producto nro 3)';cr.
panf eliminarProducto: pro.
"Imprimir lista Productos"
Transcript show: 'Imprimir lista Productos';cr.
panf verListaProductos do: [:p | Transcript show:( p imprimir ) ; cr].
Transcript cr.