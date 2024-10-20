| listProv listProd prov |

Proveedor initialize.
listProv := OrderedCollection new.
1 to: 5 do:[:i | prov := Proveedor crearProveedorNombre: ('proveedor ', i printString) dir: ('Dir ', i printString) tel: ('15- 5555 -000', i printString) tip: ('tipo ', i printString).
listProv add: prov.].

"Imprimir lista Proveedores"
Transcript show: 'imprimir lista Proveedores';cr.
listProv do: [:p | Transcript show:( p imprimir ); cr].
Transcript cr.

"Eliminar provedor existente"



Producto initialize.
listProd := OrderedCollection new.
1 to: 5 do:[:i | prov := Producto crearProductoNombre: ('producto ', i printString) tip: ('tipo ', i printString) prec: (1000.1 * i).
listProd add: prov.].

"Imprimir lista Productos"
Transcript show: 'Imprimir lista Productos';cr.
listProd do: [:p | Transcript show:( p imprimir ) ; cr].
Transcript cr.
