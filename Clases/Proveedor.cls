"Filed out from Dolphin Smalltalk"!

Object subclass: #Proveedor
	instanceVariableNames: 'nroProveedor direccion telefono tipoProductos nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'idProveedor'!

Proveedor guid: (GUID fromString: '{4d142482-4e17-4a30-bff2-851d3c202d10}')!

Proveedor comment: ''!

!Proveedor categoriesForClass!Kernel-Objects! !

!Proveedor methodsFor!

imprimir
	"Retorna los datos de la instancia de Proveedor"

	| cad |
	cad := 'Proveedor Nro: ', nroProveedor printString, ' | Nom: ', nombre, ' | Dir: ', direccion, ' | Tel: ', telefono, ' | Tipo: ', tipoProductos.
	^cad.!

iniProveedorNombre: unNom dir: unaDir tel: unTel tip: unTipo
	"Inicializa una instancia de Proveedor"

	nroProveedor := Proveedor nextId.
	nombre := unNom.
	direccion := unaDir.
	telefono := unTel.
	tipoProductos := unTipo!

modDireccion: unaDire
	"modifica la direccion del proovedor"

	direccion := unaDire!

modNombre: unNom
	"modifica el nombre del proovedor"

	nombre := unNom.!

modTelefono: unTel
	"modifica el telefono del proovedor"

	telefono := unTel!

modTipoProductos: unTipo
	"modifica el tipo de Producto que proovee el proveedor"

	tipoProductos := unTipo!

verDireccion
	"retorna la direccion del proovedor"

	^direccion!

verNombre
	"retorna el nombre del proovedor"

	^nombre.!

verNroProveedor
	"retorna el numero del proveedor"

	^nroProveedor.!

verTelefono
	"retorna el telefono del proveedor"

	^telefono!

verTipoProductos
	"retorna el tipo de Producto que proovee el proveedor"

	^tipoProductos.! !

!Proveedor categoriesForMethods!
imprimir!public! !
iniProveedorNombre:dir:tel:tip:!public! !
modDireccion:!public! !
modNombre:!public! !
modTelefono:!public! !
modTipoProductos:!public! !
verDireccion!public! !
verNombre!public! !
verNroProveedor!public! !
verTelefono!public! !
verTipoProductos!public! !
!

!Proveedor class methodsFor!

crearProveedorNombre: unNom dir: unaDir tel: unTel tip: unTipo
	"retorna una instancia de Proveedor inicializada"

	^self new
		iniProveedorNombre: unNom
		dir: unaDir
		tel: unTel
		tip: unTipo!

initialize
	"inicializa la variable de Clase idProveedor"

	idProveedor := 1!

nextId
	"retorna un id unico para una nueva instancia de Proveedor"

	| id |
	id := idProveedor.
	idProveedor := idProveedor + 1.
	^id! !

!Proveedor class categoriesForMethods!
crearProveedorNombre:dir:tel:tip:!public! !
initialize!public! !
nextId!public! !
!

