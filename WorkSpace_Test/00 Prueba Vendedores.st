|v listVen sigue leg nom dir tel|

listVen := OrderedCollection new.

"Creamos dos Repartidores manualmente y agregamos a la Lista de Repartidores"
sigue:= true.
[sigue] whileTrue: [
leg:= Prompter prompt: 'ingrese legajo: '.
leg:= leg asNumber.
nom:= Prompter prompt: 'ingrese nombre: '.
dir:= Prompter prompt: 'ingrese direccion: '.
tel:= Prompter prompt: 'ingrese telefono: '.
v:= Vendedor crearVendedorLegajo: leg nom: nom dire: dir tel: tel.
MessageBox warning:  'Se creo el Vendedor Nro', v verNroVendedor printString , ' nom: ',  v verNombre.
listVen add: v. 
sigue:= MessageBox confirm: '¿Desea seguir creando Vendedores?'].

"Creamos varios Vendedores y agregamos a la Lista de Vendedores"
v:= Vendedor crearVendedorLegajo: 301 nom: 'Sandra' dire: 'calle 301' tel: '300-301'.
listVen add: v.
v:= Vendedor crearVendedorLegajo: 302 nom: 'Santiago' dire: 'calle 302' tel: '300-302'.
listVen add: v.
v:= Vendedor crearVendedorLegajo: 303 nom: 'Gisela' dire: 'calle 303' tel: '300-303'.
listVen add: v.
v:= Vendedor crearVendedorLegajo: 304 nom: 'Martin' dire: 'calle 304' tel: '300-304'.
listVen add: v.
v:= Vendedor crearVendedorLegajo: 305 nom: 'Marcelo' dire: 'calle 305' tel: '300-305'.
listVen add: v.
v:= Vendedor crearVendedorLegajo: 306 nom: 'Silvio' dire: 'calle 306' tel: '300-306'.
listVen add: v.

"Listar los Vendedores"
Transcript nextPutAll: 'Lista de Vendedores '.
Transcript cr.
listVen do:[:ve | ve printOn: Transcript cr].
