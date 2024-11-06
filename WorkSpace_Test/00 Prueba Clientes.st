|c listCl nom dir tel|

listCl := OrderedCollection new.

"Creamos dos Clientes manualmente y agregamos a la Lista de Clientes"
1 to: 2 do: [:i | 
nom:= Prompter prompt: 'ingrese nombre: '.
dir:= Prompter prompt: 'ingrese direccion: '.
tel:= Prompter prompt: 'ingrese telefono: '.
c:= Cliente crearClienteNom: nom dire: dir tel: tel.
MessageBox warning:  'Se creo el Cliente Nro', c verNroCliente printString , 'nom: ',  c verNombre.
listCl add: c. ].

"Creamos varios Clientes y agregamos a la Lista de Clientes"
c:= Cliente crearClienteNom: 'Carlos' dire: 'calle 401'  tel: '445-121'.
listCl add: c.
c:= Cliente crearClienteNom: 'Raul' dire: 'calle 402'  tel: '445-122'.
listCl add: c.
c:= Cliente crearClienteNom: 'Clara' dire: 'calle 403'  tel: '445-123'.
listCl add: c.
c:= Cliente crearClienteNom: 'Manuel' dire: 'calle 404'  tel: '445-124'.
listCl add: c.
c:= Cliente crearClienteNom: 'Rita' dire: 'calle 405'  tel: '445-125'.
listCl add: c.
c:= Cliente crearClienteNom: 'Jose' dire: 'calle 405'  tel: '445-126'.
listCl add: c.
c:= Cliente crearClienteNom: 'Juan' dire: 'calle 405'  tel: '445-127'.
listCl add: c.

"Listar los Clientes"
Transcript nextPutAll: 'Lista de Clientes '.
Transcript cr.
listCl do:[:cl | cl printOn: Transcript cr].




