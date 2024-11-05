|r listRep sigue leg nom dir tel msj|

listRep := OrderedCollection new.

"Creamos dos Repartidores manualmente y agregamos a la Lista de Repartidores"
sigue:= true.
[sigue] whileTrue: [
leg:= Prompter prompt: 'ingrese legajo: '.
leg asNumber.
nom:= Prompter prompt: 'ingrese nombre: '.
dir:= Prompter prompt: 'ingrese direccion: '.
tel:= Prompter prompt: 'ingrese telefono: '.
r:= Repartidor crearRepartidorLegajo: leg nom: nom dire: dir tel: tel.
listRep add: r. 
msj:=  'Se creo con exito el Repartidor Nro', r verNroRepartidor printString , 'nom: ',  r verNombre.
sigue:= MessageBox confirm: msj, '  ¿Desea seguir creando Repartidores?'.
].

"Creamos varios Repartidores y agregamos a la Lista de Reparatidores"
r:= Repartidor crearRepartidorLegajo: 201 nom: 'Federico' dire: 'calle 201' tel: '200-201'.
listRep add: r.
r:= Repartidor crearRepartidorLegajo: 202 nom: 'Jaqueline' dire: 'calle 202' tel: '200-202'.
listRep add: r.
r:= Repartidor crearRepartidorLegajo: 203 nom: 'Benjamin' dire: 'calle 203' tel: '200-203'.
listRep add: r.
r:= Repartidor crearRepartidorLegajo: 204 nom: 'Tatiana' dire: 'calle 204' tel: '200-204'.
listRep add: r.
r:= Repartidor crearRepartidorLegajo: 205 nom: 'Ana' dire: 'calle 205' tel: '200-205'.
listRep add: r.
r:= Repartidor crearRepartidorLegajo: 206 nom: 'Joel' dire: 'calle 206' tel: '200-206'.
listRep add: r.



"Listar los Repartidores"
Transcript show:  'Lista de repartidores '.

listRep do:[:re | re printOn: Transcript cr].
Transcript cr.