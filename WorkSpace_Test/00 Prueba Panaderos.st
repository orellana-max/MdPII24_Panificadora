|p listPanaderos leg nom dir tel  dic op pue |

listPanaderos := OrderedCollection new.

"Dictionary para puestos de Panadero"
dic := Dictionary new at:'1' put:'Pastelero'; at:'2' put:'Panadero'; at:'3' put:'Facturero'; at:'4' put:'Maestro'; yourself.

"Creamos un Panadero manualmente"
leg:= Prompter prompt: 'ingrese legajo: '.
leg:= leg asNumber.
nom:= Prompter prompt: 'ingrese nombre: '.
dir:= Prompter prompt: 'ingrese direccion: '.
tel:= Prompter prompt: 'ingrese telefono: '.
pue := '0'.
[pue = '0'] whileTrue: [
	op:= Prompter prompt: 'ingrese nro de puesto: 1- Pastelero, 2- Panadero, 3- Facturero, 4- Maestro'. 
	pue:= dic at: op ifAbsent: ['0'].
	].
p:= Panadero crearPanaderoLegajo: leg nom: nom dire: dir tel: tel puesto: pue.
MessageBox warning:  'Se creo con exito el Panadero ', p verPuesto , ' leg:', p verLegajo printString , 'nom: ',  p verNombre.
"Agregamos el Panadero a la lista"
listPanaderos add: p.

"imprimimos el panadero creado en System Transcript"
"p printOn: Transcript."

p:= Panadero crearPanaderoLegajo: 2 nom: 'Maximiliano' dire: 'calle 102' tel: '123-564' puesto: 'Pastelero'.
listPanaderos add: p.
p:= Panadero crearPanaderoLegajo: 3 nom: 'Daniel' dire: 'calle 103' tel: '123-564' puesto: 'Panadero'.
listPanaderos add: p.
p:= Panadero crearPanaderoLegajo: 4 nom: 'Juan' dire: 'calle 104' tel: '123-564' puesto: 'Maestro'.
listPanaderos add: p.
p:= Panadero crearPanaderoLegajo: 5 nom: 'Gonzalo' dire: 'calle 105' tel: '123-564' puesto: 'Facturero'.
listPanaderos add: p.

"Listar los panaderos"
Transcript nextPutAll: 'Lista de Panaderos: '.
Transcript cr.
listPanaderos do:[:pa | pa printOn: Transcript cr].
