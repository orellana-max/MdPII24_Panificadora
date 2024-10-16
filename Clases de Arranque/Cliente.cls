"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Cliente
	instanceVariableNames: 'nroCliente nombre direccion telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Cliente guid: (GUID fromString: '{B9DC76AF-91B0-47BD-9DDD-07AC331B74FC}')!
Cliente comment: ''!
!Cliente categoriesForClass!Kernel-Objects! !
!Cliente methodsFor!

iniClienteNro: unNum nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Cliente"

	nroCliente := unNum.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel!

modDireccion:unaDireccion
"Modifica la direccion del cliente"
direccion:=unaDireccion.!

modNombre:unNombre
"Modifica el nombre del cliente"
nombre:=unNombre.!

modNroCliente:unNumero
"Modifica el numero de Cliente del cliente"
nroCliente:= unNumero.!

modTelefono:unTelefono
"Modifica el telefono del cliente"
telefono =unTelefono .!

verDireccion
"Retorna la direccion del cliente"
^direccion.!

verNombre
"Retorna el nombre del cliente"
^nombre.!

verNroCliente
"Retorna el numero del cliente"
^nroCliente.!

verTelefono
"Retorna el telefono del cliente"
^telefono.! !
!Cliente categoriesFor: #iniClienteNro:nom:dire:tel:!public! !
!Cliente categoriesFor: #modDireccion:!public! !
!Cliente categoriesFor: #modNombre:!public! !
!Cliente categoriesFor: #modNroCliente:!public! !
!Cliente categoriesFor: #modTelefono:!public! !
!Cliente categoriesFor: #verDireccion!public! !
!Cliente categoriesFor: #verNombre!public! !
!Cliente categoriesFor: #verNroCliente!public! !
!Cliente categoriesFor: #verTelefono!public! !

!Cliente class methodsFor!

crearClienteNro:unNum nom:unNom dire:unaDire tel:unTel
"Retorna una instancia de Cliente inicializada"
^(self new) iniClienteNro: unNum nom: unNom dire: unaDire tel: unTel.! !
!Cliente class categoriesFor: #crearClienteNro:nom:dire:tel:!public! !

