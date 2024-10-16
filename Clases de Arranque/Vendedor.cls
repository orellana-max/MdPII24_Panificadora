"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Vendedor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Vendedor guid: (GUID fromString: '{4355CCF0-7EEA-498A-9E87-CCEF4959DAED}')!
Vendedor comment: ''!
!Vendedor categoriesForClass!Kernel-Objects! !
!Vendedor methodsFor!

iniVendedorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel
	"Inicializa una instancia de Vendedor"

	legajo := unLegajo.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel.
	sueldo := 0! !
!Vendedor categoriesFor: #iniVendedorLegajo:nom:dire:tel:!public! !

!Vendedor class methodsFor!

crearVendedorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Vendedor inicializada"

	^self new
		iniVendedorLegajo: unLegajo
		nom: unNom
		dire: unaDire
		tel: unTel! !
!Vendedor class categoriesFor: #crearVendedorLegajo:nom:dire:tel:!public! !

