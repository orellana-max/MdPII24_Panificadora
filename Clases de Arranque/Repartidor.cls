"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Repartidor
	instanceVariableNames: 'nroRepartidor viajesRealizados libre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'nroRepartidorStatic'!
Repartidor guid: (GUID fromString: '{E520E4A8-E744-4822-9CF7-93BF17A6E60E}')!
Repartidor comment: ''!
!Repartidor categoriesForClass!Kernel-Objects! !
!Repartidor methodsFor!

iniRepartidorLegajo: unLegajo nom: unNombre dire: unaDire tel: unTel num: unNroRepartidor
	"Inicializa una instancia de Repartidor"
	

	legajo := unLegajo.
	nombre := unNombre.
	direccion := unaDire.
	telefono := unTel.
	sueldo := 0.
	nroRepartidor := unNroRepartidor.
	libre := true.!

verNroRepartidor
	^nroRepartidor.! !
!Repartidor categoriesFor: #iniRepartidorLegajo:nom:dire:tel:num:!public! !
!Repartidor categoriesFor: #verNroRepartidor!public! !

!Repartidor class methodsFor!

crearRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel
	"Retorna una instancia de Repartidor inicializada"
	nroRepartidorStatic = nroRepartidorStatic + 1.

	^(self new) iniRepartidorLegajo: unLegajo nom: unNom dire: unaDire tel: unTel num: nroRepartidorStatic.! !
!Repartidor class categoriesFor: #crearRepartidorLegajo:nom:dire:tel:!public! !

