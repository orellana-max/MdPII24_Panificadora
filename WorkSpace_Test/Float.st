|precio cant precio2 idx|

precio :=''.
	"comprobamos que se ingreso un numero"
[precio isNumber] whileFalse: [
	precio := Prompter prompt: 'Ingrese Precio del Producto  '.
	cant := precio occurrencesOf: $.. "ocurrencias de puntos"

	((precio ='') or: [cant > 1]) "si ingreso varios puntos o vacio"
			ifTrue: [MessageBox warning: 'no ingreso un numero'] 
			ifFalse: [
			(cant = 0)  "si no tiene puntos"
					ifTrue: [ "Bloque si no contiene letras ni punto"
					((precio detect: [ :pre | pre isDigit not] ifNone:['vacio']) = 'vacio' ) 
					ifTrue: [precio := precio asNumber asFloat.] 
					ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
					].
			(cant = 1) "si tiene un punto"
					ifTrue:[
					idx := precio indexOf: $..
					precio2 := (precio copyFrom: 1 to: idx -1), (precio copyFrom: idx +1).
					((precio2 detect: [ :pre2 | pre2 isDigit not] ifNone:['vacio']) = 'vacio' ) 
						ifTrue: [ "Bloque si no contiene letras es un numero"
						precio := precio asNumber asFloat.
					] "fin Bloque si no contiene letras es un numero"
					ifFalse:[MessageBox warning: 'no ingreso un numero']."--> Bloque si contiene letras"
					].
			].
].
