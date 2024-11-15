
|op pan|

pan := Panificadora crearPanificadoraNom: 'Santa Fe' dire: 'avenida san martin 456' tel: '45678911'.

App load:pan.

op :=''.
Transcript clear.
[op ~= '0']  whileTrue: [
	Transcript show: 'Funciones: ';cr.
	Transcript show: 'Elige opción: ';cr.
	Transcript show: 'Opción 1: Imprimir pedidos de un cliente';cr.
	Transcript show: 'Opción 2: Imprimir pedidos mayores o iguales a un costo';cr.
	Transcript show: 'Opción 3: Imprimir todos los pedidos ordenados por costo de mayor a menor';cr.
	Transcript show: 'Opción 0: Volver al menú principal';cr.
	op:= Prompter prompt: 'Funciones. Ingrese opción: '.
	Transcript show: 'ingreso: ', op printString; cr.
	(op = nil)ifTrue: [ op :='-1'].
	op:= op asNumber.

	(op>=0 and: [op<=3]) ifTrue:[
			(op = 1) ifTrue:[ Transcript show: 'seleccionó 1 Imprimir pedidos de un cliente.';cr. 
					App imprimirPedidosCliente: pan.
					op:=''. ].		
			(op = 2) ifTrue:[ Transcript show: 'seleccionó 2 .Imprimir pedidos iguales o mayores a un costo ';cr. 
					App imprimirPedidosMayorIgualA: pan.
					op:=''. ].		
			(op = 3) ifTrue:[ Transcript show: 'seleccionó 3 Imprimir todos los pedidos ordenados por costo de mayor a menor';cr. 
					App imprimirPedidosMayorAMenor: pan.
					op:=''. ].		
			(op = 0) ifTrue:[ Transcript show: 'seleccionó 0. Volviendo al menú principal';cr.
					op:='0'.] ]
		ifFalse: [ Transcript show: 'no seleccionó una opcion correcta';cr.
			op:=''. ].
	Transcript cr.
]