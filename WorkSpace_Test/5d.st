"Pedidos ordenados de mayor a menor costo"

|panificadora pedidosOrdenCosto|

"creacion de una Panificadora para simular trabajar sobre ella"
panificadora := Panificadora crearPanificadoraNom: 'Santa Fe' dire: 'Avenida San Martin 567, Quilmes' tel: '1143542490'.

"obtener una coleccion con los pedidos que tiene la panificadora pero ordenados por costo de mayor a menor"
pedidosOrdenCosto := (panificadora verListaPedidos) asSortedCollection: [ :x :y | x verTotal > y verTotal].

"imprimir los datos ordenados"
Transcript show: 'Lista de pedidos ordenados por costo de mayor a menor'; cr.
pedidosOrdenCosto do:[
					: p |
					Transcript show:
									'Nro cliente: ', p verNroCliente,
									'| Fecha: ', (p verFechaEntrega) printString,	
									'| Costo: ', (p verTotal) printString; cr		
					].