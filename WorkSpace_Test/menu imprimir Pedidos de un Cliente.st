| salir op pan volver sigue c p r v nom tel dir dic cant leg pue pro tip precio precio2 idx impMenu listPP|

Transcript clear.

pan:= Panificadora crearPanificadoraNom: 'pa' dire: 'Av.7 123'  tel: '4257-8897'.
App load: pan.
listPP := App crearListaProductosPedidos: pan.
App cargarPedidosAlaPanificadora: pan.



pan verListaClientes do:[:c | Transcript show: c imprimir; cr ].
pan verListaProductos do:[:c | Transcript show: c imprimir; cr ].
pan verListaRepartidores do:[:c | Transcript show: c imprimir; cr ].
pan verListaVendedores do:[:c | Transcript show: c imprimir; cr ].
pan verListaPanaderos do:[:c | Transcript show: c imprimir; cr ].
listPP do:[:pp | Transcript show: pp imprimir; cr ].
pan verListaPedidos do:[:p | Transcript show: p imprimir;cr ].
pan verListaPedidos do:[:p | p imprimirTrpt ].


App imprimirPedidosCliente: pan.

