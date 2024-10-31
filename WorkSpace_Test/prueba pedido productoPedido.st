|ped prod prodPed list|

list:= OrderedCollection new.
list add: ProductoPedido crearProductoPedido: 1 pedido: 0 cant: 5.
list add: ProductoPedido crearProductoPedido: 2 pedido: 0 cant: 4.
list add: ProductoPedido crearProductoPedido: 3 pedido: 0 cant: 2.


ped:= Pedido crearPedido: unNroClien listProdPed: lista.