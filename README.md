# **shamir-secret-sharing**

## Descripción general

El [esquema de secreto compartido de Shamir](https://es.wikipedia.org/wiki/Esquema_de_Shamir) hace posible que un sólo dato pueda ser ocultado de manera que, a partir de él, se generan n diferentes datos y que con, al menos t ≤ n cualesquiera de ellos sea posible recuperar el dato original.

## Instalación

En el caso de que no esté instalado es necesario instalar **stack**:

`$ wget -qO- https://get.haskellstack.org/ | sh`

Se pueden consultar otros modos de instalación en [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Es necesario tener **stack** actualizado:

`$ stack upgrade`


Para construir el paquete es necesario pararse dentro del directorio *shamirs-secret-sharing* y ejecutar:
```
$ stack init
$ stack build
```
Y por último hay que copiar los binarios a una ruta conocida por el sistema:

`$ stack install`

*Nota: Al ejecutar este último comando se mostrará en pantalla la ruta en la que se guardó el binario por si se le quiere eliminar posteriormente.*

## Modo de uso 

Ahora es posible ejecutar el programa haciendo uso del comando `shamir-secret-sharing-exe` junto con las opciones:

* `c n t plano pts cif` - Para encriptar:
  * `n` - Número de puntos que se van a generar.
  * `t` - Número de puntos necesarios para desencriptar.
  * `plano` - Ruta del archivo que se quiere encriptar.
  * `pts` - Ruta del archivo donde se van a guardar los n puntos.
  * `cif` - Ruta donde se va a guardar el archivo cifrado.
  
  En el caso de que los argumentos sean válidos será necesario ingresar una contraseña de máximo 30 caracteres. 
  
* `d pts cif plano` - Para desencriptar:
  * `pts` - Ruta del archivo donde se encuentran al menos t puntos.
  * `cif` - Ruta donde se encuentra el archivo encriptado.
  * `plano` - Ruta donde se quiere guardar el archivo desencriptado.

## Documentación

En el caso de que se quiera generar la documentación es necesario ejecutar:

`$ stack haddock`

En la última línea que se despliegue en la terminal se mostrará la ruta del `index.html`, el cual puede ser abierto con cualquier explorador web para ver la documentación. 


## Construído con
* [Stack](https://docs.haskellstack.org) - Programa multi-plataforma para desarrollar projectos en Haskell. 
* [Cryptonite](https://github.com/haskell-crypto/cryptonite) - Biblioteca de primitivas criptográficas.

