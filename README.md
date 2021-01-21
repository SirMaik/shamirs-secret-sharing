# **shamir-secret-sharing**

## Descripción general

El esquema de secreto compartido de Shamir hace posible que un sólo dato pueda ser ocultado de manera que, a partir de él, se generan n diferentes datos y que con, al menos t ≤ n cualesquiera de ellos sea posible recuperar el dato original.

## Instalación

En el caso de que no esté instalado es necesario instalar **stack**:

`$ wget -qO- https://get.haskellstack.org/ | sh`

Se pueden consultar otros modos de instalación en [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Es necesario tener **stack** actualizado:

`$ stack upgrade`


Para construir el paquete es necesario pararse dentro del directorio *cobertura-nubosa* y ejecutar:
```
$ stack init
$ stack build
```
Y por último hay que copiar los binarios a una ruta conocida por el sistema:

`$ stack install`

*Nota: Al ejecutar este último comando se mostrará en pantalla la ruta en la que se guardó el binario por si se le quiere eliminar posteriormente.*

## Modo de uso 

Ahora es posible ejecutar el programa haciendo uso del comando `shamir-secret-sharing-exe` junto con las opciones:

* `c n t plain pts ciph` - Para encriptar:
  * `n` - Número de puntos que se van a generar.
  * `t` - Número de puntos necesarios para desencriptar.
  * `plain` - Ruta del archivo que se quiere encriptar.
  * `pts` - Ruta del archivo donde se van a guardar los n puntos.
  * `ciph` - Ruta donde se va a guardar el archivo cifrado.
* `d pts ciph plain` - Para desencriptar:
  * `pts` - Ruta del archivo donde se encuentran al menos t puntos.
  * `ciph` - Ruta donde se encuentra el archivo encriptado.
  * `plain` - Ruta donde se quiere guardar el archivo desencriptado.

### Ejemplos



## Construído con
* [Stack](https://docs.haskellstack.org) - Programa multi-plataforma para desarrollar projectos en Haskell. 
* [Cryptonite](https://github.com/haskell-crypto/cryptonite) - Biblioteca de primitivas criptográficas.

