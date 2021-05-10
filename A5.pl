class(a).
class(b).
class(c).

interface(d).
interface(e).
interface(f).

extends(a,b).
extends(b,c).

extends(d,e).
extends(e,f).

implements(c,d).

subclass(X,Y):- extends(X,Y).
subclass(X,Y):- extends(X,Z), subclass(Z,Y).

superclass(X,Y):- subclass(Y,X).

superinterface(Y, X):- interface(Y), class(X), (implements(X, Z), (Z=Y;subclass(Z,Y)));(superclass(W,X), implements(W,Z), (Z=Y;subclass(Z,Y))).