Via
===

Trivial proxy objects for common lisp.

We dont have interfaces and some types are not extendable so couldnt implement an interface anyway. We can use proxy objects instead.

A proxy object is an struct that references an object and the functions needed to forfill the proxies' specification.

You can pass around the proxy object and call the methods which will call the target.

When provided, `via` will write type declaration in the functions to help your compiler generate fast code.

Proxies. Not the prettiest solution to a problem, but they'll do.
