# Things we need to put in the report:

We made some assumptions to simplify our WLP such that,
there is only an assume at beginning of program, and assert at end of program.

such that:

```
var x {
  var x {
  }
}
```

~

```
var x {
  var x' {
  }
}
```

~

```
var x, x' {
}
```

such that the WLP is

 forall x, x' : (wlp body)


This would not be true in general if we have:

```
var x {
  var x' {
    assume x' > 0
  }
}
```
As we cannot move a `forall` outside an implication safely


This means for program call, we cannot use specificaiton,
but we have to use unrolling.




## Arrays


We need to check if    "x+1 == 1+x" and than we can replace both
  a[x+1] and a[1+x]  with  `a_y`  where `a_y` is some variable


What about the case  "x+1 == y+1" :  this should work as well, as during
smallChecking we know the value of "x" and "y".

a[x+1] -> a_x  and a[y+1] -> a_y   . Now we can check with what we already have!


