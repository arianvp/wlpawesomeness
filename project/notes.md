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

