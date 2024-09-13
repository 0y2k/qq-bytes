# qq-bytes

`qq-bytes` offers QuasiQuoter for byte sequence and FromBytes typeclass.

## usage

```haskell
>>> [bin|00100000|]
" "
>>> [hex|48 65 6c 6c 6f|]
"Hello"
```

These values are typed as `FromBytes a => a`.
