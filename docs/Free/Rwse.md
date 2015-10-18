## Module Free.Rwse

#### `Rwse`

``` purescript
type Rwse reader writer state error = Free (RwseF reader writer state error)
```

#### `RwseF`

``` purescript
data RwseF reader writer state error a
  = Ask (reader -> a)
  | Tell writer a
  | Get (state -> a)
  | Put state a
  | Throw error a
```

#### `ask`

``` purescript
ask :: forall reader writer state error. Rwse reader writer state error reader
```

#### `tell`

``` purescript
tell :: forall reader writer state error. writer -> Rwse reader writer state error Unit
```

#### `get`

``` purescript
get :: forall reader writer state error. Rwse reader writer state error state
```

#### `put`

``` purescript
put :: forall reader writer state error. state -> Rwse reader writer state error Unit
```

#### `modify`

``` purescript
modify :: forall reader writer state error. (state -> state) -> Rwse reader writer state error Unit
```

#### `throw`

``` purescript
throw :: forall reader writer state error. error -> Rwse reader writer state error Unit
```


