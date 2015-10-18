## Module Free.Rwse.Transformer

#### `rwseN`

``` purescript
rwseN :: forall reader writer state error monad. (MonadRWS reader writer state monad, MonadError error monad) => NaturalTransformation (RwseF reader writer state error) monad
```


