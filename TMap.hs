module TMap where
type TMap k v = k->v

emptyTMap :: Eq k => v -> TMap k v
emptyTMap def _ = def

updateTMap :: Eq k => TMap k v -> k -> v -> TMap k v
updateTMap m k v i = if i==k then v else m i


type State = TMap String Integer

emptyState:: State
emptyState = emptyTMap 0
