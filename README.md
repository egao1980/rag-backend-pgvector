# rag-backend-pgvector

[`pgvector`](https://github.com/pgvector/pgvector) ANN store for [`rag-protocol`](https://github.com/egao1980/rag-protocol). Cosine distance (`<=>`) in Postgres. Score is **similarity** (`1 - distance`) so higher-is-better matches the memory / SQL stores.

Text wire only: `'[1,0]'::vector`. No cl-postgres vector OID codec.

```lisp
(asdf:load-system "sql-backend-postgres")
(asdf:load-system "rag-backend-pgvector")

(let ((store (rag-backend-pgvector:make-pgvector-store
              :host "localhost" :database-name "postgres"
              :username "postgres" :password "postgres"
              :dimension 2)))
  (stack-rag:upsert store
                    (stack-rag:make-rag-chunk
                     :id "a" :text "alpha" :embedding #(1.0 0.0)))
  (stack-rag:query-store store #(1.0 0.0) :top-k 5)
  (rag-backend-pgvector:close-pgvector-store store))
```

Needs `CREATE EXTENSION vector` (superuser once). Default table `rag_chunks` (`[A-Za-z_][A-Za-z0-9_]*`). HNSW (`vector_cosine_ops`) once the column is `vector(N)` — pass `:dimension` or let the first upsert pin it (`:index nil` skips). `:filter` is still a Lisp function — SQL `LIMIT` only when filter is nil.

Not here: hybrid BM25, IVFFlat knobs, binary vector codecs.

Part of [cl-stack](https://github.com/egao1980/cl-stack). Cookbook: [rag.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/rag.md).

## Tests

```bash
# encode / table-name only (live suite skips)
ros -e '(asdf:test-system "rag-backend-pgvector")' -q

# live — same env as sql-protocol; image must be pgvector/pgvector, not stock postgres
SQL_POSTGRES=1 ros -e '(asdf:test-system "rag-backend-pgvector")' -q
```

## License

MIT — see [LICENSE](LICENSE).
