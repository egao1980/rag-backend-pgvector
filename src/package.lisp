(defpackage #:rag-backend-pgvector
  (:use #:cl)
  (:export #:pgvector-store
           #:make-pgvector-store
           #:use-pgvector-store
           #:close-pgvector-store
           #:ensure-pgvector-schema
           #:pgvector-store-connection
           #:pgvector-store-table
           #:pgvector-store-dimension
           #:encode-pgvector
           #:decode-pgvector))

(in-package #:rag-backend-pgvector)
