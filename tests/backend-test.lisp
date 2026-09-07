(in-package #:rag-backend-pgvector/tests)

(defun %vec (&rest xs)
  (map 'vector (lambda (x) (float x 1f0)) xs))

(defun %chunk (id text emb &key (document-id "d"))
  (rag-protocol:make-rag-chunk :id id :document-id document-id
                               :text text :embedding emb))

(defun %live-enabled-p ()
  (equal "1" (uiop:getenv "SQL_POSTGRES")))

(defun %connect-keys ()
  (list :host (or (uiop:getenv "SQL_POSTGRES_HOST") "localhost")
        :port (parse-integer (or (uiop:getenv "SQL_POSTGRES_PORT") "5432"))
        :database-name (or (uiop:getenv "SQL_POSTGRES_DB") "postgres")
        :username (or (uiop:getenv "SQL_POSTGRES_USER") "postgres")
        :password (or (uiop:getenv "SQL_POSTGRES_PASSWORD") "postgres")))

(defun %fresh-table ()
  (format nil "rag_pgv_~d" (random (expt 10 9))))

(defun %drop-table (store)
  (ignore-errors
    (sql-protocol:execute
     (rag-backend-pgvector:pgvector-store-connection store)
     (format nil "DROP TABLE IF EXISTS ~a"
             (rag-backend-pgvector:pgvector-store-table store)))))

(defun %ensure-postgres-backend ()
  (unless (find-package :sql-backend-postgres)
    (asdf:load-system "sql-backend-postgres")))

(defmacro with-live-store ((store &rest args) &body body)
  `(cond
     ((not (%live-enabled-p))
      (skip "set SQL_POSTGRES=1 to enable live pgvector tests"))
     (t
      (%ensure-postgres-backend)
      (let ((,store (apply #'rag-backend-pgvector:make-pgvector-store
                           :table (%fresh-table)
                           (append (list ,@args) (%connect-keys)))))
        (unwind-protect (progn ,@body)
          (when ,store
            (%drop-table ,store)
            (rag-backend-pgvector:close-pgvector-store ,store)))))))

(deftest encode-roundtrip
  (ok (equalp (%vec 1 0)
              (rag-backend-pgvector:decode-pgvector
               (rag-backend-pgvector:encode-pgvector (%vec 1 0)))))
  (ok (equal "[1.0,0.0]" (rag-backend-pgvector:encode-pgvector (%vec 1 0))))
  (ok (equalp (%vec 1 0) (rag-backend-pgvector:decode-pgvector "[1,0]")))
  (ok (equalp (%vec 1 0.5) (rag-backend-pgvector:decode-pgvector "[1.0, 0.5]")))
  (ok (equalp (%vec 1 0) (rag-backend-pgvector:decode-pgvector (%vec 1 0))))
  (ok (equalp (%vec) (rag-backend-pgvector:decode-pgvector "[]")))
  (ok (signals (rag-backend-pgvector:decode-pgvector "1,2")
               'rag-protocol:rag-error)))

(deftest invalid-table-name
  (ok (signals (rag-backend-pgvector:make-pgvector-store
                :table "chunks;drop" :ensure-schema nil)
               'rag-protocol:rag-error)))

(deftest use-pgvector-binds
  (if (not (%live-enabled-p))
      (skip "set SQL_POSTGRES=1 to enable live pgvector tests")
      (progn
        (%ensure-postgres-backend)
        (let ((rag-protocol:*rag-store* nil)
              (store nil))
          (unwind-protect
               (progn
                 (setf store (apply #'rag-backend-pgvector:use-pgvector-store
                                    :table (%fresh-table)
                                    (%connect-keys)))
                 (ok (typep rag-protocol:*rag-store*
                            'rag-backend-pgvector:pgvector-store)))
            (when store
              (%drop-table store)
              (rag-backend-pgvector:close-pgvector-store store)
              (setf rag-protocol:*rag-store* nil)))))))

(deftest upsert-query-ranks
  (with-live-store (store)
    (rag-protocol:upsert store
                         (list (%chunk "a" "alpha" (%vec 1 0))
                               (%chunk "b" "beta" (%vec 0 1))))
    (let ((hits (rag-protocol:query-store store (%vec 1 0) :top-k 2)))
      (ok (= 2 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits)))))
      (ok (> (rag-protocol:rag-hit-score (first hits))
             (rag-protocol:rag-hit-score (second hits))))
      (ok (> (rag-protocol:rag-hit-score (first hits)) 0.9)))))

(deftest persist-across-reconnect
  (if (not (%live-enabled-p))
      (skip "set SQL_POSTGRES=1 to enable live pgvector tests")
      (progn
        (%ensure-postgres-backend)
        (let* ((table (%fresh-table))
               (keys (append (list :table table) (%connect-keys)))
               (store (apply #'rag-backend-pgvector:make-pgvector-store keys)))
          (unwind-protect
               (progn
                 (rag-protocol:upsert store (%chunk "a" "keep" (%vec 1 0)))
                 (rag-backend-pgvector:close-pgvector-store store)
                 (setf store (apply #'rag-backend-pgvector:make-pgvector-store keys))
                 (let ((hits (rag-protocol:query-store store (%vec 1 0) :top-k 1)))
                   (ok (equal "keep" (rag-protocol:rag-chunk-text
                                      (rag-protocol:rag-hit-chunk (first hits)))))
                   (ok (= 2 (rag-backend-pgvector:pgvector-store-dimension store)))))
            (when store
              (%drop-table store)
              (rag-backend-pgvector:close-pgvector-store store)))))))

(deftest replace-same-id
  (with-live-store (store)
    (rag-protocol:upsert store (%chunk "a" "old" (%vec 1 0)))
    (rag-protocol:upsert store (%chunk "a" "new" (%vec 0 1)))
    (let ((hits (rag-protocol:query-store store (%vec 0 1) :top-k 1)))
      (ok (equal "new" (rag-protocol:rag-chunk-text
                        (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest delete-and-missing
  (with-live-store (store)
    (rag-protocol:upsert store (%chunk "a" "x" (%vec 1 0)))
    (ok (equal '("a") (rag-protocol:delete-ids store '("a"))))
    (ok (signals (rag-protocol:delete-ids store "a")
                 'rag-protocol:rag-not-found))))

(deftest dimension-mismatch
  (with-live-store (store)
    (rag-protocol:upsert store (%chunk "a" "x" (%vec 1 0)))
    (ok (signals (rag-protocol:upsert store (%chunk "b" "y" (%vec 1 0 0)))
                 'rag-protocol:rag-dimension-mismatch))
    (ok (signals (rag-protocol:query-store store (%vec 1 0 0) :top-k 1)
                 'rag-protocol:rag-dimension-mismatch))))

(deftest query-filter
  (with-live-store (store)
    (rag-protocol:upsert store
                         (list (%chunk "a" "keep" (%vec 1 0))
                               (%chunk "b" "drop" (%vec 1 0))))
    (let ((hits (rag-protocol:query-store
                 store (%vec 1 0) :top-k 5
                 :filter (lambda (ch)
                           (equal "keep" (rag-protocol:rag-chunk-text ch))))))
      (ok (= 1 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest create-with-dimension
  (with-live-store (store :dimension 2)
    (ok (= 2 (rag-backend-pgvector:pgvector-store-dimension store)))
    (rag-protocol:upsert store (%chunk "a" "x" (%vec 1 0)))
    (ok (signals (rag-protocol:upsert store (%chunk "b" "y" (%vec 1 0 0)))
                 'rag-protocol:rag-dimension-mismatch))))
