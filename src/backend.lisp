(in-package #:rag-backend-pgvector)

;;; pgvector ANN store. Wire format is text '[1,2,3]' — no vector OID codec.
;;; Distance operator is <=> (cosine). Score is 1 - distance (higher is better).

(defclass pgvector-store (rag-protocol:rag-vector-store)
  ((connection :initarg :connection :accessor pgvector-store-connection)
   (owns-connection :initarg :owns-connection :accessor pgvector-store-owns-connection
                    :initform nil)
   (table :initarg :table :accessor pgvector-store-table :initform "rag_chunks")
   (dimension :initarg :dimension :accessor pgvector-store-dimension :initform nil)
   (index-p :initarg :index-p :accessor pgvector-store-index-p :initform t)))

(defun %table-name (name)
  (let ((s (string name)))
    (unless (and (plusp (length s))
                 (let ((c (char s 0)))
                   (or (alpha-char-p c) (char= c #\_)))
                 (every (lambda (c)
                          (or (alphanumericp c) (char= c #\_)))
                        s))
      (error 'rag-protocol:rag-error
             :message (format nil "invalid table name ~s" name)))
    s))

(defun %as-list (x)
  (if (listp x) x (list x)))

(defun %as-integer (x)
  (etypecase x
    (integer x)
    (string (parse-integer x :junk-allowed t))))

(defun %as-single-float (x)
  (etypecase x
    (real (float x 1f0))
    (string
     (float (with-standard-io-syntax
              (let ((*read-eval* nil))
                (read-from-string x)))
            1f0))))

(defun encode-pgvector (vec)
  "Encode a real sequence as a pgvector text literal, e.g. \"[1.0,0.0]\"."
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for i from 0 below (length vec)
          for x = (elt vec i)
          do (when (plusp i)
               (write-char #\, out))
             (write-string (format nil "~F" (float x 1d0)) out))
    (write-char #\] out)))

(defun decode-pgvector (x)
  "Decode a pgvector text literal, vector, or list into a single-float vector."
  (cond
    ((null x)
     (make-array 0 :element-type 'single-float))
    ((stringp x)
     (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) x)))
       (unless (and (>= (length s) 2)
                    (char= (char s 0) #\[)
                    (char= (char s (1- (length s))) #\]))
         (error 'rag-protocol:rag-error
                :message (format nil "not a pgvector literal: ~s" x)))
       (let ((inner (string-trim '(#\Space) (subseq s 1 (1- (length s))))))
         (if (zerop (length inner))
             (make-array 0 :element-type 'single-float)
             (map 'vector
                  (lambda (part)
                    (float (with-standard-io-syntax
                             (let ((*read-eval* nil))
                               (read-from-string (string-trim '(#\Space) part))))
                           1f0))
                  (uiop:split-string inner :separator ","))))))
    ((and (vectorp x) (not (stringp x)))
     (map 'vector (lambda (e) (float e 1f0)) x))
    ((listp x)
     (map 'vector (lambda (e) (float e 1f0)) x))
    (t
     (error 'rag-protocol:rag-error
            :message (format nil "not a pgvector value: ~s" x)))))

(defun %encode-lisp (value)
  (with-standard-io-syntax
    (let ((*print-readably* t)
          (*print-pretty* nil)
          (*package* (find-package :cl)))
      (prin1-to-string value))))

(defun %decode-lisp (string)
  (when (and string (plusp (length string)))
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package :cl)))
        (read-from-string string)))))

(defun %encode-metadata (meta)
  (when meta
    (%encode-lisp meta)))

(defun %decode-metadata (string)
  (%decode-lisp string))

(defun %exec (store sql &optional params)
  (sql-protocol:execute (pgvector-store-connection store) sql params))

(defun %fetch (store sql &optional params)
  (sql-protocol:fetch (%exec store sql params)))

(defun %fetch-all (store sql &optional params)
  (sql-protocol:fetch-all (%exec store sql params)))

(defun %vector-sql-type (dimension)
  (if dimension
      (format nil "vector(~d)" dimension)
      "vector"))

(defun %parse-vector-type (type-string)
  "\"vector\" → NIL; \"vector(3)\" → 3."
  (when (and type-string (plusp (length type-string)))
    (let ((s (string-trim '(#\Space) type-string)))
      (cond
        ((string-equal s "vector") nil)
        ((and (>= (length s) 8)
              (string-equal (subseq s 0 7) "vector("))
         (parse-integer s :start 7 :junk-allowed t))
        (t nil)))))

(defun %probe-column-dim (store)
  (let* ((table (%table-name (pgvector-store-table store)))
         (row (%fetch store
                      (format nil
                              "SELECT format_type(atttypid, atttypmod) AS typ
FROM pg_attribute
WHERE attrelid = '~a'::regclass AND attname = 'embedding' AND NOT attisdropped"
                              table))))
    (when row
      (%parse-vector-type (getf row :typ)))))

(defun %probe-row-dim (store)
  (let* ((table (%table-name (pgvector-store-table store)))
         (row (%fetch store
                      (format nil
                              "SELECT vector_dims(embedding) AS dim FROM ~a
WHERE embedding IS NOT NULL LIMIT 1"
                              table))))
    (when row
      (let ((d (getf row :dim)))
        (when d (%as-integer d))))))

(defun %ensure-index (store)
  (let ((table (%table-name (pgvector-store-table store)))
        (dim (or (pgvector-store-dimension store) (%probe-column-dim store))))
    (when (and (pgvector-store-index-p store) dim)
      (%exec store
             (format nil
                     "CREATE INDEX IF NOT EXISTS ~a_embedding_hnsw
ON ~a USING hnsw (embedding vector_cosine_ops)"
                     table table)))))

(defun %lock-column-dimension (store dim)
  "Pin an unconstrained embedding column to VECTOR(DIM), then HNSW if requested."
  (let ((table (%table-name (pgvector-store-table store)))
        (current (%probe-column-dim store)))
    (cond
      ((null current)
       (%exec store
              (format nil
                      "ALTER TABLE ~a ALTER COLUMN embedding TYPE vector(~d)"
                      table dim)))
      ((/= current dim)
       (error 'rag-protocol:rag-dimension-mismatch
              :expected current
              :actual dim
              :message (format nil "table ~a dim ~d, store dim ~d"
                               table current dim))))
    (%ensure-index store)))

(defun ensure-pgvector-schema (store)
  (let ((table (%table-name (pgvector-store-table store)))
        (dim (pgvector-store-dimension store)))
    (%exec store "CREATE EXTENSION IF NOT EXISTS vector")
    (%exec store
           (format nil
                   "CREATE TABLE IF NOT EXISTS ~a (
  id TEXT PRIMARY KEY,
  document_id TEXT,
  text TEXT NOT NULL,
  embedding ~a NOT NULL,
  metadata TEXT)"
                   table
                   (%vector-sql-type dim)))
    (let ((probed (or (%probe-column-dim store) (%probe-row-dim store))))
      (cond
        ((and dim probed (/= dim probed))
         (error 'rag-protocol:rag-dimension-mismatch
                :expected probed
                :actual dim
                :message (format nil "store dim ~d, table ~a dim ~d"
                                 dim table probed)))
        ((and (null (pgvector-store-dimension store)) probed)
         (setf (pgvector-store-dimension store) probed))))
    (%ensure-index store)
    store))

(defun %connect-keys (&key host port database-name username password)
  (append (when host (list :host host))
          (when port (list :port (if (integerp port)
                                     port
                                     (parse-integer (princ-to-string port)))))
          (when database-name (list :database-name database-name))
          (when username (list :username username))
          (when password (list :password password))))

(defun make-pgvector-store (&key connection
                                 host
                                 port
                                 database-name
                                 username
                                 password
                                 (table "rag_chunks")
                                 dimension
                                 (index t)
                                 (ensure-schema t))
  (let ((table (%table-name table)))
    (when (and dimension (not (and (integerp dimension) (plusp dimension))))
      (error 'rag-protocol:rag-error
             :message (format nil "invalid dimension ~s" dimension)))
    (let* ((owns (null connection))
           (conn (or connection
                     (apply #'sql-protocol:connect
                            :driver :postgres
                            (%connect-keys :host host
                                           :port port
                                           :database-name database-name
                                           :username username
                                           :password password))))
           (store (make-instance 'pgvector-store
                                 :connection conn
                                 :owns-connection owns
                                 :table table
                                 :dimension dimension
                                 :index-p index)))
      (when ensure-schema
        (ensure-pgvector-schema store))
      store)))

(defun use-pgvector-store (&rest args &key &allow-other-keys)
  (setf rag-protocol:*rag-store* (apply #'make-pgvector-store args)))

(defun close-pgvector-store (store)
  (when (and (pgvector-store-owns-connection store)
             (pgvector-store-connection store))
    (ignore-errors (sql-protocol:disconnect (pgvector-store-connection store)))
    (setf (pgvector-store-connection store) nil
          (pgvector-store-owns-connection store) nil))
  store)

(defun %accepted-embedding (store chunk)
  (let ((emb (rag-protocol:rag-chunk-embedding chunk)))
    (unless (and emb (plusp (length emb)))
      (error 'rag-protocol:rag-error
             :message (format nil "chunk ~s has no embedding"
                              (rag-protocol:rag-chunk-id chunk))))
    (tagbody
     :retry
       (let ((dim (length emb)))
         (cond
           ((null (pgvector-store-dimension store))
            (setf (pgvector-store-dimension store) dim)
            (%lock-column-dimension store dim)
            (return-from %accepted-embedding emb))
           ((= dim (pgvector-store-dimension store))
            (return-from %accepted-embedding emb))
           (t
            (restart-case
                (error 'rag-protocol:rag-dimension-mismatch
                       :expected (pgvector-store-dimension store)
                       :actual dim
                       :id (rag-protocol:rag-chunk-id chunk)
                       :message (format nil "chunk ~s: expected dim ~d, got ~d"
                                        (rag-protocol:rag-chunk-id chunk)
                                        (pgvector-store-dimension store)
                                        dim))
              (continue ()
                :report "Skip this chunk"
                (return-from %accepted-embedding nil))
              (use-value (value)
                :report "Use a supplied embedding vector"
                (setf emb value
                      (rag-protocol:rag-chunk-embedding chunk) value)
                (go :retry)))))))))

(defun %chunk-from-row (row)
  (rag-protocol:make-rag-chunk
   :id (getf row :id)
   :document-id (getf row :document_id)
   :text (or (getf row :text) "")
   :embedding (decode-pgvector (getf row :embedding))
   :metadata (%decode-metadata (getf row :metadata))))

(defun %hit-from-row (row)
  (rag-protocol:make-rag-hit
   :chunk (%chunk-from-row row)
   :score (%as-single-float (or (getf row :score) 0))))

(defmethod rag-protocol:upsert ((store pgvector-store) chunks)
  (let ((table (%table-name (pgvector-store-table store))))
    (sql-protocol:with-transaction ((pgvector-store-connection store))
      (dolist (ch (%as-list chunks))
        (let ((emb (%accepted-embedding store ch)))
          (when emb
            (unless (rag-protocol:rag-chunk-id ch)
              (error 'rag-protocol:rag-error :message "chunk id required for upsert"))
            (%exec store
                   (format nil
                           "INSERT INTO ~a (id, document_id, text, embedding, metadata)
VALUES (?, ?, ?, ?::vector, ?)
ON CONFLICT (id) DO UPDATE SET
  document_id = EXCLUDED.document_id,
  text = EXCLUDED.text,
  embedding = EXCLUDED.embedding,
  metadata = EXCLUDED.metadata"
                           table)
                   (list (rag-protocol:rag-chunk-id ch)
                         (rag-protocol:rag-chunk-document-id ch)
                         (rag-protocol:rag-chunk-text ch)
                         (encode-pgvector emb)
                         (%encode-metadata (rag-protocol:rag-chunk-metadata ch)))))))))
  store)

(defmethod rag-protocol:delete-ids ((store pgvector-store) ids)
  (let* ((table (%table-name (pgvector-store-table store)))
         (ids (%as-list ids))
         (missing '())
         (deleted '()))
    (sql-protocol:with-transaction ((pgvector-store-connection store))
      (dolist (id ids)
        (if (%fetch store (format nil "SELECT id FROM ~a WHERE id = ?" table) (list id))
            (progn
              (%exec store (format nil "DELETE FROM ~a WHERE id = ?" table) (list id))
              (push id deleted))
            (push id missing))))
    (setf missing (nreverse missing)
          deleted (nreverse deleted))
    (when missing
      (restart-case
          (error 'rag-protocol:rag-not-found
                 :ids missing
                 :message (format nil "unknown chunk ids: ~s" missing))
        (continue ()
          :report "Skip missing ids"
          (return-from rag-protocol:delete-ids deleted))
        (use-value (value)
          :report "Return a supplied value"
          (return-from rag-protocol:delete-ids value))))
    deleted))

(defmethod rag-protocol:query-store ((store pgvector-store) query &key top-k filter)
  (let* ((vec (rag-protocol:query-vector query))
         (table (%table-name (pgvector-store-table store)))
         (k (or top-k 5))
         (encoded (encode-pgvector vec)))
    (when (and (pgvector-store-dimension store)
               (/= (length vec) (pgvector-store-dimension store)))
      (error 'rag-protocol:rag-dimension-mismatch
             :expected (pgvector-store-dimension store)
             :actual (length vec)
             :message (format nil "query dim ~d, store dim ~d"
                              (length vec) (pgvector-store-dimension store))))
    (let ((sql (format nil
                       "SELECT id, document_id, text, embedding::text, metadata,
       1 - (embedding <=> ?::vector) AS score
FROM ~a
ORDER BY embedding <=> ?::vector~a"
                       table
                       (if filter "" " LIMIT ?")))
          (params (if filter
                      (list encoded encoded)
                      (list encoded encoded k))))
      (let ((hits (loop for row in (%fetch-all store sql params)
                        for chunk = (%chunk-from-row row)
                        when (or (null filter) (funcall filter chunk))
                          collect (%hit-from-row row))))
        (rag-protocol:rerank (rag-protocol:make-identity-reranker)
                             query hits :top-k k)))))
