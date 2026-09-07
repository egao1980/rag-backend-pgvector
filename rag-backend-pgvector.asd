(defsystem "rag-backend-pgvector"
  :version "0.1.0"
  :description "pgvector ANN store for rag-protocol (cosine <=>, text vector wire)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("rag-protocol" "sql-protocol")
  :properties
  (:cl-repo
   (:ci (:with ("sql-backend-postgres" "cl-postgres")
         :load-before-test ("sql-backend-postgres"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "rag-backend-pgvector/tests"))))

(defsystem "rag-backend-pgvector/tests"
  :depends-on ("rag-backend-pgvector" "sql-backend-postgres" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
