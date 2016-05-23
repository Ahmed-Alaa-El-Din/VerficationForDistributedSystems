(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(load (compile-file "verifier.asd"))
(asdf::load-system 'verifier :force t)

(declaim (optimize (sb-cover:store-coverage-data 0)))

(load (compile-file "verifier-test.asd"))
(asdf::load-system 'verifier-test :force t)

(asdf::test-system :verifier)


(sb-cover:report "/tmp/verifier/cov/test/")
