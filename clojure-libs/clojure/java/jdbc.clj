;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  jdbc.clj
;;
;;  A Clojure interface to sql databases via jdbc
;;
;;  scgilardi (gmail)
;;  Created 2 April 2008
;;
;;  seancorfield (gmail)
;;  Migrated from clojure.contrib.sql 17 April 2011

(ns
  ^{
    :author "Stephen C. Gilardi, Sean Corfield",
    :doc "A Clojure interface to SQL databases via JDBC

clojure.java.jdbc provides a simple abstraction for CRUD (create, read,
update, delete) operations on a SQL database, along with basic transaction
support. Basic DDL operations are also supported (create table, drop table,
access to table metadata).

Maps are used to represent records, making it easy to store and retrieve
data. Results can be processed using any standard sequence operations.

For most operations, Java's PreparedStatement is used so your SQL and
parameters can be represented as simple vectors where the first element
is the SQL string, with ? for each parameter, and the remaining elements
are the parameter values to be substituted. In general, operations return
the number of rows affected, except for a single record insert where any
generated keys are returned (as a map)." }
  clojure.java.jdbc
  (:import [java.net URI]
           [java.sql BatchUpdateException DriverManager PreparedStatement ResultSet SQLException Statement]
           [java.util Hashtable Map Properties]
           [javax.naming InitialContext Name]
           [javax.sql DataSource])
  (:refer-clojure :exclude [resultset-seq])
  (:require [clojure.string :as str]
            [clojure.java.jdbc.sql :as sql]))

(def ^{:private true :dynamic true
       :doc "The default entity naming strategy is to do nothing."}
  *as-str* 
  identity)

(def ^{:private true :dynamic true
       :doc "The default keyword naming strategy is to lowercase the entity."}
  *as-key*
  str/lower-case)

(defn as-str
  "Given a naming strategy and a keyword, return the keyword as a
   string per that naming strategy. Given (a naming strategy and)
   a string, return it as-is.
   A keyword of the form :x.y is treated as keywords :x and :y,
   both are turned into strings via the naming strategy and then
   joined back together so :x.y might become `x`.`y` if the naming
   strategy quotes identifiers with `."
  [f x]
  (if (instance? clojure.lang.Named x)
    (let [n (name x)
          i (.indexOf n (int \.))]
      (if (= -1 i)
        (f n)
        (str/join "." (map f (.split n "\\.")))))
    (str x)))

(defn as-key
  "Given a naming strategy and a string, return the string as a
   keyword per that naming strategy. Given (a naming strategy and)
   a keyword, return it as-is."
  [f x]
  (if (instance? clojure.lang.Named x)
    x
    (keyword (f (str x)))))

(defn as-identifier
  "Given a keyword, convert it to a string using the current naming
   strategy.
   Given a string, return it as-is."
  ([x] (as-identifier x *as-str*))
  ([x f-entity] (as-str f-entity x)))

(defn as-keyword
  "Given an entity name (string), convert it to a keyword using the
   current naming strategy.
   Given a keyword, return it as-is."
  ([x] (as-keyword x *as-key*))
  ([x f-keyword] (as-key f-keyword x)))

(defn- ^Properties as-properties
  "Convert any seq of pairs to a java.utils.Properties instance.
   Uses as-str to convert both keys and values into strings."
  [m]
  (let [p (Properties.)]
    (doseq [[k v] m]
      (.setProperty p (as-str identity k) (as-str identity v)))
    p))

(def ^{:private true :dynamic true} *db* {:connection nil :level 0})

(def ^{:private true :doc "Map of classnames to subprotocols"} classnames
  {"postgresql"     "org.postgresql.Driver"
   "mysql"          "com.mysql.jdbc.Driver"
   "sqlserver"      "com.microsoft.sqlserver.jdbc.SQLServerDriver"
   "jtds:sqlserver" "net.sourceforge.jtds.jdbc.Driver"
   "derby"          "org.apache.derby.jdbc.EmbeddedDriver"
   "hsqldb"         "org.hsqldb.jdbcDriver"
   "sqlite"         "org.sqlite.JDBC"})

(def ^{:private true :doc "Map of schemes to subprotocols"} subprotocols
  {"postgres" "postgresql"})

(defn- parse-properties-uri [^URI uri]
  (let [host (.getHost uri)
        port (if (pos? (.getPort uri)) (.getPort uri))
        path (.getPath uri)
        scheme (.getScheme uri)]
    (merge
     {:subname (if port
                 (str "//" host ":" port path)
                 (str "//" host path))
      :subprotocol (subprotocols scheme scheme)}
     (if-let [user-info (.getUserInfo uri)]
             {:user (first (str/split user-info #":"))
              :password (second (str/split user-info #":"))}))))

(defn- strip-jdbc [^String spec]
  (if (.startsWith spec "jdbc:")
    (.substring spec 5)
    spec))

(defn get-connection
  "Creates a connection to a database. db-spec is a map containing connection
  parameters. db-spec is a map containing values for one of the following
  parameter sets:

  Existing Connection:
    :connection  (required) an existing open connection that can be used
                 but cannot be closed (only the parent connection can be closed)

  Factory:
    :factory     (required) a function of one argument, a map of params
    (others)     (optional) passed to the factory function in a map

  DriverManager:
    :subprotocol (required) a String, the jdbc subprotocol
    :subname     (required) a String, the jdbc subname
    :classname   (optional) a String, the jdbc driver class name
    (others)     (optional) passed to the driver as properties.

  DataSource:
    :datasource  (required) a javax.sql.DataSource
    :username    (optional) a String
    :password    (optional) a String, required if :username is supplied

  JNDI:
    :name        (required) a String or javax.naming.Name
    :environment (optional) a java.util.Map

  Raw:
    :connection-uri (required) a String
                 Passed directly to DriverManager/getConnection

  URI:
    Parsed JDBC connection string - see below
  
  String:
    subprotocol://user:password@host:post/subname
                 An optional prefix of jdbc: is allowed."
  [{:keys [connection
           factory
           connection-uri
           classname subprotocol subname
           datasource username password
           name environment]
    :as db-spec}]
  (cond
   connection
   connection
   
   (instance? URI db-spec)
   (get-connection (parse-properties-uri db-spec))
   
   (string? db-spec)
   (get-connection (URI. (strip-jdbc db-spec)))
   
   factory
   (factory (dissoc db-spec :factory))
   
   connection-uri
   (DriverManager/getConnection connection-uri)
   
   (and subprotocol subname)
   (let [url (format "jdbc:%s:%s" subprotocol subname)
         etc (dissoc db-spec :classname :subprotocol :subname)
         classname (or classname (classnames subprotocol))]
     (clojure.lang.RT/loadClassForName classname)
     (DriverManager/getConnection url (as-properties etc)))
   
   (and datasource username password)
   (.getConnection ^DataSource datasource ^String username ^String password)
   
   datasource
   (.getConnection ^DataSource datasource)
   
   name
   (let [env (and environment (Hashtable. ^Map environment))
         context (InitialContext. env)
         ^DataSource datasource (.lookup context ^String name)]
     (.getConnection datasource))
   
   :else
   (let [^String msg (format "db-spec %s is missing a required parameter" db-spec)]
     (throw (IllegalArgumentException. msg)))))

(defn- make-name-unique
  "Given a collection of column names and a new column name,
   return the new column name made unique, if necessary, by
   appending _N where N is some unique integer suffix."
  [cols col-name n]
  (let [suffixed-name (if (= n 1) col-name (str col-name "_" n))]
    (if (apply distinct? suffixed-name cols)
      suffixed-name
      (recur cols col-name (inc n)))))

(defn- make-cols-unique
  "Given a collection of column names, rename duplicates so
   that the result is a collection of unique column names."
  [cols]
  (if (or (empty? cols) (apply distinct? cols))
    cols
    (reduce (fn [unique-cols col-name] (conj unique-cols (make-name-unique unique-cols col-name 1))) []  cols)))

(defn resultset-seq
  "Creates and returns a lazy sequence of maps corresponding to
   the rows in the java.sql.ResultSet rs. Based on clojure.core/resultset-seq
   but it respects the current naming strategy. Duplicate column names are
   made unique by appending _N before applying the naming strategy (where
   N is a unique integer)."
  [^ResultSet rs & {:keys [identifiers]
                    :or {identifiers *as-key*}}]
    (let [rsmeta (.getMetaData rs)
          idxs (range 1 (inc (.getColumnCount rsmeta)))
          keys (->> idxs
                 (map (fn [^Integer i] (.getColumnLabel rsmeta i)))
                 make-cols-unique
                 (map (comp keyword identifiers)))
          row-values (fn [] (map (fn [^Integer i] (.getObject rs i)) idxs))
          ;; This used to use create-struct (on keys) and then struct to populate each row.
          ;; That had the side effect of preserving the order of columns in each row. As
          ;; part of JDBC-15, this was changed because structmaps are deprecated. We don't
          ;; want to switch to records so we're using regular maps instead. We no longer
          ;; guarantee column order in rows but using into {} should preserve order for up
          ;; to 16 columns (because it will use a PersistentArrayMap). If someone is relying
          ;; on the order-preserving behavior of structmaps, we can reconsider...
          rows (fn thisfn []
                 (when (.next rs)
                   (cons (zipmap keys (row-values)) (lazy-seq (thisfn)))))]
      (rows)))

(defn as-quoted-str
  "Given a quoting pattern - either a single character or a vector pair of
   characters - and a string, return the quoted string:
     (as-quoted-str X foo) will return XfooX
     (as-quoted-str [A B] foo) will return AfooB"
  [q x]
  (if (vector? q)
    (str (first q) x (last q))
    (str q x q)))

(defn as-named-identifier
  "Given a naming strategy and a keyword, return the keyword as a string using the 
   entity naming strategy.
   Given a naming strategy and a string, return the string as-is.
   The naming strategy should either be a function (the entity naming strategy) or 
   a map containing :entity and/or :keyword keys which provide the entity naming
   strategy and/or keyword naming strategy respectively."
  [naming-strategy x]
  (as-identifier x (if (map? naming-strategy) (or (:entity naming-strategy) identity) naming-strategy)))

(defn as-named-keyword
  "Given a naming strategy and a string, return the string as a keyword using the 
   keyword naming strategy.
   Given a naming strategy and a keyword, return the keyword as-is.
   The naming strategy should either be a function (the entity naming strategy) or 
   a map containing :entity and/or :keyword keys which provide the entity naming
   strategy and/or keyword naming strategy respectively.
   Note that providing a single function will cause the default keyword naming
   strategy to be used!"
  [naming-strategy x]
  (as-keyword x (if (and (map? naming-strategy) (:keyword naming-strategy)) (:keyword naming-strategy) str/lower-case)))

(defn as-quoted-identifier
  "Given a quote pattern - either a single character or a pair of characters in
   a vector - and a keyword, return the keyword as a string using a simple
   quoting naming strategy.
   Given a qote pattern and a string, return the string as-is.
     (as-quoted-identifier X :name) will return XnameX as a string.
     (as-quoted-identifier [A B] :name) will return AnameB as a string."
  [q x]
  (as-identifier x (partial as-quoted-str q)))

(defmacro with-naming-strategy
  "Evaluates body in the context of a naming strategy.
   The naming strategy is either a function - the entity naming strategy - or
   a map containing :entity and/or :keyword keys which provide the entity naming
   strategy and/or the keyword naming strategy respectively. The default entity
   naming strategy is identity; the default keyword naming strategy is lower-case."
  [naming-strategy & body ]
  `(binding [*as-str* (if (map? ~naming-strategy) (or (:entity ~naming-strategy) identity) ~naming-strategy)
             *as-key* (if (map? ~naming-strategy) (or (:keyword ~naming-strategy) str/lower-case))] ~@body))

(defmacro with-quoted-identifiers
  "Evaluates body in the context of a simple quoting naming strategy."
  [q & body ]
  `(binding [*as-str* (partial as-quoted-str ~q)] ~@body))

(defn- execute-batch
  "Executes a batch of SQL commands and returns a sequence of update counts.
   (-2) indicates a single operation operating on an unknown number of rows.
   Specifically, Oracle returns that and we must call getUpdateCount() to get
   the actual number of rows affected. In general, operations return an array
   of update counts, so this may not be a general solution for Oracle..."
  [^Statement stmt]
  (let [result (.executeBatch stmt)]
    (if (and (= 1 (count result)) (= -2 (first result)))
      (list (.getUpdateCount stmt))
      (seq result))))

(def ^{:private true
       :doc "Map friendly :concurrency values to ResultSet constants."} 
  result-set-concurrency
  {:read-only ResultSet/CONCUR_READ_ONLY
   :updatable ResultSet/CONCUR_UPDATABLE})

(def ^{:private true
       :doc "Map friendly :cursors values to ResultSet constants."} 
  result-set-holdability
  {:hold ResultSet/HOLD_CURSORS_OVER_COMMIT
   :close ResultSet/CLOSE_CURSORS_AT_COMMIT})

(def ^{:private true
       :doc "Map friendly :type values to ResultSet constants."} 
  result-set-type
  {:forward-only ResultSet/TYPE_FORWARD_ONLY
   :scroll-insensitive ResultSet/TYPE_SCROLL_INSENSITIVE
   :scroll-sensitive ResultSet/TYPE_SCROLL_SENSITIVE})

(defn prepare-statement
  "Create a prepared statement from a connection, a SQL string and an
   optional list of parameters:
     :return-keys true | false - default false
     :result-type :forward-only | :scroll-insensitive | :scroll-sensitive
     :concurrency :read-only | :updatable
     :fetch-size n
     :max-rows n"
  [^java.sql.Connection con ^String sql & {:keys [return-keys result-type concurrency cursors fetch-size max-rows]}]
  (let [^PreparedStatement stmt (cond
                                  return-keys (try
                                                (.prepareStatement con sql java.sql.Statement/RETURN_GENERATED_KEYS)
                                                (catch Exception _
                                                  ;; assume it is unsupported and try basic PreparedStatement:
                                                  (.prepareStatement con sql)))
                                  (and result-type concurrency) (if cursors
                                                                  (.prepareStatement con sql 
                                                                                     (result-type result-set-type)
                                                                                     (concurrency result-set-concurrency)
                                                                                     (cursors result-set-holdability))
                                                                  (.prepareStatement con sql 
                                                                                     (result-type result-set-type)
                                                                                     (concurrency result-set-concurrency)))
                                  :else (.prepareStatement con sql))]
    (when fetch-size (.setFetchSize stmt fetch-size))
    (when max-rows (.setMaxRows stmt max-rows))
    stmt))

(defn- set-parameters
  "Add the parameters to the given statement."
  [^PreparedStatement stmt params]
  (dorun
    (map-indexed
      (fn [ix value]
        (.setObject stmt (inc ix) value))
      params)))

(defn create-table-ddl
  "Given a table name and column specs with an optional table-spec
   return the DDL string for creating a table based on that."
  [name & specs]
  (let [split-specs (partition-by #(= :table-spec %) specs)
        col-specs (first split-specs)
        table-spec (first (second (rest split-specs)))
        table-spec-str (or (and table-spec (str " " table-spec)) "")
        specs-to-string (fn [specs]
                          (apply str
                                 (map as-identifier
                                      (apply concat
                                             (interpose [", "]
                                                        (map (partial interpose " ") specs))))))]
    (format "CREATE TABLE %s (%s)%s"
            (as-identifier name)
            (specs-to-string col-specs)
            table-spec-str)))

(defn print-sql-exception
  "Prints the contents of an SQLException to *out*"
  [^SQLException exception]
  (let [^Class exception-class (class exception)]
    (println
      (format (str "%s:" \newline
                   " Message: %s" \newline
                   " SQLState: %s" \newline
                   " Error Code: %d")
              (.getSimpleName exception-class)
              (.getMessage exception)
              (.getSQLState exception)
              (.getErrorCode exception)))))

(defn print-sql-exception-chain
  "Prints a chain of SQLExceptions to *out*"
  [^SQLException exception]
  (loop [e exception]
    (when e
      (print-sql-exception e)
      (recur (.getNextException e)))))

(def ^{:private true} special-counts
  {Statement/EXECUTE_FAILED "EXECUTE_FAILED"
   Statement/SUCCESS_NO_INFO "SUCCESS_NO_INFO"})

(defn print-update-counts
  "Prints the update counts from a BatchUpdateException to *out*"
  [^BatchUpdateException exception]
  (println "Update counts:")
  (dorun 
    (map-indexed 
      (fn [index count] 
        (println (format " Statement %d: %s"
                         index
                         (get special-counts count count)))) 
      (.getUpdateCounts exception))))

;; java.jdbc pieces rewritten to not use dynamic bindings

(defn db-find-connection
  "Returns the current database connection (or nil if there is none)"
  ^java.sql.Connection [db]
  (:connection db))

(defn db-connection
  "Returns the current database connection (or throws if there is none)"
  ^java.sql.Connection [db]
  (or (db-find-connection db)
      (throw (Exception. "no current database connection"))))

(defn- db-rollback
  "Accessor for the rollback flag on the current connection"
  ([db]
     (deref (:rollback db)))
  ([db val]
     (swap! (:rollback db) (fn [_] val))))

(defn- throw-non-rte
  "This ugliness makes it easier to catch SQLException objects
  rather than something wrapped in a RuntimeException which
  can really obscure your code when working with JDBC from
  Clojure... :("
  [^Throwable ex]
  (cond (instance? java.sql.SQLException ex) (throw ex)
        (and (instance? RuntimeException ex) (.getCause ex)) (throw-non-rte (.getCause ex))
        :else (throw ex)))

(defn db-set-rollback-only
  "Marks the outermost transaction such that it will rollback rather than
  commit when complete"
  [db]
  (db-rollback db true))

(defn db-is-rollback-only
  "Returns true if the outermost transaction will rollback rather than
  commit when complete"
  [db]
  (db-rollback db))

(defn db-transaction*
  "Evaluates func as a transaction on the open database connection. Any
  nested transactions are absorbed into the outermost transaction. By
  default, all database updates are committed together as a group after
  evaluating the outermost body, or rolled back on any uncaught
  exception. If rollback is set within scope of the outermost transaction,
  the entire transaction will be rolled back rather than committed when
  complete."
  [db func]
  (let [nested-db (update-in db [:level] inc)]
    (if (= (:level nested-db) 1)
      (let [^java.sql.Connection con (get-connection nested-db)
            nested-db (assoc nested-db :connection con)
            auto-commit (.getAutoCommit con)]
        (io!
         (.setAutoCommit con false)
         (try
           (let [result (func nested-db)]
             (if (db-rollback nested-db)
               (.rollback con)
               (.commit con))
             result)
           (catch Exception e
             (.rollback con)
             (throw-non-rte e))
           (finally
            (db-rollback nested-db false)
            (.setAutoCommit con auto-commit)))))
      (try
        (func nested-db)
        (catch Exception e
          (throw-non-rte e))))))

(defmacro db-transaction
  "Evaluates body in the context of a transaction on the specified database connection.
  The binding provides the dataabase connection for the transaction and the name to which
  that is bound for evaluation of the body.
  See db-transaction* for more details."
  [binding & body]
  `(db-transaction* (let [db# ~(second binding)]
                      (assoc db# :level (or (:level db#) 0) :rollback (or (:rollback db#) (atom false))))
                    (fn [~(first binding)]
                      ~@body)))

(defn db-do-commands
  "Executes SQL commands on the specified database connection. Wraps the commands
  in a transaction if transaction? is true."
  [db transaction? & commands]
  (with-open [^Statement stmt (let [^java.sql.Connection con (get-connection db)] (.createStatement con))]
    (doseq [^String cmd commands]
      (.addBatch stmt cmd))
    (if transaction?
      (db-transaction [db (assoc db :connection (.getConnection stmt))] (execute-batch stmt))
      (try
        (execute-batch stmt)
        (catch Exception e
          (throw-non-rte e))))))

(defn db-do-prepared-return-keys
  "Executes an (optionally parameterized) SQL prepared statement on the
  open database connection. The param-group is a seq of values for all of
  the parameters.
  Return the generated keys for the (single) update/insert."
  [db transaction? sql param-group]
  (with-open [^PreparedStatement stmt (prepare-statement (get-connection db) sql :return-keys true)]
    (set-parameters stmt param-group)
    (letfn [(exec-and-return-keys []
              (let [counts (.executeUpdate stmt)]
                (try
                  (let [rs (.getGeneratedKeys stmt)
                        result (first (resultset-seq rs))]
                    ;; sqlite (and maybe others?) requires
                    ;; record set to be closed
                    (.close rs)
                    result)
                  (catch Exception _
                    ;; assume generated keys is unsupported and return counts instead: 
                    counts))))]
      (if transaction?
        (db-transaction [db (assoc db :connection (.getConnection stmt))] (exec-and-return-keys))
        (try
          (exec-and-return-keys)
          (catch Exception e
            (throw-non-rte e)))))))

(defn db-do-prepared
  "Executes an (optionally parameterized) SQL prepared statement on the
  open database connection. Each param-group is a seq of values for all of
  the parameters.
  Return a seq of update counts (one count for each param-group)."
  [db transaction? sql & param-groups]
  (with-open [^PreparedStatement stmt (prepare-statement (get-connection db) sql)]
    (if (empty? param-groups)
      (if transaction?
        (db-transaction [db (assoc db :connection (.getConnection stmt))] (vector (.executeUpdate stmt)))
        (try
          (vector (.executeUpdate stmt))
          (catch Exception e
            (throw-non-rte e))))
      (do
        (doseq [param-group param-groups]
          (set-parameters stmt param-group)
          (.addBatch stmt))
        (if transaction?
          (db-transaction [db (assoc db :connection (.getConnection stmt))] (execute-batch stmt))
          (try
            (execute-batch stmt)
            (catch Exception e
              (throw-non-rte e))))))))

(defn db-with-query-results*
  "Executes a query, then evaluates func passing in a seq of the results as
  an argument. The first argument is a vector containing either:
    [sql & params] - a SQL query, followed by any parameters it needs
    [stmt & params] - a PreparedStatement, followed by any parameters it needs
                      (the PreparedStatement already contains the SQL query)
    [options sql & params] - options and a SQL query for creating a
                      PreparedStatement, follwed by any parameters it needs
  See prepare-statement for supported options."
  [db sql-params func identifiers]
  (when-not (vector? sql-params)
    (let [^Class sql-params-class (class sql-params)
          ^String msg (format "\"%s\" expected %s %s, found %s %s"
                              "sql-params"
                              "vector"
                              "[sql param*]"
                              (.getName sql-params-class)
                              (pr-str sql-params))] 
      (throw (IllegalArgumentException. msg))))
  (let [special (first sql-params)
        sql-is-first (string? special)
        options-are-first (map? special)
        sql (cond sql-is-first special 
                  options-are-first (second sql-params))
        params (vec (cond sql-is-first (rest sql-params)
                          options-are-first (rest (rest sql-params))
                          :else (rest sql-params)))
        prepare-args (when (map? special) (flatten (seq special)))]
    (with-open [^PreparedStatement stmt (if (instance? PreparedStatement special)
                                          special
                                          (apply prepare-statement (get-connection db) sql prepare-args))]
      (set-parameters stmt params)
      (with-open [rset (.executeQuery stmt)]
        (func (resultset-seq rset :identifiers identifiers))))))

;; top-level API for actual SQL operations

(defn query
  "Given a database connection and a vector containing SQL and optional parameters,
  perform a simple database query. The optional keyword arguments specify how to
  construct the result set:
    :result-set-fn - applied to the entire result set, default doall
    :row-fn - applied to each row as the result set is constructed, default identity
    :identifiers - applied to each column name in the result set, default lower-case"
  [db sql-params & {:keys [result-set-fn row-fn identifiers]
                    :or {result-set-fn doall row-fn identity identifiers sql/lower-case}}]
  (if-let [con (:connection db)]
    (db-with-query-results*
      db
      (vec sql-params)
      (fn [rs]
        (result-set-fn (map row-fn rs)))
      identifiers)
    (with-open [con (get-connection db)]
      (db-with-query-results*
        (assoc db :connection con)
        (vec sql-params)
        (fn [rs]
          (result-set-fn (map row-fn rs)))
        identifiers))))

(defn execute!
  "Given a database connection and a vector containing SQL and optional parameters,
  perform a general (non-select) SQL operation. The optional keyword argument specifies
  whether to run the operation in a transaction or not (default true)."
  [db sql-params & {:keys [transaction?]
                    :or {transaction? true}}]
  (if-let [con (:connection db)]
    (db-do-prepared db
                    transaction?
                    (first sql-params)
                    (rest sql-params))
    (with-open [con (get-connection db)]
      (db-do-prepared (assoc db :connection con)
                      transaction?
                      (first sql-params)
                      (rest sql-params)))))

(defn delete!
  "Given a database connection, a table name and a where clause of columns to match,
  perform a delete. The optional keyword arguments specify how to transform
  column names in the map (default 'as-is') and whether to run the delete in
  a transaction (default true).
  Example:
    (delete! db :person {:zip 94546})
  is equivalent to:
    (execute! db [\"DELETE FROM person WHERE zip = ?\" 94546])"
  [db table where-clause & {:keys [entities transaction?]
                            :or {entities sql/as-is transaction? true}}]
  (execute! db
            (sql/delete table where-clause :entities entities)
            :transaction? transaction?))

(defn insert!
  "Given a database connection, a table name and either maps representing rows or
  a list of column names followed by lists of column values, perform an insert.
  Currently the insert is always run in a transaction."
  [db table & maps-or-cols-and-values-etc]
  (let [stmts (apply sql/insert table maps-or-cols-and-values-etc)
        transaction? true]
    (if-let [con (:connection db)]
      (if (string? (first stmts))
          (apply db-do-prepared
                 db
                 transaction?
                 (first stmts)
                 (rest stmts))
          (doall (map (fn [row]
                        (let [result (db-do-prepared-return-keys
                                      db
                                      ;; bad idea - this is nested 
                                      transaction?
                                      (first row)
                                      (rest row))]
                          result))
                      stmts)))
      (with-open [con (get-connection db)]
        (if (string? (first stmts))
          (apply db-do-prepared
                 (assoc db :connection con)
                 transaction?
                 (first stmts)
                 (rest stmts))
          (doall (map (fn [row]
                        (let [result (db-do-prepared-return-keys
                                      (assoc db :connection con)
                                      ;; bad idea - this is nested
                                      transaction?
                                      (first row)
                                      (rest row))]
                          result))
                      stmts)))))))

(defn update!
  "Given a database connection, a table name, a map of column values to set and a
  where clause of columns to match, perform an update. The optional keyword arguments
  specify how column names (in the set / match maps) should be transformed (default
  'as-is') and whether to run the update in a transaction (default true).
  Example:
    (update! db :person {:zip 94540} (where {:zip 94546}))
  is equivalent to:
    (execute! db [\"UPDATE person SET zip = ? WHERE zip = ?\" 94540 94546])"
  [db table set-map where-clause & {:keys [entities transaction?]
                                    :or {entities sql/as-is transaction? true}}]
  (execute! db
            (sql/update table set-map where-clause :entities entities)
            :transaction? transaction?))

;; original API mostly rewritten in terms of new API primarily without dynamic binding

(defn ^{:doc "Returns the current database connection (or nil if there is none)"
        :deprecated "0.3.0"}
  find-connection
  ^java.sql.Connection []
  (db-find-connection *db*))

(defn ^{:doc "Returns the current database connection (or throws if there is none)"
        :deprecated "0.3.0"}
  connection
  ^java.sql.Connection []
  (db-connection *db*))

(defn ^{:doc "Evaluates func in the context of a new connection to a database then
              closes the connection."
        :deprecated "0.3.0"}
  with-connection*
  [db-spec func]
  (with-open [^java.sql.Connection con (get-connection db-spec)]
    (binding [*db* (assoc *db* :connection con :level 0 :rollback (atom false))]
      (func))))

(defmacro ^{:doc "Evaluates body in the context of a new connection to a database then
                  closes the connection."
            :deprecated "0.3.0"}
  with-connection
  [db-spec & body]
  `(with-connection* ~db-spec (fn [] ~@body)))

(defn transaction*
  ^{:doc "Evaluates func as a transaction on the open database connection. Any
          nested transactions are absorbed into the outermost transaction. By
          default, all database updates are committed together as a group after
          evaluating the outermost body, or rolled back on any uncaught
          exception. If rollback is set within scope of the outermost transaction,
          the entire transaction will be rolled back rather than committed when
          complete."
    :deprecated "0.3.0"}
  [func]
  (binding [*db* (update-in *db* [:level] inc)]
    (if (= (:level *db*) 1)
      (let [^java.sql.Connection con (get-connection *db*)
            auto-commit (.getAutoCommit con)]
        (io!
         (.setAutoCommit con false)
         (try
           (let [result (func)]
             (if (db-rollback *db*)
               (.rollback con)
               (.commit con))
             result)
           (catch Exception e
             (.rollback con)
             (throw-non-rte e))
           (finally
            (db-rollback *db* false)
            (.setAutoCommit con auto-commit)))))
      (try
        (func)
        (catch Exception e
          (throw-non-rte e))))))

(defmacro
  ^{:doc "Evaluates body as a transaction on the open database connection. Any
          nested transactions are absorbed into the outermost transaction. By
          default, all database updates are committed together as a group after
          evaluating the outermost body, or rolled back on any uncaught
          exception. If set-rollback-only is called within scope of the outermost
          transaction, the entire transaction will be rolled back rather than
          committed when complete."
    :deprecated "0.3.0"}
  transaction
  [& body]
  `(transaction* (fn [] ~@body)))

(defn
  ^{:doc "Marks the outermost transaction such that it will rollback rather than
          commit when complete"
    :deprecated "0.3.0"}
  set-rollback-only
  []
  (db-set-rollback-only *db*))

(defn
  ^{:doc "Returns true if the outermost transaction will rollback rather than
          commit when complete"
    :deprecated "0.3.0"}
  is-rollback-only
  []
  (db-is-rollback-only *db*))

(defn
  ^{:doc "Executes SQL commands on the open database connection."
    :deprecated "0.3.0"}
  do-commands
  [& commands]
  (apply db-do-commands *db* true commands))

(defn
  ^{:doc "Executes an (optionally parameterized) SQL prepared statement on the
          open database connection. Each param-group is a seq of values for all of
          the parameters.
          Return a seq of update counts (one count for each param-group)."
    :deprecated "0.3.0"}
  do-prepared
  [sql & param-groups]
  (apply db-do-prepared *db* true sql param-groups))

(defn create-table
  "Creates a table on the open database connection given a table name and
  specs. Each spec is either a column spec: a vector containing a column
  name and optionally a type and other constraints, or a table-level
  constraint: a vector containing words that express the constraint. An
  optional suffix to the CREATE TABLE DDL describing table attributes may
  by provided as :table-spec {table-attributes-string}. All words used to
  describe the table may be supplied as strings or keywords."
  ;; technically deprecated but we don't yet have a replacement
  [name & specs]
  (do-commands (apply create-table-ddl name specs)))

(defn drop-table
  "Drops a table on the open database connection given its name, a string
  or keyword"
  ;; technically deprecated but we don't yet have a replacement
  [name]
  (do-commands
    (format "DROP TABLE %s" (as-identifier name))))

(defn
  ^{:doc "Executes an (optionally parameterized) SQL prepared statement on the
          open database connection. The param-group is a seq of values for all of
          the parameters.
          Return the generated keys for the (single) update/insert."
    :deprecated "0.3.0"}
  do-prepared-return-keys
  [sql param-group]
  (db-do-prepared-return-keys *db* true sql param-group))

(defn
  ^{:doc "Inserts rows into a table with values for specified columns only.
          column-names is a vector of strings or keywords identifying columns. Each
          value-group is a vector containing a values for each column in
          order. When inserting complete rows (all columns), consider using
          insert-rows instead.
          If a single set of values is inserted, returns a map of the generated keys."
    :deprecated "0.3.0"}
  ;; technically not fully deprecated since the nil column-names case
  ;; is not (yet) supported in the new API... JDBC-45
  insert-values
  [table column-names & value-groups]
  (let [column-strs (map as-identifier column-names)
        n (count (first value-groups))
        return-keys (= 1 (count value-groups))
        prepared-statement (if return-keys do-prepared-return-keys do-prepared)
        template (apply str (interpose "," (repeat n "?")))
        columns (if (seq column-names)
                  (format "(%s)" (apply str (interpose "," column-strs)))
                  "")]
    (apply prepared-statement
           (format "INSERT INTO %s %s VALUES (%s)"
                   (as-identifier table) columns template)
           value-groups)))

(defn insert-rows
  "Inserts complete rows into a table. Each row is a vector of values for
  each of the table's columns in order.
  If a single row is inserted, returns a map of the generated keys."
  [table & rows]
  ;; will be deprecated after JDBC-45 is implemented
  (apply insert-values table nil rows))

(defn
  ^{:doc "Inserts records into a table. records are maps from strings or keywords
          (identifying columns) to values. Inserts the records one at a time.
          Returns a sequence of maps containing the generated keys for each record."
    :deprecated "0.3.0"}
  insert-records
  [table & records]
  (apply insert! *db* table records))

(defn
  ^{:doc "Inserts a single record into a table. A record is a map from strings or
          keywords (identifying columns) to values.
          Returns a map of the generated keys."
    :deprecated "0.3.0"}
  insert-record
  [table record]
  (first (insert! *db* table record)))

(defn
  ^{:doc "Deletes rows from a table. where-params is a vector containing a string
          providing the (optionally parameterized) selection criteria followed by
          values for any parameters."
    :deprecated "0.3.0"}
  delete-rows
  [table where-params]
  (delete! *db* table where-params))

(defn
  ^{:doc "Updates values on selected rows in a table. where-params is a vector
          containing a string providing the (optionally parameterized) selection
          criteria followed by values for any parameters. record is a map from
          strings or keywords (identifying columns) to updated values."
    :deprecated "0.3.0"}
  update-values
  [table where-params record]
  (update! *db* table record where-params))

(defn update-or-insert-values
  "Updates values on selected rows in a table, or inserts a new row when no
  existing row matches the selection criteria. where-params is a vector
  containing a string providing the (optionally parameterized) selection
  criteria followed by values for any parameters. record is a map from
  strings or keywords (identifying columns) to updated values."
  [table where-params record]
  (transaction
   (let [result (update-values table where-params record)]
     (if (zero? (first result))
       (insert-values table (keys record) (vals record))
       result))))

(defn
  ^{:doc "Executes a query, then evaluates func passing in a seq of the results as
          an argument. The first argument is a vector containing either:
            [sql & params] - a SQL query, followed by any parameters it needs
            [stmt & params] - a PreparedStatement, followed by any parameters it needs
                             (the PreparedStatement already contains the SQL query)
            [options sql & params] - options and a SQL query for creating a
                             PreparedStatement, follwed by any parameters it needs
          See prepare-statement for supported options."
    :deprecated "0.3.0"}
  with-query-results*
  [sql-params func]
  (when-not (vector? sql-params)
    (let [^Class sql-params-class (class sql-params)
          ^String msg (format "\"%s\" expected %s %s, found %s %s"
                              "sql-params"
                              "vector"
                              "[sql param*]"
                              (.getName sql-params-class)
                              (pr-str sql-params))] 
      (throw (IllegalArgumentException. msg))))
  (let [special (first sql-params)
        sql-is-first (string? special)
        options-are-first (map? special)
        sql (cond sql-is-first special 
                  options-are-first (second sql-params))
        params (vec (cond sql-is-first (rest sql-params)
                          options-are-first (rest (rest sql-params))
                          :else (rest sql-params)))
        prepare-args (when (map? special) (flatten (seq special)))]
    (with-open [^PreparedStatement stmt (if (instance? PreparedStatement special) special (apply prepare-statement (get-connection *db*) sql prepare-args))]
      (set-parameters stmt params)
      (with-open [rset (.executeQuery stmt)]
        (binding [*db* (assoc *db* :connection (.getConnection stmt))]
          (func (resultset-seq rset)))))))

(defmacro
  ^{:doc "Executes a query, then evaluates body with results bound to a seq of the
          results. sql-params is a vector containing either:
            [sql & params] - a SQL query, followed by any parameters it needs
            [stmt & params] - a PreparedStatement, followed by any parameters it needs
                              (the PreparedStatement already contains the SQL query)
            [options sql & params] - options and a SQL query for creating a
                              PreparedStatement, follwed by any parameters it needs
          See prepare-statement for supported options."
    :deprecated "0.3.0"}
  with-query-results
  [results sql-params & body]
  `(with-query-results* ~sql-params (fn [~results] ~@body)))
